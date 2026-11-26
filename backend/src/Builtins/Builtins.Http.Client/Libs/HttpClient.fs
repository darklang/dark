/// HTTP client builtins: one call, read two ways.
///
/// - `httpClientRequest` reads the whole response and returns a Response with
///   `body : Bytes`. A GET or HEAD is a read, handed back in flight
///   (`Interpreter.callReadsOnly`); any other method is a write, in program order.
/// - `httpClientStream` returns a Response whose `body : Stream<Bytes>` is
///   pulled a chunk at a time as it arrives; for large bodies, server-sent events, etc.
///
/// Both answer the one generic `Stdlib.HttpClient.Response<'body>`, with the
/// headers as the server sent them: names unchanged, values unchanged, a
/// repeated header as separate pairs.
///
/// A call is an exchange (`exchange`): the request, then the redirects it follows and the
/// retries it makes, under one deadline. Following and retrying are here rather than in Dark
/// middleware because both must look at a response, and a GET that waited for its response where
/// it was sent would no longer be in flight. Dark decides whether and how often
/// (`Stdlib.HttpClient.Middleware.followRedirects`, `retry`); each send is its own checked host
/// operation (`LibExecution.HostHttp`), so the grants and the network guard apply to every hop.
module Builtins.Http.Client.Libs.HttpClient

open System.Threading.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes
open LibExecution.Effects
module VT = ValueType
module RTE = RuntimeError
module NR = LibExecution.RuntimeTypes.NameResolution
module Bytes = LibExecution.Bytes
module Stream = LibExecution.Stream
module Host = LibExecution.Host
module PermissionCheck = LibExecution.PermissionCheck
module Interpreter = LibExecution.Interpreter

let responseOKType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.response ())
let responseErrorType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.requestError ())


module BadHeader =
  let toDT (err : HostTypes.HttpBadHeader) : Dval =
    let (caseName, fields) =
      match err with
      | HostTypes.HttpBadHeader.EmptyKey -> "EmptyKey", []
      | HostTypes.HttpBadHeader.InvalidContentType -> "InvalidContentType", []
      | HostTypes.HttpBadHeader.InvalidName -> "InvalidName", []
      | HostTypes.HttpBadHeader.InvalidValue -> "InvalidValue", []
    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.badHeader ())
    DEnum(typeName, typeName, [], caseName, fields)

module BadUrl =
  let toDT (err : HostTypes.HttpBadUrl) : Dval =
    let (caseName, fields) =
      match err with
      | HostTypes.HttpBadUrl.UnsupportedProtocol -> "UnsupportedProtocol", []
      | HostTypes.HttpBadUrl.InvalidHost -> "InvalidHost", []
      | HostTypes.HttpBadUrl.Loopback -> "Loopback", []
      | HostTypes.HttpBadUrl.PrivateNetwork -> "PrivateNetwork", []
      | HostTypes.HttpBadUrl.InvalidUri -> "InvalidUri", []
      | HostTypes.HttpBadUrl.InvalidRequest -> "InvalidRequest", []

    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.badUrlDetails ())
    DEnum(typeName, typeName, [], caseName, fields)

module NetworkTarget =
  let toDT (target : HostTypes.HttpNetworkTarget) : Dval =
    let caseName, host =
      match target with
      | HostTypes.HttpNetworkTarget.Destination host -> "Destination", host
      | HostTypes.HttpNetworkTarget.Proxy host -> "Proxy", host
    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.networkTarget ())
    DEnum(typeName, typeName, [], caseName, [ DString host ])

module NetworkFailure =
  let toDT (failure : HostTypes.HttpNetworkFailure) : Dval =
    let caseName, fields =
      match failure with
      | HostTypes.HttpNetworkFailure.DnsFailure target ->
        "DnsFailure", [ NetworkTarget.toDT target ]
      | HostTypes.HttpNetworkFailure.ConnectionFailure(target, detail) ->
        "ConnectionFailure",
        [ NetworkTarget.toDT target
          Dval.option KTString (detail |> Option.map DString) ]
      | HostTypes.HttpNetworkFailure.IoFailure(authority, detail) ->
        "IoFailure", [ DString authority; DString detail ]
    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.networkFailure ())
    DEnum(typeName, typeName, [], caseName, fields)

module RequestError =
  let toDT (err : HostTypes.HttpRequestError) : Dval =
    let (caseName, fields) =
      match err with
      | HostTypes.HttpRequestError.BadUrl details ->
        "BadUrl", [ BadUrl.toDT details ]
      | HostTypes.HttpRequestError.Timeout -> "Timeout", []
      | HostTypes.HttpRequestError.BadHeader err ->
        "BadHeader", [ BadHeader.toDT err ]
      | HostTypes.HttpRequestError.NetworkError failure ->
        "NetworkError", [ NetworkFailure.toDT failure ]
      | HostTypes.HttpRequestError.TlsError message ->
        "TlsError", [ DString message ]
      | HostTypes.HttpRequestError.BadMethod -> "BadMethod", []

    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.requestError ())
    DEnum(typeName, typeName, [], caseName, fields)


let headersType = TList(TTuple(TString, TString, []))

let private headersToDval (headers : List<string * string>) : Dval =
  headers
  |> List.map (fun (k, v) ->
    DTuple(
      DString(String.normalizeOutside k),
      DString(String.normalizeOutside v),
      []
    ))
  |> Dval.list (KTTuple(VT.string, VT.string, []))

/// The Dval header list, as written; the interpreter has checked its type.
/// Trimming and the empty-key check are Dark's (`Stdlib.HttpClient`); the host
/// refuses anything unsafe to send.
let private parseHeaders (reqHeaders : List<Dval>) : List<string * string> =
  reqHeaders
  |> List.map (fun item ->
    match item with
    | DTuple(DString k, DString v, []) -> (k, v)
    | _ -> Exception.raiseInternal "a header is not a pair of strings" [])

open LibExecution.Builtin.Shortcuts


/// `Ok` and `Error` of a `Result<Response<'body>, RequestError>`, for a body of `bodyType`.
let private resultOf (bodyType : ValueType) =
  let ok = KTCustomType(responseOKType (), [ bodyType ])
  let error = KTCustomType(responseErrorType (), [])
  (Dval.resultOk ok error, Dval.resultError ok error)

let private resultTypeOf (bodyType : TypeReference) =
  TypeReference.result
    (TCustomType(NR.ok (responseOKType ()), [ bodyType ]))
    (TCustomType(NR.ok (responseErrorType ()), []))

/// A `Response<'body>` record, headers as the host answered them, from `url`, where the last
/// redirect led.
let private responseOf
  (bodyType : ValueType)
  (url : string)
  (statusCode : int)
  (headers : List<string * string>)
  (body : Dval)
  : Dval =
  let typ = responseOKType ()
  let fields =
    [ ("statusCode", Dval.dint64 (int64 statusCode))
      ("headers", headersToDval headers)
      ("body", body)
      ("url", DString url) ]
  DRecord(typ, typ, [ bodyType ], RecordFields.ofList fields)

let private parameters =
  [ Param.make "method" TString ""
    Param.make "uri" TString ""
    Param.make "headers" headersType ""
    Param.make "body" TBytes ""
    Param.make
      "timeoutMs"
      (TypeReference.option TInt64)
      "How long the exchange may take in milliseconds, redirects and retries included: all of it for a buffered response, until the last response's headers for a stream. {{None}} is the host's default (30s); 0 is no limit"
    Param.make
      "redirects"
      TInt64
      "How many redirects to follow; 0 answers a redirect as it came"
    Param.make
      "retries"
      TInt64
      "How many more times an idempotent request may be sent after a failure worth retrying"
    Param.make
      "retryDelayMs"
      TInt64
      "The wait before the first retry, doubled for each one after"
    Param.make
      "decompress"
      TBool
      "Undo a gzip or brotli Content-Encoding on the body" ]

/// Milliseconds as the host takes them. Past `Int32.MaxValue` (24 days) is as good as forever.
let private clampMs (ms : int64) : int =
  int (max 0L (min ms (int64 System.Int32.MaxValue)))

/// One send of an exchange.
type Hop =
  { method : string; uri : string; headers : List<string * string>; body : byte[] }

/// What the request asks of its exchange beyond the first send. `deadline` is on
/// `Environment.TickCount64`, `None` for no limit.
type private Policy =
  { deadline : Option<int64>
    redirects : int64
    retries : int64
    retryDelayMs : int64 }

let private headerValue
  (name : string)
  (headers : List<string * string>)
  : Option<string> =
  headers
  |> List.tryFind (fun (k, _) -> String.equalsCaseInsensitive k name)
  |> Option.map snd

let private withoutHeaders
  (names : List<string>)
  (headers : List<string * string>)
  : List<string * string> =
  headers
  |> List.filter (fun (k, _) ->
    not (List.exists (String.equalsCaseInsensitive k) names))

let private originOf (uri : System.Uri) =
  (uri.Scheme.ToLowerInvariant(), uri.IdnHost.ToLowerInvariant(), uri.Port)

/// Start each redirect chain with no cookies and keep cookies separate by origin.
type RedirectCookies() =
  let jars =
    System.Collections.Generic.Dictionary<string * string * int, System.Net.CookieContainer>()

  member _.Remember(url : string, headers : List<string * string>) : unit =
    let cookies =
      headers
      |> List.choose (fun (name, value) ->
        if String.equalsCaseInsensitive name "Set-Cookie" then Some value else None)

    match cookies, System.Uri.TryCreate(url, System.UriKind.Absolute) with
    | [], _
    | _, (false, _) -> ()
    | _, (true, uri) ->
      let origin = originOf uri
      let jar =
        match jars.TryGetValue origin with
        | true, jar -> jar
        | false, _ ->
          let jar = System.Net.CookieContainer()
          jars.Add(origin, jar)
          jar

      for cookie in cookies do
        try
          jar.SetCookies(uri, cookie)
        with :? System.Net.CookieException ->
          ()

  member _.ForSend(hop : Hop) : Hop =
    match
      headerValue "Cookie" hop.headers,
      System.Uri.TryCreate(hop.uri, System.UriKind.Absolute)
    with
    | None, (true, uri) ->
      match jars.TryGetValue(originOf uri) with
      | true, jar ->
        let cookies = jar.GetCookieHeader uri
        if cookies = "" then
          hop
        else
          { hop with headers = ("Cookie", cookies) :: hop.headers }
      | false, _ -> hop
    | _ -> hop


type private CookiePolicy =
  | IgnoreCookies
  | UseRedirectCookies


/// The next send after `status`, when it is a redirect with a `Location`: there, resolved against
/// where this one went. As fetch and curl -L do, a POST after a 301 or 302, and anything but a
/// GET or HEAD after a 303, becomes a GET with no body; otherwise the method and body are kept.
/// Credentials and cookies stay behind when it leaves the origin (scheme, host and port).
let redirectOf
  (hop : Hop)
  (status : int)
  (headers : List<string * string>)
  : Option<Hop> =
  match status, headerValue "Location" headers with
  | (301 | 302 | 303 | 307 | 308), Some location ->
    match System.Uri.TryCreate(hop.uri, System.UriKind.Absolute) with
    | true, from ->
      match System.Uri.TryCreate(from, location) with
      | true, next ->
        let asGet =
          match status with
          | 301
          | 302 -> String.equalsCaseInsensitive hop.method "POST"
          | 303 ->
            not (
              String.equalsCaseInsensitive hop.method "GET"
              || String.equalsCaseInsensitive hop.method "HEAD"
            )
          | _ -> false
        let headers =
          if asGet then
            withoutHeaders
              [ "Content-Type"
                "Content-Length"
                "Content-Encoding"
                "Content-Language"
                "Content-Location" ]
              hop.headers
          else
            hop.headers
        let headers =
          if originOf from = originOf next then
            headers
          else
            withoutHeaders
              [ "Authorization"; "Cookie"; "Proxy-Authorization" ]
              headers
        Some
          { method = if asGet then "GET" else hop.method
            uri = next.AbsoluteUri
            headers = headers
            body = if asGet then [||] else hop.body }
      | _ -> None
    | _ -> None
  | _ -> None

/// RFC 9110's idempotent methods: sending one twice does what sending it once does.
let private isIdempotent (method : string) : bool =
  [ "GET"; "HEAD"; "PUT"; "DELETE"; "OPTIONS"; "TRACE" ]
  |> List.exists (String.equalsCaseInsensitive method)

let private retryableStatus (status : int) : bool =
  status = 429 || status = 502 || status = 503 || status = 504

let private retryableError (err : HostTypes.HttpRequestError) : bool =
  match err with
  | HostTypes.HttpRequestError.Timeout
  | HostTypes.HttpRequestError.NetworkError _ -> true
  | _ -> false

/// The longest a retry waits, whatever a `Retry-After` asks and with no deadline to stop it.
let private longestWaitMs = 60_000L

/// How long a `Retry-After` asks for, seconds or an HTTP date, up to `longestWaitMs`.
let private retryAfterMs (headers : List<string * string>) : Option<int64> =
  headerValue "Retry-After" headers
  |> Option.bind (fun value ->
    let value = value.Trim()
    match System.Int64.TryParse value with
    | true, seconds when seconds >= 0L -> Some(min (seconds * 1000L) longestWaitMs)
    | _ ->
      match
        System.DateTimeOffset.TryParse(
          value,
          System.Globalization.CultureInfo.InvariantCulture,
          System.Globalization.DateTimeStyles.AssumeUniversal
        )
      with
      | true, at ->
        let ms = int64 (at - System.DateTimeOffset.UtcNow).TotalMilliseconds
        Some(min (max 0L ms) longestWaitMs)
      | _ -> None)

/// The wait before retry `n` (0 for the first): `delayMs` doubled `n` times, up to
/// `longestWaitMs`, then a random point in its upper half, so clients that failed together do not
/// all come back together.
let private backoffMs (delayMs : int64) (n : int64) : int64 =
  let full = min (min delayMs longestWaitMs <<< int (min n 16L)) longestWaitMs
  full / 2L + System.Random.Shared.NextInt64(full / 2L + 1L)

let private tooManyRedirects () : Dval =
  let typ = responseErrorType ()
  DEnum(typ, typ, [], "TooManyRedirects", [])

/// Send `first`, following redirects and retrying as `policy` allows, all under its one deadline.
/// Each send is its own checked host operation, so every hop is checked against the grants and the
/// network guard for wherever it goes. The first is named to the interpreter (`requestHost`), as
/// every builtin's is, so `ps` can say what the process waits on; the hops after it are performed
/// here, from its continuation, which a read's has to be. A response left behind (a redirect, a
/// 503 tried again) is `drop`ped, which for a stream closes it.
let private exchange
  (state : ExecutionState)
  (vm : VMState)
  (policy : Policy)
  (cookiePolicy : CookiePolicy)
  (operation : Hop -> int -> Host.Operation)
  (expect : Host.Response -> Result<'r, HostTypes.HttpRequestError>)
  (head : 'r -> int * List<string * string>)
  (drop : 'r -> unit)
  (finish : Result<Hop * 'r, Dval> -> Dval)
  (first : Hop)
  : Dval =
  let access = vm.activeAccess
  let cookies =
    match cookiePolicy with
    | IgnoreCookies -> None
    | UseRedirectCookies -> Some(RedirectCookies())
  let remaining () =
    policy.deadline |> Option.map (fun at -> at - System.Environment.TickCount64)
  let fits (waitMs : int64) =
    match remaining () with
    | Some left -> waitMs < left
    | None -> true
  // What the host takes: what is left of the deadline, and 0 for no limit.
  let timeoutMs () =
    remaining () |> Option.map (max 1L >> clampMs) |> Option.defaultValue 0
  let answerOf (outcome : Result<Host.Response, Host.Failure>) =
    match outcome with
    | Error failure ->
      Exception.raiseInternal
        "http request failed outside the typed error surface"
        [ "message", failure.message ]
    | Ok response -> expect response
  let send (hop : Hop) : Task<Result<'r, HostTypes.HttpRequestError>> =
    match remaining () with
    | Some left when left <= 0L ->
      Task.FromResult(Error HostTypes.HttpRequestError.Timeout)
    | _ ->
      let request =
        cookies |> Option.map (fun jar -> jar.ForSend hop) |> Option.defaultValue hop
      PermissionCheck.performHostWithAccess
        state
        vm
        access
        (operation request (timeoutMs ()))
      |> Task.map answerOf
  let rec settle
    (hop : Hop)
    (redirects : int64)
    (retries : int64)
    (answer : Result<'r, HostTypes.HttpRequestError>)
    : Task<Result<Hop * 'r, Dval>> =
    task {
      let again = retries > 0L && isIdempotent hop.method
      let backoff () = backoffMs policy.retryDelayMs (policy.retries - retries)
      match answer with
      | Error err when again && retryableError err ->
        let wait = backoff ()
        if fits wait then
          do! Task.Delay(clampMs wait)
          let! answer = send hop
          return! settle hop redirects (retries - 1L) answer
        else
          return Error(RequestError.toDT err)
      | Error err -> return Error(RequestError.toDT err)
      | Ok r ->
        let (status, headers) = head r
        cookies |> Option.iter (fun jar -> jar.Remember(hop.uri, headers))
        let next =
          if policy.redirects > 0L then redirectOf hop status headers else None
        match next with
        | Some _ when redirects = 0L ->
          drop r
          return Error(tooManyRedirects ())
        | Some next ->
          drop r
          let! answer = send next
          return! settle next (redirects - 1L) retries answer
        | None ->
          if again && retryableStatus status then
            let wait = retryAfterMs headers |> Option.defaultWith backoff
            if fits wait then
              drop r
              do! Task.Delay(clampMs wait)
              let! answer = send hop
              return! settle hop redirects (retries - 1L) answer
            else
              return Ok(hop, r)
          else
            return Ok(hop, r)
    }
  Interpreter.requestHost vm (operation first (timeoutMs ())) (fun outcome ->
    settle first policy.redirects policy.retries (answerOf outcome)
    |> Pending.map finish)

/// The builtins' shared body: read the arguments, run the exchange, and answer its last
/// response with `answer`, or why there is none.
let private run
  (state : ExecutionState)
  (vm : VMState)
  (args : Dval[])
  (cookiePolicy : CookiePolicy)
  (operation : Hop -> int -> bool -> Host.Operation)
  (expect : Host.Response -> Result<'r, HostTypes.HttpRequestError>)
  (head : 'r -> int * List<string * string>)
  (drop : 'r -> unit)
  (answer : string -> 'r -> Dval)
  (failed : Dval -> Dval)
  : Dval =
  match args with
  | [| DString method
       DString uri
       DList(_, Items(reqHeaders))
       DBytes bodyRef
       timeout
       DInt64 redirects
       DInt64 retries
       DInt64 retryDelayMs
       DBool decompress |] ->
    let overall =
      match timeout with
      | DEnum(_, _, _, "Some", [ DInt64 ms ]) -> ms
      | _ -> int64 HostTypes.defaultHttpTimeoutMs
    let policy =
      { deadline =
          if overall > 0L then
            Some(System.Environment.TickCount64 + int64 (clampMs overall))
          else
            None
        redirects = max 0L redirects
        retries = max 0L retries
        retryDelayMs = max 0L retryDelayMs }
    let headers = parseHeaders reqHeaders
    let finish result =
      match result with
      | Ok(hop, r) -> answer hop.uri r
      | Error e -> failed e
    let operation hop timeoutMs = operation hop timeoutMs decompress
    Bytes.withBytes state bodyRef (fun body ->
      { method = method; uri = uri; headers = headers; body = body }
      |> exchange state vm policy cookiePolicy operation expect head drop finish)
  | _ -> incorrectArgs ()


let private bufferedRequest name cookiePolicy description : BuiltInFn =
  { name = fn name 0
    typeParams = []
    parameters = parameters
    returnType = resultTypeOf TBytes
    description = description
    fn =
      let (resultOk, resultError) = resultOf VT.bytes
      (fun (struct (state, vm, _, args)) ->
        run
          state
          vm
          args
          cookiePolicy
          (fun hop timeoutMs decompress ->
            Host.Operation.HttpRequest(
              HostTypes.HttpProfile.Guest,
              hop.method,
              hop.uri,
              hop.headers,
              hop.body,
              Some timeoutMs,
              decompress
            ))
          Host.expectHttp
          (fun (r : HostTypes.HttpResponse) -> (r.statusCode, r.headers))
          ignore<HostTypes.HttpResponse>
          (fun url r ->
            responseOf
              VT.bytes
              url
              r.statusCode
              r.headers
              (Bytes.newEphemeral r.body)
            |> resultOk)
          resultError)
    previewable = Impure
    callEffects = set [ Effect.Http ]
    deprecated = NotDeprecated }


let fns () : List<BuiltInFn> =
  [ bufferedRequest
      "httpClientRequest"
      IgnoreCookies
      ("Send an HTTP request to <param uri> and read the full response. "
       + "A GET or HEAD without a body runs in the background until its response is needed. "
       + "Other requests run in program order. Returns {{ Ok }} with the response "
       + "or {{ Error }} with the reason the request failed.")
    bufferedRequest
      "httpClientRequestWithRedirectCookies"
      UseRedirectCookies
      ("Send an HTTP request and read the full response. "
       + "Keep a temporary cookie jar for each origin during the redirect chain.")

    // ——————————————————————————————————————————————————————————
    // Streaming HTTP.
    //
    // The body is not buffered into a byte[]; the host opens the response and
    // keeps it in its stream table, and the DStream reads chunks through the
    // host-issued handle, each once, kept for every holder of the body. Each
    // read is one Bytes element of up to 64 KB.
    //
    // The disposer releases the host-side response when the body is read to
    // its end, on `Builtin.streamClose`, or when the GC finds nothing left
    // that could read it (`StreamSource`'s finalizer). Chunk reads and the close go directly to `HostHttp` — the
    // transfer was authorized when the stream was opened, and the handle is
    // host-issued, so no further policy decision is involved.
    // ——————————————————————————————————————————————————————————
    { name = fn "httpClientStream" 0
      typeParams = []
      parameters = parameters
      returnType = resultTypeOf (TStream TBytes)
      description =
        "Make a streaming HTTP call to <param uri>, in program order. Returns a <type "
        + "Response> whose `body` is a lazy <type Stream> of <type Bytes> chunks, "
        + "read as they arrive. Read with `Stdlib.Stream.toList`/`toBytes`, or "
        + "compose with `streamMap`/`streamFilter`/etc. The underlying HTTP "
        + "response is released when the stream is read to its end or "
        + "`Builtin.streamClose`d."
      fn =
        let streamOfBytes = ValueType.Known(KTStream VT.bytes)
        let (resultOk, resultError) = resultOf streamOfBytes
        (fun (struct (state, vm, _, args)) ->
          run
            state
            vm
            args
            IgnoreCookies
            (fun hop timeoutMs decompress ->
              Host.Operation.HttpStreamOpen(
                HostTypes.HttpProfile.Guest,
                hop.method,
                hop.uri,
                hop.headers,
                hop.body,
                Some timeoutMs,
                decompress
              ))
            Host.expectHttpStream
            (fun (head : HostTypes.HttpStreamHead) ->
              (head.statusCode, head.headers))
            (fun head -> Host.httpStreamClose head.handle)
            (fun url head ->
              let nextChunk (maxBytes : int) : ValueTask<Option<byte[]>> =
                ValueTask<Option<byte[]>>(Host.httpStreamRead head.handle maxBytes)
              let disposer () = Host.httpStreamClose head.handle
              let body = Stream.newBytes nextChunk (Some disposer)
              responseOf streamOfBytes url head.statusCode head.headers body
              |> resultOk)
            resultError)
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated } ]


let builtins () = Builtin.make [] (fns ())
