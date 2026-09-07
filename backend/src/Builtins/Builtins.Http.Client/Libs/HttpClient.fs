/// HTTP client builtins.
///
/// Two builtins, two API surfaces:
/// - `httpClientRequest` returns a Response with `body : Blob` —
///   buffers the whole body up front; the simple/common case.
/// - `httpClientStream` returns a StreamResponse with
///   `body : Stream<UInt8>` — lazy/chunked; for large bodies, SSE, etc.
///
/// The network machinery lives behind the checked host boundary
/// (`LibExecution.HostHttp`): these builtins parse Dvals, build a
/// `Host.Operation`, and map the structured result back to the
/// Stdlib.HttpClient types. Guest requests run under the host's guest HTTP
/// configuration (`HostTypes.HttpProfile.Guest`); the trusted sync pull
/// under `Sync`.
///
/// TODO collapse into a single builtin: `httpClientStream` becomes the only F#
/// builtin, and `Stdlib.HttpClient.request` a Dark-side wrapper that streams,
/// drains via `Stream.toBlob`, and repacks into a `Response`. Four gates before
/// the collapse stops regressing existing callers:
///   (1) a `body : Blob` param on the stream builtin (today it always sends `[||]`);
///   (2) a body-read timeout (`ResponseHeadersRead` means the cancel token only
///       covers header arrival, so a drain can hang indefinitely);
///   (3) drain-time error translation (an IOException during the drain must become
///       `Result.Error NetworkError`, not an uncaught RuntimeError);
///   (4) telemetry parity with `makeRequest` (one span through the drain, same tags).
/// Until those land, the buffered builtin stays; request construction is shared via
/// the helpers below, so the remaining duplication is small.
module Builtins.Http.Client.Libs.HttpClient

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes
open LibExecution.Effects
module VT = ValueType
module RTE = RuntimeError
module NR = LibExecution.RuntimeTypes.NameResolution
module Blob = LibExecution.Blob
module Stream = LibExecution.Stream
module Host = LibExecution.Host
module PermissionCheck = LibExecution.PermissionCheck

let responseOKType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.response ())
let responseErrorType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.requestError ())
let streamResponseType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.streamResponse ())


module BadHeader =
  let toDT (err : HostTypes.HttpBadHeader) : Dval =
    let (caseName, fields) =
      match err with
      | HostTypes.HttpBadHeader.EmptyKey -> "EmptyKey", []
      | HostTypes.HttpBadHeader.InvalidContentType -> "InvalidContentType", []
    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.badHeader ())
    DEnum(typeName, typeName, [], caseName, fields)

module BadUrl =
  let toDT (err : HostTypes.HttpBadUrl) : Dval =
    let (caseName, fields) =
      match err with
      | HostTypes.HttpBadUrl.UnsupportedProtocol -> "UnsupportedProtocol", []
      | HostTypes.HttpBadUrl.InvalidHost -> "InvalidHost", []
      | HostTypes.HttpBadUrl.InvalidUri -> "InvalidUri", []
      | HostTypes.HttpBadUrl.InvalidRequest -> "InvalidRequest", []

    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.badUrlDetails ())
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
      | HostTypes.HttpRequestError.NetworkError -> "NetworkError", []
      | HostTypes.HttpRequestError.BadMethod -> "BadMethod", []

    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.requestError ())
    DEnum(typeName, typeName, [], caseName, fields)


let headersType = TList(TTuple(TString, TString, []))

let private headersToDval (headers : List<string * string>) : Dval =
  headers
  |> List.map (fun (k, v) ->
    DTuple(DString(String.toLowercase k), DString(String.toLowercase v), []))
  |> Dval.list (KTTuple(VT.string, VT.string, []))

/// Parse the Dval header list. A non-pair element is a type error (RTE); an
/// empty key is the guest-visible EmptyKey error.
let private parseHeaders
  (vm : VMState)
  (fnName : FQFnName.FQFnName)
  (reqHeaders : List<Dval>)
  : Result<List<string * string>, HostTypes.HttpBadHeader> =
  reqHeaders
  |> List.map (fun item ->
    match item with
    | DTuple(DString k, DString v, []) ->
      let k = String.trim k
      if k = "" then
        // CLEANUP reconsider if we should error here
        Error HostTypes.HttpBadHeader.EmptyKey
      else
        Ok((k, v))
    | notAPair ->
      RTE.Applications.FnParameterNotExpectedType(
        fnName,
        2,
        "headers",
        None,
        VT.list (VT.tuple VT.string VT.string []),
        Dval.toValueType notAPair,
        notAPair
      )
      |> RTE.Apply
      |> raiseRTE vm.threadID)
  |> Result.collect

/// Refuse the private-network sync transport unless the host explicitly enabled
/// it for this execution and every non-root frame is bundled Darklang code.
/// Checking only the immediate caller is insufficient: an untrusted package can
/// otherwise call a bundled wrapper and turn it into a confused deputy.
let private requireBundledCaller
  (state : ExecutionState)
  (vm : VMState)
  (builtinName : string)
  : unit =
  let rec fnOf (point : ExecutionPoint) : Option<FQFnName.Package> =
    match point with
    | ExecutionPoint.Function(FQFnName.Package p) -> Some p
    | ExecutionPoint.Lambda(parent, _) -> fnOf parent
    | _ -> None
  let rec allBundled (frameID : System.Guid) (sawPackage : bool) : bool =
    match vm.callFrames.TryGetValue frameID with
    | false, _ -> false
    | true, frame ->
      let frameTrusted, sawPackage =
        match fnOf frame.executionPoint with
        | Some p -> state.isBundledPackageFn p, true
        // Source-derived frames -- the host's own expression, and any lambda defined in it --
        // are trusted here because the separate host capability above is what distinguishes
        // `dark sync` from a guest `run`/`eval`. Requiring a PARENTLESS frame was too strict:
        // the CLI reaches the transport through a lambda of its entry expression, whose parent
        // is the source frame, so every sync command refused itself. A lambda inside a package
        // fn is unaffected -- `fnOf` walks to that fn and checks it.
        | None -> true, sawPackage
      if not frameTrusted then
        false
      else
        match frame.parent with
        | ValueNone -> sawPackage
        | ValueSome(parentID, _, _) -> allBundled parentID sawPackage
  let trusted = state.canUsePrivateNetworkHttp && allBundled vm.currentFrameID false
  if not trusted then
    RuntimeError.UncaughtException(
      $"permission denied: `{builtinName}` is restricted to trusted first-party (Darklang) code",
      []
    )
    |> raiseUntargetedRTE


/// Decode a Dark `List<(String, String)>` of request headers; anything not that shape is
/// dropped. The lenient twin of `parseHeaders`, for the caller-gated sync family only.
let private headerPairs (dvals : List<Dval>) : List<string * string> =
  dvals
  |> List.choose (fun h ->
    match h with
    | DTuple(DString k, DString v, []) -> Some(k, v)
    | _ -> None)

/// Build and perform one Sync-profile host request. The caller gate has already run; the
/// instance policy scopes the URL, which is what replaced the origin allowlist.
let private syncRequest
  (state : ExecutionState)
  (vm : VMState)
  (method : string)
  (uri : string)
  (headers : List<string * string>)
  (body : byte array)
  : Ply<Result<Host.Response, Host.Failure>> =
  PermissionCheck.performHost
    state
    vm
    (Host.Operation.HttpRequest(HostTypes.HttpProfile.Sync, method, uri, headers, body))

/// Shape a completed sync exchange: a 2xx body is Ok bytes; a non-2xx is a FAILURE, not a
/// body -- Ok for anything that completed would hand the caller a relay's 400 as a
/// successful fetch whose payload happens to be an error page, and `dark branch push`
/// would print "pushed branch ..." for a 400 it never saw.
let private fetchOutcome
  (verb : string)
  (response : Result<Host.Response, Host.Failure>)
  : Dval =
  match response with
  // A `Failure` here is the host refusing the request -- a malformed url, or a policy denial.
  // These builtins promise a `Result` and are swept with arguments a person would get wrong
  // (`dark sync zzz-not-a-url`), so a refusal is an Error to report, never an exception that
  // takes the command down. `httpClientRequest` keeps raising, since its own typed error
  // surface already covers the cases it can meet.
  | Error failure -> Dval.resultError KTBlob KTString (DString failure.message)
  | Ok response ->
    match Host.expectHttp response with
    | Ok r when r.statusCode >= 200 && r.statusCode < 300 ->
      Dval.resultOk KTBlob KTString (Blob.newEphemeral r.body)
    | Ok r ->
      let snippet =
        try
          let t = System.Text.Encoding.UTF8.GetString(r.body)
          if t.Length > 200 then t.Substring(0, 200) + "..." else t
        with _ ->
          ""
      Dval.resultError KTBlob KTString (DString $"HTTP {r.statusCode}: {snippet}")
    | Error err ->
      let reason =
        match err with
        | HostTypes.HttpRequestError.BadUrl _ -> "bad url"
        | HostTypes.HttpRequestError.Timeout -> "timeout"
        | HostTypes.HttpRequestError.BadHeader _ -> "bad header"
        | HostTypes.HttpRequestError.NetworkError -> "network error"
        | HostTypes.HttpRequestError.BadMethod -> "bad method"
      Dval.resultError KTBlob KTString (DString $"{verb} failed: {reason}")

/// In-flight prefetches, keyed by a handle rather than by url: a pull can have the same
/// url in flight twice, and a dictionary keyed by url would hand the second caller the
/// first one's response.
let private pendingFetches =
  System.Collections.Concurrent.ConcurrentDictionary<
    System.Guid,
    Task<Result<Host.Response, Host.Failure>>>()

open LibExecution.Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "httpClientRequest" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" TBlob "" ]
      returnType =
        TypeReference.result
          (TCustomType(NR.ok (responseOKType ()), []))
          (TCustomType(NR.ok (responseErrorType ()), []))
      description =
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where "
        + "the response is wrapped in {{ Ok }} if a response was successfully "
        + "received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        let responseTypeOK = KTCustomType(responseOKType (), [])
        let responseTypeErr = KTCustomType(responseErrorType (), [])
        let resultOk = Dval.resultOk responseTypeOK responseTypeErr
        let resultError = Dval.resultError responseTypeOK responseTypeErr
        (function
        | state,
          vm,
          _,
          [| DString method; DString uri; DList(_, reqHeaders); DBlob bodyRef |] ->
          uply {
            let! reqBodyBytes = Blob.readBytes state bodyRef
            let headers =
              parseHeaders
                vm
                (FQFnName.fqPackage (PackageRefs.Fn.Stdlib.HttpClient.request ()))
                reqHeaders
            match headers with
            | Error headerError ->
              return
                resultError (
                  RequestError.toDT (
                    HostTypes.HttpRequestError.BadHeader headerError
                  )
                )
            | Ok headers ->
              let op =
                Host.Operation.HttpRequest(
                  HostTypes.HttpProfile.Guest,
                  method,
                  uri,
                  headers,
                  reqBodyBytes
                )
              match! PermissionCheck.performHost state vm op with
              | Error failure ->
                return
                  Exception.raiseInternal
                    "http request failed outside the typed error surface"
                    [ "message", failure.message ]
              | Ok response ->
                match Host.expectHttp response with
                | Error err -> return resultError (RequestError.toDT err)
                | Ok response ->
                  let typ = responseOKType ()
                  let fields =
                    [ ("statusCode", Dval.int (bigint response.statusCode))
                      ("headers", headersToDval response.headers)
                      ("body", Blob.newEphemeral response.body) ]
                  return resultOk (DRecord(typ, typ, [], Map fields))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }


    // GET with SSRF guards OFF, returning raw BYTES — for pulling a peer's op wire over the tailnet.
    // (The safe `httpClientRequest` bans loopback/RFC-1918/tailnet, which a peer's sync server sits behind;
    // the Blob variant hands the body back as bytes for the caller to decode — `Stdlib.Blob.toString` for the
    // JSON wire.) TRUSTED-CLI use: the caller IS the code author; used by `Sync.pull` / `dark sync fetch <url>`.
    // Gated to first-party callers by `requireBundledCaller` below: though
    // registered in the general builtin set, a third-party pulled package
    // cannot call it, so its SSRF-guards-off reach is confined to the bundled
    // sync code it exists for.
    { name = fn "httpGetUnsafeBytes" 0
      typeParams = []
      parameters =
        [ Param.make
            "uri"
            TString
            "URL to GET with SSRF guards OFF (loopback/RFC-1918/tailnet reachable)" ]
      returnType = TypeReference.result TBlob TString
      description =
        "GET <param uri> with NO SSRF guards, returning the raw response body as "
        + "Bytes (Ok) or a message (Error). A non-2xx status is an Error too: a "
        + "peer's 404 page is not a store. For pulling a peer's store over the "
        + "tailnet."
      fn =
        (function
        | state, vm, _, [| DString uri |] ->
          uply {
            // SSRF guards off (loopback/RFC-1918/tailnet reachable): only the
            // bundled sync code may call this, not a third-party package.
            requireBundledCaller state vm "httpGetUnsafeBytes"
            let! response = syncRequest state vm "GET" uri [] [||]
            return fetchOutcome "fetch" response
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }


    // Start a sync GET WITHOUT waiting for it, and collect it later.
    //
    // A pull is a chain of pages: fetch one, import it, fetch the next. Network and CPU are
    // each about 40% of a pull, so done strictly in turn the total is their sum. With these
    // two the client starts the next page's fetch as soon as it knows the cursor, imports
    // the page in hand while that flies, and pays `max` instead.
    //
    // The caller gate is checked HERE, at start, so a refusal is immediate and loud; a
    // policy refusal from the host surfaces at the await, out of the same typed surface.
    { name = fn "httpGetUnsafeBytesStart" 0
      typeParams = []
      parameters =
        [ Param.make "uri" TString "URL to begin GETting with SSRF guards OFF" ]
      returnType = TypeReference.result TUuid TString
      description =
        "Begin a GET of <param uri> with NO SSRF guards and return a handle to collect "
        + "it with `httpAwaitBytes`. The request is already in flight when this returns."
      fn =
        (function
        | state, vm, _, [| DString uri |] ->
          uply {
            requireBundledCaller state vm "httpGetUnsafeBytesStart"
            // Started, not awaited: `Ply.toTask` materializes the running request, so it
            // is on the wire before this builtin returns.
            let started = syncRequest state vm "GET" uri [] [||] |> Ply.toTask
            let handle = System.Guid.NewGuid()
            pendingFetches[handle] <- started
            return Dval.resultOk KTUuid KTString (DUuid handle)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }


    // Collect a fetch begun by `httpGetUnsafeBytesStart`. Same result shape as
    // `httpGetUnsafeBytes`, including treating a non-2xx as a failure rather than a body.
    { name = fn "httpAwaitBytes" 0
      typeParams = []
      parameters =
        [ Param.make "handle" TUuid "a handle from `httpGetUnsafeBytesStart`" ]
      returnType = TypeReference.result TBlob TString
      description =
        "Wait for the fetch named by <param handle> and return its body (Ok) or an "
        + "error message (Error). A handle may only be collected once."
      fn =
        (function
        | _, _, _, [| DUuid handle |] ->
          uply {
            match pendingFetches.TryRemove handle with
            | false, _ ->
              return
                Dval.resultError
                  KTBlob
                  KTString
                  (DString
                    "that fetch handle is unknown, or has already been collected")
            | true, pending ->
              let! response = pending
              return fetchOutcome "fetch" response
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }


    // The read twin of `httpPostUnsafeBytes`. Separate from `httpGetUnsafeBytes` because
    // that one's arity is part of its contract. Exists so a read can carry an Authorization
    // header: a relay's branch endpoints hand back unmerged work, and a secret belongs in a
    // header, not a logged query string.
    { name = fn "httpGetUnsafeBytesWithHeaders" 0
      typeParams = []
      parameters =
        [ Param.make "uri" TString "URL to GET with SSRF guards OFF"
          Param.make
            "headers"
            (TList(TTuple(TString, TString, [])))
            "request headers" ]
      returnType = TypeReference.result TBlob TString
      description =
        "GET <param uri> with NO SSRF guards and the given <param headers>, returning the "
        + "raw response body as Bytes (Ok) or an error message (Error)."
      fn =
        (function
        | state, vm, _, [| DString uri; DList(_, headerList) |] ->
          uply {
            requireBundledCaller state vm "httpGetUnsafeBytesWithHeaders"
            // The credential is attached HERE, not passed in: the write secret must not
            // reach Dark, where a pulled package could read it.
            let headers =
              headerPairs headerList @ LibExecution.UnguardedOrigins.authHeadersFor uri
            let! response = syncRequest state vm "GET" uri headers [||]
            return fetchOutcome "fetch" response
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }


    // The push half of the sync transport, mirror of `httpGetUnsafeBytes`: same caller
    // gate, same profile. Body is sent as application/json (the wire codec).
    { name = fn "httpPostUnsafeBytes" 0
      typeParams = []
      parameters =
        [ Param.make
            "uri"
            TString
            "URL to POST with SSRF guards OFF (loopback/RFC-1918/tailnet reachable)"
          Param.make "body" TBlob "request body, sent as application/json"
          Param.make
            "headers"
            (TList(TTuple(TString, TString, [])))
            "extra request headers, e.g. an Authorization for a relay that requires one" ]
      returnType = TypeReference.result TBlob TString
      description =
        "POST <param body> to <param uri> with NO SSRF guards, returning the raw "
        + "response body as Bytes (Ok) or an error message (Error). For pushing to a "
        + "peer's store over the tailnet."
      fn =
        (function
        | state, vm, _, [| DString uri; DBlob bodyRef; DList(_, headers) |] ->
          uply {
            requireBundledCaller state vm "httpPostUnsafeBytes"
            let! body = Blob.readBytes state bodyRef
            // Caller headers go AFTER the content type so a caller cannot accidentally
            // unset it. A relay write secret arrives as a header rather than in the query
            // string, which would put it in every access log and proxy trace between here
            // and there; the stored credential is attached here, not passed in -- see
            // `httpGetUnsafeBytesWithHeaders`.
            let allHeaders =
              ("Content-Type", "application/json")
              :: (headerPairs headers
                  @ LibExecution.UnguardedOrigins.authHeadersFor uri)
            let! response = syncRequest state vm "POST" uri allHeaders body
            return fetchOutcome "push" response
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }

    // ——————————————————————————————————————————————————————————
    // Streaming HTTP.
    //
    // The body is not buffered into a byte[]; the host opens the response and
    // keeps it in its stream table, and the DStream pulls chunks through the
    // host-issued handle. Bulk consumers (`streamToBlob`) pull whole buffers
    // via `nextChunk`; byte-wise consumers (`streamNext`) see one `DUInt8` at
    // a time synthesised from the same buffer.
    //
    // The disposer releases the host-side response when the consumer drains
    // to EOF or calls `Builtin.streamClose`. Abandoning a stream mid-drain
    // falls back to the GC-triggered finalizer on `Dval.StreamFinalizer`,
    // which runs the same disposer chain when the DStream becomes
    // unreachable. Chunk reads and the close go directly to `HostHttp` — the
    // transfer was authorized when the stream was opened, and the handle is
    // host-issued, so no further policy decision is involved.
    // ——————————————————————————————————————————————————————————
    { name = fn "httpClientStream" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType "" ]
      returnType =
        TypeReference.result
          (TCustomType(NR.ok (streamResponseType ()), []))
          (TCustomType(NR.ok (responseErrorType ()), []))
      description =
        "Make a streaming HTTP call to <param uri>. Returns a <type "
        + "StreamResponse> whose `body` is a lazy <type Stream> that yields bytes "
        + "as they arrive. Drain with `Builtin.streamToList`/`streamToBlob`, or "
        + "compose with `streamMap`/`streamFilter`/etc. The underlying HTTP "
        + "response is released when the stream is drained to completion or "
        + "`Builtin.streamClose`d."
      fn =
        let streamTypeOk = KTCustomType(streamResponseType (), [])
        let streamTypeErr = KTCustomType(responseErrorType (), [])
        let resultOk = Dval.resultOk streamTypeOk streamTypeErr
        let resultError = Dval.resultError streamTypeOk streamTypeErr
        (function
        | state, vm, _, [| DString method; DString uri; DList(_, reqHeaders) |] ->
          uply {
            let headers =
              parseHeaders
                vm
                (FQFnName.fqPackage (PackageRefs.Fn.Stdlib.HttpClient.stream ()))
                reqHeaders
            match headers with
            | Error headerError ->
              return
                resultError (
                  RequestError.toDT (
                    HostTypes.HttpRequestError.BadHeader headerError
                  )
                )
            | Ok headers ->
              let op =
                Host.Operation.HttpStreamOpen(
                  HostTypes.HttpProfile.Guest,
                  method,
                  uri,
                  headers
                )
              match! PermissionCheck.performHost state vm op with
              | Error failure ->
                return
                  Exception.raiseInternal
                    "http stream open failed outside the typed error surface"
                    [ "message", failure.message ]
              | Ok response ->
                match Host.expectHttpStream response with
                | Error err -> return resultError (RequestError.toDT err)
                | Ok head ->
                  let nextChunk (maxBytes : int) : Ply<Option<byte[]>> =
                    uply {
                      let! chunk = Host.httpStreamRead head.handle maxBytes
                      return chunk
                    }

                  let disposer () = Host.httpStreamClose head.handle

                  let body = Stream.newChunked VT.uint8 nextChunk (Some disposer)

                  let typ = streamResponseType ()
                  let fields =
                    [ ("statusCode", Dval.int (bigint head.statusCode))
                      ("headers", headersToDval head.headers)
                      ("body", body) ]
                  return resultOk (DRecord(typ, typ, [], Map fields))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated } ]


let builtins () = Builtin.make [] (fns ())
