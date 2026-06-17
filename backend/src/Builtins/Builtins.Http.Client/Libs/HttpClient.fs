/// HTTP client builtins.
///
/// Two builtins, two API surfaces:
/// - `httpClientRequest` returns a Response with `body : Blob` —
///   buffers the whole body up front; the simple/common case.
/// - `httpClientStream` returns a StreamResponse with
///   `body : Stream<UInt8>` — lazy/chunked; for large bodies, SSE,
///   etc.
///
/// TODO collapse into a single builtin. The intended end state is:
///   - `httpClientRequest` is gone from the F# side.
///   - `httpClientStream` is the only F# builtin, and it takes a
///     `body : Blob` (currently always sends `[||]`).
///   - `Stdlib.HttpClient.request` is a Dark-side wrapper: call
///     `HttpClient.stream`, drain the body via `Stream.toBlob`,
///     repack into a `Response`. ~5 lines of Dark.
///
/// Four gates need to land in this file before the collapse stops
/// regressing existing callers:
///   (1) Add `body : Blob` to the stream builtin's parameters and
///       thread it into `openStreamingRequest`.
///   (2) Body-read timeout. Today the stream path uses
///       `HttpCompletionOption.ResponseHeadersRead`, so the cancel
///       token only fires on header-arrival lag — body reads can
///       hang indefinitely. The buffered path's whole-request
///       timeout has to be re-applied to the drain (carry the
///       CancellationToken through the FromIO closure and pass it
///       into `responseStream.ReadAsync`).
///   (3) Drain-time error translation. `makeRequest` catches
///       IOException at the top and returns `Result.Error
///       NetworkError`; on the streaming path that exception
///       happens during `Stream.toBlob` and bubbles up as an
///       uncaught RuntimeError. Need a `Stream.toBlob`-style
///       primitive that returns `Result<Blob, NetworkError>` —
///       either a new builtin or a wrapping helper that catches
///       inside the FromIO closure.
///   (4) Telemetry parity. `makeRequest` wraps the whole call in
///       `telemetryInitialize` (one coherent span) and tags
///       `request.content_type`, `request.content_length`,
///       `response.version`. `openStreamingRequest` inlines tags
///       and only records `response.status_code`. The collapse
///       wants the streaming span to extend through the drain
///       (span ends at toBlob completion / streamClose) and to
///       record the same set of tags on errors.
///
/// Until those four are in, the buffered builtin stays. Header /
/// URL / request-message construction are shared via the helpers
/// below so the duplication that does remain is small.
module Builtins.Http.Client.Libs.HttpClient

open System.IO
open System.Net.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes
module VT = ValueType
module RTE = RuntimeError
module NR = LibExecution.RuntimeTypes.NameResolution
module Blob = LibExecution.Blob
module Stream = LibExecution.Stream

type Method = HttpMethod

let responseOKType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.response ())
let responseErrorType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.requestError ())

module Headers =
  type Header = string * string
  type T = List<Header>

type Body = byte array

type Request = { url : string; method : Method; headers : Headers.T; body : Body }

type Response = { statusCode : int; headers : Headers.T; body : Body }


module BadHeader =
  type BadHeader =
    | EmptyKey
    | InvalidContentType

  let toDT (err : BadHeader) : Dval =
    let (caseName, fields) =
      match err with
      | EmptyKey -> "EmptyKey", []
      | InvalidContentType -> "InvalidContentType", []
    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.badHeader ())
    DEnum(typeName, typeName, [], caseName, fields)

module BadUrl =
  type BadUrlDetails =
    | UnsupportedProtocol
    | InvalidHost
    | InvalidUri
    | InvalidRequest

  let toDT (err : BadUrlDetails) : Dval =
    let (caseName, fields) =
      match err with
      | UnsupportedProtocol -> "UnsupportedProtocol", []
      | InvalidHost -> "InvalidHost", []
      | InvalidUri -> "InvalidUri", []
      | InvalidRequest -> "InvalidRequest", []

    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.badUrlDetails ())
    DEnum(typeName, typeName, [], caseName, fields)

module RequestError =
  // inspired by Elm's HttpError type
  // https://package.elm-lang.org/packages/elm/http/latest/Http#Error
  type RequestError =
    | BadUrl of BadUrl.BadUrlDetails
    | Timeout
    | BadHeader of BadHeader.BadHeader
    | NetworkError
    | BadMethod

  let toDT (err : RequestError) : Dval =
    let (caseName, fields) =
      match err with
      | BadUrl details -> "BadUrl", [ BadUrl.toDT details ]
      | Timeout -> "Timeout", []
      | BadHeader err -> "BadHeader", [ BadHeader.toDT err ]
      | NetworkError -> "NetworkError", []
      | BadMethod -> "BadMethod", []

    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.requestError ())
    DEnum(typeName, typeName, [], caseName, fields)


type RequestResult = Result<Response, RequestError.RequestError>

type Configuration =
  { timeoutInMs : int
    allowedIP : System.Net.IPAddress -> bool
    allowedHost : string -> bool
    allowedScheme : string -> bool
    allowedHeaders : Headers.T -> bool
    // telemetryInitialize allows us wrap the code with a span
    telemetryInitialize : (unit -> Task<RequestResult>) -> Task<RequestResult>
    telemetryAddTag : string -> obj -> unit
    telemetryAddException : Metadata -> System.Exception -> unit }

module BaseClient =
  // There are a number of different configuration options we want to enable:
  // WASM:
  //   when using Blazor/WASM, dotnet doesn't allow using a SocketsHttpHandler
  //   (errors at runtime). So we need to use a HttpClientHandler instead.
  // Cloud:
  //   when in the cloud, we want to include telemetry, as well as security measures
  //   to prevent access to local infrastructure (this is defense-in-depth: obvi we
  //   also use a firewall)
  // Local:
  //   when running locally, we want to use SocketsHttpHandler and no cloud
  //   features/restrictions.
  //
  // We enable these in two ways:
  // - if SocketsHttpHandler is avaiable (cloud and local), we use that
  // - we provide a Configuration record when initializing, that allows
  //   telemetry/etc. (emptyConfig can be used for no telemetry/etc)


  module SocketBasedHandler =
    // There has been quite a history of .NET's HttpClient having problems,
    // including socket exhaustion and DNS results not expiring.
    // The history is outlined well here:
    // https://www.stevejgordon.co.uk/httpclient-connection-pooling-in-dotnet-core
    //
    // As of .NET 6 it seems we no longer need to worry about either socket
    // exhaustion or DNS issues. It appears that we can use either multiple HTTP
    // clients or just one, we use just one for efficiency.
    // See https://docs.microsoft.com/en-us/aspnet/core/fundamentals/http-requests?view=aspnetcore-7.0#alternatives-to-ihttpclientfactory
    //
    // Note that the number of sockets was verified manually, with:
    // `sudo netstat -apn | grep _WAIT`
    let handler (config : Configuration) : HttpMessageHandler =
      let connectionFilter
        (context : SocketsHttpConnectionContext)
        (cancellationToken : System.Threading.CancellationToken)
        : ValueTask<Stream> =
        vtask {
          try
            // While this DNS call is expensive, it should be cached.
            //
            // Connect against the resolved IPs, not the hostname: the OS
            // would otherwise re-resolve at connect time and a malicious
            // resolver can return public-then-private within ms,
            // bypassing the allow-list (DNS-rebinding TOCTOU).
            let ips = System.Net.Dns.GetHostAddresses context.DnsEndPoint.Host

            if Array.isEmpty ips || not (Array.forall config.allowedIP ips) then
              // Use this to hide more specific errors when looking at loopback
              Exception.raiseInternal "Could not connect" []

            let socket =
              new System.Net.Sockets.Socket(
                System.Net.Sockets.SocketType.Stream,
                System.Net.Sockets.ProtocolType.Tcp
              )
            socket.NoDelay <- true

            let endpoint = System.Net.IPEndPoint(ips[0], context.DnsEndPoint.Port)
            do! socket.ConnectAsync(endpoint, cancellationToken)
            return new System.Net.Sockets.NetworkStream(socket, true)
          with :? System.ArgumentException ->
            return Exception.raiseInternal "Could not connect" []
        }
      new SocketsHttpHandler(
        // Avoid DNS problems
        PooledConnectionIdleTimeout = System.TimeSpan.FromMinutes 5.0,
        PooledConnectionLifetime = System.TimeSpan.FromMinutes 10.0,
        ConnectTimeout = System.TimeSpan.FromSeconds 10.0,

        // HttpClientTODO avail function that handles redirect behaviour
        AllowAutoRedirect = false,

        // Users share the HttpClient, don't let them share cookies!
        UseCookies = false,

        // HttpClientTODO avail functions to compress/decompress with common
        // compression algorithms (gzip, brottli, deflate)
        //
        // HttpClientTODO consider: is there any reason to think that ASP.NET
        // does something fancy such that automatic .net httpclient -level
        // decompression would be notably more efficient than doing so 'manually'
        // via some function? There will certainly be more bytes passed around -
        // probably not a big deal?
        AutomaticDecompression = System.Net.DecompressionMethods.None,

        // Don't add a RequestId header for opentelemetry
        ActivityHeadersPropagator = null,

        // Don't allow access to local stuff
        ConnectCallback = connectionFilter
      )

  module WasmHandler =
    let handler (_config : Configuration) : HttpMessageHandler =
      new HttpClientHandler(
        // These settings are also enabled in SocketBasedHandler - see comments above for discussion
        AllowAutoRedirect = false
      // These can't be set in WASM, even though they exist (PlatformNotSupportedException)
      // UseCookies = false,
      // AutomaticDecompression = System.Net.DecompressionMethods.None
      )

  let create (config : Configuration) : HttpClient =
    let handler =
      if SocketsHttpHandler.IsSupported then
        SocketBasedHandler.handler config
      else
        // For Blazor
        WasmHandler.handler config

    new HttpClient(
      handler,
      disposeHandler = false,
      Timeout = System.TimeSpan.FromSeconds 30.0,
      MaxResponseContentBufferSize = 1024L * 1024L * 100L // 100MB
    )


/// Unbounded configuration. No SSRF guards, no scheme/header limits.
/// Suitable only for trusted environments where the caller IS the code
/// author (e.g. a power-user shelling out from their own CLI session
/// to a localhost dev server). Don't wire this into anything that
/// runs untrusted Dark code.
let looseConfig =
  { timeoutInMs = 30000
    allowedIP = fun _ -> true
    allowedHost = fun _ -> true
    allowedScheme = fun _ -> true
    allowedHeaders = fun _ -> true
    telemetryInitialize = fun f -> f ()
    telemetryAddTag = fun _ _ -> ()
    telemetryAddException = fun _ _ -> () }


/// SSRF guard: predicates that block access to internal IP ranges,
/// loopback, link-local, and well-known cloud-provider metadata
/// endpoints.
///
/// Used by `strictConfig` (below) to compose a configuration safe
/// for HTTP servers that execute untrusted handler code. Exposed
/// directly so callers can build a custom configuration that mixes
/// these predicates with their own allow-lists.
module LocalAccess =

  // RFC 1918 private ranges
  // https://datatracker.ietf.org/doc/html/rfc1918#section-3
  let private ten = System.Net.IPNetwork.Parse "10.0.0.0/8"
  let private oneSevenTwo = System.Net.IPNetwork.Parse "172.16.0.0/12"
  let private oneNineTwo = System.Net.IPNetwork.Parse "192.168.0.0/16"

  // RFC 6598 carrier-grade NAT
  // https://datatracker.ietf.org/doc/html/rfc6598#section-7
  let private oneHundred = System.Net.IPNetwork.Parse "100.64.0.0/10"

  // Google Cloud Run private endpoints (199.36.153.4/30, 199.36.153.8/30)
  let private oneNineNineFour = System.Net.IPNetwork.Parse "199.36.153.4/30"
  let private oneNineNineEight = System.Net.IPNetwork.Parse "199.36.153.8/30"

  // 169.254.0.0/16 — link-local addresses (incl. cloud metadata IPs)
  let private oneSixNine = System.Net.IPNetwork.Parse "169.254.0.0/16"

  let private zero = System.Net.IPAddress.Parse "0.0.0.0"

  let bannedIPv4 (ip : System.Net.IPAddress) : bool =
    System.Net.IPAddress.IsLoopback ip // 127.*
    || ten.Contains ip
    || oneSevenTwo.Contains ip
    || oneNineTwo.Contains ip
    || oneHundred.Contains ip
    || oneNineNineFour.Contains ip
    || oneNineNineEight.Contains ip
    || oneSixNine.Contains ip
    || zero = ip

  let bannedIp (ip : System.Net.IPAddress) : bool =
    if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetworkV6 then
      if ip.IsIPv4MappedToIPv6 then
        bannedIPv4 (ip.MapToIPv4())
      else
        ip.IsIPv6LinkLocal // ipv6 equivalent of 169.254.*
        || ip.IsIPv6SiteLocal // ipv6 equivalent of 10/172.16/192.168
        || System.Net.IPAddress.IsLoopback ip
    else if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork then
      bannedIPv4 ip
    else
      true // not ipv4 or ipv6, so banned

  let bannedHost (host : string) : bool =
    let host = host.Trim().ToLower()
    let badIP =
      let mutable ip = null
      if System.Net.IPAddress.TryParse(host, &ip) then bannedIp ip else false
    badIP
    || host = "localhost"
    || host = "metadata"
    || host = "metadata.google.internal"

  /// Disallow headers that would request the GCP Instance Metadata service.
  let hasInstanceMetadataHeader (headers : Headers.T) : bool =
    let eq = String.equalsCaseInsensitive
    headers
    |> List.find (fun (k, v) ->
      let (k, v) = (String.trim k, String.trim v)
      (eq k "Metadata-Flavor" && eq v "Google")
      // Old but allowed
      // https://cloud.google.com/compute/docs/metadata/overview#querying
      || (eq k "X-Google-Metadata-Request" && eq v "True"))
    |> Option.isSome


/// Default configuration. SSRF guards on (no internal IPs, no
/// loopback, no link-local, no cloud-metadata endpoints), scheme
/// restricted to http/https. Safe to wire into anything that runs
/// untrusted Dark code (e.g. `darklang serve` handlers, package
/// initialisation evals). For the rare local-dev case where the
/// caller authored the code themselves and wants `localhost`
/// access, wire `looseConfig` instead.
let defaultConfig : Configuration =
  { looseConfig with
      allowedIP = fun ip -> not (LocalAccess.bannedIp ip)
      allowedHost = fun host -> not (LocalAccess.bannedHost host)
      allowedScheme = fun scheme -> scheme = "https" || scheme = "http"
      allowedHeaders =
        fun headers -> not (LocalAccess.hasInstanceMetadataHeader headers) }


/// Compatibility alias for callers (TestUtils, etc.) that referenced
/// `strictConfig` when the default was unsafe. New code should just
/// use `defaultConfig`.
let strictConfig : Configuration = defaultConfig


// ————————————————————————————————————————————————————————————
// Shared request-prep helpers used by both the buffered and the
// streaming builtins. Pulled out so that the duplication between
// the two paths is just the SendAsync call + response handling.
// ————————————————————————————————————————————————————————————

/// Run the configured allow-lists over `httpRequest` and return the
/// canonical request URI string. May throw `System.UriFormatException`
/// or `System.ArgumentException` for malformed URIs — callers catch
/// at the outer try block.
let private validateAndBuildUri
  (config : Configuration)
  (httpRequest : Request)
  : Result<string, RequestError.RequestError> =
  let uri = System.Uri(httpRequest.url, System.UriKind.Absolute)
  let host = uri.Host.Trim().ToLower()
  if not (config.allowedHost host) then
    Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidHost)
  elif not (config.allowedHeaders httpRequest.headers) then
    Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidRequest)
  elif not (config.allowedScheme uri.Scheme) then
    Error(RequestError.BadUrl BadUrl.BadUrlDetails.UnsupportedProtocol)
  else
    System.UriBuilder(
      Scheme = uri.Scheme,
      Host = uri.Host,
      Port = uri.Port,
      Path = uri.AbsolutePath,
      Query = uri.Query
    )
    |> string
    |> Ok


/// Build the request message used for both the buffered and the
/// streaming path. Header version policy is the same in both cases.
let private buildHttpRequestMessage
  (method : Method)
  (reqUri : string)
  (body : Body)
  : HttpRequestMessage =
  new HttpRequestMessage(
    method,
    reqUri,
    Content = new ByteArrayContent(body),
    // Support both Http 2.0 and 3.0
    // https://learn.microsoft.com/en-us/dotnet/api/system.net.http.httpversionpolicy?view=net-7.0
    // TODO: test this (against requestbin or something that allows us
    // to control the HTTP protocol version)
    Version = System.Net.HttpVersion.Version30,
    VersionPolicy = HttpVersionPolicy.RequestVersionOrLower
  )


/// Apply the caller-supplied headers to `req`. Content-Type goes on
/// `req.Content.Headers`; everything else lives on `req.Headers`
/// unless .NET rejects it (then we fall back to Content.Headers).
/// Returns `Error InvalidContentType` if Content-Type can't be
/// parsed; otherwise `Ok ()`.
let private applyRequestHeaders
  (req : HttpRequestMessage)
  (headers : Headers.T)
  : Result<unit, BadHeader.BadHeader> =
  headers
  |> List.map (fun (k, v) ->
    // .NET handles "content headers" separately from other headers.
    // They're put into `req.Content.Headers` rather than `req.Headers`
    // https://docs.microsoft.com/en-us/dotnet/api/system.net.http.headers.httpcontentheaders?view=net-6.0
    if String.equalsCaseInsensitive k "content-type" then
      try
        req.Content.Headers.ContentType <- Headers.MediaTypeHeaderValue.Parse(v)
        Ok()
      with :? System.FormatException ->
        Error BadHeader.InvalidContentType
    else
      let added = req.Headers.TryAddWithoutValidation(k, v)
      // Headers are split between req.Headers and req.Content.Headers
      // so just try both.
      if not added then req.Content.Headers.Add(k, v)
      Ok())
  |> Result.collect
  |> Result.map (fun _ -> ())


/// Flatten the response and content header collections into a flat
/// list, joining repeated values with ',' as ASP.NET surfaces them.
let private headersForAspNetResponse
  (response : HttpResponseMessage)
  : List<string * string> =
  let fromAspNetHeaders (headers : Headers.HttpHeaders) : List<string * string> =
    headers
    |> Seq.map Tuple2.fromKeyValuePair
    |> Seq.map (fun (k, v) -> (k, v |> Seq.toList |> String.concat ","))
    |> Seq.toList
  fromAspNetHeaders response.Headers @ fromAspNetHeaders response.Content.Headers


/// Lowercase keys + values across a response-header list. Both the
/// buffered and streaming paths surface the same flattened shape.
let private normalizeResponseHeaders
  (headers : List<string * string>)
  : List<string * string> =
  headers |> List.map (fun (k, v) -> (String.toLowercase k, String.toLowercase v))

let makeRequest
  (config : Configuration)
  (httpClient : HttpClient)
  (httpRequest : Request)
  : Task<RequestResult> =
  config.telemetryInitialize (fun () ->
    task {
      config.telemetryAddTag "request.url" httpRequest.url
      config.telemetryAddTag "request.method" httpRequest.method
      try
        match validateAndBuildUri config httpRequest with
        | Error e -> return Error e
        | Ok reqUri ->
          use req =
            buildHttpRequestMessage httpRequest.method reqUri httpRequest.body

          match applyRequestHeaders req httpRequest.headers with
          | Error e -> return Error(RequestError.RequestError.BadHeader e)
          | Ok() ->
            config.telemetryAddTag
              "request.content_type"
              req.Content.Headers.ContentType
            config.telemetryAddTag
              "request.content_length"
              req.Content.Headers.ContentLength

            // Allow timeout
            let source =
              new System.Threading.CancellationTokenSource(config.timeoutInMs)
            let cancellationToken = source.Token

            use! response = httpClient.SendAsync(req, cancellationToken)

            config.telemetryAddTag "response.status_code" response.StatusCode
            config.telemetryAddTag "response.version" response.Version
            use! responseStream = response.Content.ReadAsStreamAsync()
            use memoryStream = new MemoryStream()
            do! responseStream.CopyToAsync(memoryStream)
            let respBody = memoryStream.ToArray()

            let headers =
              response |> headersForAspNetResponse |> normalizeResponseHeaders

            return
              { statusCode = int response.StatusCode
                headers = headers
                body = respBody }
              |> Ok

      with
      | :? TaskCanceledException ->
        config.telemetryAddTag "error" true
        config.telemetryAddTag "error.msg" "Timeout"
        return Error RequestError.Timeout

      | :? System.ArgumentException as e when
        e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')"
        ->
        // We know of one specific case indicating Unsupported Protocol
        // If we get this otherwise, return generic error
        config.telemetryAddTag "error" true
        config.telemetryAddTag "error.msg" "Unsupported Protocol"
        return Error(RequestError.BadUrl BadUrl.BadUrlDetails.UnsupportedProtocol)

      | :? System.UriFormatException ->
        config.telemetryAddTag "error" true
        config.telemetryAddTag "error.msg" "Invalid URI"
        return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidUri)
      | :? IOException -> return Error(RequestError.NetworkError)
      | :? HttpRequestException as e ->
        // This is a bit of an awkward case. I'm unsure how it fits into our model.
        // We've made a request, and _potentially_ (according to .NET) have a status
        // code. That should return some sort of Error - but our Error case type
        // doesn't have a good slot to include the status code. We could have a new
        // case of `| ErrorHandlingResponse of statusCode: int` but that feels wrong.
        let statusCode = if e.StatusCode.HasValue then int e.StatusCode.Value else 0

        config.telemetryAddException [ "error.status_code", statusCode ] e

        return Error(RequestError.NetworkError)
    })


let headersType = TList(TTuple(TString, TString, []))


open LibExecution.Builtin.Shortcuts


/// Open a streaming request. Unlike [makeRequest], the body isn't
/// copied into a byte[] — the caller gets back the live
/// HttpResponseMessage plus its readable stream, which must be
/// disposed when the consumer is done.
///
/// Used by the `HttpClient.stream` builtin to surface a lazy
/// `Stream<UInt8>` body to Dark.
let openStreamingRequest
  (config : Configuration)
  (httpClient : HttpClient)
  (httpRequest : Request)
  : Task<Result<HttpResponseMessage * Headers.T, RequestError.RequestError>> =
  // Deliberately skips [config.telemetryInitialize] — its signature
  // is specialised to the buffered `RequestResult`. Inline telemetry
  // calls here cover the same tags.
  task {
    config.telemetryAddTag "request.url" httpRequest.url
    config.telemetryAddTag "request.method" httpRequest.method
    config.telemetryAddTag "request.streaming" true
    try
      match validateAndBuildUri config httpRequest with
      | Error e -> return Error e
      | Ok reqUri ->
        let req = buildHttpRequestMessage httpRequest.method reqUri httpRequest.body

        match applyRequestHeaders req httpRequest.headers with
        | Error e -> return Error(RequestError.RequestError.BadHeader e)
        | Ok() ->
          let source =
            new System.Threading.CancellationTokenSource(config.timeoutInMs)
          // Deliberately no `use!` — the response must outlive this
          // function; the caller (or the FromIO disposer) owns it.
          let! response =
            httpClient.SendAsync(
              req,
              HttpCompletionOption.ResponseHeadersRead,
              source.Token
            )

          config.telemetryAddTag "response.status_code" response.StatusCode

          let headers =
            response |> headersForAspNetResponse |> normalizeResponseHeaders

          return Ok(response, headers)
    with
    | :? TaskCanceledException -> return Error RequestError.Timeout
    | :? System.ArgumentException as e when
      e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')"
      ->
      return Error(RequestError.BadUrl BadUrl.BadUrlDetails.UnsupportedProtocol)
    | :? System.UriFormatException ->
      return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidUri)
    | :? IOException -> return Error(RequestError.NetworkError)
    | :? HttpRequestException -> return Error(RequestError.NetworkError)
  }


let streamResponseType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.streamResponse ())


let fns (config : Configuration) : List<BuiltInFn> =
  let httpClient = BaseClient.create config
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
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where
      the response is wrapped in {{ Ok }} if a response was successfully
      received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        let responseTypeOK = KTCustomType(responseOKType (), [])
        let responseTypeErr = KTCustomType(responseErrorType (), [])
        let resultOk = Dval.resultOk responseTypeOK responseTypeErr
        let resultError = Dval.resultError responseTypeOK responseTypeErr
        (function
        | state,
          vm,
          _,
          [ DString method; DString uri; DList(_, reqHeaders); DBlob bodyRef ] ->
          uply {
            // precise check: this exact method+URL must be covered (the gate only checked http presence).
            LibExecution.CapabilityCheck.requireHttp state.grantedCaps method uri
            let! reqBodyBytes = Blob.readBytes state bodyRef
            let! (reqHeaders : Result<List<string * string>, BadHeader.BadHeader>) =
              reqHeaders
              |> Ply.List.mapSequentially (fun item ->
                uply {
                  match item with
                  | DTuple(DString k, DString v, []) ->
                    let k = String.trim k
                    if k = "" then
                      // CLEANUP reconsider if we should error here
                      return Error BadHeader.EmptyKey
                    else
                      return Ok((k, v))

                  | notAPair ->
                    return
                      RTE.Applications.FnParameterNotExpectedType(
                        FQFnName.fqPackage (
                          PackageRefs.Fn.Stdlib.HttpClient.request ()
                        ),
                        2,
                        "headers",
                        VT.list (VT.tuple VT.string VT.string []),
                        Dval.toValueType notAPair,
                        notAPair
                      )
                      |> RTE.Apply
                      |> raiseRTE vm.threadID

                })
              |> Ply.map Result.collect

            let method =
              try
                Some(HttpMethod method)
              with _ ->
                None

            let! (result : Result<Dval, RequestError.RequestError>) =
              uply {
                match reqHeaders, method with
                | Ok reqHeaders, Some method ->
                  let request =
                    { url = uri
                      method = method
                      headers = reqHeaders
                      body = reqBodyBytes }

                  let! response = makeRequest config httpClient request

                  match response with
                  | Ok response ->
                    let responseHeaders =
                      response.headers
                      |> List.map (fun (k, v) ->
                        DTuple(
                          DString(String.toLowercase k),
                          DString(String.toLowercase v),
                          []
                        ))
                      |> Dval.list (KTTuple(VT.string, VT.string, []))

                    let typ =
                      FQTypeName.fqPackage (
                        PackageRefs.Type.Stdlib.HttpClient.response ()
                      )

                    let fields =
                      [ ("statusCode", Dval.int (bigint response.statusCode))
                        ("headers", responseHeaders)
                        ("body", Blob.newEphemeral response.body) ]

                    return Ok(DRecord(typ, typ, [], Map fields) |> resultOk)

                  | Error err -> return Error err

                | Error reqHeadersErr, _ ->
                  return Error(RequestError.BadHeader reqHeadersErr)

                | _, None -> return Error RequestError.BadMethod
              }
            match result with
            | Ok result -> return result
            | Error err -> return resultError (RequestError.toDT err)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.http
      deprecated = NotDeprecated }


    // ——————————————————————————————————————————————————————————
    // Streaming HTTP.
    //
    // The body is not buffered into a byte[]; instead the response's
    // readable Stream is wrapped in a chunked DStream. Bulk consumers
    // (`streamToBlob`) pull whole buffers via `nextChunk`; byte-wise
    // consumers (`streamNext`) see one `DUInt8` at a time synthesised
    // from the same buffer — no boxing until a byte-wise consumer
    // actually asks for bytes.
    //
    // The disposer tears down the HttpResponseMessage + response
    // stream when the consumer drains to EOF or calls
    // `Builtin.streamClose`. Abandoning a stream mid-drain falls back
    // to the GC-triggered finalizer on `Dval.StreamFinalizer`, which
    // runs the same disposer chain when the DStream becomes
    // unreachable.
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
        "Make a streaming HTTP call to <param uri>. Returns a <type StreamResponse>
      whose `body` is a lazy <type Stream> that yields bytes as they arrive.
      Drain with `Builtin.streamToList`/`streamToBlob`, or compose with
      `streamMap`/`streamFilter`/etc. The underlying HTTP response is released
      when the stream is drained to completion or `Builtin.streamClose`d."
      fn =
        let streamTypeOk = KTCustomType(streamResponseType (), [])
        let streamTypeErr = KTCustomType(responseErrorType (), [])
        let resultOk = Dval.resultOk streamTypeOk streamTypeErr
        let resultError = Dval.resultError streamTypeOk streamTypeErr
        (function
        | state, vm, _, [ DString method; DString uri; DList(_, reqHeaders) ] ->
          uply {
            // precise check: this exact method+URL must be covered (the gate only checked http presence).
            LibExecution.CapabilityCheck.requireHttp state.grantedCaps method uri
            let! (reqHeaders : Result<List<string * string>, BadHeader.BadHeader>) =
              reqHeaders
              |> Ply.List.mapSequentially (fun item ->
                uply {
                  match item with
                  | DTuple(DString k, DString v, []) ->
                    let k = String.trim k
                    if k = "" then
                      return Error BadHeader.EmptyKey
                    else
                      return Ok((k, v))
                  | notAPair ->
                    return
                      RTE.Applications.FnParameterNotExpectedType(
                        FQFnName.fqPackage (
                          PackageRefs.Fn.Stdlib.HttpClient.stream ()
                        ),
                        2,
                        "headers",
                        VT.list (VT.tuple VT.string VT.string []),
                        Dval.toValueType notAPair,
                        notAPair
                      )
                      |> RTE.Apply
                      |> raiseRTE vm.threadID
                })
              |> Ply.map Result.collect

            let method =
              try
                Some(HttpMethod method)
              with _ ->
                None

            match reqHeaders, method with
            | Ok reqHeaders, Some method ->
              let request : Request =
                { url = uri; method = method; headers = reqHeaders; body = [||] }

              let! setup = openStreamingRequest config httpClient request
              match setup with
              | Error err -> return resultError (RequestError.toDT err)
              | Ok(response, respHeaders) ->
                let! responseStream = response.Content.ReadAsStreamAsync()

                // Hand back a whole chunk per pull when the consumer
                // wants bytes in bulk. `streamToBlob` and the SSE
                // byte accumulator route through
                // `Stream.readChunk`, which uses this callback
                // directly — no per-byte `DUInt8` boxing on the hot
                // path. `Stream.newChunked` synthesises a
                // byte-wise `next` from the same source so
                // `streamNext` semantics are preserved.
                let nextChunk (maxBytes : int) : Ply<Option<byte[]>> =
                  uply {
                    let cap = max 1 maxBytes
                    let buf = Array.zeroCreate<byte> cap
                    let! n = responseStream.ReadAsync(buf, 0, cap)
                    if n = 0 then
                      return None
                    elif n = cap then
                      return Some buf
                    else
                      let trimmed = Array.zeroCreate<byte> n
                      System.Array.Copy(buf, 0, trimmed, 0, n)
                      return Some trimmed
                  }

                // Released on drain-to-EOF or streamClose. Ordered
                // response first so the stream is closed before the
                // message — Dispose chains naturally either way, but
                // this mirrors idiomatic .NET cleanup.
                let disposer () =
                  responseStream.Dispose()
                  response.Dispose()

                let body = Stream.newChunked VT.uint8 nextChunk (Some disposer)

                let headersDval =
                  respHeaders
                  |> List.map (fun (k, v) ->
                    DTuple(
                      DString(String.toLowercase k),
                      DString(String.toLowercase v),
                      []
                    ))
                  |> Dval.list (KTTuple(VT.string, VT.string, []))

                let typ = streamResponseType ()
                let fields =
                  [ ("statusCode", Dval.int (bigint (int response.StatusCode)))
                    ("headers", headersDval)
                    ("body", body) ]
                return DRecord(typ, typ, [], Map fields) |> resultOk

            | Error reqHeadersErr, _ ->
              return
                resultError (
                  RequestError.toDT (RequestError.BadHeader reqHeadersErr)
                )
            | _, None ->
              return resultError (RequestError.toDT RequestError.BadMethod)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.http
      deprecated = NotDeprecated } ]


let builtins config = Builtin.make [] (fns config)
