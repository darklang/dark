module BuiltinExecution.Libs.HttpClient

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
module Exe = Execution

type Method = HttpMethod

let responseOKType = FQTypeName.fqPackage PackageRefs.Type.Stdlib.HttpClient.response
let responseErrorType =
  FQTypeName.fqPackage PackageRefs.Type.Stdlib.HttpClient.requestError

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
    let typeName = FQTypeName.fqPackage PackageRefs.Type.Stdlib.HttpClient.badHeader
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
      FQTypeName.fqPackage PackageRefs.Type.Stdlib.HttpClient.badUrlDetails
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
      FQTypeName.fqPackage PackageRefs.Type.Stdlib.HttpClient.requestError
    DEnum(typeName, typeName, [], caseName, fields)


type RequestResult = Result<Response, RequestError.RequestError>


// STREAMING TYPES

/// Result of initiating a streaming request (headers only, body streams later)
type StreamingResponse = { statusCode : int; headers : Headers.T }

/// Chunk types for streaming
module StreamChunk =
  type StreamChunk =
    | Data of byte array
    | Done
    | Error of string

  let toDT (chunk : StreamChunk) : Dval =
    let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.HttpClient.streamChunk
    let (caseName, fields) =
      match chunk with
      | Data bytes -> "Data", [ Dval.byteArrayToDvalList bytes ]
      | Done -> "Done", []
      | Error msg -> "Error", [ DString msg ]
    DEnum(typeName, typeName, [], caseName, fields)

/// SSE event parsed from a Server-Sent Events stream
type SSEEvent = { data : string; eventType : string; id : string }

module SSEChunk =
  type SSEChunk =
    | Event of SSEEvent
    | Done
    | Error of string

  let toDT (chunk : SSEChunk) : Dval =
    let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.HttpClient.sseChunk
    let (caseName, fields) =
      match chunk with
      | Event evt ->
        let evtTypeName =
          FQTypeName.fqPackage PackageIDs.Type.Stdlib.HttpClient.sseEvent
        let evtDval =
          DRecord(
            evtTypeName,
            evtTypeName,
            [],
            Map
              [ ("data", DString evt.data)
                ("eventType", DString evt.eventType)
                ("id", DString evt.id) ]
          )
        "Event", [ evtDval ]
      | Done -> "Done", []
      | Error msg -> "Error", [ DString msg ]
    DEnum(typeName, typeName, [], caseName, fields)

type StreamingResult = Result<StreamingResponse, RequestError.RequestError>


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
            // While this DNS call is expensive, it should be cached
            let ips = System.Net.Dns.GetHostAddresses context.DnsEndPoint.Host

            if not (Array.forall config.allowedIP ips) then
              // Use this to hide more specific errors when looking at loopback
              Exception.raiseInternal "Could not connect" []

            let socket =
              new System.Net.Sockets.Socket(
                System.Net.Sockets.SocketType.Stream,
                System.Net.Sockets.ProtocolType.Tcp
              )
            socket.NoDelay <- true

            do! socket.ConnectAsync(context.DnsEndPoint, cancellationToken)
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


/// This configuration has no limits, and so is only suitable for trusted
/// environments (the command line), and is not suitable for untrusted environments
/// (eg the cloud)
let defaultConfig =
  { timeoutInMs = 30000
    allowedIP = fun _ -> true
    allowedHost = fun _ -> true
    allowedScheme = fun _ -> true
    allowedHeaders = fun _ -> true
    telemetryInitialize = fun f -> f ()
    telemetryAddTag = fun _ _ -> ()
    telemetryAddException = fun _ _ -> () }

let makeRequest
  (config : Configuration)
  (httpClient : HttpClient)
  (httpRequest : Request)
  : Task<RequestResult> =
  config.telemetryInitialize (fun () ->
    task {
      config.telemetryAddTag "request.url" httpRequest.url
      config.telemetryAddTag "request.method" httpRequest.method
      config.telemetryAddTag "request.method" httpRequest.method
      try
        let uri = System.Uri(httpRequest.url, System.UriKind.Absolute)

        let host = uri.Host.Trim().ToLower()
        if not (config.allowedHost host) then
          return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidHost)
        else if not (config.allowedHeaders httpRequest.headers) then
          return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidRequest)
        else if not (config.allowedScheme uri.Scheme) then
          return Error(RequestError.BadUrl BadUrl.BadUrlDetails.UnsupportedProtocol)
        else
          let reqUri =
            System.UriBuilder(
              Scheme = uri.Scheme,
              Host = uri.Host,
              Port = uri.Port,
              Path = uri.AbsolutePath,
              Query = uri.Query
            )
            |> string

          use req =
            new HttpRequestMessage(
              httpRequest.method,
              reqUri,
              Content = new ByteArrayContent(httpRequest.body),

              // Support both Http 2.0 and 3.0
              // https://learn.microsoft.com/en-us/dotnet/api/system.net.http.httpversionpolicy?view=net-7.0
              // TODO: test this (against requestbin or something that allows us to control the HTTP protocol version)
              Version = System.Net.HttpVersion.Version30,
              VersionPolicy = HttpVersionPolicy.RequestVersionOrLower
            )

          // headers
          let headerResults =
            httpRequest.headers
            |> List.map (fun (k, v) ->
              // .NET handles "content headers" separately from other headers.
              // They're put into `req.Content.Headers` rather than `req.Headers`
              // https://docs.microsoft.com/en-us/dotnet/api/system.net.http.headers.httpcontentheaders?view=net-6.0
              if String.equalsCaseInsensitive k "content-type" then
                try
                  req.Content.Headers.ContentType <-
                    Headers.MediaTypeHeaderValue.Parse(v)
                  Ok()
                with :? System.FormatException ->
                  Error BadHeader.InvalidContentType
              else
                let added = req.Headers.TryAddWithoutValidation(k, v)

                // Headers are split between req.Headers and req.Content.Headers so just try both
                if not added then req.Content.Headers.Add(k, v)
                Ok())


          match Result.collect headerResults with
          | Ok _ ->
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

            let headersForAspNetResponse
              (response : HttpResponseMessage)
              : List<string * string> =
              let fromAspNetHeaders
                (headers : Headers.HttpHeaders)
                : List<string * string> =
                headers
                |> Seq.map Tuple2.fromKeyValuePair
                |> Seq.map (fun (k, v) -> (k, v |> Seq.toList |> String.concat ","))
                |> Seq.toList
              fromAspNetHeaders response.Headers
              @ fromAspNetHeaders response.Content.Headers


            let headers =
              response
              |> headersForAspNetResponse
              |> List.map (fun (k, v) ->
                (String.toLowercase k, String.toLowercase v))

            return
              { statusCode = int response.StatusCode
                headers = headers
                body = respBody }
              |> Ok

          | Error e -> return Error(RequestError.RequestError.BadHeader e)

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


/// Callback type for raw streaming - receives a chunk, returns true to continue
type StreamCallback = StreamChunk.StreamChunk -> Task<bool>

/// Callback type for SSE streaming - receives an SSE chunk, returns true to continue
type SSECallback = SSEChunk.SSEChunk -> Task<bool>

/// Helper to set up an HTTP request message with headers, returning the validated request
/// or a RequestError. Shared by makeStreamingRequest and makeSSERequest.
let private setupStreamingRequest
  (config : Configuration)
  (httpClient : HttpClient)
  (httpRequest : Request)
  (cancellationToken : System.Threading.CancellationToken)
  : Task<Result<HttpResponseMessage * Headers.T, RequestError.RequestError>> =
  task {
    let uri = System.Uri(httpRequest.url, System.UriKind.Absolute)

    let host = uri.Host.Trim().ToLower()
    if not (config.allowedHost host) then
      return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidHost)
    else if not (config.allowedHeaders httpRequest.headers) then
      return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidRequest)
    else if not (config.allowedScheme uri.Scheme) then
      return Error(RequestError.BadUrl BadUrl.BadUrlDetails.UnsupportedProtocol)
    else
      let reqUri =
        System.UriBuilder(
          Scheme = uri.Scheme,
          Host = uri.Host,
          Port = uri.Port,
          Path = uri.AbsolutePath,
          Query = uri.Query
        )
        |> string

      use req =
        new HttpRequestMessage(
          httpRequest.method,
          reqUri,
          Content = new ByteArrayContent(httpRequest.body),
          Version = System.Net.HttpVersion.Version11
        )

      let headerResults =
        httpRequest.headers
        |> List.map (fun (k, v) ->
          if String.equalsCaseInsensitive k "content-type" then
            try
              req.Content.Headers.ContentType <-
                Headers.MediaTypeHeaderValue.Parse(v)
              Ok()
            with :? System.FormatException ->
              Error BadHeader.InvalidContentType
          else
            let added = req.Headers.TryAddWithoutValidation(k, v)
            if not added then req.Content.Headers.Add(k, v)
            Ok())

      match Result.collect headerResults with
      | Ok _ ->
        config.telemetryAddTag "request.content_type" req.Content.Headers.ContentType

        // Don't use `use!` here — caller needs the response alive for stream reading
        let! response =
          httpClient.SendAsync(
            req,
            HttpCompletionOption.ResponseHeadersRead,
            cancellationToken
          )

        config.telemetryAddTag "response.status_code" response.StatusCode
        config.telemetryAddTag "response.version" response.Version

        let headersForAspNetResponse (response : HttpResponseMessage) : Headers.T =
          let fromAspNetHeaders (headers : Headers.HttpHeaders) : Headers.T =
            headers
            |> Seq.map Tuple2.fromKeyValuePair
            |> Seq.map (fun (k, v) -> (k, v |> Seq.toList |> String.concat ","))
            |> Seq.toList
          fromAspNetHeaders response.Headers
          @ fromAspNetHeaders response.Content.Headers

        let headers =
          response
          |> headersForAspNetResponse
          |> List.map (fun (k, v) -> (String.toLowercase k, String.toLowercase v))

        return Ok(response, headers)

      | Error e -> return Error(RequestError.RequestError.BadHeader e)
  }

/// Shared wrapper for streaming HTTP requests.
/// Handles setup, telemetry, and error handling common to both raw and SSE streaming.
let private executeStreamingRequest
  (config : Configuration)
  (httpClient : HttpClient)
  (httpRequest : Request)
  (streamingTag : string)
  (onError : string -> Task<unit>)
  (processResponse :
    HttpResponseMessage
      -> Headers.T
      -> System.Threading.CancellationToken
      -> Task<StreamingResult>)
  : Task<StreamingResult> =
  task {
    config.telemetryAddTag "request.url" httpRequest.url
    config.telemetryAddTag "request.method" httpRequest.method
    config.telemetryAddTag streamingTag true
    try
      let source = new System.Threading.CancellationTokenSource(config.timeoutInMs)
      let cancellationToken = source.Token

      let! setupResult =
        setupStreamingRequest config httpClient httpRequest cancellationToken

      match setupResult with
      | Ok(response, headers) ->
        use _response = response
        return! processResponse response headers cancellationToken
      | Error e -> return Error e

    with
    | :? TaskCanceledException ->
      config.telemetryAddTag "error" true
      config.telemetryAddTag "error.msg" "Timeout"
      do! onError "Request timed out"
      return Error RequestError.Timeout

    | :? System.ArgumentException as e when
      e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')"
      ->
      config.telemetryAddTag "error" true
      config.telemetryAddTag "error.msg" "Unsupported Protocol"
      return Error(RequestError.BadUrl BadUrl.BadUrlDetails.UnsupportedProtocol)

    | :? System.UriFormatException ->
      config.telemetryAddTag "error" true
      config.telemetryAddTag "error.msg" "Invalid URI"
      return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidUri)

    | :? IOException as e ->
      do! onError $"Network error: {e.Message}"
      return Error(RequestError.NetworkError)

    | :? HttpRequestException as e ->
      let statusCode = if e.StatusCode.HasValue then int e.StatusCode.Value else 0
      config.telemetryAddException [ "error.status_code", statusCode ] e
      do! onError $"HTTP error: {e.Message}"
      return Error(RequestError.NetworkError)
  }


/// Make a raw streaming HTTP request.
/// Reads raw bytes in 8KB buffer chunks — no text interpretation or SSE parsing.
/// Calls the callback for each chunk; callback returns false to stop early.
let makeStreamingRequest
  (config : Configuration)
  (httpClient : HttpClient)
  (httpRequest : Request)
  (onChunk : StreamCallback)
  : Task<StreamingResult> =
  let onError msg =
    task {
      let! _ = onChunk (StreamChunk.Error msg)
      return ()
    }
  executeStreamingRequest
    config
    httpClient
    httpRequest
    "request.streaming"
    onError
    (fun response headers cancellationToken ->
      task {
        use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken)
        let buffer = Array.zeroCreate<byte> 8192

        let mutable reading = true
        let mutable totalBytes = 0L

        while reading do
          let! bytesRead =
            responseStream.ReadAsync(buffer, 0, buffer.Length, cancellationToken)
          if bytesRead = 0 then
            reading <- false
            let! _ = onChunk StreamChunk.Done
            ()
          else
            totalBytes <- totalBytes + int64 bytesRead
            let chunk = Array.sub buffer 0 bytesRead
            let! shouldContinue = onChunk (StreamChunk.Data chunk)
            if not shouldContinue then reading <- false

        config.telemetryAddTag "response.total_bytes" totalBytes

        return Ok { statusCode = int response.StatusCode; headers = headers }
      })


/// Make an SSE (Server-Sent Events) streaming HTTP request.
/// Parses W3C-compliant SSE: data/event/id fields, empty line = event boundary.
/// Calls the callback for each parsed event; callback returns false to stop early.
let makeSSERequest
  (config : Configuration)
  (httpClient : HttpClient)
  (httpRequest : Request)
  (onEvent : SSECallback)
  : Task<StreamingResult> =
  let onError msg =
    task {
      let! _ = onEvent (SSEChunk.Error msg)
      return ()
    }
  executeStreamingRequest
    config
    httpClient
    httpRequest
    "request.streaming.sse"
    onError
    (fun response headers cancellationToken ->
      task {
        use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken)
        use reader = new StreamReader(responseStream)

        let mutable reading = true
        let mutable totalBytes = 0L

        // Accumulated SSE event fields
        let mutable dataLines = System.Collections.Generic.List<string>()
        let mutable eventType = ""
        let mutable lastEventId = ""

        while reading do
          let! line = reader.ReadLineAsync()
          if isNull line then
            // EOF — dispatch any accumulated event, then Done.
            // Per W3C SSE spec: if the data buffer is empty, the event is not dispatched.
            // This means event/id-only frames (no data: field) are intentionally dropped.
            if dataLines.Count > 0 then
              let data = System.String.Join("\n", dataLines)
              let evt =
                { data = data
                  eventType = (if eventType = "" then "message" else eventType)
                  id = lastEventId }
              let! shouldContinue = onEvent (SSEChunk.Event evt)
              if not shouldContinue then reading <- false
              dataLines.Clear()
              eventType <- ""
            if reading then
              reading <- false
              let! _ = onEvent SSEChunk.Done
              ()
          else
            totalBytes <- totalBytes + int64 line.Length
            if line = "" then
              // Empty line = event boundary
              if dataLines.Count > 0 then
                let data = System.String.Join("\n", dataLines)
                let evt =
                  { data = data
                    eventType = (if eventType = "" then "message" else eventType)
                    id = lastEventId }
                let! shouldContinue = onEvent (SSEChunk.Event evt)
                if not shouldContinue then reading <- false
                dataLines.Clear()
                eventType <- ""
            else if line.StartsWith(":") then
              ()
            else
              // Parse field
              let colonIdx = line.IndexOf(':')
              let fieldName, fieldValue =
                if colonIdx < 0 then
                  line, ""
                else
                  let name = line.Substring(0, colonIdx)
                  let value = line.Substring(colonIdx + 1)
                  // Strip single leading space from value per spec
                  let value =
                    if value.StartsWith(" ") then value.Substring(1) else value
                  name, value
              match fieldName with
              | "data" -> dataLines.Add(fieldValue)
              | "event" -> eventType <- fieldValue
              | "id" -> lastEventId <- fieldValue
              | _ -> () // Unknown fields ignored per spec

        config.telemetryAddTag "response.total_bytes" totalBytes

        return Ok { statusCode = int response.StatusCode; headers = headers }
      })


let headersType = TList(TTuple(TString, TString, []))


open LibExecution.Builtin.Shortcuts


/// Shared implementation for streaming builtin functions (raw and SSE).
/// Handles header validation, method parsing, callback wrapping, response
/// construction, and error propagation — parameterized by chunk type and
/// request function.
let private streamingRequestHandler
  (config : Configuration)
  (httpClient : HttpClient)
  (packageFnId : System.Guid)
  (chunkToDval : 'chunk -> Dval)
  (makeRequest :
    Configuration
      -> HttpClient
      -> Request
      -> ('chunk -> Task<bool>)
      -> Task<StreamingResult>)
  =
  let streamingResponseType =
    FQTypeName.fqPackage PackageIDs.Type.Stdlib.HttpClient.streamingResponse
  let responseTypeOK = KTCustomType(streamingResponseType, [])
  let responseTypeErr = KTCustomType(responseErrorType, [])
  let resultOk = Dval.resultOk responseTypeOK responseTypeErr
  let resultError = Dval.resultError responseTypeOK responseTypeErr
  (function
  | exeState,
    vm,
    _,
    [ DString method
      DString uri
      DList(_, reqHeaders)
      DList(_, reqBody)
      DApplicable callbackApplicable ] ->
    uply {
      let! (reqHeaders : Result<List<string * string>, BadHeader.BadHeader>) =
        reqHeaders
        |> Ply.List.mapSequentially (fun item ->
          uply {
            match item with
            | DTuple(DString k, DString v, []) ->
              let k = String.trim k
              if k = "" then return Error BadHeader.EmptyKey else return Ok((k, v))
            | notAPair ->
              return
                RTE.Applications.FnParameterNotExpectedType(
                  FQFnName.Package packageFnId,
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
                body = Dval.dlistToByteArray reqBody }

            // Track callback errors to propagate after streaming completes
            let mutable callbackError : Option<RuntimeError.Error * CallStack> = None

            let onChunk (chunk : 'chunk) : Task<bool> =
              task {
                let chunkDval = chunkToDval chunk
                let! result =
                  Exe.executeApplicable
                    exeState
                    vm
                    callbackApplicable
                    (NEList.singleton chunkDval)
                match result with
                | Error(rte, cs) ->
                  callbackError <- Some(rte, cs)
                  return false
                | Ok _ -> return true
              }

            let! response = makeRequest config httpClient request onChunk

            // Propagate callback error if one occurred
            match callbackError with
            | Some(rte, _cs) -> return raiseRTE vm.threadID rte
            | None ->
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

                let fields =
                  [ ("statusCode", DInt64(int64 response.statusCode))
                    ("headers", responseHeaders) ]

                return
                  Ok(
                    DRecord(
                      streamingResponseType,
                      streamingResponseType,
                      [],
                      Map fields
                    )
                    |> resultOk
                  )

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


let fns (config : Configuration) : List<BuiltInFn> =
  let httpClient = BaseClient.create config
  [ { name = fn "httpClientRequest" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" (TList TUInt8) "" ]
      returnType =
        TypeReference.result
          (TCustomType(NR.ok responseOKType, []))
          (TCustomType(NR.ok responseErrorType, []))
      description =
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where
      the response is wrapped in {{ Ok }} if a response was successfully
      received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        let responseTypeOK = KTCustomType(responseOKType, [])
        let responseTypeErr = KTCustomType(responseErrorType, [])
        let resultOk = Dval.resultOk responseTypeOK responseTypeErr
        let resultError = Dval.resultError responseTypeOK responseTypeErr
        (function
        | _,
          vm,
          _,
          [ DString method; DString uri; DList(_, reqHeaders); DList(_, reqBody) ] ->
          uply {
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
                        FQFnName.fqPackage PackageRefs.Fn.Stdlib.HttpClient.request,
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
                      body = Dval.dlistToByteArray reqBody }

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
                      FQTypeName.fqPackage
                        PackageRefs.Type.Stdlib.HttpClient.response

                    let fields =
                      [ ("statusCode", DInt64(int64 response.statusCode))
                        ("headers", responseHeaders)
                        ("body", Dval.byteArrayToDvalList response.body) ]

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
      deprecated = NotDeprecated }


    // Streaming HTTP request builtin (raw bytes)
    { name = fn "httpClientRequestStreaming" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" (TList TUInt8) ""
          Param.makeWithArgs
            "onChunk"
            (TFn(
              NEList.singleton (
                TCustomType(
                  Ok(
                    FQTypeName.fqPackage
                      PackageIDs.Type.Stdlib.HttpClient.streamChunk
                  ),
                  []
                )
              ),
              TUnit
            ))
            "Callback function called for each chunk of raw bytes"
            [ "chunk" ] ]
      returnType =
        TypeReference.result
          (TCustomType(
            Ok(
              FQTypeName.fqPackage
                PackageIDs.Type.Stdlib.HttpClient.streamingResponse
            ),
            []
          ))
          (TCustomType(Ok responseErrorType, []))
      description =
        "Make streaming HTTP call to <param uri>. Delivers raw bytes as they arrive.
        Calls <param onChunk> for each chunk of data received. Returns a <type Result>
        with status code and headers when complete, or an error if the request failed.
        For SSE (Server-Sent Events) streams, use requestSSE instead."
      fn =
        streamingRequestHandler
          config
          httpClient
          PackageIDs.Fn.Stdlib.HttpClient.requestStreaming
          StreamChunk.toDT
          makeStreamingRequest
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // SSE (Server-Sent Events) streaming HTTP request builtin
    { name = fn "httpClientRequestSSE" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" (TList TUInt8) ""
          Param.makeWithArgs
            "onEvent"
            (TFn(
              NEList.singleton (
                TCustomType(
                  Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.HttpClient.sseChunk),
                  []
                )
              ),
              TUnit
            ))
            "Callback function called for each SSE event"
            [ "chunk" ] ]
      returnType =
        TypeReference.result
          (TCustomType(
            Ok(
              FQTypeName.fqPackage
                PackageIDs.Type.Stdlib.HttpClient.streamingResponse
            ),
            []
          ))
          (TCustomType(Ok responseErrorType, []))
      description =
        "Make SSE (Server-Sent Events) streaming HTTP call to <param uri>.
        Parses the SSE protocol and calls <param onEvent> for each parsed event.
        Returns a <type Result> with status code and headers when complete,
        or an error if the request failed."
      fn =
        streamingRequestHandler
          config
          httpClient
          PackageIDs.Fn.Stdlib.HttpClient.requestSSE
          SSEChunk.toDT
          makeSSERequest
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins config = Builtin.make [] (fns config)
