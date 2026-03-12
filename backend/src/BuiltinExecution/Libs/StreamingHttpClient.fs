/// Experimental streaming HTTP client — raw byte streaming and SSE parsing.
///
/// Separated from the stable HttpClient so we can iterate on the streaming
/// design without risk to the core request path.
module BuiltinExecution.Libs.StreamingHttpClient

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

// Re-use core types from the stable HttpClient module
module HC = BuiltinExecution.Libs.HttpClient


/// Result of initiating a streaming request (headers only, body streams later)
type StreamingResponse = { statusCode : int; headers : HC.Headers.T }

/// Chunk types for streaming
module StreamChunk =
  type StreamChunk =
    | Data of byte array
    | Done
    | Error of string

  let toDT (chunk : StreamChunk) : Dval =
    let typeName =
      FQTypeName.fqPackage (
        PackageRefs.Type.Stdlib.StreamingHttpClient.streamChunk ()
      )
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
    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.StreamingHttpClient.sseChunk ())
    let (caseName, fields) =
      match chunk with
      | Event evt ->
        let evtTypeName =
          FQTypeName.fqPackage (
            PackageRefs.Type.Stdlib.StreamingHttpClient.sseEvent ()
          )
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

type StreamingResult = Result<StreamingResponse, HC.RequestError.RequestError>


/// Callback type for raw streaming - receives a chunk, returns true to continue
type StreamCallback = StreamChunk.StreamChunk -> Task<bool>

/// Callback type for SSE streaming - receives an SSE chunk, returns true to continue
type SSECallback = SSEChunk.SSEChunk -> Task<bool>

/// Helper to set up an HTTP request message with headers, returning the validated request
/// or a RequestError. Shared by makeStreamingRequest and makeSSERequest.
let private setupStreamingRequest
  (config : HC.Configuration)
  (httpClient : HttpClient)
  (httpRequest : HC.Request)
  (cancellationToken : System.Threading.CancellationToken)
  : Task<Result<HttpResponseMessage * HC.Headers.T, HC.RequestError.RequestError>> =
  task {
    let uri = System.Uri(httpRequest.url, System.UriKind.Absolute)

    let host = uri.Host.Trim().ToLower()
    if not (config.allowedHost host) then
      return Error(HC.RequestError.BadUrl HC.BadUrl.BadUrlDetails.InvalidHost)
    else if not (config.allowedHeaders httpRequest.headers) then
      return Error(HC.RequestError.BadUrl HC.BadUrl.BadUrlDetails.InvalidRequest)
    else if not (config.allowedScheme uri.Scheme) then
      return
        Error(HC.RequestError.BadUrl HC.BadUrl.BadUrlDetails.UnsupportedProtocol)
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
          Version = System.Net.HttpVersion.Version30,
          VersionPolicy = HttpVersionPolicy.RequestVersionOrLower
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
              Error HC.BadHeader.InvalidContentType
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

        let headersForAspNetResponse
          (response : HttpResponseMessage)
          : HC.Headers.T =
          let fromAspNetHeaders (headers : Headers.HttpHeaders) : HC.Headers.T =
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

      | Error e -> return Error(HC.RequestError.RequestError.BadHeader e)
  }

/// Shared wrapper for streaming HTTP requests.
/// Handles setup, telemetry, and error handling common to both raw and SSE streaming.
let private executeStreamingRequest
  (config : HC.Configuration)
  (httpClient : HttpClient)
  (httpRequest : HC.Request)
  (streamingTag : string)
  (onError : string -> Task<unit>)
  (processResponse :
    HttpResponseMessage
      -> HC.Headers.T
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
      return Error HC.RequestError.Timeout

    | :? System.ArgumentException as e when
      e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')"
      ->
      config.telemetryAddTag "error" true
      config.telemetryAddTag "error.msg" "Unsupported Protocol"
      return
        Error(HC.RequestError.BadUrl HC.BadUrl.BadUrlDetails.UnsupportedProtocol)

    | :? System.UriFormatException ->
      config.telemetryAddTag "error" true
      config.telemetryAddTag "error.msg" "Invalid URI"
      return Error(HC.RequestError.BadUrl HC.BadUrl.BadUrlDetails.InvalidUri)

    | :? IOException as e ->
      do! onError $"Network error: {e.Message}"
      return Error(HC.RequestError.NetworkError)

    | :? HttpRequestException as e ->
      let statusCode = if e.StatusCode.HasValue then int e.StatusCode.Value else 0
      config.telemetryAddException [ "error.status_code", statusCode ] e
      do! onError $"HTTP error: {e.Message}"
      return Error(HC.RequestError.NetworkError)
  }


/// Make a raw streaming HTTP request.
/// Reads raw bytes in 8KB buffer chunks — no text interpretation or SSE parsing.
/// Calls the callback for each chunk; callback returns false to stop early.
let makeStreamingRequest
  (config : HC.Configuration)
  (httpClient : HttpClient)
  (httpRequest : HC.Request)
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
            if not shouldContinue then
              reading <- false
              let! _ = onChunk StreamChunk.Done
              ()

        config.telemetryAddTag "response.total_bytes" totalBytes

        return Ok { statusCode = int response.StatusCode; headers = headers }
      })


/// Make an SSE (Server-Sent Events) streaming HTTP request.
/// Parses W3C-compliant SSE: data/event/id fields, empty line = event boundary.
/// Calls the callback for each parsed event; callback returns false to stop early.
let makeSSERequest
  (config : HC.Configuration)
  (httpClient : HttpClient)
  (httpRequest : HC.Request)
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
        let mutable dataLines = ResizeArray<string>()
        let mutable eventType = ""
        let mutable lastEventId = ""

        /// Dispatch accumulated event if data is present, then clear for next event.
        /// Returns false if the callback signalled early stop.
        let dispatchEvent () =
          task {
            if dataLines.Count > 0 then
              let data = System.String.Join("\n", dataLines)
              let evt =
                { data = data
                  eventType = (if eventType = "" then "message" else eventType)
                  id = lastEventId }
              let! shouldContinue = onEvent (SSEChunk.Event evt)
              dataLines.Clear()
              eventType <- ""
              return shouldContinue
            else
              return true
          }

        while reading do
          let! line = reader.ReadLineAsync(cancellationToken)
          if isNull line then
            // EOF — dispatch any accumulated event, then Done.
            // Per W3C SSE spec: if the data buffer is empty, the event is not dispatched.
            // This means event/id-only frames (no data: field) are intentionally dropped.
            let! shouldContinue = dispatchEvent ()
            if not shouldContinue then reading <- false
            if reading then reading <- false
            let! _ = onEvent SSEChunk.Done
            ()
          else
            totalBytes <- totalBytes + int64 line.Length
            if line = "" then
              // Empty line = event boundary
              let! shouldContinue = dispatchEvent ()
              if not shouldContinue then
                reading <- false
                let! _ = onEvent SSEChunk.Done
                ()
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


open LibExecution.Builtin.Shortcuts


/// Shared implementation for streaming builtin functions (raw and SSE).
/// Handles header validation, method parsing, callback wrapping, response
/// construction, and error propagation — parameterized by chunk type and
/// request function.
let private streamingRequestHandler
  (config : HC.Configuration)
  (httpClient : HttpClient)
  (packageFnId : string)
  (chunkToDval : 'chunk -> Dval)
  (makeRequest :
    HC.Configuration
      -> HttpClient
      -> HC.Request
      -> ('chunk -> Task<bool>)
      -> Task<StreamingResult>)
  =
  let streamingResponseType =
    FQTypeName.fqPackage (
      PackageRefs.Type.Stdlib.StreamingHttpClient.streamingResponse ()
    )
  let responseTypeOK = KTCustomType(streamingResponseType, [])
  let responseTypeErr = KTCustomType(HC.responseErrorType (), [])
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
      let! (reqHeaders : Result<List<string * string>, HC.BadHeader.BadHeader>) =
        reqHeaders
        |> Ply.List.mapSequentially (fun item ->
          uply {
            match item with
            | DTuple(DString k, DString v, []) ->
              let k = String.trim k
              if k = "" then
                return Error HC.BadHeader.EmptyKey
              else
                return Ok((k, v))
            | notAPair ->
              return
                RTE.Applications.FnParameterNotExpectedType(
                  FQFnName.fqPackage packageFnId,
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

      let! (result : Result<Dval, HC.RequestError.RequestError>) =
        uply {
          match reqHeaders, method with
          | Ok reqHeaders, Some method ->
            let request : HC.Request =
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
                | Ok(DBool false) -> return false
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
            return Error(HC.RequestError.BadHeader reqHeadersErr)

          | _, None -> return Error HC.RequestError.BadMethod
        }
      match result with
      | Ok result -> return result
      | Error err -> return resultError (HC.RequestError.toDT err)
    }
  | _ -> incorrectArgs ())


let fns (config : HC.Configuration) : List<BuiltInFn> =
  let httpClient = HC.BaseClient.create config
  [ // Streaming HTTP request builtin (raw bytes)
    { name = fn "httpClientRequestStreaming" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" HC.headersType ""
          Param.make "body" (TList TUInt8) ""
          Param.makeWithArgs
            "onChunk"
            (TFn(
              NEList.singleton (
                TCustomType(
                  NR.ok (
                    FQTypeName.fqPackage (
                      PackageRefs.Type.Stdlib.StreamingHttpClient.streamChunk ()
                    )
                  ),
                  []
                )
              ),
              TBool
            ))
            "Callback function called for each chunk of raw bytes. Return false to stop."
            [ "chunk" ] ]
      returnType =
        TypeReference.result
          (TCustomType(
            NR.ok (
              FQTypeName.fqPackage (
                PackageRefs.Type.Stdlib.StreamingHttpClient.streamingResponse ()
              )
            ),
            []
          ))
          (TCustomType(NR.ok (HC.responseErrorType ()), []))
      description =
        "Make streaming HTTP call to <param uri>. Delivers raw bytes as they arrive.
        Calls <param onChunk> for each chunk of data received. Returns a <type Result>
        with status code and headers when complete, or an error if the request failed.
        For SSE (Server-Sent Events) streams, use requestSSE instead."
      fn =
        streamingRequestHandler
          config
          httpClient
          (PackageRefs.Fn.Stdlib.StreamingHttpClient.requestStreaming ())
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
          Param.make "headers" HC.headersType ""
          Param.make "body" (TList TUInt8) ""
          Param.makeWithArgs
            "onEvent"
            (TFn(
              NEList.singleton (
                TCustomType(
                  NR.ok (
                    FQTypeName.fqPackage (
                      PackageRefs.Type.Stdlib.StreamingHttpClient.sseChunk ()
                    )
                  ),
                  []
                )
              ),
              TBool
            ))
            "Callback function called for each SSE event. Return false to stop."
            [ "chunk" ] ]
      returnType =
        TypeReference.result
          (TCustomType(
            NR.ok (
              FQTypeName.fqPackage (
                PackageRefs.Type.Stdlib.StreamingHttpClient.streamingResponse ()
              )
            ),
            []
          ))
          (TCustomType(NR.ok (HC.responseErrorType ()), []))
      description =
        "Make SSE (Server-Sent Events) streaming HTTP call to <param uri>.
        Parses the SSE protocol and calls <param onEvent> for each parsed event.
        Returns a <type Result> with status code and headers when complete,
        or an error if the request failed."
      fn =
        streamingRequestHandler
          config
          httpClient
          (PackageRefs.Fn.Stdlib.StreamingHttpClient.requestSSE ())
          SSEChunk.toDT
          makeSSERequest
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins config = Builtin.make [] (fns config)
