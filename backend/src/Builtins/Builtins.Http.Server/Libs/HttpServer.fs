/// HTTP server builtin: starts a listener, hands every request to a Dark
/// handler fn, writes the response. All routing lives Dark-side.
module Builtins.Http.Server.Libs.HttpServer

open System
open System.IO
open System.Net
open System.Threading
open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Execution = LibExecution.Execution
module Http = Builtins.Http.Server.Http
module AT = LibExecution.AnalysisTypes
module Tracing = LibDB.Tracing


/// Default request body cap (30 MB).
let defaultMaxBodyBytes : int64 = 30L * 1024L * 1024L

/// HSTS header value matching the historical default for HTTP services.
let private hstsHeaderValue = "max-age=31536000; includeSubDomains; preload"


// ───────── pure stateless helpers ─────────

/// Read the request body up to `maxBytes`. Returns `Error()` when the
/// declared `Content-Length` exceeds the cap, OR when the actual stream
/// grows past it (covers chunked encoding where C-L is absent / -1).
let private readRequestBodyWithLimit
  (req : HttpListenerRequest)
  (maxBytes : int64)
  : Task<Result<byte[], unit>> =
  if req.ContentLength64 > maxBytes then
    Task.FromResult(Error())
  // A GET has no body, and every request paid 8 KB for the read buffer plus a `MemoryStream` and
  // its internal buffer to discover that. Byte arrays were 15% of the profile of a server returning
  // a constant string.
  elif req.ContentLength64 = 0L then
    Task.FromResult(Ok [||])
  // A declared length means the size is known, so read straight into an array of exactly that size:
  // no MemoryStream, no copy out of it, and the 8 KB scratch buffer is not needed either.
  elif req.ContentLength64 > 0L then
    task {
      let body = Array.zeroCreate (int req.ContentLength64)
      let mutable read = 0
      let mutable eof = false
      while read < body.Length && not eof do
        let! n = req.InputStream.ReadAsync(body, read, body.Length - read)
        if n = 0 then eof <- true else read <- read + n
      // A client that declared more than it sent gets what arrived, same as before.
      return Ok(if read = body.Length then body else Array.sub body 0 read)
    }
  else
    // Chunked, so the length is unknown (`ContentLength64` is -1) and it has to be accumulated.
    task {
      use ms = new MemoryStream()
      let buffer = Array.zeroCreate 8192
      let mutable totalRead = 0L
      let mutable keepReading = true
      let mutable overLimit = false
      while keepReading && not overLimit do
        let! n = req.InputStream.ReadAsync(buffer, 0, buffer.Length)
        if n = 0 then
          keepReading <- false
        else
          totalRead <- totalRead + int64 n
          if totalRead > maxBytes then
            overLimit <- true
          else
            do! ms.WriteAsync(buffer, 0, n)
      if overLimit then return Error() else return Ok(ms.ToArray())
    }


/// Flatten HttpListener's NameValueCollection into the (key, value) list
/// shape that `Http.Request` expects. Multi-value keys become multiple
/// entries. The final `x-http-method` entry is a CLEANUP — Dark handlers
/// still read it.
let private extractHeaders (req : HttpListenerRequest) : List<string * string> =
  let headers = ResizeArray<string * string>()
  for key in req.Headers.AllKeys do
    if not (isNull key) then
      let values = req.Headers.GetValues(key)
      if not (isNull values) then
        for value in values do
          headers.Add(key, value)
  ("x-http-method", req.HttpMethod) :: List.ofSeq headers


/// If `X-Forwarded-Proto: https` is present, rewrite scheme → https / port → 443.
let private canonicalizeUrlFromForwardedProto
  (url : string)
  (headers : List<string * string>)
  : string =
  let isHttps =
    headers
    |> List.exists (fun (k, v) ->
      String.equalsCaseInsensitive k "x-forwarded-proto"
      && String.equalsCaseInsensitive v "https")
  if isHttps then
    try
      let uri = System.UriBuilder(url)
      uri.Port <- 443
      uri.Scheme <- "https"
      string uri.Uri
    with _ ->
      url
  else
    url


/// Add `Server: darklang` + HSTS unless the handler already set them.
let private maybeInjectStandardHeaders
  (inject : bool)
  (headers : List<string * string>)
  : List<string * string> =
  if not inject then
    headers
  else
    let hasKey name =
      headers |> List.exists (fun (k, _) -> String.equalsCaseInsensitive k name)
    let extras =
      [ if not (hasKey "server") then ("Server", "darklang")
        if not (hasKey "strict-transport-security") then
          ("Strict-Transport-Security", hstsHeaderValue) ]
    headers @ extras


let private logRequest
  (ctx : HttpListenerContext)
  (status : int)
  (started : System.DateTime)
  : unit =
  let durationMs = (System.DateTime.UtcNow - started).TotalMilliseconds |> int64
  let methodStr = ctx.Request.HttpMethod
  let pathAndQuery =
    try
      ctx.Request.Url.PathAndQuery
    with _ ->
      "?"
  print $"[HttpServer] {methodStr} {pathAndQuery} {status} {durationMs}ms"
  Telemetry.event
    "httpserver.request"
    [ "method", methodStr
      "path", pathAndQuery
      "status", string status
      "duration_ms", string durationMs ]


// ───────── per-request dispatch ─────────

let private executeHandler
  (exeState : ExecutionState)
  (handler : Applicable)
  (arg : Dval)
  : Task<Dval> =
  task {
    let! result = Execution.executeApplicable exeState handler (NEList.singleton arg)
    match result with
    | Ok dval -> return dval
    | Error(rte, _callStack) ->
      let! errorStrResult = Execution.runtimeErrorToString exeState rte
      let errorStr =
        match errorStrResult with
        | Ok(DString s) -> s
        | Ok other -> string other
        | Error _ -> string rte
      return DString $"Handler error: {errorStr}"
  }


/// Process a single request: parse → dispatch → write response. Errors
/// surface as 500s; full detail goes to `logRequest` rather than the wire.
let private handleRequest
  (exeState : ExecutionState)
  (handler : Applicable)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (ctx : HttpListenerContext)
  : Task<unit> =
  task {
    let started = System.DateTime.UtcNow
    // Ephemeral blobs carry their bytes inline (lifetime is GC), so there's no
    // shared blob store for concurrent requests to race over.
    try
      try
        let! bodyResult = readRequestBodyWithLimit ctx.Request maxBodyBytes
        match bodyResult with
        | Error() ->
          ctx.Response.StatusCode <- 413
          let msg = UTF8.toBytes "413 Payload Too Large"
          ctx.Response.ContentLength64 <- int64 msg.Length
          do! ctx.Response.OutputStream.WriteAsync(msg, 0, msg.Length)
        | Ok reqBody ->
          let reqHeaders = extractHeaders ctx.Request
          let rawUrl = ctx.Request.Url.ToString()
          let url =
            if canonicalizeFromForwardedProto then
              canonicalizeUrlFromForwardedProto rawUrl reqHeaders
            else
              rawUrl

          let requestDval = Http.Request.fromRequest url reqHeaders reqBody

          // Per-request tracer — same shape as `eval`/`run` so HTTP traces
          // appear alongside CLI traces with no consumer-side changes.
          let traceID = AT.TraceID.create ()
          let traceDesc =
            try
              $"{ctx.Request.HttpMethod} {ctx.Request.Url.PathAndQuery}"
            with _ ->
              "(http request)"
          let tracer =
            Tracing.createCliTracer traceID traceDesc "request" requestDval
          let perRequestState = { exeState with tracing = tracer.executionTracing }

          let! result = executeHandler perRequestState handler requestDval
          let! response = Http.Response.toHttpResponse perRequestState result
          do! tracer.storeTraceResults perRequestState |> Ply.toTask

          let respHeaders =
            maybeInjectStandardHeaders injectStandardHeaders response.headers

          ctx.Response.StatusCode <- response.statusCode
          for (key, value) in respHeaders do
            ctx.Response.Headers.Add(key, value)
          ctx.Response.ContentLength64 <- int64 response.body.Length
          do!
            ctx.Response.OutputStream.WriteAsync(
              response.body,
              0,
              response.body.Length
            )
      with _ex ->
        // Don't leak ex.Message — can carry stack hints / sensitive
        // strings. Detail goes to `logRequest` (which sees the 500
        // status). 4xx + handler-set codes flow through
        // `Response.toHttpResponse`; this path is F#-side failures only.
        ctx.Response.StatusCode <- 500
        let errorBytes = UTF8.toBytes "Internal server error"
        ctx.Response.ContentLength64 <- int64 errorBytes.Length
        do! ctx.Response.OutputStream.WriteAsync(errorBytes, 0, errorBytes.Length)
    finally
      if logRequests then
        try
          logRequest ctx ctx.Response.StatusCode started
        with _ ->
          ()
      try
        ctx.Response.OutputStream.Close()
        ctx.Response.Close()
      with _ ->
        ()
  }


// ───────── listener loop ─────────

// TODO: replace `HttpListener` with raw `TcpListener` + a hand-rolled HTTP/1.1
// parser. ~80 ms/connection on loopback today; PoC drops it to near-zero.
// HttpListener gives us free-but-not-cheap defenses (T-E + C-L smuggling,
// slow-loris timeouts, per-connection caps, malformed-input handling) — all
// of which the swap PR has to re-implement before going public-facing.
// See `notes/merge-readiness-report.md` for the dotnet-trace numbers.


/// Bind a listener to <param port>, or say why not in one human sentence.
///
/// Separate from `runListener` so the bind is the CALLER's to report: inside the serve task, a failure comes
/// back out of `listenerTask.Wait()` wrapped in an AggregateException — a stack trace where a sentence
/// belongs — and only after the caller has already announced "Listening on {port}", which by then is a lie.
let bindListener (port : int64) : Result<HttpListener, string> =
  let listener = new HttpListener()
  listener.Prefixes.Add($"http://*:{port}/")
  try
    listener.Start()
    Ok listener
  with :? HttpListenerException as e ->
    // "Port's taken" has a different number on every platform — 98 EADDRINUSE on Linux, 48 on BSD/macOS,
    // 183 ERROR_ALREADY_EXISTS / 32 ERROR_SHARING_VIOLATION on Windows — and .NET surfaces the native errno
    // here. Match the message too: the codes are what the platform calls it, the message is what it means,
    // and getting this wrong just means falling back to the raw text (which is what shipped before).
    let inUse =
      List.contains e.ErrorCode [ 98; 48; 183; 32 ]
      || e.Message.Contains("Address already in use")
      || e.Message.Contains("address already in use")
    if inUse then
      Error(
        $"port {port} is already in use — something else is listening there. "
        + "Stop it, or serve on another port."
      )
    else
      Error $"couldn't listen on port {port}: {e.Message}"


/// Serve requests off an ALREADY-BOUND listener (see `bindListener`), until cancelled.
let runListener
  (exeState : ExecutionState)
  (listener : HttpListener)
  (port : int64)
  (handler : Applicable)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (cancellationToken : CancellationToken)
  : Task<unit> =
  task {
    Telemetry.event
      "httpserver.listening"
      [ "port", string port
        "maxBodyBytes", string maxBodyBytes
        "injectStandardHeaders", string injectStandardHeaders
        "canonicalizeFromForwardedProto", string canonicalizeFromForwardedProto
        "logRequests", string logRequests ]

    // Cancellation → listener.Stop() unblocks pending GetContextAsync
    // by raising HttpListenerException / ObjectDisposedException, both
    // caught below as normal exit.
    use _registration =
      cancellationToken.Register(fun () ->
        try
          listener.Stop()
        with _ ->
          ())

    while not cancellationToken.IsCancellationRequested do
      try
        let! ctx = listener.GetContextAsync()
        Task.Run(fun () ->
          task {
            try
              do!
                handleRequest
                  exeState
                  handler
                  maxBodyBytes
                  injectStandardHeaders
                  canonicalizeFromForwardedProto
                  logRequests
                  ctx
            with _ ->
              ()
          }
          :> Task)
        |> ignore<Task>
      with
      | :? HttpListenerException -> ()
      | :? ObjectDisposedException -> ()

    try
      listener.Close()
    with _ ->
      ()

    Telemetry.event "httpserver.shutdown" [ "port", string port ]
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "httpServerServe" 0
      typeParams = []
      parameters =
        [ Param.make "port" TInt "TCP port to listen on"
          Param.makeWithArgs
            "handler"
            // CLEANUP real types
            (TFn(NEList.singleton (TVariable "request"), TVariable "response"))
            "Handler function: request -> response"
            [ "request" ]
          Param.make
            "maxBodyBytes"
            TInt
            "Maximum request body size in bytes (over-limit → 413)"
          Param.make
            "injectStandardHeaders"
            TBool
            "If true, auto-add `Server: darklang` and HSTS to responses unless the handler set them"
          Param.make
            "canonicalizeFromForwardedProto"
            TBool
            "If true, rewrite request.url to https:// when X-Forwarded-Proto: https is present"
          Param.make
            "logRequests"
            TBool
            "If true, emit a per-request stdout line and Telemetry.event 'httpserver.request' with method/path/status/duration_ms"
          Param.makeWithArgs
            "onListening"
            (TFn(NEList.singleton TUnit, TUnit))
            "Fired once the port is bound — announce here, so a banner is never printed before it's true"
            [ "unit" ] ]
      returnType = TypeReference.result TUnit TString
      description =
        "Start an HTTP server. Calls handler for each request. Runs onListening once the port is bound; "
        + "returns Error with a plain message if the port can't be bound. Blocks until SIGINT."
      fn =
        (function
        | struct (exeState, vm, _,
                  [| DInt portArg
                     DApplicable handler
                     DInt maxBodyBytesArg
                     DBool injectStandardHeaders
                     DBool canonicalizeFromForwardedProto
                     DBool logRequests
                     DApplicable onListening |]) ->
          uply {
            // maxBodyBytes is a comparison threshold; a negative limit would
            // reject every request (treated as over-limit), so reject it. 0 is
            // valid (allow no body).
            let maxBodyBytes = intToInt64 vm maxBodyBytesArg
            if maxBodyBytes < 0L then
              RuntimeError.Ints.OutOfRange
              |> RuntimeError.Int
              |> raiseRTE vm.threadID
            // A TCP port must be in [0, 65535]. intToInt64 alone would let
            // larger-but-int64-sized values reach HttpListener.Start and throw a
            // host exception, so validate the real port range up front.
            let port = intToInt64 vm portArg
            if
              port < int64 IPEndPoint.MinPort || port > int64 IPEndPoint.MaxPort
            then
              RuntimeError.Ints.OutOfRange
              |> RuntimeError.Int
              |> raiseRTE vm.threadID
            use _serveSpan =
              Telemetry.span "httpserver.serve" [ "port", string port ]

            // Bind BEFORE announcing, so the caller's onListening banner is only printed once it's actually
            // bound. A bind failure (e.g. the port is taken) comes back as a clean Error the caller can print
            // itself, instead of a runtime-error wrapper with a stack.
            match bindListener port with
            | Error msg -> return Dval.resultError KTUnit KTString (DString msg)
            | Ok listener ->
              let! _ =
                Execution.executeApplicable
                  exeState
                  onListening
                  (NEList.singleton DUnit)

              // SIGINT → cancel; in-flight requests drain by virtue of being
              // fire-and-forget Tasks.
              let cts = new CancellationTokenSource()
              let cancelHandler =
                ConsoleCancelEventHandler(fun _ args ->
                  args.Cancel <- true
                  cts.Cancel())
              Console.CancelKeyPress.AddHandler cancelHandler

              let listenerTask =
                runListener
                  exeState
                  listener
                  port
                  handler
                  maxBodyBytes
                  injectStandardHeaders
                  canonicalizeFromForwardedProto
                  logRequests
                  cts.Token

              listenerTask.Wait()

              Console.CancelKeyPress.RemoveHandler cancelHandler

              return Dval.resultOk KTUnit KTString DUnit
          }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.httpServer
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
