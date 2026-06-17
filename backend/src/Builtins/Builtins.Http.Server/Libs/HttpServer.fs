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
  task {
    if req.ContentLength64 > maxBytes then
      return Error()
    else
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
let runListener
  (exeState : ExecutionState)
  (port : int64)
  (handler : Applicable)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (cancellationToken : CancellationToken)
  : Task<unit> =
  task {
    let listener = new HttpListener()
    listener.Prefixes.Add($"http://*:{port}/")
    listener.Start()

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
            "If true, emit a per-request stdout line and Telemetry.event 'httpserver.request' with method/path/status/duration_ms" ]
      returnType = TUnit
      description =
        "Start an HTTP server. Calls handler for each request. Blocks until SIGINT."
      fn =
        (function
        | exeState,
          _,
          _,
          [ DInt portArg
            DApplicable handler
            DInt maxBodyBytesArg
            DBool injectStandardHeaders
            DBool canonicalizeFromForwardedProto
            DBool logRequests ] ->
          uply {
            // port/maxBodyBytes are the arbitrary-precision Int; the listener
            // plumbing below uses int64.
            let port = int64 (DarkInt.toBigInt portArg)
            let maxBodyBytes = int64 (DarkInt.toBigInt maxBodyBytesArg)
            use _serveSpan =
              Telemetry.span "httpserver.serve" [ "port", string port ]

            print $"[HttpServer] Listening on port {port}"

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
                port
                handler
                maxBodyBytes
                injectStandardHeaders
                canonicalizeFromForwardedProto
                logRequests
                cts.Token

            listenerTask.Wait()

            Console.CancelKeyPress.RemoveHandler cancelHandler

            return DUnit
          }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.httpServer
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
