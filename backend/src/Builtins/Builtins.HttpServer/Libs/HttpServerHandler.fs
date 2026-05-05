/// Per-request handler dispatch: read body → build Request dval → call
/// the Dark handler → write response → trace + log.
///
/// Pulled out of `Libs/HttpServer.fs` so the listener loop's accept-and-
/// dispatch responsibility stays separate from the per-request flow.
module Builtins.HttpServer.Libs.HttpServerHandler

open System.Net
open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Dval = LibExecution.Dval
module Execution = LibExecution.Execution
module Blob = LibExecution.Blob
module Http = Builtins.HttpServer.Http
module AT = LibExecution.AnalysisTypes
module Tracing = LibDB.Tracing
module Helpers = Builtins.HttpServer.Libs.HttpServerHelpers


/// Execute a handler (named fn or lambda) against the given request. Lambdas
/// registered anywhere under this `exeState` (including the caller VM) are
/// findable here since `lambdaInstrCache` lives on `exeState`.
let executeHandler
  (exeState : ExecutionState)
  (handler : Applicable)
  (arg : Dval)
  : Task<Dval> =
  task {
    let! result = Execution.executeApplicable exeState handler (NEList.singleton arg)
    match result with
    | Ok dval -> return dval
    | Error(rte, _callStack) ->
      // runtimeErrorToString returns Task<ExecutionResult> (Result-wrapped Dval).
      // Unwrap so we don't render the F# Result type signature into the body.
      let! errorStrResult = Execution.runtimeErrorToString exeState rte
      let errorStr =
        match errorStrResult with
        | Ok(DString s) -> s
        | Ok other -> string other
        | Error _ -> string rte
      return DString $"Handler error: {errorStr}"
  }


/// Process a single request: parse → dispatch → write response. Errors are
/// caught and returned as 500s so a single bad request can't kill the loop.
let handleRequest
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
    // Push a fresh blob-scope per request so ephemeral blobs minted by the
    // handler (and by any toHttpResponse materialisation) are reclaimed once
    // the response is sent. Without this, a long-lived http-server VM leaks
    // blobStore entries across requests.
    Blob.pushScope exeState
    try
      try
        let! bodyResult = Helpers.readRequestBodyWithLimit ctx.Request maxBodyBytes
        match bodyResult with
        | Error() ->
          ctx.Response.StatusCode <- 413
          let msg = UTF8.toBytes "413 Payload Too Large"
          ctx.Response.ContentLength64 <- int64 msg.Length
          do! ctx.Response.OutputStream.WriteAsync(msg, 0, msg.Length)
        | Ok reqBody ->
          let reqHeaders = Helpers.extractHeaders ctx.Request
          let rawUrl = ctx.Request.Url.ToString()
          let url =
            if canonicalizeFromForwardedProto then
              Helpers.canonicalizeUrlFromForwardedProto rawUrl reqHeaders
            else
              rawUrl

          let requestDval = Http.Request.fromRequest exeState url reqHeaders reqBody

          // Per-request tracer. Same shape as `darklang eval/run` produces
          // (`Tracing.createCliTracer` in `Builtins.CliHost/Libs/Cli.fs`), so
          // `traces list/view/replay` shows HTTP traces alongside CLI traces
          // with no consumer-side changes. The trace description carries the
          // method + path so per-endpoint browsing (Step 2 in the plan)
          // already has somewhere to live.
          let traceID = AT.TraceID.create ()
          let traceDesc =
            try
              $"{ctx.Request.HttpMethod} {ctx.Request.Url.PathAndQuery}"
            with _ ->
              "(http request)"
          let tracer =
            Tracing.createCliTracer
              System.Guid.Empty
              traceID
              traceDesc
              "request"
              requestDval
          let perRequestState =
            { exeState with tracing = tracer.executionTracing }

          let! result = executeHandler perRequestState handler requestDval
          let! response = Http.Response.toHttpResponse perRequestState result
          do! tracer.storeTraceResults perRequestState |> Ply.toTask

          let respHeaders =
            Helpers.maybeInjectStandardHeaders injectStandardHeaders response.headers

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
      with ex ->
        ctx.Response.StatusCode <- 500
        let errorBytes = UTF8.toBytes $"Internal server error: {ex.Message}"
        ctx.Response.ContentLength64 <- int64 errorBytes.Length
        do! ctx.Response.OutputStream.WriteAsync(errorBytes, 0, errorBytes.Length)
    finally
      Blob.popScope exeState
      if logRequests then
        try
          Helpers.logRequest ctx ctx.Response.StatusCode started
        with _ ->
          ()
      try
        ctx.Response.OutputStream.Close()
        ctx.Response.Close()
      with _ ->
        ()
  }
