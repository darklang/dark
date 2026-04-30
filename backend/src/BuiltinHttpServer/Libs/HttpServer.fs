/// HTTP Server builtin
///
/// Starts a server on the given port and defers to Darklang code to
/// handle the request.
///
/// The F# side only handles:
/// - Starting the HTTP listener
/// - Converting HTTP requests to Darklang Request type
/// - Calling the Darklang handler function
/// - Converting Darklang Response back to HTTP
///
/// All routing and handler management lives in Darklang.
///
/// Implementation note: this used to be ASP.NET Kestrel via
/// WebApplication.CreateBuilder. It was rewritten on top of
/// System.Net.HttpListener to drop the Microsoft.AspNetCore.App
/// FrameworkReference from the CLI's AOT graph. HttpListener is in
/// the BCL, AOT-clean, and gives us the same surface this file uses.
module BuiltinHttpServer.Libs.HttpServer

open System
open System.Net
open System.IO
open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Execution = LibExecution.Execution
module Blob = LibExecution.Blob
module Http = LibHttpMiddleware.Http


/// Execute a handler (named fn or lambda) against the given request. Lambdas
/// registered anywhere under this `exeState` (including the caller VM) are
/// findable here since `lambdaInstrCache` lives on `exeState`.
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
      let! errorStr = Execution.runtimeErrorToString exeState rte
      return DString $"Handler error: {errorStr}"
  }


/// Read the entire request body into a byte array.
let private readRequestBody (req : HttpListenerRequest) : Task<byte[]> =
  task {
    use ms = new MemoryStream()
    do! req.InputStream.CopyToAsync(ms)
    return ms.ToArray()
  }


/// Flatten HttpListener's NameValueCollection of headers into the (key, value)
/// list shape that LibHttpMiddleware.Http.Request expects. A single header key
/// with multiple values becomes multiple entries (matching the prior ASP.NET
/// behavior).
let private extractHeaders (req : HttpListenerRequest) : List<string * string> =
  let headers = ResizeArray<string * string>()
  for key in req.Headers.AllKeys do
    if not (isNull key) then
      let values = req.Headers.GetValues(key)
      if not (isNull values) then
        for value in values do
          headers.Add(key, value)
  // x-http-method preserved from the prior ASP.NET implementation. CLEANUP
  // once Dark-side handlers stop relying on it.
  ("x-http-method", req.HttpMethod) :: List.ofSeq headers


/// Process a single request: parse → dispatch → write response. Errors are
/// caught and returned as 500s so a single bad request can't kill the loop.
let private handleRequest
  (exeState : ExecutionState)
  (handler : Applicable)
  (ctx : HttpListenerContext)
  : Task<unit> =
  task {
    // Push a fresh blob-scope per request so ephemeral blobs minted by the
    // handler (and by any toHttpResponse materialisation) are reclaimed once
    // the response is sent. Without this, a long-lived http-server VM leaks
    // blobStore entries across requests.
    LibExecution.Blob.pushScope exeState
    try
      try
        let! reqBody = readRequestBody ctx.Request
        let reqHeaders = extractHeaders ctx.Request
        let url = ctx.Request.Url.ToString()

        let requestDval = Http.Request.fromRequest exeState url reqHeaders reqBody

        let! result = executeHandler exeState handler requestDval
        let! response = Http.Response.toHttpResponse exeState result

        ctx.Response.StatusCode <- response.statusCode
        for (key, value) in response.headers do
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
      LibExecution.Blob.popScope exeState
      try
        ctx.Response.OutputStream.Close()
        ctx.Response.Close()
      with _ ->
        ()
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "httpServerServe" 0
      typeParams = []
      parameters =
        [ Param.make "port" TInt64 "TCP port to listen on"
          Param.makeWithArgs
            "handler"
            // CLEANUP real types
            (TFn(NEList.singleton (TVariable "request"), TVariable "response"))
            "Handler function: request -> response"
            [ "request" ] ]
      returnType = TUnit
      description = "Start an HTTP server. Calls handler for each request."
      fn =
        (function
        | exeState, _, _, [ DInt64 port; DApplicable handler ] ->
          // Outer fn returns DvalTask. The accept loop itself is a Task<unit>,
          // run-to-completion blockingly so the builtin doesn't return until
          // the listener is shut down.
          uply {
            let listener = new HttpListener()
            listener.Prefixes.Add($"http://*:{port}/")
            listener.Start()

            print $"[HttpServer] Listening on port {port}"

            let acceptLoop : Task<unit> =
              task {
                while true do
                  let! ctx = listener.GetContextAsync()
                  // Fire-and-forget: errors are caught inside handleRequest;
                  // anything escaping that try/catch is also swallowed here.
                  Task.Run(fun () ->
                    task {
                      try
                        do! handleRequest exeState handler ctx
                      with _ ->
                        ()
                    }
                    :> Task)
                  |> ignore<Task>
              }

            // Block forever (matches prior `app.RunAsync().Wait()`).
            acceptLoop.Wait()

            return DUnit
          }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
