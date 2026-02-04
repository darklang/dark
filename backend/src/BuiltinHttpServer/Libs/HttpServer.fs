/// HTTP Server builtin
///
/// Starts a server on the given port
/// , and defers to Darklang code to handle the request
///
/// The F# side only handles:
/// - Starting the ASP.NET Core server
/// - Converting HTTP requests to Darklang Request type
/// - Calling the Darklang handler function
/// - Converting Darklang Response back to HTTP
///
/// All routing and handler management lives in Darklang
module BuiltinHttpServer.Libs.HttpServer

open System
open System.Threading.Tasks
open FSharp.Control.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Primitives

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Execution = LibExecution.Execution
module Http = LibHttpMiddleware.Http


/// Execute a named function with the given argument
let private executeNamedFn
  (exeState : ExecutionState)
  (namedFn : ApplicableNamedFn)
  (arg : Dval)
  : Task<Dval> =
  task {
    let! result =
      Execution.executeFunction
        exeState
        namedFn.name
        namedFn.typeArgs
        (NEList.singleton arg)
    match result with
    | Ok dval -> return dval
    | Error(rte, _callStack) ->
      let! errorStr = Execution.runtimeErrorToString exeState rte
      return DString $"Handler error: {errorStr}"
  }


let fns : List<BuiltInFn> =
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
        | exeState, _, _, [ DInt64 port; DApplicable(AppNamedFn handlerFn) ] ->
          uply {
            let builder = WebApplication.CreateBuilder()
            builder.WebHost.UseUrls($"http://*:{port}") |> ignore<IWebHostBuilder>

            let app = builder.Build()

            app.Map(
              "/{**path}",
              Func<HttpContext, Task>(fun ctx ->
                task {
                  try
                    // Read request body
                    let ms = new System.IO.MemoryStream()
                    do! ctx.Request.Body.CopyToAsync(ms)
                    let reqBody = ms.ToArray()

                    // Get headers, including method as x-http-method
                    let reqHeaders =
                      ctx.Request.Headers
                      |> Seq.collect (fun kvp ->
                        kvp.Value.ToArray() |> Array.map (fun v -> (kvp.Key, v)))
                      |> Seq.toList
                    let reqHeaders =
                      // CLEANUP should we drop this?
                      ("x-http-method", ctx.Request.Method) :: reqHeaders

                    // Build request Dval
                    let url = ctx.Request.GetDisplayUrl()
                    let requestDval =
                      Http.Request.fromRequest url reqHeaders reqBody

                    // Call the Darklang code
                    let! result = executeNamedFn exeState handlerFn requestDval

                    // Convert result to HTTP response
                    let! response = Http.Response.toHttpResponse exeState result

                    ctx.Response.StatusCode <- response.statusCode
                    for (key, value) in response.headers do
                      ctx.Response.Headers[key] <- StringValues(value)
                    ctx.Response.ContentLength <- int64 response.body.Length
                    do!
                      ctx.Response.Body.WriteAsync(
                        response.body,
                        0,
                        response.body.Length
                      )

                  with ex ->
                    ctx.Response.StatusCode <- 500
                    let errorBytes =
                      UTF8.toBytes $"Internal server error: {ex.Message}"
                    ctx.Response.ContentLength <- int64 errorBytes.Length
                    do!
                      ctx.Response.Body.WriteAsync(errorBytes, 0, errorBytes.Length)
                })
            )
            |> ignore<IEndpointConventionBuilder>

            print $"[HttpServer] Listening on port {port}"

            // This blocks forever
            let runTask = app.RunAsync()
            runTask.Wait()

            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
