module BwdServer

(* open Microsoft.AspNetCore.Http *)
open FSharp.Control.Tasks

open System
(* open System.Security.Claims *)
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
(* open Microsoft.AspNetCore.Http.Features *)
open Microsoft.Extensions.Hosting
(* open Microsoft.Extensions.Configuration *)
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Prelude
open FSharpx

// This boilerplate is copied from Giraffe. I elected not to use Giraffe
// because we don't need any of its feature, but the types it chose are very
// nice.
// https://github.com/giraffe-fsharp/Giraffe/blob/9598682f4f68e23217c4199a48f30ca3457b037e/src/Giraffe/Core.fs

type HttpFuncResult = Task<HttpContext option>
type HttpFunc = HttpContext -> HttpFuncResult
type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

let compose (handler1 : HttpHandler) (handler2 : HttpHandler) : HttpHandler =
  fun (final : HttpFunc) ->
    let func = final |> handler2 |> handler1
    fun (ctx : HttpContext) ->
      match ctx.Response.HasStarted with
      | true -> final ctx
      | false -> func ctx

let (>=>) = compose

type BwdMiddleware(next : RequestDelegate, handler : HttpHandler) =
  let earlyReturn = Some >> Task.FromResult
  let func : HttpFunc = handler earlyReturn

  member __.Invoke(ctx : HttpContext) =
    task {
      let! result = func ctx
      if (result.IsNone) then return! next.Invoke ctx
    }

// End stuff copied from Giraffe

let recordEventMiddleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let record404Middleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let recordHeapioMiddleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let recordHoneycombMiddleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let sanitizeUrlPath (path : string) : string =
  path
  |> FsRegEx.replace "//+" "/"
  |> String.trimEnd [| '/' |]
  |> fun str -> if str = "" then "/" else str

let normalizeRequest : HttpHandler =
  fun next ctx ->
    ctx.Request.Path <- ctx.Request.Path.Value |> sanitizeUrlPath |> PathString
    next ctx

open LibExecution.Framework.Handler
open LibExecution.Framework

let runDarkHandler : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let executionID = LibExecution.Runtime.Shortcuts.gid ()
      let canvasName = "test-hello"
      let owner = "test"
      let path = "/"
      let method = "GET"
      let! userID = LibBackend.Serialization.userIDForUsername owner
      let! canvasID = LibBackend.Serialization.canvasIDForCanvas userID canvasName

      let! exprs =
        LibBackend.Serialization.loadHttpHandlersFromCache
          canvasName
          canvasID
          userID
          path
          method

      match exprs with
      | [ TLHandler { spec = HTTPHandler _; ast = expr; tlid = _ } ] ->
          let fns = LibExecution.StdLib.fns @ LibBackend.StdLib.fns
          let! result = LibExecution.Execution.run [] fns expr
          // FSTODO - might not be JSON
          let result = result.toJSON().ToString()
          // FSTODO - might not be UTF8
          let bytes = System.Text.Encoding.UTF8.GetBytes result

          do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)

          return! next ctx
      | [] ->
          ctx.Response.StatusCode <- 404
          return Some ctx
      | _ ->
          ctx.Response.StatusCode <- 500
          return Some ctx
    }

let webApp : HttpHandler =
  // FSTODO use giraffe's builtin httpsRedirect
  normalizeRequest
  >=> recordEventMiddleware
  >=> record404Middleware
  >=> recordHeapioMiddleware
  >=> recordHoneycombMiddleware
  >=> runDarkHandler

let configureApp (app : IApplicationBuilder) =
  app.UseDeveloperExceptionPage().UseMiddleware<BwdMiddleware> |> ignore

let configureLogging (builder : ILoggingBuilder) =
  let filter (l : LogLevel) : bool = true

  // Configure the logging factory
  builder.AddFilter(filter) // Optional filter
         .AddConsole() // Set up the Console logger
         .AddDebug() // Set up the Debug logger
  // Add additional loggers if wanted...
  |> ignore

let configureServices (services : IServiceCollection) = ()

let webserver port =
  let url = $"http://*:{port}"
  Host.CreateDefaultBuilder()
      .ConfigureWebHostDefaults(fun webHostBuilder ->
      webHostBuilder.Configure(configureApp).ConfigureServices(configureServices)
                    .UseKestrel(fun kestrel -> kestrel.AddServerHeader <- false)
                    .ConfigureLogging(configureLogging).UseUrls(url)
      |> ignore).Build()


[<EntryPoint>]
let main _ =
  (webserver 9001).Run()
  0
