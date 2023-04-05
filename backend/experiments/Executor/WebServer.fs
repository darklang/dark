module Executor.WebServer

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

type StringValues = Microsoft.Extensions.Primitives.StringValues

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

// open Http
// open Middleware

// module Config = LibBackend.Config

// module FireAndForget = LibService.FireAndForget
// module Kubernetes = LibService.Kubernetes
// module Rollbar = LibService.Rollbar
// module Telemetry = LibService.Telemetry

type Packages = List<LibExecution.ProgramTypes.Package.Fn>

type HttpHandler = HttpContext -> Task

open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Primitives
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

[<Extension>]
type HttpContextExtensions() =

  [<Extension>]
  static member WriteClientJsonAsync<'T>
    (
      ctx : HttpContext,
      value : 'T
    ) : Task<option<HttpContext>> =
    task {
      // use t = startTimer "serialize-json" ctx
      // addTag "json_flavor" "vanilla"
      ctx.Response.ContentType <- "application/json; charset=utf-8"
      // Use a client-specific ApiServer
      let serialized = Json.Vanilla.serialize value
      let bytes = System.ReadOnlyMemory(UTF8.toBytes serialized)
      ctx.Response.ContentLength <- int64 bytes.Length
      // t.next "write-json-async"
      let! (_ : System.IO.Pipelines.FlushResult) =
        ctx.Response.BodyWriter.WriteAsync(bytes)
      return Some ctx
    }

  [<Extension>]
  static member WriteTextAsync
    (
      ctx : HttpContext,
      value : string
    ) : Task<option<HttpContext>> =
    task {
      // use t = startTimer "text-to-bytes" ctx
      ctx.Response.ContentType <- "text/plain; charset=utf-8"
      let bytes = System.ReadOnlyMemory(UTF8.toBytes value)
      ctx.Response.ContentLength <- int64 bytes.Length
      // t.next "write-text-async"
      let! (_ : System.IO.Pipelines.FlushResult) =
        ctx.Response.BodyWriter.WriteAsync(bytes)
      return Some ctx
    }

/// Helper to write a value as serialized JSON response body
let clientJsonHandler (f : HttpContext -> Task<'a>) : HttpHandler =
  (fun ctx ->
    task {
      let! result = f ctx
      return! ctx.WriteClientJsonAsync result
    })


// --------------------
// Handlers
// --------------------

let addRoutes (app : IApplicationBuilder) : IApplicationBuilder =
  let ab = app
  let app = app :?> WebApplication

  let builder = RouteBuilder(ab)

  let addRoute (pattern : string) (handler : HttpHandler) =
    builder.MapPost(pattern, handler) |> ignore<IRouteBuilder>

  let clientJsonApi (name : string) (version : int) (f : HttpContext -> Task<'a>) =
    let handler = clientJsonHandler f
    let route = $"/api/v{version}/{name}"
    addRoute route handler

  clientJsonApi "execute-text" 0 API.ExecuteText.post
  clientJsonApi "execute-json" 0 API.ExecuteJson.post

  let versionHandler : (HttpContext -> Task) =
    clientJsonHandler (fun _ -> task { return VersionInfo.info () })
  builder.MapGet("/api/v0/version", versionHandler) |> ignore<IRouteBuilder>

  app.UseRouter(builder.Build())

// --------------------
// Web stack
// --------------------

open System
open System.Net
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

type ExceptionOutput = { msgs : string list; metadata : Metadata }

type ErrorHandlingMiddleware(next : RequestDelegate) =
  member this.Invoke(context : HttpContext) : Task =
    task {
      try
        // Make sure to await the response here so that try/catch works
        let! response = next.Invoke(context)
        return response
      with
      | e -> return! this.HandleException(context, e)
    }

  member private this.HandleException(context : HttpContext, e : exn) : Task =
    task {
      let result =
        try
          let metadata = Exception.toMetadata e
          let msgs = Exception.getMessages e
          "An exception occurred in the web stack: \n\n"
          + "Message(s): \n"
          + (msgs |> String.concat "\n")
          + "\n\nMetadata: \n"
          + (string metadata)
        with
        | e -> $"Exception while handling exception: {e}"

      // Add to the log
      print result

      // Return to the caller
      context.Response.ContentType <- "application/json"
      context.Response.StatusCode <- 500

      return context.Response.WriteAsync(result)
    }



// let rollbarCtxToMetadata (ctx : HttpContext) : (Rollbar.Person * Metadata) =
//   let person =
//     try
//       loadUserInfo ctx
//     with
//     | _ -> None

//   let canvas =
//     try
//       string (loadCanvasInfo ctx).name
//     with
//     | _ -> null

//   let clientVersion =
//     try
//       string ctx.Request.Headers["x-darklang-client-version"] |> String.take 7
//     with
//     | _ -> null

//   (person, [ "canvas", canvas; "client_version", clientVersion ])

let configureApp (appBuilder : WebApplication) =
  appBuilder
  // |> fun app -> app.UseServerTiming() // must go early or this is dropped
  // |> fun app -> Rollbar.AspNet.addRollbarToApp app rollbarCtxToMetadata None
  |> fun app -> app.UseMiddleware<ErrorHandlingMiddleware>()
  |> fun app -> app.UseRouting()
  // must go after UseRouting
  // |> Kubernetes.configureApp LibService.Config.apiServerKubernetesPort
  |> addRoutes
  |> ignore<IApplicationBuilder>

// A service is a value that's added to each request, to be used by some middleware.
// For example, ServerTiming adds a ServerTiming value that is then used by the ServerTiming middleware
let configureServices (services : IServiceCollection) : unit =
  services
  // |> Rollbar.AspNet.addRollbarToServices
  // |> Telemetry.AspNet.addTelemetryToServices "ApiServer" Telemetry.TraceDBQueries
  // |> Kubernetes.configureServices [ LibBackend.Canvas.healthCheck ]
  // |> fun s -> s.AddServerTiming()
  |> ignore<IServiceCollection>

let webserver (httpPort : int) (_healthCheckPort : int) : WebApplication =
  // let hcUrl = Kubernetes.url healthCheckPort

  let builder = WebApplication.CreateBuilder()
  configureServices builder.Services
  // Kubernetes.registerServerTimeout builder.WebHost

  builder.WebHost
  // |> fun wh -> wh.ConfigureLogging(loggerSetup)
  // |> fun wh -> wh.UseKestrel(LibService.Kestrel.configureKestrel)
  // |> fun wh -> wh.UseUrls(hcUrl, $"http://localhost:{httpPort}")
  |> fun wh -> wh.UseUrls($"http://localhost:{httpPort}")
  |> ignore<IWebHostBuilder>

  let app = builder.Build()
  configureApp app
  app

let runServer (_debug : bool) (port : int) (hcPort : int) : unit =
  System.Console.WriteLine
    $"Starting server on port {port}, health check on {hcPort}"
  (webserver port hcPort).Run()
