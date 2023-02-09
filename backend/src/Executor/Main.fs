module Executor.Main

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

module Config = LibBackend.Config

module FireAndForget = LibService.FireAndForget
module Kubernetes = LibService.Kubernetes
module Rollbar = LibService.Rollbar
module Telemetry = LibService.Telemetry

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

/// Helper to write a Optional value as serialized JSON response body
///
/// In the case of a None, responds with 404
let clientJsonOptionHandler (f : HttpContext -> Task<Option<'a>>) : HttpHandler =
  (fun ctx ->
    task {
      match! f ctx with
      | Some result -> return! ctx.WriteClientJsonAsync result
      | None ->
        ctx.Response.StatusCode <- 404
        return! ctx.WriteTextAsync "Not found"
    })

// --------------------
// Handlers
// --------------------
let addRoutes (app : IApplicationBuilder) : IApplicationBuilder =
  let ab = app
  let app = app :?> WebApplication

  let builder = RouteBuilder(ab)

  let addRoute (verb : string) (pattern : string) (handler : HttpHandler) =
    builder.MapMiddlewareVerb(
      verb,
      pattern,
      (fun appBuilder -> appBuilder.Run(handler))
    )
    |> ignore<IRouteBuilder>

  let clientJsonGETApi name version f =
    let handler = clientJsonHandler f
    let route = $"/api/v{version}/{name}"
    addRoute "GET" route handler

  let clientJsonApi name version f =
    let handler = clientJsonHandler f
    let route = $"/api/v{version}/{name}"
    addRoute "POST" route handler

  let clientJsonApiOption name version f =
    let handler = clientJsonOptionHandler f
    let route = $"/api/v{version}/{name}"
    addRoute "POST" route handler

  clientJsonApi "v0/execute-text" 0 API.ExecuteText.post
  clientJsonApi "v0/execute-json" 0 API.ExecuteJson.post

  app.UseRouter(builder.Build())


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

let webserver
  (loggerSetup : ILoggingBuilder -> unit)
  (httpPort : int)
  (healthCheckPort : int)
  : WebApplication =
  let hcUrl = Kubernetes.url healthCheckPort

  let builder = WebApplication.CreateBuilder()
  configureServices builder.Services
  // Kubernetes.registerServerTimeout builder.WebHost

  builder.WebHost
  |> fun wh -> wh.ConfigureLogging(loggerSetup)
  |> fun wh -> wh.UseKestrel(LibService.Kestrel.configureKestrel)
  |> fun wh -> wh.UseUrls(hcUrl, $"http://localhost:{httpPort}")
  |> ignore<IWebHostBuilder>

  let app = builder.Build()
  configureApp app
  app

let runServer (port : int) (hcPort : int) : unit =
  (webserver LibService.Logging.noLogger port hcPort).Run()


// Generally speaking, this should be a superset of BwdServer's list.
let initSerializers () =
  // universally-serializable types
  Json.Vanilla.allow<pos> "Prelude"

// one-off types used internally
// Json.Vanilla.allow<LibExecution.ProgramTypes.Oplist> "Canvas.loadJsonFromDisk"
// Json.Vanilla.allow<LibExecution.ProgramTypes.Position> "Canvas.saveTLIDs"
// Json.Vanilla.allow<LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.Dval>
//   "RoundtrippableSerializationFormatV0.Dval"
// Json.Vanilla.allow<LibBackend.Analytics.HeapIOMetadata> "heap.io metadata"
// Json.Vanilla.allow<LibBackend.EventQueueV2.NotificationData> "eventqueue storage"
// Json.Vanilla.allow<LibBackend.PackageManager.ParametersDBFormat> "PackageManager"
// Json.Vanilla.allow<LibBackend.Session.JsonData> "LibBackend session db storage"
// Json.Vanilla.allow<LibBackend.TraceCloudStorage.CloudStorageFormat>
//   "TraceCloudStorageFormat"
// Json.Vanilla.allow<LibService.Rollbar.HoneycombJson> "Rollbar"

// for API request/response payloads
// Json.Vanilla.allow<CTApi.Workers.WorkerStats.Request> "ApiServer.Workers"
// Json.Vanilla.allow<CTApi.Workers.WorkerStats.Response> "ApiServer.Workers"



[<EntryPoint>]
let main _ =
  // try
  let name = "Executor"
  print "Starting Executor"

  initSerializers ()

  // LibService.Init.init name
  // (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
  // (LibRealExecution.Init.init name).Result

  runServer 3004 13004
  0
// LibService.Init.shutdown name 0
// with
// | e -> LibService.Rollbar.lastDitchBlockAndPage "Error starting ApiServer" e
