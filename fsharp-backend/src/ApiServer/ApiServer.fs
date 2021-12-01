module ApiServer.ApiServer

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks

module Auth = LibBackend.Authorization

open Prelude
open Tablecloth

module Config = LibBackend.Config

// --------------------
// Handlers
// --------------------
let endpoints : Endpoint list =
  let h = Middleware.apiHandler
  let oh = Middleware.apiOptionHandler

  let R = Auth.Read
  let RW = Auth.ReadWrite

  let api
    (name : string)
    (fn : string -> HttpFunc -> HttpContext -> HttpFuncResult)
    =
    // FSTODO: trace is_admin, username, and canvas
    routef (PrintfFormat<_, _, _, _, _>("/api/%s/" + name)) fn

  [ GET [ route "/login" Login.loginPage
          route "/logout" Login.logout
          routef "/a/%s" (Middleware.canvasHtmlHandler Ui.uiHandler R) ]

    POST [ route "/login" Login.loginHandler
           route "/logout" Login.logout
           api "add_op" (h AddOps.addOp RW)
           api "all_traces" (h Traces.AllTraces.fetchAll R)
           api "delete_404" (h F404s.Delete.delete RW)
           api "delete_secret" (h Secrets.Delete.delete RW)
           api "execute_function" (h Execution.Function.execute RW)
           api "get_404s" (h F404s.List.get R)
           api "get_db_stats" (h DBs.DBStats.getStats R)
           api "get_trace_data" (oh Traces.TraceData.getTraceData R)
           api "get_unlocked_dbs" (h DBs.Unlocked.get R)
           api "get_worker_stats" (h Workers.WorkerStats.getStats R)
           api "initial_load" (h InitialLoad.initialLoad R)
           api "insert_secret" (h Secrets.Insert.insert RW)
           api "packages" (h Packages.List.packages R)
           // FSLATER: packages/upload_function
           // FSLATER: save_test handler
           api "trigger_handler" (h Execution.Handler.trigger RW)
           api "worker_schedule" (h Workers.Scheduler.updateSchedule RW) ] ]


// --------------------
// Standard handlers
// --------------------
let notFoundHandler = "Not Found" |> text |> RequestErrors.notFound

let errorHandler (ex : Exception) (logger : ILogger) =
  print $"Exception: {ex.Message}"
  print (string ex)
  // FSTODO: configure logger and don't print the message to output
// logger.LogError
//   (EventId(),
//    ex,
//    "An unhandled exception has occurred while executing the request.")
  Giraffe.Core.compose clearResponse (ServerErrors.INTERNAL_ERROR ex.Message)

// --------------------
// Setup web server
// --------------------

let configureStaticContent (app : IApplicationBuilder) : IApplicationBuilder =
  if Config.apiServerServeStaticContent then
    app.UseStaticFiles(
      StaticFileOptions(
        ServeUnknownFileTypes = true,
        FileProvider = new PhysicalFileProvider(Config.webrootDir),
        OnPrepareResponse =
          (fun ctx -> ctx.Context.SetHttpHeader("Access-Control-Allow-Origin", "*"))
      )
    )
  else
    app

let configureApp (appBuilder : IApplicationBuilder) =
  appBuilder
  |> fun app -> app.UseServerTiming() // must go early or this is dropped
  // FSTODO: use ConfigureWebHostDefaults + AllowedHosts
  |> fun app -> LibService.Rollbar.AspNet.addRollbarToApp (app, (fun ctx -> []))
  |> fun app -> app.UseHttpsRedirection()
  |> fun app -> app.UseRouting()
  // must go after UseRouting
  |> LibService.Kubernetes.configureApp LibService.Config.apiServerKubernetesPort
  |> configureStaticContent
  |> fun app -> app.UseGiraffeErrorHandler(errorHandler)
  |> fun app -> app.UseGiraffe(endpoints)
  |> fun app -> app.UseGiraffe(notFoundHandler)

let configureServices (services : IServiceCollection) : unit =
  services
  |> LibService.Rollbar.AspNet.addRollbarToServices
  |> LibService.Telemetry.AspNet.addTelemetryToServices "ApiServer"
  |> LibService.Kubernetes.configureServices
  |> fun s -> s.AddServerTiming()
  |> fun s -> s.AddGiraffe()
  |> fun s ->
       // this should say `s.AddSingleton<Json.ISerializer>(`. Fantomas has a habit of stripping
       // the `<Json.ISerializer>` part, which causes the serializer not to load.
       s.AddSingleton<Json.ISerializer>(
         NewtonsoftJson.Serializer(Json.OCamlCompatible._settings)
       )
  |> ignore<IServiceCollection>



[<EntryPoint>]
let main _ =
  try
    print "Starting ApiServer"
    LibBackend.Init.init "ApiServer"
    // Breaks tests as they're being run simultaneously by the ocaml server
    // LibBackend.Migrations.init ()

    let k8sUrl = LibService.Kubernetes.url LibService.Config.apiServerKubernetesPort
    let url = $"http://darklang.localhost:{LibService.Config.apiServerPort}"

    let builder = WebApplication.CreateBuilder()
    configureServices builder.Services
    LibService.Kubernetes.registerServerTimeout builder.WebHost

    builder.WebHost
    |> fun wh -> wh.UseKestrel(LibService.Kestrel.configureKestrel)
    |> fun wh -> wh.UseUrls(k8sUrl, url)
    |> ignore<IWebHostBuilder>

    let app = builder.Build()
    configureApp app
    app.Run()

    0
  with
  | e ->
    LibService.Rollbar.lastDitchBlocking
      "Error starting ApiServer"
      (ExecutionID "apiserver")
      []
      e
    (-1)
