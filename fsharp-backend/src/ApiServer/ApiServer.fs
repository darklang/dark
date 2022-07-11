module ApiServer.ApiServer

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

open Http
open Middleware
open Microsoft.AspNetCore.StaticFiles

module Auth = LibBackend.Authorization
module Config = LibBackend.Config

module FireAndForget = LibService.FireAndForget
module Kubernetes = LibService.Kubernetes
module Rollbar = LibService.Rollbar
module Telemetry = LibService.Telemetry

type Packages = List<LibExecution.ProgramTypes.Package.Fn>

// --------------------
// Handlers
// --------------------
let addRoutes
  (packages : Packages)
  (app : IApplicationBuilder)
  : IApplicationBuilder =
  let ab = app
  let app = app :?> WebApplication

  let R = Some Auth.Read
  let RW = Some Auth.ReadWrite

  let builder = RouteBuilder(ab)

  // This route is used so that we know that the http proxy is actually proxying the server
  let checkApiserver : HttpHandler =
    (fun (ctx : HttpContext) ->
      task { return ctx.Response.WriteAsync("success: this is apiserver") })

  let addRoute
    (verb : string)
    (pattern : string)
    (middleware : HttpMiddleware)
    (perm : Option<Auth.Permission>)
    (handler : HttpHandler)
    =
    builder.MapMiddlewareVerb(
      verb,
      pattern,
      (fun appBuilder ->
        appBuilder.UseMiddleware(middleware)
        // Do this inside the other middleware so we still get canvas, etc
        Option.tap (fun perm -> appBuilder.UseMiddleware(canvasMiddleware perm)) perm
        appBuilder.Run(handler))
    )
    |> ignore<IRouteBuilder>

  let std = standardMiddleware
  let html = htmlMiddleware

  // CLEANUP: switch everything over to clientJson and get rid of
  // ocamlCompatible. We want to get rid of both OCamlTypes and Serializers,
  // both of which exist because of these APIs. We want to move these APIs
  // to the `dark-editor` canvas
  let clientJsonApi name perm f =
    let handler = clientJsonHandler f
    let route = $"/api/{{canvasName}}/{name}"
    addRoute "POST" route std perm handler

  let ocamlCompatibleApi name perm f =
    let handler = ocamlCompatibleJsonHandler f
    let route = $"/api/{{canvasName}}/{name}"
    addRoute "POST" route std perm handler


  let clientJsonApiOption name perm f =
    let handler = clientJsonOptionHandler f
    let route = $"/api/{{canvasName}}/{name}"
    addRoute "POST" route std perm handler

  let ocamlCompatibleApiOption name perm f =
    let handler = ocamlCompatibleJsonOptionHandler f
    let route = $"/api/{{canvasName}}/{name}"
    addRoute "POST" route std perm handler

  addRoute "GET" "/login" html None Login.loginPage
  addRoute "POST" "/login" html None Login.loginHandler
  addRoute "GET" "/logout" html None Login.logout
  addRoute "POST" "/logout" html None Login.logout

  // Provide an unauthenticated route to check the server
  builder.MapGet("/check-apiserver", checkApiserver) |> ignore<IRouteBuilder>

  addRoute "GET" "/a/{canvasName}" html R (htmlHandler Ui.uiHandler)

  // For internal testing - please don't test this out, it might page me
  let exceptionFn (ctx : HttpContext) =
    let userInfo = loadUserInfo ctx
    Exception.raiseInternal "triggered test exception" [ "user", userInfo.username ]
  addRoute "GET" "/a/{canvasName}/trigger-exception" std R exceptionFn

  ocamlCompatibleApi "add_op" RW AddOps.addOp
  clientJsonApi "v1/add_op" RW AddOps.addOp
  ocamlCompatibleApi "all_traces" R Traces.AllTraces.fetchAll
  clientJsonApi "v1/all_traces" R Traces.AllTraces.fetchAll
  ocamlCompatibleApi "delete_404" RW F404s.Delete.delete
  clientJsonApi "v1/delete_404" RW F404s.Delete.delete
  ocamlCompatibleApiOption "delete-toplevel-forever" RW Toplevels.Delete.delete
  clientJsonApiOption "v1/delete-toplevel-forever" RW Toplevels.Delete.delete
  ocamlCompatibleApi "delete_secret" RW Secrets.Delete.delete
  clientJsonApi "v1/delete_secret" RW Secrets.Delete.delete
  ocamlCompatibleApi "execute_function" RW Execution.Function.execute
  clientJsonApi "v1/execute_function" RW Execution.Function.execute
  ocamlCompatibleApi "get_404s" R F404s.List.get
  clientJsonApi "v1/get_404s" R F404s.List.get
  ocamlCompatibleApi "get_db_stats" R DBs.DBStats.getStats
  clientJsonApi "v1/get_db_stats" R DBs.DBStats.getStats
  clientJsonApiOption "get_trace_data" R Traces.TraceData.getTraceData
  clientJsonApiOption "v1/get_trace_data" R Traces.TraceData.getTraceData
  clientJsonApi "get_unlocked_dbs" R DBs.Unlocked.get
  ocamlCompatibleApi "get_worker_stats" R Workers.WorkerStats.getStats
  clientJsonApi "v1/get_worker_stats" R Workers.WorkerStats.getStats
  ocamlCompatibleApi "initial_load" R InitialLoad.initialLoad
  clientJsonApi "v1/initial_load" R InitialLoad.initialLoad
  ocamlCompatibleApi "insert_secret" RW Secrets.Insert.insert
  clientJsonApi "v1/insert_secret" RW Secrets.Insert.insert
  ocamlCompatibleApi "packages" R (Packages.List.packages packages)
  clientJsonApi "v1/packages" R (Packages.List.packages packages)
  // CLEANUP: packages/upload_function
  // CLEANUP: save_test handler
  ocamlCompatibleApi "trigger_handler" RW Execution.Handler.trigger
  clientJsonApi "v1/trigger_handler" RW Execution.Handler.trigger
  ocamlCompatibleApi "worker_schedule" RW Workers.Scheduler.updateSchedule
  clientJsonApi "v1/worker_schedule" RW Workers.Scheduler.updateSchedule
  app.UseRouter(builder.Build())


// --------------------
// Setup web server
// --------------------
let configureStaticContent (app : IApplicationBuilder) : IApplicationBuilder =
  if Config.apiServerServeStaticContent then
    let contentTypeProvider = FileExtensionContentTypeProvider()
    // See also scripts/deployment/_push-assets-to-cdn
    contentTypeProvider.Mappings[ ".wasm" ] <- "application/wasm"
    contentTypeProvider.Mappings[ ".pdb" ] <- "text/plain"
    contentTypeProvider.Mappings[ ".dll" ] <- "application/octet-stream"
    contentTypeProvider.Mappings[ ".dat" ] <- "application/octet-stream"
    contentTypeProvider.Mappings[ ".blat" ] <- "application/octet-stream"

    app.UseStaticFiles(
      StaticFileOptions(
        ServeUnknownFileTypes = true,
        FileProvider = new PhysicalFileProvider(Config.webrootDir),
        OnPrepareResponse =
          (fun ctx ->
            ctx.Context.Response.Headers[ "Access-Control-Allow-Origin" ] <-
              StringValues([| "*" |])),
        ContentTypeProvider = contentTypeProvider
      )
    )
  else
    app

let rollbarCtxToMetadata (ctx : HttpContext) : (Rollbar.Person * Metadata) =
  let person =
    try
      loadUserInfo ctx |> LibBackend.Account.userInfoToPerson
    with
    | _ -> None
  let canvas =
    try
      string (loadCanvasInfo ctx).name
    with
    | _ -> null
  (person, [ "canvas", canvas ])

let configureApp (packages : Packages) (appBuilder : WebApplication) =
  appBuilder
  |> fun app -> app.UseServerTiming() // must go early or this is dropped
  |> fun app -> Rollbar.AspNet.addRollbarToApp app rollbarCtxToMetadata None
  |> fun app -> app.UseHttpsRedirection()
  |> fun app -> app.UseHsts()
  |> fun app -> app.UseRouting()
  // must go after UseRouting
  |> Kubernetes.configureApp LibService.Config.apiServerKubernetesPort
  |> configureStaticContent
  |> addRoutes packages
  |> ignore<IApplicationBuilder>

// A service is a value that's added to each request, to be used by some middleware.
// For example, ServerTiming adds a ServerTiming value that is then used by the ServerTiming middleware
let configureServices (services : IServiceCollection) : unit =
  services
  |> Rollbar.AspNet.addRollbarToServices
  |> Telemetry.AspNet.addTelemetryToServices "ApiServer" Telemetry.TraceDBQueries
  |> Kubernetes.configureServices []
  |> fun s -> s.AddServerTiming()
  |> fun s -> s.AddHsts(LibService.HSTS.setConfig)
  |> ignore<IServiceCollection>

let webserver
  (packages : Packages)
  (loggerSetup : ILoggingBuilder -> unit)
  (httpPort : int)
  (healthCheckPort : int)
  : WebApplication =
  let hcUrl = Kubernetes.url healthCheckPort

  let builder = WebApplication.CreateBuilder()
  configureServices builder.Services
  LibService.Kubernetes.registerServerTimeout builder.WebHost

  builder.WebHost
  |> fun wh -> wh.ConfigureLogging(loggerSetup)
  |> fun wh -> wh.UseKestrel(LibService.Kestrel.configureKestrel)
  |> fun wh -> wh.UseUrls(hcUrl, $"http://darklang.localhost:{httpPort}")
  |> ignore<IWebHostBuilder>

  let app = builder.Build()
  configureApp packages app
  app

let run (packages : Packages) : unit =
  let port = LibService.Config.apiServerPort
  let k8sPort = LibService.Config.apiServerKubernetesPort
  (webserver packages LibService.Logging.noLogger port k8sPort).Run()

let initSerializers () =
  Json.OCamlCompatible.allow<AddOps.Params> "ApiServer.AddOps"
  Json.Vanilla.allow<AddOps.Params> "ApiServer.AddOps"
  Json.OCamlCompatible.allow<AddOps.T> "ApiServer.AddOps"
  Json.Vanilla.allow<AddOps.T> "ApiServer.AddOps"
  Json.OCamlCompatible.allow<DBs.DBStats.Params> "ApiServer.DBs"
  Json.Vanilla.allow<DBs.DBStats.Params> "ApiServer.DBs"
  Json.OCamlCompatible.allow<DBs.DBStats.T> "ApiServer.DBs"
  Json.Vanilla.allow<DBs.DBStats.T> "ApiServer.DBs"
  Json.Vanilla.allow<DBs.Unlocked.T> "ApiServer.DBs"
  Json.OCamlCompatible.allow<Execution.Function.Params> "ApiServer.Execution"
  Json.Vanilla.allow<Execution.Function.Params> "ApiServer.Execution"
  Json.OCamlCompatible.allow<Execution.Function.T> "ApiServer.Execution"
  Json.Vanilla.allow<Execution.Function.T> "ApiServer.Execution"
  Json.OCamlCompatible.allow<Execution.Handler.Params> "ApiServer.Execution"
  Json.Vanilla.allow<Execution.Handler.Params> "ApiServer.Execution"
  Json.OCamlCompatible.allow<Execution.Handler.T> "ApiServer.Execution"
  Json.Vanilla.allow<Execution.Handler.T> "ApiServer.Execution"
  Json.OCamlCompatible.allow<F404s.Delete.Params> "ApiServer.F404s"
  Json.Vanilla.allow<F404s.Delete.Params> "ApiServer.F404s"
  Json.OCamlCompatible.allow<F404s.Delete.T> "ApiServer.F404s"
  Json.Vanilla.allow<F404s.Delete.T> "ApiServer.F404s"
  Json.OCamlCompatible.allow<F404s.List.T> "ApiServer.F404s"
  Json.Vanilla.allow<F404s.List.T> "ApiServer.F404s"
  Json.Vanilla.allow<List<Functions.FunctionMetadata>> "ApiServer.Functions"
  Json.OCamlCompatible.allow<InitialLoad.T> "ApiServer.InitialLoad"
  Json.Vanilla.allow<InitialLoad.T> "ApiServer.InitialLoad"
  Json.OCamlCompatible.allow<Packages.List.T> "ApiServer.Packages"
  Json.Vanilla.allow<Packages.List.T> "ApiServer.Packages"
  Json.OCamlCompatible.allow<Secrets.Delete.Params> "ApiServer.Secrets"
  Json.Vanilla.allow<Secrets.Delete.Params> "ApiServer.Secrets"
  Json.OCamlCompatible.allow<Secrets.Delete.T> "ApiServer.Secrets"
  Json.Vanilla.allow<Secrets.Delete.T> "ApiServer.Secrets"
  Json.OCamlCompatible.allow<Secrets.Insert.Params> "ApiServer.Secrets"
  Json.Vanilla.allow<Secrets.Insert.Params> "ApiServer.Secrets"
  Json.OCamlCompatible.allow<Secrets.Insert.T> "ApiServer.Secrets"
  Json.Vanilla.allow<Secrets.Insert.T> "ApiServer.Secrets"
  Json.OCamlCompatible.allow<Toplevels.Delete.Params> "ApiServer.Toplevels"
  Json.Vanilla.allow<Toplevels.Delete.Params> "ApiServer.Toplevels"
  Json.OCamlCompatible.allow<Toplevels.Delete.T> "ApiServer.Toplevels"
  Json.Vanilla.allow<Toplevels.Delete.T> "ApiServer.Toplevels"
  Json.OCamlCompatible.allow<Traces.AllTraces.T> "ApiServer.Traces"
  Json.Vanilla.allow<Traces.AllTraces.T> "ApiServer.Traces"
  Json.OCamlCompatible.allow<Traces.TraceData.Params> "ApiServer.Traces"
  Json.Vanilla.allow<Traces.TraceData.Params> "ApiServer.Traces"
  Json.Vanilla.allow<Traces.TraceData.T> "ApiServer.Traces"
  Json.OCamlCompatible.allow<Workers.Scheduler.Params> "ApiServer.Workers"
  Json.Vanilla.allow<Workers.Scheduler.Params> "ApiServer.Workers"
  Json.OCamlCompatible.allow<Workers.Scheduler.T> "ApiServer.Workers"
  Json.Vanilla.allow<Workers.Scheduler.T> "ApiServer.Workers"
  Json.OCamlCompatible.allow<Workers.WorkerStats.Params> "ApiServer.Workers"
  Json.Vanilla.allow<Workers.WorkerStats.Params> "ApiServer.Workers"
  Json.OCamlCompatible.allow<Workers.WorkerStats.T> "ApiServer.Workers"
  Json.Vanilla.allow<Workers.WorkerStats.T> "ApiServer.Workers"
  Json.Vanilla.allow<Map<string, string>> "ApiServer.UI"



[<EntryPoint>]
let main _ =
  try
    let name = "ApiServer"
    print "Starting ApiServer"
    Prelude.init ()
    LibService.Init.init name
    LibExecution.Init.init ()
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    (LibRealExecution.Init.init name).Result

    if Config.createAccounts then
      LibBackend.Account.initializeDevelopmentAccounts(name).Result

    initSerializers ()

    let packages = LibBackend.PackageManager.allFunctions().Result
    run packages
    // CLEANUP I suspect this isn't called
    LibService.Init.shutdown name
    0
  with
  | e -> LibService.Rollbar.lastDitchBlockAndPage "Error starting ApiServer" e
