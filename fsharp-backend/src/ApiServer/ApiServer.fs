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
        // Do this inside the other middleware so we still get executionID, etc
        Option.tap (fun perm -> appBuilder.UseMiddleware(canvasMiddleware perm)) perm
        appBuilder.Run(handler))
    )
    |> ignore<IRouteBuilder>

  let std = standardMiddleware
  let html = htmlMiddleware

  let api name perm f =
    let handler = jsonHandler f
    let route = $"/api/{{canvasName}}/{name}"
    addRoute "POST" route std perm handler

  let apiOption name perm f =
    let handler = jsonOptionHandler f
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

  api "add_op" RW AddOps.addOp
  api "clean_ops" RW AddOps.clean
  api "all_traces" R Traces.AllTraces.fetchAll
  api "delete_404" RW F404s.Delete.delete
  apiOption "delete-toplevel-forever" RW Toplevels.Delete.delete
  api "delete_secret" RW Secrets.Delete.delete
  api "execute_function" RW Execution.Function.execute
  api "get_404s" R F404s.List.get
  api "get_db_stats" R DBs.DBStats.getStats
  apiOption "get_trace_data" R Traces.TraceData.getTraceData
  api "get_unlocked_dbs" R DBs.Unlocked.get
  api "get_worker_stats" R Workers.WorkerStats.getStats
  api "initial_load" R InitialLoad.initialLoad
  api "insert_secret" RW Secrets.Insert.insert
  api "packages" R (Packages.List.packages packages)
  // CLEANUP: packages/upload_function
  // CLEANUP: save_test handler
  api "trigger_handler" RW Execution.Handler.trigger
  api "worker_schedule" RW Workers.Scheduler.updateSchedule
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

let rollbarCtxToMetadata
  (ctx : HttpContext)
  : (LibService.Rollbar.Person * Metadata) =
  let person =
    try
      loadUserInfo ctx |> LibBackend.Account.userInfoToPerson
    with
    | _ -> LibService.Rollbar.emptyPerson
  let canvas =
    try
      string (loadCanvasInfo ctx).name
    with
    | _ -> null
  (person, [ "canvas", canvas ])

let configureApp (packages : Packages) (appBuilder : WebApplication) =
  appBuilder
  |> fun app -> app.UseServerTiming() // must go early or this is dropped
  |> fun app ->
       LibService.Rollbar.AspNet.addRollbarToApp app rollbarCtxToMetadata None
  |> fun app -> app.UseHttpsRedirection()
  |> fun app -> app.UseHsts()
  |> fun app -> app.UseRouting()
  // must go after UseRouting
  |> LibService.Kubernetes.configureApp LibService.Config.apiServerKubernetesPort
  |> configureStaticContent
  |> addRoutes packages
  |> ignore<IApplicationBuilder>

// A service is a value that's added to each request, to be used by some middleware.
// For example, ServerTiming adds a ServerTiming value that is then used by the ServerTiming middleware
let configureServices (services : IServiceCollection) : unit =
  services
  |> LibService.Rollbar.AspNet.addRollbarToServices
  |> LibService.Telemetry.AspNet.addTelemetryToServices
       "ApiServer"
       LibService.Telemetry.TraceDBQueries
  |> LibService.Kubernetes.configureServices [ LibBackend.Init.legacyServerCheck ]
  |> fun s -> s.AddServerTiming()
  |> fun s -> s.AddHsts(LibService.HSTS.setConfig)
  |> ignore<IServiceCollection>

let run (packages : Packages) : unit =
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
  configureApp packages app
  app.Run()


[<EntryPoint>]
let main _ =
  try
    let name = "ApiServer"
    print "Starting ApiServer"
    LibService.Init.init name
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    (LibRealExecution.Init.init name).Result

    if Config.createAccounts then
      LibBackend.Account.initializeDevelopmentAccounts(name).Result

    let packages = LibBackend.PackageManager.allFunctions().Result
    run packages
    // CLEANUP I suspect this isn't called
    (LibBackend.Init.shutdown name).Result
    LibService.Init.shutdown name
    0
  with
  | e -> LibService.Rollbar.lastDitchBlockAndPage "Error starting ApiServer" e
