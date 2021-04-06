module ApiServer.ApiServer

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.EndpointRouting

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

  let api (name : string) fn =
    routef (PrintfFormat<_, _, _, _, _>("/api/%s/" + name)) fn

  [ GET [ route "/login" Login.loginPage
          route "/logout" Login.logout
          routef "/a/%s" (Middleware.canvasHtmlHandler Ui.uiHandler R) ]

    POST [ route "/login" Login.loginHandler
           route "/logout" Login.logout
           api "get_unlocked_dbs" (h DBs.Unlocked.get R)
           api "get_db_stats" (h DBs.DBStats.getStats R)
           api "execute_function" (h Execution.Function.execute RW)
           api "trigger_handler" (h Execution.Handler.trigger RW)
           api "get_404s" (h F404s.List.get R)
           api "delete_404" (h F404s.Delete.delete RW)
           api "initial_load" (h InitialLoad.initialLoad R)
           api "packages" (h Packages.List.packages R)
           api "insert_secret" (h Secrets.insertSecret RW)
           api "get_trace_data" (oh Traces.TraceData.getTraceData R)
           api "all_traces" (h Traces.AllTraces.fetchAll R)
           api "get_worker_stats" (h Workers.WorkerStats.getStats R)
           api "worker_schedule" (h Workers.Scheduler.updateSchedule RW) ] ]

// TODO AddOps.endpoints


// --------------------
// Standard handlers
// --------------------
let notFoundHandler = "Not Found" |> text |> RequestErrors.notFound

let errorHandler (ex : Exception) (logger : ILogger) =
  printfn "Exception: %s" ex.Message
  printfn "%s" (ex.ToString())
  // FSTODO: configure logger and don't print the message to output
// logger.LogError
//   (EventId(),
//    ex,
//    "An unhandled exception has occurred while executing the request.")
  Giraffe.Core.compose clearResponse (ServerErrors.INTERNAL_ERROR ex.Message)

// --------------------
// Setup web server
// --------------------
let configureApp (appBuilder : IApplicationBuilder) =
  appBuilder
  // FSTODO: use ConfigureWebHostDefaults + AllowedHosts
  |> fun app -> app.UseHttpsRedirection()
  |> fun app -> app.UseRouting()
  |> fun app -> app.UseServerTiming()
  |> fun app ->
       // FSTODO: use a Config value
       if LibBackend.Config.staticHost.Contains "localhost:8000" then
         app.UseStaticFiles(
           StaticFileOptions(
             FileProvider =
               new PhysicalFileProvider(
                 System.IO.Path.Combine(
                   System.IO.Directory.GetCurrentDirectory(),
                   "backend/static"
                 )
               )
           )
         )
       else
         app

  |> fun app -> app.UseGiraffeErrorHandler(errorHandler)
  |> fun app -> app.UseGiraffe(endpoints)
  |> fun app -> app.UseGiraffe(notFoundHandler)

let configureServices (services : IServiceCollection) =
  services
    .AddServerTiming()
    .AddRouting()
    .AddGiraffe()
    .AddSingleton<Json.ISerializer>(
      NewtonsoftJson.Serializer(Json.OCamlCompatible._settings)
    )
  |> ignore

[<EntryPoint>]
let main args =
  printfn "Starting ApiServer"
  LibBackend.Init.init ()

  WebHost.CreateDefaultBuilder(args)
  |> fun wh -> wh.UseKestrel()
  |> fun wh -> wh.ConfigureServices(configureServices)
  |> fun wh -> wh.Configure(configureApp)
  // FSTODO: use a config value
  |> fun wh -> wh.UseUrls("http://darklang.localhost:9000")
  |> fun wh -> wh.Build()
  |> fun wh -> wh.Run()

  0
