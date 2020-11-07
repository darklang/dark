module BwdServer

(* open Giraffe.Core *)
(* open Giraffe.ResponseWriters *)

(* open Microsoft.AspNetCore.Http *)
open Giraffe
open FSharp.Control.Tasks

open System
(* open System.Security.Claims *)
(* open System.Threading *)
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
(* open Microsoft.AspNetCore.Http.Features *)
(* open Microsoft.AspNetCore.Authentication *)
open Microsoft.Extensions.Hosting
(* open Microsoft.AspNetCore.Authentication.Cookies *)
(* open Microsoft.Extensions.Configuration *)
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Prelude

let getCanvasIDMiddleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let findUserMiddleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let useDarkFaviconMiddleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

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

let runDarkHandler : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    // httpsRedirect // TODO use built-in handler
    task {
      let executionID = 1
      let url = ctx.GetRequestUrl()
      let headers = "todo"
      let body = "todo"
      let expr = LibExecution.Runtime.Shortcuts.eFn "" "test" 0 []

      let fns = LibExecution.StdLib.fns @ LibBackend.StdLib.fns

      let! result = LibExecution.Execution.run [] fns expr

      let result = result.toJSON().ToString()

      return! text result next ctx
    }

let webApp : HttpHandler =
  getCanvasIDMiddleware
  >=> findUserMiddleware
  >=> recordEventMiddleware
  >=> record404Middleware
  >=> recordHeapioMiddleware
  >=> recordHoneycombMiddleware
  >=> useDarkFaviconMiddleware
  >=> runDarkHandler

let configureApp (app : IApplicationBuilder) =
  let errorHandler (ex : Exception) (logger : ILogger) =
    // TODO add rollbar
    // TODO add honeycomb
    logger.LogError
      (EventId(),
       ex,
       "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

  app.UseDeveloperExceptionPage().UseGiraffeErrorHandler(errorHandler)
     .UseGiraffe webApp

let configureLogging (builder : ILoggingBuilder) =
  let filter (l : LogLevel) = l.Equals LogLevel.Error

  // Configure the logging factory
  builder.AddFilter(filter) // Optional filter
         .AddConsole() // Set up the Console logger
         .AddDebug() // Set up the Debug logger
  // Add additional loggers if wanted...
  |> ignore

let configureServices (services : IServiceCollection) =
  services.AddGiraffe() |> ignore


[<EntryPoint>]
let main _ =
  Host.CreateDefaultBuilder()
      .ConfigureWebHostDefaults(fun webHostBuilder ->
      webHostBuilder.Configure(configureApp).ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging).UseUrls("http://*:9001")
      |> ignore).Build().Run()
  0
