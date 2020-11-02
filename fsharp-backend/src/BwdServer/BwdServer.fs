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

type DarkMiddleware<'a, 'b> = 'a -> ('a -> 'b) -> 'b

let runAsync e =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let executionID = 1
      let url = ctx.GetRequestUrl()

      // let darkMiddleware =
      //   req
      //   |> loadHttpHandler
      //   |> createHTTPRequestObject
      //   |> addJsonBody
      //   |> addFormBody
      //   |> addCookies
      //   |> addHeaders
      //   |> processErrorRail
      //
      // bindHttpVariables
      // ctx
      // |> catchExceptionsMiddleware
      // |> httpsRedirectMiddleware
      // |> textPingMiddleware // move inside Dark stack
      // |> sitemapFaviconMiddleware // move inside Dark stack
      // |> getCanvasIDMiddleware
      // |> findUserMiddleware
      // |> useDarkFaviconMiddleware // if 404ing, use the Dark favicon
      // |> optionsHandlerMiddleware // move inside Dark stack
      // |> headHandlerMiddleware // move inside Dark stack
      // |> recordEventMiddleware
      // |> record404Middleware
      // |> recordHeapioMiddleware
      // |> recordHoneycombMiddleware
      // |> darkHandler


      // middleware: get user, 404 of not
      let fns = LibExecution.StdLib.fns @ LibBackend.StdLib.fns

      let! result = LibExecution.Execution.run [] fns e

      let result = result.toJSON().ToString()

      return! text result next ctx
    }

let errorHandler (ex : Exception) (logger : ILogger) =
  logger.LogError
    (EventId(),
     ex,
     "An unhandled exception has occurred while executing the request.")
  clearResponse >=> setStatusCode 500 >=> text ex.Message

let webApp = choose [ GET >=> choose [] ]

let configureApp (app : IApplicationBuilder) =
  app.UseDeveloperExceptionPage().UseGiraffeErrorHandler(errorHandler)
     .UseGiraffe webApp

let configureServices (services : IServiceCollection) =
  services.AddGiraffe() |> ignore



[<EntryPoint>]
let main _ =
  Host.CreateDefaultBuilder()
      .ConfigureWebHostDefaults(fun webHostBuilder ->
      webHostBuilder.Configure(configureApp).ConfigureServices(configureServices)
                    .UseUrls("http://*:9001")
      (* .ConfigureLogging(configureLogging) *)
      |> ignore).Build().Run()
  0
