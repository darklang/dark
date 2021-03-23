module ApiServer.ApiServer

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Mvc.NewtonsoftJson
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.EndpointRouting

open FSharpPlus

open Prelude
open Tablecloth

module Config = LibBackend.Config

// --------------------
// Handlers
// --------------------
let endpoints : Endpoint list = Login.endpoints ++ Ui.endpoints ++ Api.endpoints

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
    .AddRouting()
    .AddGiraffe()
    .AddSingleton<Json.ISerializer>(NewtonsoftJson.Serializer
                                      (Json.OCamlCompatible._settings))
  |> ignore

[<EntryPoint>]
let main args =
  printfn "Starting BwdServer"
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
