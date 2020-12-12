module ApiServer.ApiServer

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.EndpointRouting

let apiHandler : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) -> "api test" |> ctx.WriteTextAsync

let apiEndpoints = [ GET [ routef "/%s" (fun guid -> apiHandler) ] ]

let uiEndpoints = GET [ routef "/a/%s" (fun canvas -> htmlString (Ui.ui canvas)) ]

let endpoints = [ uiEndpoints; subRoute "/api" apiEndpoints ]


let notFoundHandler = "Not Found" |> text |> RequestErrors.notFound

let configureApp (appBuilder : IApplicationBuilder) =
  appBuilder
  |> fun ab -> ab.UseRouting()
  |> fun ab -> ab.UseGiraffe(endpoints)
  |> fun ab -> ab.UseGiraffe(notFoundHandler)

let configureServices (services : IServiceCollection) =
  services.AddRouting().AddGiraffe() |> ignore

[<EntryPoint>]
let main args =
  WebHost.CreateDefaultBuilder(args)
  |> fun wh -> wh.UseKestrel()
  |> fun wh -> wh.Configure(configureApp)
  |> fun wh -> wh.ConfigureServices(configureServices)
  |> fun wh -> wh.UseUrls("http://darklang.localhost:9000")
  |> fun wh -> wh.Build()
  |> fun wh -> wh.Run()
  0
