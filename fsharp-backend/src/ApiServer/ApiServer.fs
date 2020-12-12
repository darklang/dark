module ApiServer.ApiServer

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.EndpointRouting

let handler1 : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) -> ctx.WriteTextAsync "Hello World"

let handler2 (firstName : string, age : int) : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) ->
    sprintf "Hello %s, you are %i years old." firstName age |> ctx.WriteTextAsync

let handler3 (a : string, b : string, c : string, d : int) : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) ->
    sprintf "Hello %s %s %s %i" a b c d |> ctx.WriteTextAsync

let endpoints =
  [ subRoute "/foo" [ GET [ route "/bar" (text "Aloha!") ] ]
    GET [ route "/" (text "Hello World")
          routef "/%s/%i" handler2
          routef "/%s/%s/%s/%i" handler3 ]
    GET_HEAD [ route "/foo" (text "Bar")
               route "/x" (text "y")
               route "/abc" (text "def") ]
    // Not specifying a http verb means it will listen to all verbs
    subRoute "/sub" [ route "/test" handler1 ] ]

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
