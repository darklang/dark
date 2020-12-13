module ApiServer.ApiServer

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.StaticFiles
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.EndpointRouting

let apiHandler : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) -> "api test" |> ctx.WriteTextAsync

let uiHandler (canvasName : string) : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) ->
    let localhostAssets = ctx.TryGetQueryStringValue "localhost-assets"
    if localhostAssets.IsSome then ctx.SetHttpHeader
                                     ("Access-Control-Allow_origin", "*")

    // Clickjacking: Don't allow any other websites to put this in an iframe;
    // this prevents "clickjacking" attacks.
    // https://www.owasp.org/index.php/Clickjacking_Defense_Cheat_Sheet#Content-Security-Policy:_frame-ancestors_Examples
    // It would be nice to use CSP to limit where we can load scripts etc from,
    // but right now we load from CDNs, <script> tags, etc. So the only thing
    // we could do is script-src: 'unsafe-inline', which doesn't offer us any
    // additional security.
    ctx.SetHttpHeader("Content-security-policy", "frame-ancestors 'none';")

    ctx.WriteHtmlStringAsync(Ui.ui canvasName localhostAssets)

let apiEndpoints = [ GET [ routef "/%s" (fun guid -> apiHandler) ] ]
let uiEndpoints = GET [ routef "/a/%s" uiHandler ]
let endpoints = [ uiEndpoints; subRoute "/api" apiEndpoints ]

let notFoundHandler = "Not Found" |> text |> RequestErrors.notFound

let errorHandler (ex : Exception) (logger : ILogger) =
  printfn "%s" ex.Message
  clearResponse >=> ServerErrors.INTERNAL_ERROR ex.Message
// FSTODO: configure logger and don't print the message to output
// logger.LogError
//   (EventId(),
//    ex,
//    "An unhandled exception has occurred while executing the request.")

let configureApp (appBuilder : IApplicationBuilder) =
  appBuilder
  // FSTODO: use ConfigureWebHostDefaults + AllowedHosts
  |> fun app -> app.UseGiraffeErrorHandler(errorHandler)
  |> fun app -> app.UseHttpsRedirection()
  |> fun app -> app.UseRouting()
  |> fun app ->
       if LibBackend.Config.staticHost.Contains "localhost:8000" then
         app.UseStaticFiles
           (StaticFileOptions
             (FileProvider =
               new PhysicalFileProvider(System.IO.Path.Combine
                                          (System.IO.Directory.GetCurrentDirectory(),
                                           "backend/static"))))
       else
         app

  |> fun app -> app.UseGiraffe(endpoints)
  |> fun app -> app.UseGiraffe(notFoundHandler)

let configureServices (services : IServiceCollection) =
  services.AddRouting().AddGiraffe() |> ignore

[<EntryPoint>]
let main args =
  WebHost.CreateDefaultBuilder(args)
  |> fun wh -> wh.UseKestrel()
  |> fun wh -> wh.ConfigureServices(configureServices)
  |> fun wh -> wh.Configure(configureApp)
  |> fun wh -> wh.UseUrls("http://darklang.localhost:9000")
  |> fun wh -> wh.Build()
  |> fun wh -> wh.Run()
  0
