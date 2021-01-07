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

open System.Threading.Tasks
open FSharp.Control.Tasks

let apiHandler : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) -> "api test" |> ctx.WriteTextAsync

type LoginKind =
  | Local
  | Live

let uiHandler (canvasName : string) : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) ->
    task {

      let liveLogin, loginUrl, logoutUri =
        if LibBackend.Config.useLoginDarklangComForLogin then
          Live, "https://login.darklang.com", "https://logout.darklang.com/logout"
        else
          Local, "/login", "/logout"

      let sessionKey = ctx.Session.Get("__session")

      match! LibBackend.Session.get sessionKey with
      | None ->
          // FSTODO: redirect to Login
          ctx.Response.StatusCode <- 401
          return! ctx.WriteTextAsync "Not Authorized"
      | Some sessionData ->
          let localhostAssets = ctx.TryGetQueryStringValue "localhost-assets"
          let user = LibBackend.Account.getUser sessionData.username
          let ownerName = LibBackend.Account.ownerFromHost canvasName
          let csrfToken = sessionData.csrfToken

          // FSTODO: support integration tests
          // if integration_test then Canvas.load_and_resave_from_test_file canvas ;
          if not (LibBackend.Authorization.canViewCanvas canvasName user.username) then
            ctx.Response.StatusCode <- 404
            return! ctx.WriteTextAsync "Not found"
          else
            let canvasID = LibBackend.Account.fetchCanvasID canvasName

            if localhostAssets.IsSome then
              ctx.SetHttpHeader("Access-Control-Allow_origin", "*")
            else
              ()

            ctx.SetHttpHeader("Content-type", "text/html; charset=utf-8")
            // Clickjacking: Don't allow any other websites to put this in an iframe;
            // this prevents "clickjacking" attacks.
            // https://www.owasp.org/index.php/Clickjacking_Defense_Cheat_Sheet#Content-Security-Policy:_frame-ancestors_Examples
            // It would be nice to use CSP to limit where we can load scripts etc from,
            // but right now we load from CDNs, <script> tags, etc. So the only thing
            // we could do is script-src: 'unsafe-inline', which doesn't offer us any
            // additional security.
            ctx.SetHttpHeader("Content-security-policy", "frame-ancestors 'none';")

            return!
              ctx.WriteHtmlStringAsync(Ui.uiHtml canvasID canvasName localhostAssets)
    }

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
