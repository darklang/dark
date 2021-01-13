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
open FSharpPlus
open Prelude
open Prelude.Tablecloth

module Config = LibBackend.Config
module Session = LibBackend.Session
module Account = LibBackend.Account

let apiPackages (canvasName : string) : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) ->
    task {
      let! fns = LibBackend.PackageManager.allFunctions ()
      let fns = List.map LibBackend.PackageManager.toFrontendPackage fns
      ctx.SetContentType "application/json"
      return! ctx.WriteTextAsync(Json.AutoSerialize.serialize fns)
    }

let apiHandler : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) -> "api test" |> ctx.WriteTextAsync

type LoginKind =
  | Local
  | Live

let uiHandler (canvasName : string) : HttpHandler =
  fun (_ : HttpFunc) (ctx : HttpContext) ->
    task {

      let liveLogin, loginUrl, logoutUri =
        if Config.useLoginDarklangComForLogin then
          Live, "https://login.darklang.com", "https://logout.darklang.com/logout"
        else
          Local, "/login", "/logout"

      let sessionKey = ctx.Request.Cookies.Item "__session"

      match! Session.get sessionKey with
      | None ->
          // FSTODO: redirect to Login
          ctx.Response.StatusCode <- 401
          return! ctx.WriteTextAsync "Not Authorized"
      | Some sessionData ->
          let localhostAssets = ctx.TryGetQueryStringValue "localhost-assets"
          // FSTODO: validate csrfToken before doing anything else
          let csrfToken = sessionData.csrfToken

          match! Account.getUser (UserName.create sessionData.username) with
          | None ->
              ctx.Response.StatusCode <- 404
              return! ctx.WriteTextAsync "Not found"
          | Some user ->
              let canvasName = CanvasName.create canvasName
              // FSTODO: support integration tests
              // if integration_test then Canvas.load_and_resave_from_test_file canvas ;
              let! canView =
                LibBackend.Authorization.canViewCanvas canvasName user.username

              if not canView then
                ctx.Response.StatusCode <- 404
                return! ctx.WriteTextAsync "Not found"
              else
                // FSTODO: this has only the read permission, but this will create a canvas
                let! ownerID =
                  (Account.ownerNameFromCanvasName canvasName).toUserName
                  |> Account.ownerID
                  |> Task.map Option.someOrRaise

                let! canvasID =
                  LibBackend.Canvas.canvasIDForCanvasName ownerID canvasName

                let! createdAt = Account.getUserCreatedAt user.username

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
                ctx.SetHttpHeader(
                  "Content-security-policy",
                  "frame-ancestors 'none';"
                )

                return!
                  ctx.WriteHtmlStringAsync(
                    Ui.uiHtml
                      canvasID
                      canvasName
                      csrfToken
                      localhostAssets
                      createdAt
                      user
                  )
    }

let apiEndpoints : Endpoint list =
  [
    // TODO: why is this a POST?
    POST [ routef "/api/%s/packages" apiPackages ] ]

let uiEndpoints : Endpoint list = [ GET [ routef "/a/%s" uiHandler ] ]
let endpoints : Endpoint list = uiEndpoints ++ apiEndpoints

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

let configureApp (appBuilder : IApplicationBuilder) =
  appBuilder
  // FSTODO: use ConfigureWebHostDefaults + AllowedHosts
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

  |> fun app -> app.UseGiraffeErrorHandler(errorHandler)
  |> fun app -> app.UseGiraffe(endpoints)
  |> fun app -> app.UseGiraffe(notFoundHandler)

let configureServices (services : IServiceCollection) =
  services.AddRouting().AddGiraffe() |> ignore

[<EntryPoint>]
let main args =
  LibBackend.ProgramSerialization.OCamlInterop.Binary.init ()

  WebHost.CreateDefaultBuilder(args)
  |> fun wh -> wh.UseKestrel()
  |> fun wh -> wh.ConfigureServices(configureServices)
  |> fun wh -> wh.Configure(configureApp)
  |> fun wh -> wh.UseUrls("http://darklang.localhost:9000")
  |> fun wh -> wh.Build()
  |> fun wh -> wh.Run()

  0
