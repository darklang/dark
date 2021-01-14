module ApiServer.Middleware

// Middlewares used by the API server. Includes middleware functions and middleware stacks

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
module Auth = LibBackend.Authorization

let (>=>) = Giraffe.Core.compose

// --------------------
// Generic middlewares
// --------------------
let unauthorized (ctx : HttpContext) : Task<HttpContext option> =
  task {
    ctx.SetStatusCode 401
    return! ctx.WriteTextAsync "Not Authorized"
  }

let notFound (ctx : HttpContext) : Task<HttpContext option> =
  task {
    ctx.SetStatusCode 404
    return! ctx.WriteTextAsync "Not Found"
  }

// --------------------
// Accessing data from a HttpContext
// --------------------

// Don't use strings for this interface
type dataID =
  | UserInfo
  | SessionData
  | CanvasName
  | Permission

  override this.ToString() : string =
    match this with
    | UserInfo -> "user"
    | SessionData -> "sessionData"
    | CanvasName -> "canvasName"
    | Permission -> "permission"

let save (id : dataID) (value : 'a) (ctx : HttpContext) : HttpContext =
  ctx.Items.[id.ToString()] <- value
  ctx

let load<'a> (id : dataID) (ctx : HttpContext) : 'a =
  ctx.Items.[id.ToString()] :?> 'a

// --------------------
// APIServer Middlewares
// --------------------
type LoginKind =
  | Local
  | Live

let sessionDataMiddleware : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let sessionKey = ctx.Request.Cookies.Item "__session"

      match! Session.get sessionKey with
      | None ->
          // FSTODO: redirect to Login
          let liveLogin, loginUrl, logoutUri =
            if Config.useLoginDarklangComForLogin then
              Live,
              "https://login.darklang.com",
              "https://logout.darklang.com/logout"
            else
              Local, "/login", "/logout"

          return! unauthorized ctx
      | Some sessionData -> return! next (save SessionData sessionData ctx)
    })

let loadSessionData (ctx : HttpContext) = load<Session.T> SessionData ctx

let userInfoMiddleware : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let sessionData = loadSessionData ctx

      match! Account.getUser (UserName.create sessionData.username) with
      | None -> return! notFound ctx
      | Some user -> return! next (save UserInfo user ctx)
    })

let loadUserInfo (ctx : HttpContext) = load<Account.UserInfo> UserInfo ctx

// checks permission on the canvas and continues. As a safety check, we add the
// CanvasName property here instead of the handler just fetching it. It's only
// added here only if the permission passes. This way we can not accidentally
// bypass it
let permissionMiddleware
  (permissionNeeded : Auth.Permission)
  (canvasName : CanvasName.T)
  : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let user = loadUserInfo ctx

      let! permitted =
        if permissionNeeded = Auth.Read then
          Auth.canViewCanvas canvasName user.username
        else if permissionNeeded = Auth.ReadWrite then
          Auth.canEditCanvas canvasName user.username
        else
          task { return false }

      if permitted then
        ctx
        |> save CanvasName canvasName
        |> save Permission permissionNeeded
        |> ignore // ignored as `save` is side-effecting

        return! next ctx
      else
        // Note that by design, canvasName is not saved if there is not permission
        return! unauthorized ctx
    })

let loadCanvasName (ctx : HttpContext) = load<CanvasName.T> CanvasName ctx
let loadPermission (ctx : HttpContext) = load<Auth.Permission> Permission ctx

let antiClickjackingMiddleware : HttpHandler =
  // Clickjacking: Don't allow any other websites to put this in an iframe;
  // this prevents "clickjacking" attacks.
  // https://www.owasp.org/index.php/Clickjacking_Defense_Cheat_Sheet#Content-Security-Policy:_frame-ancestors_Examples
  // It would be nice to use CSP to limit where we can load scripts etc from,
  // but right now we load from CDNs, <script> tags, etc. So the only thing
  // we could do is script-src: 'unsafe-inline', which doesn't offer us any
  // additional security.
  setHttpHeader "Content-security-policy" "frame-ancestors 'none';"

let serverVersionMiddleware : HttpHandler =
  setHttpHeader "x-darklang-server-version" Config.buildHash

let corsForLocalhostAssetsMiddleware : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let result = next ctx

      if Option.isSome (ctx.TryGetQueryStringValue "localhost-assets") then
        ctx.SetHttpHeader("Access-Control-Allow_origin", "*")

      return! result
    })


// --------------------
// Middleware stacks for the API
// --------------------
let apiHandler
  (handler : HttpContext -> Task<'a>)
  (neededPermission : Auth.Permission)
  (canvasName : string)
  : HttpHandler =
  sessionDataMiddleware
  >=> userInfoMiddleware
  >=> permissionMiddleware neededPermission (CanvasName.create canvasName)
  >=> (fun _ ctx ->
    task {
      let! result = handler ctx
      return! ctx.WriteJsonAsync result
    })
  >=> serverVersionMiddleware
  >=> setStatusCode 200

let htmlHandler
  (handler : HttpContext -> Task<string>)
  (neededPermission : Auth.Permission)
  (canvasName : string)
  : HttpHandler =
  // FSTODO: support integration tests
// if integration_test then Canvas.load_and_resave_from_test_file canvas ;
  sessionDataMiddleware
  >=> userInfoMiddleware
  >=> permissionMiddleware neededPermission (CanvasName.create canvasName)
  >=> (fun _ ctx ->
    task {
      let! result = handler ctx
      return! ctx.WriteHtmlStringAsync result
    })
  >=> corsForLocalhostAssetsMiddleware
  >=> antiClickjackingMiddleware
  >=> serverVersionMiddleware
  >=> setStatusCode 200
