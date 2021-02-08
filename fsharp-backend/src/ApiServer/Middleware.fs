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
open Tablecloth


module Config = LibBackend.Config
module Session = LibBackend.Session
module Account = LibBackend.Account
module Auth = LibBackend.Authorization

let (>=>) = Giraffe.Core.compose

// --------------------
// Generic middlewares
// --------------------

let queryString (queries : List<string * string>) : string =
  queries
  |> List.map
       (fun (k, v) ->
         let k = System.Web.HttpUtility.UrlEncode k
         let v = System.Web.HttpUtility.UrlEncode v
         $"{k}={v}")
  |> String.concat "&"

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

let htmlHandler (f : HttpContext -> Task<string>) : HttpHandler =
  handleContext
    (fun ctx ->
      task {
        let! result = f ctx
        return! ctx.WriteHtmlStringAsync result
      })

let jsonHandler (f : HttpContext -> Task<'a>) : HttpHandler =
  handleContext
    (fun ctx ->
      task {
        let! result = f ctx
        return! ctx.WriteJsonAsync result
      })

// Either redirect to a login page, or apply the passed function if a
// redirection is inappropriate (eg for the API)
let redirectOr
  (f : HttpContext -> Task<HttpContext option>)
  (ctx : HttpContext)
  : Task<HttpContext option> =
  task {
    if String.startsWith "/api/" ctx.Request.Path.Value then
      return! f ctx
    else
      let redirect = ctx.GetRequestUrl() |> System.Web.HttpUtility.UrlEncode

      let destination =
        if Config.useLoginDarklangComForLogin then
          "https://login.darklang.com"
        else
          "/login"

      let url = $"{destination}?redirect={redirect}"
      ctx.Response.Redirect(url, false)
      return Some ctx
  }
// --------------------
// Accessing data from a HttpContext
// --------------------

// Don't use strings for this interface
type dataID =
  | UserInfo
  | SessionData
  | CanvasInfo
  | Permission

  override this.ToString() : string =
    match this with
    | UserInfo -> "user"
    | SessionData -> "sessionData"
    | CanvasInfo -> "canvasName"
    | Permission -> "permission"

let save' (id : dataID) (value : 'a) (ctx : HttpContext) : HttpContext =
  ctx.Items.[id.ToString()] <- value
  ctx

let load'<'a> (id : dataID) (ctx : HttpContext) : 'a =
  ctx.Items.[$"{id}".ToString()] :?> 'a

type CanvasInfo = { name : CanvasName.T; id : CanvasID; owner : UserID }

let loadSessionData (ctx : HttpContext) : Session.T =
  load'<Session.T> SessionData ctx

let loadUserInfo (ctx : HttpContext) : Account.UserInfo =
  load'<Account.UserInfo> UserInfo ctx

let loadCanvasInfo (ctx : HttpContext) : CanvasInfo =
  load'<CanvasInfo> CanvasInfo ctx

let loadPermission (ctx : HttpContext) : Option<Auth.Permission> =
  load'<Option<Auth.Permission>> Permission ctx

let saveSessionData (s : Session.T) (ctx : HttpContext) = save' SessionData s ctx
let saveUserInfo (u : Account.UserInfo) (ctx : HttpContext) = save' UserInfo u ctx
let saveCanvasInfo (c : CanvasInfo) (ctx : HttpContext) = save' CanvasInfo c ctx
let savePermission (p : Option<Auth.Permission>) (ctx : HttpContext) = save' Permission p ctx



// --------------------
// APIServer Middlewares
// --------------------


let sessionDataMiddleware : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let sessionKey = ctx.Request.Cookies.Item Session.cookieKey

      let! session =
        if ctx.Request.Method = "GET" then
          Session.getNoCSRF sessionKey
        else
          let csrfToken = ctx.Request.Headers.Item Session.csrfHeader |> toString
          Session.get sessionKey csrfToken

      match session with
      | None -> return! redirectOr unauthorized ctx
      | Some sessionData -> return! next (saveSessionData sessionData ctx)
    })


let userInfoMiddleware : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let sessionData = loadSessionData ctx

      match! Account.getUser (UserName.create sessionData.username) with
      | None -> return! redirectOr notFound ctx
      | Some user -> return! next (saveUserInfo user ctx)
    })

// checks permission on the canvas and continues. As a safety check, we add the
// CanvasName property here instead of the handler just fetching it. It's only
// added here only if the permission passes. This way we can not accidentally
// bypass it
let withPermissionMiddleware
  (permissionNeeded : Auth.Permission)
  (canvasName : CanvasName.T)
  : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let user = loadUserInfo ctx
      // CLEANUP: reduce to one query
      // collect all the info up front so we don't spray these DB calls everywhere. We need them all anyway
      let ownerName = Account.ownerNameFromCanvasName canvasName
      let! ownerID = Account.userIDForUserName (ownerName.toUserName ())
      let! canvasID = LibBackend.Canvas.canvasIDForCanvasName ownerID canvasName
      let canvasInfo = { name = canvasName; id = canvasID; owner = ownerID }
      let! permission = Auth.permission ownerName ownerID user.username

      // This is a precarious function call, be careful
      if Auth.permitted permissionNeeded permission then
        ctx |> saveCanvasInfo canvasInfo |> savePermission permission |> ignore // ignored as `save` is side-effecting

        return! next ctx
      else
        // Note that by design, canvasName is not saved if there is not permission
        return! unauthorized ctx
    })


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

let userMiddleware : HttpHandler = sessionDataMiddleware >=> userInfoMiddleware

let canvasMiddleware
  (neededPermission : Auth.Permission)
  (canvasName : CanvasName.T)
  : HttpHandler =
  userMiddleware >=> withPermissionMiddleware neededPermission canvasName

// --------------------
// Composed middlewarestacks for the API
// --------------------
let htmlMiddleware : HttpHandler =
  serverVersionMiddleware
  >=> corsForLocalhostAssetsMiddleware
  >=> antiClickjackingMiddleware
  >=> setStatusCode 200

// --------------------
// Middleware stacks for the API
// --------------------

// Returns JSON API for calls on a particular canvas. Loads user and checks permission.
let apiHandler
  (f : HttpContext -> Task<'a>)
  (neededPermission : Auth.Permission)
  (canvasName : string)
  : HttpHandler =
  canvasMiddleware neededPermission (CanvasName.create canvasName)
  >=> jsonHandler f
  >=> serverVersionMiddleware
  >=> setStatusCode 200

let canvasHtmlHandler
  (f : HttpContext -> Task<string>)
  (neededPermission : Auth.Permission)
  (canvasName : string)
  : HttpHandler =
  canvasMiddleware neededPermission (CanvasName.create canvasName)
  >=> htmlHandler f
  >=> htmlMiddleware

// Returns HTML without doing much else
let loggedOutHtmlHandler (f : HttpContext -> Task<string>) : HttpHandler =
  htmlHandler f >=> htmlMiddleware
