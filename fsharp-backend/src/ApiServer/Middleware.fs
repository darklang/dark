/// Middlewares used by the API server.
///
/// Includes middleware functions and middleware stacks
module ApiServer.Middleware

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open FSharp.Control.Tasks

type ServerTimingMetric = Lib.AspNetCore.ServerTiming.Http.Headers.ServerTimingMetric
type ServerTiming = Lib.AspNetCore.ServerTiming.IServerTiming

open Prelude
open Tablecloth
open Http

module Canvas = LibBackend.Canvas
module Config = LibBackend.Config
module Session = LibBackend.Session
module Account = LibBackend.Account
module Auth = LibBackend.Authorization
module Telemetry = LibService.Telemetry

// ------------------
// Http types
// ------------------

type HttpMiddleware = HttpHandler -> HttpHandler


// --------------------
// APIServer Middlewares
// --------------------

/// Either redirect to a login page, or apply the passed function if a
/// redirection is inappropriate (eg for the API)
let redirectOr (f : HttpContext -> Task) (ctx : HttpContext) : Task =
  task {
    if String.startsWith "/api/" ctx.Request.Path.Value then
      return! f ctx
    else
      let redirect =
        if Config.useLoginDarklangComForLogin then
          ctx.Request.GetEncodedUrl()
        else
          string ctx.Request.Path + string ctx.Request.QueryString


      let destination =
        if Config.useLoginDarklangComForLogin then
          "https://login.darklang.com"
        else
          "/login"

      let url = $"{destination}?redirect={System.Web.HttpUtility.UrlEncode redirect}"
      ctx.Response.Redirect(url, false)
      return ()
  }


let sessionDataMiddleware : HttpMiddleware =
  (fun (next : HttpHandler) (ctx : HttpContext) ->
    task {
      use t = startTimer "session-data-middleware" ctx
      let sessionKey = ctx.Request.Cookies.Item Session.cookieKey

      let! session =
        if ctx.Request.Method = "GET" then
          Session.getNoCSRF sessionKey
        else
          let csrfToken = ctx.Request.Headers.Item Session.csrfHeader |> string
          Session.get sessionKey csrfToken

      match session with
      | Some sessionData ->
        saveSessionData sessionData ctx
        return! next ctx
      | None -> return! redirectOr unauthorized ctx
    })


let userInfoMiddleware : HttpMiddleware =
  (fun (next : HttpHandler) (ctx : HttpContext) ->
    task {
      use t = startTimer "user-info-middleware" ctx
      let sessionData = loadSessionData ctx

      match! Account.getUser sessionData.username with
      | None -> return! redirectOr notFound ctx
      | Some user ->
        // CLEANUP - change to x-darklang-username
        ctx.SetHeader("x-dark-username", string user.username)
        t.span ()
        |> Telemetry.Span.addTags [ "username", sessionData.username
                                    "userID", user.id
                                    "is_admin", user.admin ]
        saveUserInfo user ctx
        return! next ctx
    })

// checks permission on the canvas and continues. As a safety check, we add the
// CanvasName property here instead of the handler just fetching it. It's only
// added here only if the permission passes. This way we can not accidentally
// bypass it
let withPermissionMiddleware
  (permissionNeeded : Auth.Permission)
  (canvasName : CanvasName.T)
  : HttpMiddleware =
  (fun (next : HttpHandler) (ctx : HttpContext) ->
    task {
      use t = startTimer "with-permission-middleware" ctx
      let user = loadUserInfo ctx
      // CLEANUP: reduce to one query
      // collect all the info up front so we don't spray these DB calls everywhere. We need them all anyway
      let ownerName = Account.ownerNameFromCanvasName canvasName
      let! ownerID = Account.userIDForUserName (ownerName.toUserName ())
      let! canvasID = Canvas.canvasIDForCanvasName ownerID canvasName

      let canvasInfo : Canvas.Meta =
        { name = canvasName; id = canvasID; owner = ownerID }

      let! permission = Auth.permission ownerName user.username

      // This is a precarious function call, be careful
      if Auth.permitted permissionNeeded permission then
        saveCanvasInfo canvasInfo ctx
        savePermission permission ctx
        Telemetry.addTags [ "canvas", canvasName; "canvasID", canvasID ]
        return! next ctx
      else
        // Note that by design, canvasName is not saved if there is not permission
        return! unauthorized ctx
    })

let canvasMiddleware (permissionNeeded : Auth.Permission) : HttpMiddleware =
  (fun (next : HttpHandler) (ctx : HttpContext) ->
    // Last to run wraps ones which run earlier
    let canvasName = ctx.GetRouteData().Values.["canvasName"] :?> string
    let canvasName = CanvasName.create canvasName
    let middleware =
      next
      |> withPermissionMiddleware permissionNeeded canvasName
      |> userInfoMiddleware
      |> sessionDataMiddleware
    middleware ctx)


let antiClickjackingMiddleware : HttpMiddleware =
  // Clickjacking: Don't allow any other websites to put this in an iframe;
  // this prevents "clickjacking" attacks.
  // https://www.owasp.org/index.php/Clickjacking_Defense_Cheat_Sheet#Content-Security-Policy:_frame-ancestors_Examples
  // It would be nice to use CSP to limit where we can load scripts etc from,
  // but right now we load from CDNs, <script> tags, etc. So the only thing
  // we could do is script-src: 'unsafe-inline', which doesn't offer us any
  // additional security.
  (fun (next : HttpHandler) (ctx : HttpContext) ->
    task {
      ctx.SetHeader("Content-security-policy", "frame-ancestors 'none';")
      return! next ctx
    })

let serverVersionMiddleware : HttpMiddleware =
  (fun (next : HttpHandler) (ctx : HttpContext) ->
    task {
      ctx.SetHeader("x-darklang-server-version", LibService.Config.buildHash)
      return! next ctx
    })

let clientVersionMiddleware : HttpMiddleware =
  (fun (next : HttpHandler) (ctx : HttpContext) ->
    task {
      let clientVersion = ctx.Request.Headers.Item "x-darklang-client-version"
      Telemetry.addTags [ "request.header.client_version", clientVersion
                          // CLEANUP this was a bad name. Kept in until old data falls out of Honeycomb
                          "request.header.x-darklang-client-version", clientVersion ]
      return! next ctx
    })

let executionIDMiddleware : HttpMiddleware =
  (fun (next : HttpHandler) (ctx : HttpContext) ->
    task {
      let executionID = Telemetry.rootID ()
      ctx.SetHeader("x-darklang-execution-id", executionID)
      let! newCtx = next ctx
      return newCtx
    })

let corsForLocalhostAssetsMiddleware : HttpMiddleware =
  (fun (next : HttpHandler) (ctx : HttpContext) ->
    task {
      if Option.isSome (ctx.GetQueryStringValue "localhost-assets") then
        ctx.SetHeader("Access-Control-Allow_origin", "*")
      return! next ctx
    })

let standardMiddleware : HttpMiddleware =
  fun next ->
    next
    |> executionIDMiddleware
    |> clientVersionMiddleware
    |> serverVersionMiddleware

let htmlMiddleware : HttpMiddleware =
  fun next ->
    next
    |> standardMiddleware
    |> corsForLocalhostAssetsMiddleware
    |> antiClickjackingMiddleware


[<Extension>]
type ApplicationBuilderExtensions() =

  // Wrap middleware into right shape
  [<Extension>]
  static member UseMiddleware(ab : IApplicationBuilder, mw : HttpMiddleware) : unit =
    ab.Use(
      System.Func<HttpContext, RequestDelegate, Task> (fun ctx rd ->
        mw (fun ctx -> rd.Invoke ctx) ctx)
    )
    |> ignore<IApplicationBuilder>
