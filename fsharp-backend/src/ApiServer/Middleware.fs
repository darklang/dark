module ApiServer.Middleware

// Middlewares used by the API server. Includes middleware functions and middleware stacks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.StaticFiles
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Primitives
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe

type ServerTimingMetric = Lib.AspNetCore.ServerTiming.Http.Headers.ServerTimingMetric
type ServerTiming = Lib.AspNetCore.ServerTiming.IServerTiming

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Tablecloth
open LibService.Telemetry

module Canvas = LibBackend.Canvas
module Config = LibBackend.Config
module Session = LibBackend.Session
module Account = LibBackend.Account
module Auth = LibBackend.Authorization

let (>=>) = Giraffe.Core.compose

// --------------------
// Server timing metrics
// --------------------
let getServerTiming (ctx : HttpContext) : Lib.AspNetCore.ServerTiming.IServerTiming =
  ctx.RequestServices.GetService<Lib.AspNetCore.ServerTiming.IServerTiming>()

// returns a function to be called which will record the elapsed time
let startTimer (ctx : HttpContext) : (string -> unit) =
  let st = getServerTiming ctx
  let sw = System.Diagnostics.Stopwatch()
  sw.Start()

  (fun metricName ->
    let result = (sw.Elapsed.TotalMilliseconds) |> decimal
    sw.Restart()
    let name = $"%03d{st.Metrics.Count}-{metricName}"
    st.Metrics.Add(ServerTimingMetric(name, result)))

let stop (span : Span.T) (ctx : HttpContext) : unit =
  span.Stop()
  let result = span.Duration.TotalMilliseconds |> decimal
  let st = getServerTiming ctx
  let name = $"%03d{st.Metrics.Count}-{span.DisplayName}"
  st.Metrics.Add(ServerTimingMetric(name, result))

type TraceTimer = { next : string -> unit; stop : unit -> unit }

let timer (ctx : HttpContext) : TraceTimer =
  let parent = Span.current ()
  let mutable child = null
  { next =
      fun name ->
        if child <> null then stop child ctx
        child <- Span.child name parent
    stop = fun () -> if child <> null then stop child ctx }

// --------------------
// Generic middlewares
// --------------------

// Copied from Giraffe HttpContextExtensions, and extended with timing info
[<Extension>]
type HttpContextExtensions() =
  [<Extension>]
  static member WriteJsonAsync<'T>(ctx : HttpContext, dataObj : 'T) =
    let span = Span.child "write-json-async" (Span.current ())
    ctx.SetContentType "application/json; charset=utf-8"
    // TODO: it's probably slower to have a separate serialization step, then
    // written into the body, vs writing directly into the body.
    let serialized = ctx.GetJsonSerializer().SerializeToBytes dataObj
    stop span ctx
    ctx.WriteBytesAsync serialized


let queryString (queries : List<string * string>) : string =
  queries
  |> List.map (fun (k, v) ->
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
    return! ctx.WriteJsonAsync "Not Found"
  }

let htmlHandler (f : HttpContext -> Task<string>) : HttpHandler =
  handleContext (fun ctx ->
    task {
      let! result = f ctx
      let span = Span.child "write-html-response" (Span.current ())
      let! newCtx = ctx.WriteHtmlStringAsync result
      stop span ctx
      return newCtx
    })

let jsonHandler (f : HttpContext -> Task<'a>) : HttpHandler =
  handleContext (fun ctx ->
    task {
      let! result = f ctx
      let! newCtx = ctx.WriteJsonAsync result
      return newCtx
    })

let jsonOptionHandler (f : HttpContext -> Task<Option<'a>>) : HttpHandler =
  handleContext (fun ctx ->
    task {
      match! f ctx with
      | Some result ->
        let! newCtx = ctx.WriteJsonAsync result
        return newCtx
      | None ->
        ctx.SetStatusCode 404
        return! ctx.WriteJsonAsync "Not found"
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
      let redirect =
        if Config.useLoginDarklangComForLogin then
          ctx.GetRequestUrl()
        else
          string ctx.Request.Path + string ctx.Request.QueryString


      let destination =
        if Config.useLoginDarklangComForLogin then
          "https://login.darklang.com"
        else
          "/login"

      let url = $"{destination}?redirect={System.Web.HttpUtility.UrlEncode redirect}"
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
  | ExecutionID

  override this.ToString() : string =
    match this with
    | UserInfo -> "user"
    | SessionData -> "sessionData"
    | CanvasInfo -> "canvasName"
    | Permission -> "permission"
    | ExecutionID -> "executionID"

let save' (id : dataID) (value : 'a) (ctx : HttpContext) : HttpContext =
  ctx.Items[ string id ] <- value
  ctx

let load'<'a> (id : dataID) (ctx : HttpContext) : 'a = ctx.Items[string id] :?> 'a

let loadSessionData (ctx : HttpContext) : Session.T =
  load'<Session.T> SessionData ctx

let loadUserInfo (ctx : HttpContext) : Account.UserInfo =
  load'<Account.UserInfo> UserInfo ctx

let loadCanvasInfo (ctx : HttpContext) : Canvas.Meta =
  load'<Canvas.Meta> CanvasInfo ctx

let loadExecutionID (ctx : HttpContext) : ExecutionID =
  load'<ExecutionID> ExecutionID ctx

let loadPermission (ctx : HttpContext) : Option<Auth.Permission> =
  load'<Option<Auth.Permission>> Permission ctx

let saveSessionData (s : Session.T) (ctx : HttpContext) = save' SessionData s ctx
let saveUserInfo (u : Account.UserInfo) (ctx : HttpContext) = save' UserInfo u ctx
let saveCanvasInfo (c : Canvas.Meta) (ctx : HttpContext) = save' CanvasInfo c ctx
let saveExecutionID (id : ExecutionID) (ctx : HttpContext) = save' ExecutionID id ctx

let savePermission (p : Option<Auth.Permission>) (ctx : HttpContext) =
  save' Permission p ctx

// --------------------
// APIServer Middlewares
// --------------------

let sessionDataMiddleware : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let span = Span.child "session-data-middleware" (Span.current ())
      let sessionKey = ctx.Request.Cookies.Item Session.cookieKey

      let! session =
        if ctx.Request.Method = "GET" then
          Session.getNoCSRF sessionKey
        else
          let csrfToken = ctx.Request.Headers.Item Session.csrfHeader |> string
          Session.get sessionKey csrfToken

      match session with
      | None ->
        stop span ctx
        return! redirectOr unauthorized ctx
      | Some sessionData ->
        let newCtx = saveSessionData sessionData ctx
        stop span ctx
        return! next newCtx
    })


let userInfoMiddleware : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let span = Span.child "user-info-middleware" (Span.current ())
      let sessionData = loadSessionData ctx

      match! Account.getUser sessionData.username with
      | None ->
        stop span ctx
        return! redirectOr notFound ctx
      | Some user ->
        ctx.SetHttpHeader("x-darklang-username", user.username)
        Span.current ()
        |> Span.addTags [ "username", sessionData.username
                          "userID", user.id
                          "is_admin", user.admin ]
        let newCtx = saveUserInfo user ctx
        stop span ctx
        return! next newCtx
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
      let span = Span.child "with-permission-middleware" (Span.current ())
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
        let (_ : HttpContext) =
          ctx |> saveCanvasInfo canvasInfo |> savePermission permission
        stop span ctx
        Span.current ()
        |> Span.addTags [ "canvas", canvasName; "canvasID", canvasID ]
        return! next ctx
      else
        // Note that by design, canvasName is not saved if there is not permission
        stop span ctx
        return! unauthorized ctx
    })

let executionIDMiddleware : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let executionID = LibService.Telemetry.executionID ()
      let newCtx = saveExecutionID executionID ctx
      let headerValue = StringValues([| string executionID |])
      ctx.SetHttpHeader("x-darklang-execution-id", string executionID)
      return! next newCtx
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
  setHttpHeader "x-darklang-server-version" LibService.Config.buildHash

let clientVersionMiddleware : HttpHandler =
  (fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let clientVersion = ctx.Request.Headers.Item "x-darklang-client-version"
      Span.current ()
      |> Span.addTags [ "request.header.client_version", clientVersion
                        // CLEANUP this was a bad name. Kept in until old data falls out of Honeycomb
                        "request.header.x-darklang-client-version", clientVersion ]
      return! next ctx
    })

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
  executionIDMiddleware
  >=> serverVersionMiddleware
  >=> clientVersionMiddleware
  >=> corsForLocalhostAssetsMiddleware
  >=> antiClickjackingMiddleware
  >=> setStatusCode 200

// --------------------
// Middleware stacks for the API
// --------------------

// FSTODO trace the "x-darklang-client-version" header

// Returns JSON API for calls on a particular canvas. Loads user and checks permission.
let apiHandler
  (f : HttpContext -> Task<'a>)
  (neededPermission : Auth.Permission)
  (canvasName : string)
  : HttpHandler =
  executionIDMiddleware
  >=> serverVersionMiddleware
  >=> clientVersionMiddleware
  >=> canvasMiddleware neededPermission (CanvasName.create canvasName)
  >=> jsonHandler f
  >=> setStatusCode 200

let apiOptionHandler
  (f : HttpContext -> Task<Option<'a>>)
  (neededPermission : Auth.Permission)
  (canvasName : string)
  : HttpHandler =
  executionIDMiddleware
  >=> serverVersionMiddleware
  >=> clientVersionMiddleware
  >=> canvasMiddleware neededPermission (CanvasName.create canvasName)
  >=> jsonOptionHandler f
  >=> setStatusCode 200

let canvasHtmlHandler
  (f : HttpContext -> Task<string>)
  (neededPermission : Auth.Permission)
  (canvasName : string)
  : HttpHandler =
  executionIDMiddleware
  >=> serverVersionMiddleware
  >=> clientVersionMiddleware
  >=> canvasMiddleware neededPermission (CanvasName.create canvasName)
  >=> htmlHandler f
  >=> htmlMiddleware

// Returns HTML without doing much else
let loggedOutHtmlHandler (f : HttpContext -> Task<string>) : HttpHandler =
  htmlHandler f >=> htmlMiddleware
