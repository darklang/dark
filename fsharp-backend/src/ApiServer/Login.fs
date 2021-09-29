module ApiServer.Login

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Giraffe

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Tablecloth

module Config = LibBackend.Config
module Session = LibBackend.Session
module Account = LibBackend.Account

// --------------------
// Cookie stuff
// --------------------

// FSTODO: respect DARK_CONFIG_USE_LOGIN_DARKLANG_COM_FOR_LOGIN=n

// get the domain of a request
let domain (ctx : HttpContext) : string =
  // For why we use 'darklang.com' and not '.darklang.com', see
  // https://www.mxsasha.eu/blog/2014/03/04/definitive-guide-to-cookie-domains/
  // tl;dr: with a leading-dot was the specified behavior prior to
  // RFC6265 (2011), and in theory is still okay because the leading
  // dot is ignored, but .darklang.localhost doesn't work and
  // darklang.localhost does, so ... no leading dot works better for
  // us.
  ctx.GetRequestHeader "host"
  |> Result.unwrap "darklang.com"
  // Host: darklang.localhost:9000 is properly set in-cookie as
  // "darklang.localhost", the cookie domain doesn't want the
  // port
  |> FsRegEx.replace $":{LibService.Config.apiServerNginxPort}" ""

let cookieOptionsFor (ctx : HttpContext) =
  let options = CookieOptions()
  options.Domain <- domain ctx
  options.HttpOnly <- true
  options.Secure <- Config.useHttps
  options.Path <- "/"
  options.MaxAge <- System.TimeSpan(0, 0, 604800)
  options


// --------------------
// Logout
// --------------------
// FSTODO: test logout when logged in, and when logged out
let logout : HttpHandler =
  (fun next (ctx : HttpContext) ->
    // TODO move these into config urls
    task {
      try
        // if no session data, continue without deleting it
        let sessionData = Middleware.loadSessionData ctx
        do! Session.clear sessionData.key
      with
      | _ -> ()

      ctx.Response.Cookies.Delete(Session.cookieKey, cookieOptionsFor ctx)

      return!
        if Config.useLoginDarklangComForLogin then
          redirectTo false "https://login.darklang.com/logout" next ctx
        else
          redirectTo false "/login" next ctx
    })
  >=> Middleware.userMiddleware


// --------------------
// Login
// --------------------
let loginUiTemplate : string = LibBackend.File.readfile Config.Templates "login.html"

let loginPage : HttpHandler =
  // CLEANUP move these into config urls
  if Config.useLoginDarklangComForLogin then
    handleContext
      (fun ctx -> redirectTo false "https://login.darklang.com" earlyReturn ctx)
  else
    (Middleware.loggedOutHtmlHandler (fun _ -> task { return loginUiTemplate }))

let loginHandler : HttpHandler =
  (fun _ (ctx : HttpContext) ->
    task {
      let usernameOrEmail = ctx.GetFormValue "username" |> Option.unwrap ""
      let password = ctx.GetFormValue "password" |> Option.unwrap ""

      let redirect =
        ctx.GetFormValue "redirect"
        |> Option.unwrap ""
        |> System.Web.HttpUtility.UrlDecode

      match! Account.authenticate usernameOrEmail password with
      | None ->
        let redirect = if redirect = "" then [] else [ "redirect", redirect ]
        let error = [ "error", "Invalid username or password" ]
        let qs = Middleware.queryString (redirect @ error)

        return! redirectTo false $"/login?{qs}" earlyReturn ctx
      | Some username ->
        let! sessionData = Session.insert username

        ctx.Response.Cookies.Append(
          Session.cookieKey,
          sessionData.sessionKey,
          cookieOptionsFor ctx
        )

        let location = if redirect = "" then $"/a/{username}" else redirect
        return! redirectTo false location earlyReturn ctx
    })
