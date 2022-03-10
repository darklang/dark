///
module ApiServer.Login

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

open Http


module Config = LibBackend.Config
module Session = LibBackend.Session
module Account = LibBackend.Account

// --------------------
// Cookie stuff
// --------------------

// get the domain of a request
let domain (ctx : HttpContext) : string =
  // For why we use 'darklang.com' and not '.darklang.com', see
  // https://www.mxsasha.eu/blog/2014/03/04/definitive-guide-to-cookie-domains/
  // tl;dr: with a leading-dot was the specified behavior prior to
  // RFC6265 (2011), and in theory is still okay because the leading
  // dot is ignored, but .darklang.localhost doesn't work and
  // darklang.localhost does, so ... no leading dot works better for
  // us.
  ctx.GetHeader "host"
  |> Option.unwrap "darklang.com"
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

/// API endpoint to log out of a User's session
let logout : HttpHandler =
  (fun (ctx : HttpContext) ->
    // TODO move these into config urls
    task {
      try
        // if no session data, continue without deleting it
        let sessionData = loadSessionData ctx
        do! Session.clear sessionData.key
      with
      | _ -> ()

      ctx.Response.Cookies.Delete(Session.cookieKey, cookieOptionsFor ctx)
      let url =
        if Config.useLoginDarklangComForLogin then
          "https://login.darklang.com/logout"
        else
          "/login"
      ctx.Response.Redirect(url, false)
      return ()
    })



// --------------------
// Login
// --------------------
let loginUiTemplate : string = LibBackend.File.readfile Config.Templates "login.html"

/// API endpoint that returns the HTML login page
let loginPage (ctx : HttpContext) : Task =
  task {
    // CLEANUP move these into config urls
    if Config.useLoginDarklangComForLogin then
      ctx.Response.Redirect("https://login.darklang.com", false)
      return ()
    else
      // logged out login page
      return!
        Middleware.htmlMiddleware
          (htmlHandler (fun _ctx -> Task.FromResult loginUiTemplate))
          ctx

  }


/// API endpoint to handle a Login request, given credentials
let loginHandler (ctx : HttpContext) : Task =
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
      let qs = Http.queryString (redirect @ error)
      ctx.Response.Redirect($"/login?{qs}", false)
      return ()
    | Some username ->
      let! sessionData = Session.insert (UserName.create username)

      ctx.Response.Cookies.Append(
        Session.cookieKey,
        sessionData.sessionKey,
        cookieOptionsFor ctx
      )

      let location = if redirect = "" then $"/a/{username}" else redirect
      ctx.Response.Redirect(location, false)
      return ()
  }
