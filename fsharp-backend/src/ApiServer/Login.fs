module ApiServer.Login

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Tablecloth

module Config = LibBackend.Config
module Session = LibBackend.Session
module Account = LibBackend.Account
module Auth = LibBackend.Authorization

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
  ctx.GetRequestHeader("host")
  |> Result.unwrap "darklang.com"
  // Host: darklang.localhost:8000 is properly set in-cookie as
  // "darklang.localhost", the cookie domain doesn't want the
  // port
  |> FsRegEx.replace ":9000" ""

let cookieOptionsFor (ctx : HttpContext) =
  let options = CookieOptions()
  options.Domain <- domain ctx
  options.HttpOnly <- true
  options.Secure <- String.startsWith "https:" ctx.Request.Host.Host
  options.Path <- "/"
  options


// --------------------
// Logout
// --------------------
// FSTODO: test logout when logged in, and when logged out
let logout : HttpHandler =
  Middleware.userMiddleware
    (fun next (ctx : HttpContext) ->
      // CLEANUP move these into config urls
      if Config.useLoginDarklangComForLogin then
        redirectTo false "https://logout.darklang.com/logout" next ctx
      else
        task {
          let sessionData = Middleware.loadSessionData ctx
          do! Session.clear sessionData.key

          ctx.Response.Cookies.Delete(Session.cookieKey, cookieOptionsFor ctx)

          return! redirectTo false "/login" next ctx
        })

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
    (Middleware.loggedOutHtmlHandler
      (fun _ ->
        task {
          debuG "executing login page" ()
          return loginUiTemplate
        }))

let loginHandler : HttpHandler =
  handleContext
    (fun (ctx : HttpContext) ->
      task {
        debuG "executing login handler" ()
        let usernameOrEmail = ctx.Request.Form.Item "username" |> toString
        let password = ctx.Request.Form.Item "password" |> toString
        let redirect = ctx.Request.Form.Item "redirect" |> toString

        match! Account.authenticate usernameOrEmail password with
        | None ->
            fstodo "add error to query"
            fstodo "add redirect to query"
            return! redirectTo false "/login" earlyReturn ctx
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

// --------------------
// endpoints
// --------------------
let endpoints : Endpoint list =
  [ GET [ route "/login" loginPage; route "/logout" logout ]
    POST [ route "login" loginHandler; route "/logout" logout ] ]
