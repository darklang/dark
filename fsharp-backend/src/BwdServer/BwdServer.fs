module BwdServer

// This is the webserver for builtwithdark.com. It uses ASP.NET directly,
// instead of a web framework, so we can tuen the exact behaviour of headers
// and such.

open FSharp.Control.Tasks
open System.Threading.Tasks

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core


open Prelude
open Tablecloth
open FSharpx

module HealthCheck = LibService.HealthCheck
module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes
module RealExe = LibBackend.RealExecution
module Exe = LibExecution.Execution
module Interpreter = LibExecution.Interpreter
module Account = LibBackend.Account
module Canvas = LibBackend.Canvas
module Routing = LibBackend.Routing
module Pusher = LibBackend.Pusher
module TI = LibBackend.TraceInputs

// ---------------
// Headers
// ---------------
let setHeader (ctx : HttpContext) (name : string) (value : string) : unit =
  // There's an exception thrown for duplicate test name. We just want the last
  // header to win, especially since the user can add the same headers we add
  let (_ : bool) = ctx.Response.Headers.Remove name

  ctx.Response.Headers.Add(
    name,
    Microsoft.Extensions.Primitives.StringValues([| value |])
  )

let getHeader (ctx : HttpContext) (name : string) : string option =
  match ctx.Request.Headers.TryGetValue name with
  | true, vs -> vs.ToArray() |> Array.toSeq |> String.concat "," |> Some
  | false, _ -> None


// ---------------
// Responses
// ---------------
let favicon : Lazy<byte array> =
  lazy (LibBackend.File.readfileBytes LibBackend.Config.Webroot "favicon-32x32.png")

let faviconResponse (ctx : HttpContext) : Task<HttpContext> =
  task {
    // NB: we're sending back a png, not an ico - this is deliberate,
    // favicon.ico can be png, and the png is 685 bytes vs a 4+kb .ico
    let bytes = Lazy.force favicon
    setHeader ctx "Access-Control-Allow-Origin" "*"
    ctx.Response.ContentType <- "image/png"
    ctx.Response.ContentLength <- int64 bytes.Length
    ctx.Response.StatusCode <- 200
    do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
    return ctx
  }

let textPlain = Some "text/plain; charset=utf-8"

let standardResponse
  (ctx : HttpContext)
  (msg : string)
  (contentType : Option<string>)
  (code : int)
  : Task<HttpContext> =
  task {
    let bytes = System.Text.Encoding.UTF8.GetBytes msg
    setHeader ctx "Access-Control-Allow-Origin" "*"

    match contentType with
    | None -> ()
    | Some typ -> ctx.Response.ContentType <- typ

    ctx.Response.ContentLength <- int64 bytes.Length
    ctx.Response.StatusCode <- code
    do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
    return ctx
  }

let noHandlerResponse (ctx : HttpContext) : Task<HttpContext> =
  // cors
  standardResponse ctx "404 Not Found: No route matches" textPlain 404

let canvasNotFoundResponse (ctx : HttpContext) : Task<HttpContext> =
  standardResponse ctx "user not found" textPlain 404

let moreThanOneHandlerResponse (ctx : HttpContext) : Task<HttpContext> =
  let path = ctx.Request.Path.Value
  let message = $"500 Internal Server Error: More than one handler for route: {path}"
  standardResponse ctx message textPlain 500

let dincompleteResponse (ctx : HttpContext) : Task<HttpContext> =
  // maybe cors, no ct
  let message =
    "Application error: the executed code was not complete. This error can be resolved by the application author by completing the incomplete code."

  standardResponse ctx message textPlain 500

let derrorResponse (ctx : HttpContext) : Task<HttpContext> =
  // maybe cors, no ct
  let message =
    "Application error: the executed program was invalid. This problem can be resolved by the application's author by resolving the invalid code (often a type error)."

  standardResponse ctx message textPlain 500

exception LoadException of string * int

// runs a function and upon error, catches and rethrows the passed exception
let catch (msg : string) (code : int) (fn : 'a -> Task<'b>) (value : 'a) : Task<'b> =
  task {
    try
      return! fn value
    with _ -> return! raise (LoadException(msg, code))
  }

// ---------------
// CORS
// ---------------
// FSTODO: for now, we'll read the CORS settings and write the response headers. This is probably wrong but we'll see
let getCORS (ctx : HttpContext) (canvasID : CanvasID) : Task<unit> =
  task {
    let! corsSetting = Canvas.fetchCORSSetting canvasID
    let originHeader = getHeader ctx "Origin"

    let defaultOrigins =
      [ "http://localhost:3000"; "http://localhost:5000"; "http://localhost:8000" ]

    let header =
      match (originHeader, corsSetting) with
      // if there's no explicit canvas setting, allow common localhosts
      | Some origin, None when List.contains origin defaultOrigins -> Some origin
      // if there's no explicit canvas setting and no default match, fall back to "*"
      | _, None -> Some "*"
      // If there's a "*" in the setting, always use it.
      // This is help as a debugging aid since users will always see
      //   Access-Control-Allow-Origin: * in their browsers, even if the
      //   request has no Origin.
      | _, Some Canvas.AllOrigins -> Some "*"
      // if there's no supplied origin, don't set the header at all.
      | None, _ -> None
      // Return the origin if and only if it's in the setting
      | Some origin, Some (Canvas.Origins origins) when List.contains origin origins ->
          Some origin
      // Otherwise: there was a supplied origin and it's not in the setting.
      // return "null" explicitly
      | Some _, Some _ -> Some "null"

    match header with
    | Some v -> setHeader ctx "Access-Control-Allow-Origin" v
    | None -> ()

    return ()
  }

// ---------------
// HttpsRedirect
// ---------------
let shouldRedirect (ctx : HttpContext) : bool =
  LibBackend.Config.redirectHttp && not ctx.Request.IsHttps

let httpsRedirect (ctx : HttpContext) : HttpContext =
  // adapted from https://github.com/aspnet/BasicMiddleware/blob/master/src/Microsoft.AspNetCore.HttpsPolicy/HttpsRedirectionMiddleware.cs
  let req = ctx.Request
  let host = HostString req.Host.Host

  let redirectUrl =
    UriHelper.BuildAbsolute("https", host, req.PathBase, req.Path, req.QueryString)

  // CLEANUP use better status code, maybe 307
  ctx.Response.StatusCode <- 302
  setHeader ctx "Location" redirectUrl
  ctx

// ---------------
// Read from HttpContext
// ---------------
type KeyValuePair<'k, 'v> = System.Collections.Generic.KeyValuePair<'k, 'v>

type StringValues = Microsoft.Extensions.Primitives.StringValues

let getHeaders (ctx : HttpContext) : Map<string, string> =
  ctx.Request.Headers
  |> Seq.map
       (fun (kvp : KeyValuePair<string, StringValues>) ->
         (kvp.Key, kvp.Value.ToArray() |> Array.toList |> String.concat ","))
  |> Map

let getBody (ctx : HttpContext) : Task<byte array> =
  task {
    let ms = new IO.MemoryStream()
    do! ctx.Request.Body.CopyToAsync(ms)
    return ms.ToArray()
  }


// ---------------
// Handle builtwithdark request
// ---------------
let runHttp
  (c : Canvas.T)
  (tlid : tlid)
  (traceID : LibExecution.AnalysisTypes.TraceID)
  (url : string)
  (headers : Map<string, string>)
  (body : byte [])
  (inputVars : RT.Symtable)
  (expr : RT.Expr)
  : Task<RT.Dval * Exe.HashSet<tlid>> =
  task {
    let program = Canvas.toProgram c
    let! state, touchedTLIDs = RealExe.createState traceID tlid program
    let symtable = Interpreter.withGlobals state inputVars

    let headers = headers |> Map.map RT.DStr |> RT.DObj

    let! result =
      Interpreter.callFn
        state
        (RT.Expr.toID expr)
        (RT.FQFnName.stdlibFqName "Http" "middleware" 0)
        [ RT.DStr url
          RT.DBytes body
          headers
          RT.DFnVal(
            RT.Lambda
              { parameters = [ gid (), "request" ]
                symtable = symtable
                body = expr }
          ) ]
        RT.NotInPipe
        RT.NoRail
      |> TaskOrValue.toTask

    return (Exe.extractHttpErrorRail result, touchedTLIDs)
  }

let runDarkHandler (ctx : HttpContext) : Task<HttpContext> =
  task {
    setHeader ctx "Server" "Darklang"

    let msg (code : int) (msg : string) =
      task {
        let bytes = System.Text.Encoding.UTF8.GetBytes msg
        ctx.Response.StatusCode <- code
        if bytes.Length > 0 then ctx.Response.ContentType <- "text/plain"
        ctx.Response.ContentLength <- int64 bytes.Length
        do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
        return ctx
      }

    let loggerFactory = ctx.RequestServices.GetService<ILoggerFactory>()
    let logger = loggerFactory.CreateLogger("logger")
    let log msg (v : 'a) = logger.LogError("{msg}: {v}", msg, v)

    match! Routing.canvasNameFromHost ctx.Request.Host.Host with
    | Some canvasName ->
        // CLEANUP: move execution ID header up
        let executionID = gid ()
        setHeader ctx "x-darklang-execution-id" (toString executionID)
        let ownerName = Account.ownerNameFromCanvasName canvasName
        let ownerUsername = UserName.create (toString ownerName)

        let! ownerID =
          catch "user not found" 404 Account.userIDForUserName ownerUsername

        // No error checking as this will create a canvas if none exists
        let! canvasID = Canvas.canvasIDForCanvasName ownerID canvasName

        let meta : Canvas.Meta =
          { id = canvasID; owner = ownerID; name = canvasName }

        let traceID = System.Guid.NewGuid()
        let method = ctx.Request.Method
        let requestPath = ctx.Request.Path.Value |> Routing.sanitizeUrlPath

        // redirect HEADs to GET. We pass the actual HEAD method to the engine,
        // and leave it to middleware to say what it wants to do with that
        let searchMethod = if method = "HEAD" then "GET" else method

        let! c =
          Canvas.loadHttpHandlersFromCache meta requestPath searchMethod
          |> Task.map Result.unwrapUnsafe

        let pages =
          Routing.filterMatchingHandlers requestPath (Map.values c.handlers)

        match pages with
        | [ { spec = PT.Handler.HTTP (route = route); ast = expr; tlid = tlid } ] ->
            let url = ctx.Request.GetEncodedUrl()
            // FSTODO: move vars into http middleware
            let vars = Routing.routeInputVars route requestPath

            match vars with
            | Some vars ->
                let symtable = Map.ofList vars
                let! body = getBody ctx
                let expr = expr.toRuntimeType ()
                let headers = getHeaders ctx

                let! result, touchedTLIDs =
                  runHttp c tlid traceID url headers body symtable expr

                do! getCORS ctx canvasID

                // FSTODO: save event in the http pipeline

                // Do not resolve task, send this into the ether
                let tlids = HashSet.toList touchedTLIDs
                Pusher.pushNewTraceID executionID canvasID traceID tlids

                match result with
                | RT.DHttpResponse (RT.Redirect url) ->
                    ctx.Response.Redirect(url, false)
                    return ctx
                | RT.DHttpResponse (RT.Response (status, headers, RT.DBytes body)) ->
                    ctx.Response.StatusCode <- status
                    // FSTODO content type of application/json for dobj and dlist
                    List.iter (fun (k, v) -> setHeader ctx k v) headers
                    do! ctx.Response.Body.WriteAsync(body, 0, body.Length)
                    return ctx
                | RT.DIncomplete _ -> return! dincompleteResponse ctx
                | RT.DError _ -> return! derrorResponse ctx
                | other ->
                    // all other cases should be handled in middleware
                    printfn $"Not a HTTP response: {other}"
                    return! msg 500 "body is not a HttpResponse"
            | None -> // vars didnt parse
                return!
                  msg
                    500
                    $"The request ({requestPath}) does not match the route ({route})"
        | [] when toString ctx.Request.Path = "/favicon.ico" ->
            return! faviconResponse ctx
        | [] ->
            // FSTODO: The request body is created in the pipeline. Should we run this through the pipeline to a handler that returns 404?
            let event = RT.DNull

            let! timestamp =
              TI.storeEvent canvasID traceID ("HTTP", requestPath, method) event

            Pusher.pushNew404
              executionID
              canvasID
              ("HTTP", requestPath, method, timestamp, traceID)

            return! noHandlerResponse ctx
        | _ -> return! moreThanOneHandlerResponse ctx
    | None -> return! canvasNotFoundResponse ctx
  }


// ---------------
// Configure Kestrel/ASP.NET
// ---------------
let configureApp (app : IApplicationBuilder) =
  let handler (ctx : HttpContext) =
    (task {
      try
        // Do this here so we don't redirect the health check
        if shouldRedirect ctx then
          return httpsRedirect ctx
        else
          return! runDarkHandler ctx
      with
      | LoadException (msg, code) ->
          // FSTODO log/honeycomb, rollbar
          let bytes = System.Text.Encoding.UTF8.GetBytes msg
          ctx.Response.StatusCode <- code
          if bytes.Length > 0 then ctx.Response.ContentType <- "text/plain"
          ctx.Response.ContentLength <- int64 bytes.Length
          do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
          return ctx
      | e ->
          printfn "%s" (toString e)
          return raise e
     })
    :> Task

  app
  |> LibService.Rollbar.AspNet.addRollbarToApp
  |> fun app -> app.UseRouting()
  // must go after UseRouting
  |> HealthCheck.configureApp LibService.Config.bwdServerHealthCheckPort
  |> fun app -> app.Run(RequestDelegate handler)

let configureServices (services : IServiceCollection) : unit =
  let (_ : IServiceCollection) =
    services
    |> LibService.Rollbar.AspNet.addRollbarToServices
    |> LibService.Telemetry.AspNet.addTelemetryToServices "BwdServer"
    |> HealthCheck.configureServices

  ()

let webserver (shouldLog : bool) (port : int) =
  let hcUrl = HealthCheck.url LibService.Config.bwdServerHealthCheckPort

  WebHost.CreateDefaultBuilder()
  |> fun wh -> wh.UseKestrel(LibService.Kestrel.configureKestrel)
  |> fun wh -> wh.UseUrls(hcUrl, $"http://*:{port}")
  |> fun wh -> wh.ConfigureServices(configureServices)
  |> fun wh -> wh.Configure(configureApp)
  |> fun wh -> wh.Build()


[<EntryPoint>]
let main _ =
  printfn "Starting BwdServer"
  LibBackend.Init.init "Bwdserver"
  (webserver true LibService.Config.bwdServerPort).Run()
  0
