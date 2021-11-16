module BwdServer

// This is the webserver for builtwithdark.com. It uses ASP.NET directly,
// instead of a web framework, so we can tune the exact behaviour of headers
// and such.

open FSharp.Control.Tasks
open System.Threading.Tasks

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.AspNetCore.Http.Abstractions
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection

type KeyValuePair<'k, 'v> = System.Collections.Generic.KeyValuePair<'k, 'v>
type StringValues = Microsoft.Extensions.Primitives.StringValues

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module RealExe = LibRealExecution.RealExecution
module Exe = LibExecution.Execution
module Interpreter = LibExecution.Interpreter
module Account = LibBackend.Account
module Canvas = LibBackend.Canvas
module Routing = LibBackend.Routing
module Pusher = LibBackend.Pusher
module TI = LibBackend.TraceInputs
module Req = LibExecution.HttpRequest
module Resp = LibExecution.HttpResponse

// ---------------
// Headers
// ---------------
let setHeader (ctx : HttpContext) (name : string) (value : string) : unit =
  ctx.Response.Headers.[name] <- StringValues([| value |])

let getHeader (hs : IHeaderDictionary) (name : string) : string option =
  match hs.TryGetValue name with
  | true, vs -> vs.ToArray() |> Array.toSeq |> String.concat "," |> Some
  | false, _ -> None


// ---------------
// Responses
// ---------------
let favicon : Lazy<ReadOnlyMemory<byte>> =
  lazy
    (LibBackend.File.readfileBytes LibBackend.Config.Webroot "favicon-32x32.png"
     |> ReadOnlyMemory)

let faviconResponse (ctx : HttpContext) : Task<HttpContext> =
  task {
    // NB: we're sending back a png, not an ico - this is deliberate,
    // favicon.ico can be png, and the png is 685 bytes vs a 4+kb .ico
    let memory = Lazy.force favicon
    setHeader ctx "Access-Control-Allow-Origin" "*"
    ctx.Response.StatusCode <- 200
    ctx.Response.ContentType <- "image/png"
    ctx.Response.ContentLength <- int64 memory.Length
    let! (_flushResult : IO.Pipelines.FlushResult) =
      ctx.Response.BodyWriter.WriteAsync(memory)
    return ctx
  }



let textPlain = Some "text/plain; charset=utf-8"


type System.IO.Pipelines.PipeWriter with

  [<System.Runtime.CompilerServices.Extension>]
  member this.WriteAsync(bytes : byte array) : Task =
    task {
      let! (_ : IO.Pipelines.FlushResult) = this.WriteAsync(ReadOnlyMemory bytes)
      return ()
    }
    :> Task


let standardResponse
  (ctx : HttpContext)
  (msg : string)
  (contentType : Option<string>)
  (code : int)
  : Task<HttpContext> =
  task {
    setHeader ctx "Access-Control-Allow-Origin" "*"

    match contentType with
    | None -> ()
    | Some typ -> ctx.Response.ContentType <- typ

    let bytes = UTF8.toBytes msg
    ctx.Response.StatusCode <- code
    ctx.Response.ContentLength <- int64 bytes.Length
    do! ctx.Response.BodyWriter.WriteAsync(bytes)
    return ctx
  }

let errorResponse
  (ctx : HttpContext)
  (msg : string)
  (code : int)
  : Task<HttpContext> =
  // Like a standard response, but with text/plain and no CORS headers
  task {
    let bytes = UTF8.toBytes msg
    ctx.Response.StatusCode <- code
    ctx.Response.ContentType <- "text/plain"
    ctx.Response.ContentLength <- int64 bytes.Length
    do! ctx.Response.BodyWriter.WriteAsync(bytes)
    return ctx
  }



let noHandlerResponse (ctx : HttpContext) : Task<HttpContext> =
  // CLEANUP: use errorResponse
  standardResponse ctx "404 Not Found: No route matches" textPlain 404

let canvasNotFoundResponse (ctx : HttpContext) : Task<HttpContext> =
  // CLEANUP: use errorResponse
  standardResponse ctx "user not found" textPlain 404

let internalErrorResponse (ctx : HttpContext) : Task<HttpContext> =
  let msg =
    "Dark Internal Error: Dark - the service running this application - encountered an error. This problem is a bug in Dark, we're sorry! Our automated systems have noted this error and we are working to resolve it. The author of this application can post in our slack (darkcommunity.slack.com) for more information."
  // CLEANUP: use errorResponse
  standardResponse ctx msg textPlain 500


let moreThanOneHandlerResponse (ctx : HttpContext) : Task<HttpContext> =
  let path = ctx.Request.Path.Value
  let message = $"500 Internal Server Error: More than one handler for route: {path}"
  // CLEANUP: use errorResponse
  standardResponse ctx message textPlain 500

let unmatchedRouteResponse
  (ctx : HttpContext)
  (requestPath : string)
  (route : string)
  : Task<HttpContext> =
  let message = $"The request ({requestPath}) does not match the route ({route})"
  // CLEANUP use errorResponse
  standardResponse ctx message textPlain 500


exception LoadException of string * int


// runs a function and upon error, catches and rethrows the passed exception
let catch (msg : string) (code : int) (fn : 'a -> Task<'b>) (value : 'a) : Task<'b> =
  task {
    try
      return! fn value
    with
    | _ -> return! raise (LoadException(msg, code))
  }

// ---------------
// CORS
// ---------------
let inferCorsAllowOriginHeader
  (ctx : HttpContext)
  (canvasID : CanvasID)
  : Task<string option> =
  task {
    let! corsSetting = Canvas.fetchCORSSetting canvasID
    let originHeader = getHeader ctx.Request.Headers "Origin"

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
      // Access-Control-Allow-Origin: * in their browsers, even if the
      // request has no Origin.
      | _, Some Canvas.AllOrigins -> Some "*"
      // if there's no supplied origin, don't set the header at all.
      | None, _ -> None
      // Return the origin if and only if it's in the setting
      | Some origin, Some (Canvas.Origins origins) when List.contains origin origins ->
        Some origin
      // Otherwise: there was a supplied origin and it's not in the setting.
      // return "null" explicitly
      | Some _, Some _ -> Some "null"
    return header
  }

let optionsResponse (ctx : HttpContext) (canvasID : CanvasID) : Task<HttpContext> =
  task {
    // When javascript in a browser tries to make an unusual cross-origin
    // request (for example, a POST with a weird content-type or something with
    // weird headers), the browser first makes an OPTIONS request to the
    // server in order to get its permission to make that request. It includes
    // "origin", the originating origin, and "access-control-request-headers",
    // which is the list of headers the javascript would like to use.

    // FSTODO
    // (Ordinary GETs and some POSTs get handled in result_to_response, above,
    // without an OPTIONS).

    // Our strategy here is: if it's from an allowed origin (i.e., in the canvas
    // cors_setting) to return an Access-Control-Allow-Origin header for that
    // origin, to return Access-Control-Allow-Headers with the requested headers,
    // and Access-Control-Allow-Methods for all of the methods we think might
    // be useful.

    let reqHeaders = getHeader ctx.Request.Headers "access-control-request-headers"
    let allowHeaders = Option.defaultValue "*" reqHeaders

    match! inferCorsAllowOriginHeader ctx canvasID with
    | None -> return ctx
    | Some origin ->
      ctx.Response.StatusCode <- 200
      // CLEANUP: if the origin is null here, we probably shouldn't add the other headers
      setHeader ctx "Access-Control-Allow-Headers" allowHeaders
      setHeader ctx "Access-Control-Allow-Origin" origin
      let methods = "GET,PUT,POST,DELETE,PATCH,HEAD,OPTIONS"
      setHeader ctx "Access-Control-Allow-Methods" methods
      ctx.Response.ContentLength <- 0L
      return ctx
  }


// ---------------
// HttpsRedirect
// ---------------
let shouldRedirect (ctx : HttpContext) : bool =
  LibBackend.Config.useHttps && not ctx.Request.IsHttps

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
// Urls
// ---------------

// Proxies that terminate HTTPs should give us X-Forwarded-Proto: http
// or X-Forwarded-Proto: https.
// Return the URI, adding the scheme to the URI if there is an X-Forwarded-Proto.
let canonicalizeURL (toHttps : bool) (url : string) =
  if toHttps then
    let uri = System.UriBuilder url
    uri.Port <- 443
    uri.Scheme <- "https" // Switch to 443 or it will print :80
    string uri.Uri // Use .Uri or it will slip in a port number
  else
    url

// ---------------
// Read from HttpContext
// ---------------
let getHeaders (ctx : HttpContext) : List<string * string> =
  ctx.Request.Headers
  |> Seq.map
       (fun (kvp : KeyValuePair<string, StringValues>) ->
         (kvp.Key, kvp.Value.ToArray() |> Array.toList |> String.concat ","))
  |> Seq.toList

let getQuery (ctx : HttpContext) : List<string * List<string>> =
  ctx.Request.Query
  |> Seq.map
       (fun (kvp : KeyValuePair<string, StringValues>) ->
         (kvp.Key,
          // If there are duplicates, .NET puts them in the same StringValues.
          // However, it doesn't parse commas. We want a list if there are commas,
          // but we want to overwrite if there are two of the same headers.
          // CLEANUP this isn't to say that this is good behaviour
          kvp.Value.ToArray()
          |> Array.toList
          |> List.tryLast
          |> Option.defaultValue ""
          |> String.split ","))
  |> Seq.toList

open System.Buffers

let getBody (ctx : HttpContext) : Task<byte array> =
  task {
    // TODO: apparently it's faster to use a PipeReader, but that broke for us
    let ms = new IO.MemoryStream()
    do! ctx.Request.Body.CopyToAsync(ms)
    return ms.ToArray()
  }


// ---------------
// Handle builtwithdark request
// ---------------
let runHttpRequest
  (c : Canvas.T)
  (tlid : tlid)
  (traceID : LibExecution.AnalysisTypes.TraceID)
  (routeVars : List<string * RT.Dval>)
  (request : RT.Dval)
  (expr : RT.Expr)
  : Task<Resp.HttpResponse * HashSet.T<tlid>> =
  task {
    let program = Canvas.toProgram c
    let! state, touchedTLIDs = RealExe.createState traceID tlid program

    // Build request
    let symtable =
      Map.ofList routeVars
      |> Interpreter.withGlobals state
      |> Map.add "request" request

    // Execute
    let! result = Interpreter.eval state symtable expr

    return (Resp.toHttpResponse result, touchedTLIDs)
  }

let runDarkHandler (ctx : HttpContext) : Task<HttpContext> =
  task {
    setHeader ctx "Server" "Darklang"
    let executionID = LibService.Telemetry.executionID ()
    setHeader ctx "x-darklang-execution-id" (string executionID)

    let loggerFactory = ctx.RequestServices.GetService<ILoggerFactory>()
    let logger = loggerFactory.CreateLogger("logger")
    let log msg (v : 'a) = logger.LogError("{msg}: {v}", msg, v)

    match! Routing.canvasNameFromHost ctx.Request.Host.Host with
    | Some canvasName ->
      // CLEANUP: move execution ID header up
      let ownerName = Account.ownerNameFromCanvasName canvasName
      let ownerUsername = UserName.create (string ownerName)

      let! ownerID =
        catch "user not found" 404 Account.userIDForUserName ownerUsername

      // No error checking as this will create a canvas if none exists
      let! canvasID = Canvas.canvasIDForCanvasName ownerID canvasName

      let meta : Canvas.Meta = { id = canvasID; owner = ownerID; name = canvasName }

      let traceID = System.Guid.NewGuid()
      let method = ctx.Request.Method
      let requestPath = ctx.Request.Path.Value |> Routing.sanitizeUrlPath

      // redirect HEADs to GET. We pass the actual HEAD method to the engine,
      // and leave it to middleware to say what it wants to do with that
      let searchMethod = if method = "HEAD" then "GET" else method

      let! c =
        Canvas.loadHttpHandlers meta requestPath searchMethod
        |> Task.map Result.unwrapUnsafe

      let pages = Routing.filterMatchingHandlers requestPath (Map.values c.handlers)


      let url : string =
        let isHttps =
          getHeader ctx.Request.Headers "X-Forwarded-Proto" = Some "https"
        ctx.Request.GetEncodedUrl() |> canonicalizeURL isHttps

      match pages with
      | [ { spec = PT.Handler.HTTP (route = route); ast = expr; tlid = tlid } ] ->

        let routeVars = Routing.routeInputVars route requestPath

        match routeVars with
        | Some routeVars ->
          let! reqBody = getBody ctx
          let reqHeaders = getHeaders ctx
          let reqQuery = getQuery ctx
          let request = Req.fromRequest false url reqHeaders reqQuery reqBody

          // Store trace - do not resolve task, send this into the ether
          let _timestamp =
            TI.storeEvent c.meta.id traceID ("HTTP", requestPath, method) request

          // Do request
          let expr = expr.toRuntimeType ()
          let! (result, touchedTLIDs) =
            runHttpRequest c tlid traceID routeVars request expr

          // FSTODO - move cors into runHttpRequest
          match! inferCorsAllowOriginHeader ctx canvasID with
          | Some origin -> setHeader ctx "Access-Control-Allow-Origin" origin
          | None -> ()

          // Put response into ctx
          ctx.Response.StatusCode <- result.statusCode
          List.iter (fun (k, v) -> setHeader ctx k v) result.headers
          ctx.Response.ContentLength <- int64 result.body.Length
          if method <> "HEAD" then
            // TODO: benchmark - this is apparently faster than streams
            do! ctx.Response.BodyWriter.WriteAsync(result.body)

          // Send to pusher - Do not resolve task, send this into the ether
          Pusher.pushNewTraceID
            executionID
            canvasID
            traceID
            (HashSet.toList touchedTLIDs)

          return ctx

        | None -> // vars didnt parse
          // FSTODO: store event trace?
          return! unmatchedRouteResponse ctx requestPath route
      | [] when string ctx.Request.Path = "/favicon.ico" ->
        return! faviconResponse ctx
      | [] when ctx.Request.Method = "OPTIONS" ->
        return! optionsResponse ctx canvasID
      | [] ->
        let! reqBody = getBody ctx
        let reqHeaders = getHeaders ctx
        let reqQuery = getQuery ctx
        let event = Req.fromRequest true url reqHeaders reqQuery reqBody

        let! timestamp =
          TI.storeEvent canvasID traceID ("HTTP", requestPath, method) event

        // Send to pusher - do not resolve task, send this into the ether
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
let configureApp (healthCheckPort : int) (app : IApplicationBuilder) =
  let handler (ctx : HttpContext) : Task =
    (task {
      try
        // Do this here so we don't redirect the health check
        if shouldRedirect ctx then
          return httpsRedirect ctx
        else
          return! runDarkHandler ctx
      with
      | LoadException (msg, code) -> return! errorResponse ctx msg code
      | DarkException (GrandUserError msg) ->
        // Messages caused by user input should be displayed to the user
        return! errorResponse ctx msg 400
      | DarkException (DeveloperError _) ->
        // Don't tell the end user.
        // TODO: these should be saved for the user to see
        return! internalErrorResponse ctx
      | DarkException (InternalError _) ->
        // FSTODO: rollbar here
        return! internalErrorResponse ctx
      | DarkException (EditorError _) ->
        // FSTODO: rollbar here - these shouldn't reach here
        return! internalErrorResponse ctx
      | DarkException (LibraryError _) ->
        // FSTODO: rollbar here - these shouldn't reach here
        return! internalErrorResponse ctx
      | e ->
        print (string e)
        return raise e
     })
    :> Task

  app
  |> LibService.Rollbar.AspNet.addRollbarToApp
  |> fun app -> app.UseRouting()
  // must go after UseRouting
  |> LibService.Kubernetes.configureApp healthCheckPort
  |> fun app ->
       // Last chance exception handler
       let exceptionHandler (ctx : HttpContext) : Task =
         internalErrorResponse ctx |> Task.FromResult :> Task
       let exceptionHandlerOptions = ExceptionHandlerOptions()
       // FSTODO log/honeycomb
       exceptionHandlerOptions.ExceptionHandler <- RequestDelegate exceptionHandler
       app.UseExceptionHandler(exceptionHandlerOptions)
  |> fun app -> app.Run(RequestDelegate handler)

let configureServices (services : IServiceCollection) : unit =
  services
  |> LibService.Kubernetes.configureServices
  |> LibService.Rollbar.AspNet.addRollbarToServices
  |> LibService.Telemetry.AspNet.addTelemetryToServices "BwdServer"
  |> ignore<IServiceCollection>


let webserver
  (_shouldLog : bool)
  (httpPort : int)
  (healthCheckPort : int)
  : WebApplication =
  let hcUrl = LibService.Kubernetes.url healthCheckPort

  let builder = WebApplication.CreateBuilder()
  configureServices builder.Services
  LibService.Kubernetes.registerServerTimeout builder.WebHost

  builder.WebHost
  |> fun wh -> wh.UseKestrel(LibService.Kestrel.configureKestrel)
  |> fun wh -> wh.UseUrls(hcUrl, $"http://*:{httpPort}")
  |> ignore<IWebHostBuilder>

  let app = builder.Build()
  configureApp healthCheckPort app
  app


[<EntryPoint>]
let main _ =
  try
    print "Starting BwdServer"
    LibBackend.Init.init "Bwdserver"
    let port = LibService.Config.bwdServerPort
    let k8sPort = LibService.Config.bwdServerKubernetesPort
    (webserver true port k8sPort).Run()
    0
  with e ->
    print "Error starting BwdServer"
    print (string e)
    LibService.Rollbar.send (LibService.Telemetry.ExecutionID "0") [] e
    (-1)

