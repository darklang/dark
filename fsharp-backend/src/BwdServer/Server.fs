/// The webserver for builtwithdark.com, often referred to as "BWD."
///
/// All Dark programs are hosted with BWD - our grand-users hit this server, and BWD handles the requests.
/// So, BWD is what handles the "handlers" of a Dark program.
///
/// It uses ASP.NET directly, instead of a web framework, so we can tune the exact behaviour of headers and such.
module BwdServer.Server

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

type StringValues = Microsoft.Extensions.Primitives.StringValues

open Prelude
open Tablecloth

open LibService.Exception

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

module Account = LibBackend.Account
module Canvas = LibBackend.Canvas
module Routing = LibBackend.Routing
module Pusher = LibBackend.Pusher
module TI = LibBackend.TraceInputs

module Resp = HttpMiddleware.ResponseV0
module Req = HttpMiddleware.RequestV0
module Cors = HttpMiddleware.Cors
module RealExe = LibRealExecution.RealExecution

module FireAndForget = LibService.FireAndForget
module Kubernetes = LibService.Kubernetes
module Rollbar = LibService.Rollbar
module Telemetry = LibService.Telemetry

// ---------------
// Read from HttpContext
// ---------------

let getHeader (hs : IHeaderDictionary) (name : string) : string option =
  match hs.TryGetValue name with
  | true, vs -> vs.ToArray() |> Array.toSeq |> String.concat "," |> Some
  | false, _ -> None

/// Reads the incoming headers and simplifies into a list of key*value pairs
let getHeaders (ctx : HttpContext) : List<string * string> =
  ctx.Request.Headers
  |> Seq.map Tuple2.fromKeyValuePair
  |> Seq.map (fun (k, v) -> (k, v.ToArray() |> Array.toList |> String.concat ","))
  |> Seq.toList

/// Reads the incoming query parameters and simplifies into a list of key*values pairs
///
/// (multiple query params may be present with the same key)
let getQuery (ctx : HttpContext) : string = ctx.Request.QueryString.ToString()

/// Reads the incoming request body as a byte array
let getBody (ctx : HttpContext) : Task<byte array> =
  task {
    try
      // CLEANUP: this was to match ocaml - we certainly should provide a body if one is provided
      if ctx.Request.Method = "GET" then
        return [||]
      else
        // TODO: apparently it's faster to use a PipeReader, but that broke for us
        let ms = new IO.MemoryStream()
        do! ctx.Request.Body.CopyToAsync(ms)
        return ms.ToArray()
    with
    | e ->
      // Let's try to get a good error message to the user, but don't include .NET specific hints
      let tooSlowlyMessage =
        "Reading the request body timed out due to data arriving too slowly"
      let message =
        if String.startsWith tooSlowlyMessage e.Message then
          tooSlowlyMessage
        else
          e.Message
      return Exception.raiseGrandUser $"Invalid request body: {message}"
  }


// ---------------
// Responses
// ---------------

/// Sets the response header
let setHeader (ctx : HttpContext) (name : string) (value : string) : unit =
  ctx.Response.Headers[ name ] <- StringValues([| value |])

/// Reads a static (Dark) favicon image
let favicon : Lazy<ReadOnlyMemory<byte>> =
  lazy
    (LibBackend.File.readfileBytes LibBackend.Config.Webroot "favicon-32x32.png"
     |> ReadOnlyMemory)

/// Handles a request for favicon.ico, returning static Dark icon
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

let writeResponseToContext
  (ctx : HttpContext)
  (response : Resp.HttpResponse)
  : Task<unit> =
  task {
    ctx.Response.StatusCode <- response.statusCode
    response.headers |> List.iter (fun (k, v) -> setHeader ctx k v)
    ctx.Response.ContentLength <- int64 response.body.Length
    if ctx.Request.Method <> "HEAD" then
      // TODO: benchmark - this is apparently faster than streams
      do! ctx.Response.BodyWriter.WriteAsync(response.body)
  }

let standardResponse
  (ctx : HttpContext)
  (msg : string)
  (contentType : Option<string>)
  (code : int)
  : Task<HttpContext> =
  task {
    setHeader ctx "Access-Control-Allow-Origin" "*"
    setHeader ctx "Server" "darklang"

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
  // Like a standard response, but with no CORS headers
  task {
    let bytes = UTF8.toBytes msg
    ctx.Response.StatusCode <- code
    ctx.Response.ContentType <- "text/plain; charset=utf-8"
    ctx.Response.ContentLength <- int64 bytes.Length
    do! ctx.Response.BodyWriter.WriteAsync(bytes)
    return ctx
  }



let noHandlerResponse (ctx : HttpContext) : Task<HttpContext> =
  // CLEANUP: use errorResponse
  standardResponse ctx "404 Not Found: No route matches" textPlain 404

let canvasNotFoundResponse (ctx : HttpContext) : Task<HttpContext> =
  // CLEANUP: use errorResponse
  standardResponse ctx "canvas not found" textPlain 404

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


// ---------------
// HttpsRedirect
// ---------------

let isHttps (ctx : HttpContext) =
  // requests aren't actually https in production. The load balancer terminates https
  // and forwards the details using headers
  getHeader ctx.Request.Headers "X-Forwarded-Proto" = Some "https"

let shouldRedirect (ctx : HttpContext) : bool =
  LibBackend.Config.useHttps && not (isHttps ctx)

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

/// Return the URI, adding the scheme to the URI if there is an X-Forwarded-Proto.
///
/// Proxies that terminate HTTPs should give us
/// - X-Forwarded-Proto: http
/// - or X-Forwarded-Proto: https.
let canonicalizeURL (toHttps : bool) (url : string) =
  if toHttps then
    let uri = System.UriBuilder url
    uri.Port <- 443
    uri.Scheme <- "https" // Switch to 443 or it will print :80
    string uri.Uri // Use .Uri or it will slip in a port number
  else
    url

exception NotFoundException of msg : string with
  override this.Message = this.msg


// CLEANUP we'd like to get rid of corsSetting and move it out of the DB and
// entirely into code in some middleware
let canvasMetadataFromHost (host : string) : Task<Option<Canvas.Meta>> =
  task {
    match Routing.canvasSourceFromHost host with
    | Routing.Bwd canvasName ->
      match CanvasName.create canvasName with
      | Ok canvasName -> return! Canvas.getMeta canvasName
      | Error _ -> return None
    | Routing.CustomDomain customDomain ->
      return! Canvas.getMetaForCustomDomain customDomain
  }

/// ---------------
/// Handle builtwithdark request
/// ---------------
let runDarkHandler (ctx : HttpContext) : Task<HttpContext> =
  task {
    let host = Exception.catch (fun () -> ctx.Request.Host.Host)
    let! canvasMeta =
      match host with
      | None -> Task.FromResult None
      | Some host -> canvasMetadataFromHost host

    match canvasMeta with
    | Some meta ->

      ctx.Items[ "canvasName" ] <- meta.name // store for exception tracking
      ctx.Items[ "canvasOwnerID" ] <- meta.owner // store for exception tracking

      let traceID = System.Guid.NewGuid()
      let requestMethod = ctx.Request.Method
      let requestPath = ctx.Request.Path.Value |> Routing.sanitizeUrlPath
      let desc = ("HTTP", requestPath, requestMethod)

      Telemetry.addTags [ "canvas.name", meta.name
                          "canvas.id", meta.id
                          "canvas.owner_id", meta.owner
                          "trace_id", traceID ]

      // redirect HEADs to GET. We pass the actual HEAD method to the engine,
      // and leave it to middleware to say what it wants to do with that
      let searchMethod = if requestMethod = "HEAD" then "GET" else requestMethod

      // Canvas to process request against, with enough loaded to handle this
      // request
      let! canvas = Canvas.loadHttpHandlers meta requestPath searchMethod

      let url : string = ctx.Request.GetEncodedUrl() |> canonicalizeURL (isHttps ctx)

      // Filter down canvas' handlers to those (hopefully only one) that match
      let pages =
        Routing.filterMatchingHandlers requestPath (Map.values canvas.handlers)

      match pages with
      // matching handler found - process normally
      | [ { spec = PT.Handler.HTTP (route = route); ast = expr; tlid = tlid } as handler ] ->
        Telemetry.addTags [ "handler.route", route; "handler.tlid", tlid ]

        // TODO: I think we could put this into the middleware
        let routeVars = Routing.routeInputVars route requestPath

        let! reqBody = getBody ctx
        let reqHeaders = getHeaders ctx
        let reqQuery = getQuery ctx

        match routeVars with
        | Some routeVars ->
          Telemetry.addTag "handler.routeVars" routeVars

          // Do request
          use _ = Telemetry.child "executeHandler" []

          let request = Req.fromRequest false url reqHeaders reqQuery reqBody
          let inputVars = routeVars |> Map |> Map.add "request" request
          let! (result, _) =
            RealExe.executeHandler
              canvas.meta
              (PT2RT.Handler.toRT handler)
              (Canvas.toProgram canvas)
              traceID
              inputVars
              (RealExe.InitialExecution(desc, request))

          // Execute - note that these are outside the handler
          let result = Resp.toHttpResponse result
          let result = Cors.addCorsHeaders reqHeaders meta.name result

          do! writeResponseToContext ctx result

          return ctx

        | None -> // vars didnt parse
          FireAndForget.fireAndForgetTask "store-event" (fun () ->
            let request = Req.fromRequest false url reqHeaders reqQuery reqBody
            TI.storeEvent meta.id traceID desc request)

          return! unmatchedRouteResponse ctx requestPath route

      | [] when string ctx.Request.Path = "/favicon.ico" ->
        return! faviconResponse ctx

      | [] when ctx.Request.Method = "OPTIONS" ->
        let reqHeaders = getHeaders ctx
        match Cors.optionsResponse reqHeaders meta.name with
        | Some response -> do! writeResponseToContext ctx response
        | None -> ()
        return ctx

      // no matching route found - store as 404
      | [] ->
        let! reqBody = getBody ctx
        let reqHeaders = getHeaders ctx
        let reqQuery = getQuery ctx
        let event = Req.fromRequest true url reqHeaders reqQuery reqBody
        let! timestamp = TI.storeEvent meta.id traceID desc event

        // CLEANUP: move pusher into storeEvent
        // Send to pusher - do not resolve task, send this into the ether
        Pusher.pushNew404
          meta.id
          ("HTTP", requestPath, requestMethod, timestamp, traceID)

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
      // The traditional methods of using `UseHsts` and `AddHsts` within BwdServer
      // were ineffective. Somehow, the Strict-Transport-Security header was not
      // included in HTTP Reponses as a result of these efforts. Here, we manually
      // work around this by setting it manually.
      // CLEANUP: replace this with the more additional approach, if possible
      setHeader ctx "Strict-Transport-Security" LibService.HSTS.stringConfig

      setHeader ctx "x-darklang-execution-id" (Telemetry.rootID ())
      setHeader ctx "Server" "darklang"

      try
        // Do this here so we don't redirect the health check
        if shouldRedirect ctx then
          return httpsRedirect ctx
        else
          return! runDarkHandler ctx
      with
      // These errors are the only ones we want to handle here. We don't want to give
      // GrandUsers any info not intended for them. We want the rest to be caught by
      // the 500 handler, be reported, and then have a small error message printed
      | NotFoundException msg -> return! errorResponse ctx msg 404
      | :? GrandUserException as e ->
        // Messages caused by user input should be displayed to the user
        return! errorResponse ctx e.Message 400
      | e ->
        // respond and then reraise to get it to the rollbar middleware
        let! (_ : HttpContext) = internalErrorResponse ctx
        return e.Reraise()
    })

  let rollbarCtxToMetadata (ctx : HttpContext) : Rollbar.Person * Metadata =
    let canvasName =
      try
        Some(ctx.Items["canvasName"] :?> CanvasName.T)
      with
      | _ -> None

    let username =
      try
        canvasName
        |> Option.map (fun canvasName ->
          canvasName
          |> Account.ownerNameFromCanvasName
          |> fun on -> on.toUserName ())
      with
      | _ -> None

    let id =
      try
        Some(ctx.Items["canvasOwnerID"] :?> UserID)
      with
      | _ -> None

    let metadata =
      canvasName
      |> Option.map (fun cn -> [ "canvas", string cn :> obj ])
      |> Option.defaultValue []

    let person : Rollbar.Person =
      id |> Option.map (fun id -> { id = id; username = username })

    (person, metadata)

  Rollbar.AspNet.addRollbarToApp app rollbarCtxToMetadata None
  |> fun app -> app.UseRouting()
  // must go after UseRouting
  |> Kubernetes.configureApp healthCheckPort
  |> fun app -> app.Run(RequestDelegate handler)

let configureServices (services : IServiceCollection) : unit =
  services
  |> Kubernetes.configureServices []
  |> Rollbar.AspNet.addRollbarToServices
  |> Telemetry.AspNet.addTelemetryToServices "BwdServer" Telemetry.TraceDBQueries
  |> ignore<IServiceCollection>


let webserver
  (loggerSetup : ILoggingBuilder -> unit)
  (httpPort : int)
  (healthCheckPort : int)
  : WebApplication =
  let hcUrl = Kubernetes.url healthCheckPort

  let builder = WebApplication.CreateBuilder()
  configureServices builder.Services
  Kubernetes.registerServerTimeout builder.WebHost

  builder.WebHost
  |> fun wh -> wh.ConfigureLogging(loggerSetup)
  |> fun wh -> wh.UseKestrel(LibService.Kestrel.configureKestrel)
  |> fun wh -> wh.UseUrls(hcUrl, $"http://*:{httpPort}")
  |> ignore<IWebHostBuilder>

  let app = builder.Build()
  configureApp healthCheckPort app
  app

let run () : unit =
  let port = LibService.Config.bwdServerPort
  let k8sPort = LibService.Config.bwdServerKubernetesPort
  (webserver LibService.Logging.noLogger port k8sPort).Run()



[<EntryPoint>]
let main _ =
  try
    let name = "BwdServer"
    print "Starting BwdServer"
    LibService.Init.init name
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    (LibRealExecution.Init.init name).Result
    run ()
    // CLEANUP I suspect this isn't called
    LibService.Init.shutdown name
    0
  with
  | e -> Rollbar.lastDitchBlockAndPage "error starting bwdserver" e
