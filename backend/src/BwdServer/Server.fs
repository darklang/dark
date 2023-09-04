/// This is the webserver for darklang.io, often referred to as "BWD" (named after
/// our old domain - builtwithdark.com)
///
/// All Dark programs are hosted with BWD - our grand-users hit this server, and BWD
/// handles the requests.  So, BWD is what handles the "handlers" of a Dark program.
///
/// It uses ASP.NET directly, instead of a web framework, so we can tune the exact
/// behaviour of headers and such.
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

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

module Account = LibCloud.Account
module Canvas = LibCloud.Canvas
module Routing = LibCloud.Routing
module Pusher = LibCloud.Pusher

module LibHttpMiddleware = LibHttpMiddleware.Http

module RealExe = LibCloudExecution.CloudExecution

module FireAndForget = LibService.FireAndForget
module Kubernetes = LibService.Kubernetes
module Rollbar = LibService.Rollbar
module Telemetry = LibService.Telemetry

module CTPusher = LibClientTypes.Pusher

// HttpHandlerTODO there are still a number of things in this file that were
// written with the original Http handler+middleware in mind, and aren't
// appropriate more generally. Much of this should be migrated to the module in
// LibHttpMiddleware.Http.fs.

// ---------------
// Read from HttpContext
// ---------------

exception GrandUserException of string

let getHeader (hs : IHeaderDictionary) (name : string) : string option =
  match hs.TryGetValue name with
  | true, vs -> vs.ToArray() |> Array.toSeq |> String.concat "," |> Some
  | false, _ -> None

/// Reads the incoming headers and simplifies into a list of key*value pairs
let getHeadersWithoutMergingKeys (ctx : HttpContext) : List<string * string> =
  ctx.Request.Headers
  |> Seq.map Tuple2.fromKeyValuePair
  |> Seq.map (fun (k, v) ->
    v.ToArray() |> Array.toList |> List.map (fun v -> (k, v)))
  |> Seq.collect (fun pair -> pair)
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
    with e ->
      // Let's try to get a good error message to the user, but don't include .NET specific hints
      let tooSlowlyMessage =
        "Reading the request body timed out due to data arriving too slowly"
      let message =
        if String.startsWith tooSlowlyMessage e.Message then
          tooSlowlyMessage
        else
          e.Message
      return raise (GrandUserException $"Invalid request body: {message}")
  }


// ---------------
// Responses
// ---------------

/// Sets the response header
let setResponseHeader (ctx : HttpContext) (name : string) (value : string) : unit =
  ctx.Response.Headers[name] <- StringValues([| value |])

/// Reads a static (Dark) favicon image
let favicon : Lazy<ReadOnlyMemory<byte>> =
  lazy
    (LibCloud.File.readfileBytes LibCloud.Config.Webroot "favicon-32x32.png"
     |> ReadOnlyMemory)

/// Handles a request for favicon.ico, returning static Dark icon
let faviconResponse (ctx : HttpContext) : Task<HttpContext> =
  task {
    // NB: we're sending back a png, not an ico - this is deliberate,
    // favicon.ico can be png, and the png is 454 bytes vs a 4+kb .ico
    let memory = Lazy.force favicon
    Telemetry.addTag "http.completion_reason" "darklangFavicon"
    setResponseHeader ctx "Access-Control-Allow-Origin" "*"
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
  (statusCode : int)
  (headers : List<string * string>)
  (body : array<byte>)
  : Task<unit> =
  task {
    ctx.Response.StatusCode <- statusCode
    headers |> List.iter (fun (k, v) -> setResponseHeader ctx k v)
    ctx.Response.ContentLength <- int64 body.Length
    if ctx.Request.Method <> "HEAD" then
      // TODO: benchmark - this is apparently faster than streams
      do! ctx.Response.BodyWriter.WriteAsync(body)
  }

let standardResponse
  (ctx : HttpContext)
  (msg : string)
  (contentType : Option<string>)
  (code : int)
  : Task<HttpContext> =
  task {
    setResponseHeader ctx "Access-Control-Allow-Origin" "*"
    setResponseHeader ctx "Server" "darklang"

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
  Telemetry.addTag "http.completion_reason" "noHandler"
  standardResponse ctx "404 Not Found: No route matches" textPlain 404

let canvasNotFoundResponse (ctx : HttpContext) : Task<HttpContext> =
  // CLEANUP: use errorResponse
  Telemetry.addTag "http.completion_reason" "canvasNotFound"
  standardResponse ctx "canvas not found" textPlain 404

let internalErrorResponse (ctx : HttpContext) (e : exn) : Task<HttpContext> =
  Telemetry.addTag "http.completion_reason" "internalError"
  Telemetry.addTag "http.internal_exception_msg" e.Message
  // It's possible that the body has already been written to, so we need to be careful here
  if ctx.Response.HasStarted then
    Telemetry.addTag "http.body_started_before_error_logged" true
    task { return ctx }
  else
    let msg =
      "Darklang Internal Error: Darklang - the service running this application - encountered an error. This problem is a bug in Darklang, we're sorry! Our automated systems have noted this error and we are working to resolve it. The developers working on this service should contact Darklang staff for more detail if needed."
    standardResponse ctx msg textPlain 500


let moreThanOneHandlerResponse (ctx : HttpContext) : Task<HttpContext> =
  let path = ctx.Request.Path.Value
  let message = $"500 Internal Server Error: More than one handler for route: {path}"
  // CLEANUP: use errorResponse
  Telemetry.addTag "http.completion_reason" "moreThanOnHandler"
  standardResponse ctx message textPlain 500

let unmatchedRouteResponse
  (ctx : HttpContext)
  (requestPath : string)
  (route : string)
  : Task<HttpContext> =
  let message = $"The request ({requestPath}) does not match the route ({route})"
  // CLEANUP use errorResponse
  Telemetry.addTag "http.completion_reason" "unmatchedRoute"
  standardResponse ctx message textPlain 500


// ---------------
// HttpsRedirect
// ---------------

let isHttps (ctx : HttpContext) =
  // requests aren't actually https in production. The load balancer terminates https
  // and forwards the details using headers
  getHeader ctx.Request.Headers "X-Forwarded-Proto" = Some "https"

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

/// ---------------
/// Handle builtwithdark request
/// ---------------
let runDarkHandler (ctx : HttpContext) : Task<HttpContext> =
  task {
    let domain = Exception.catch (fun () -> ctx.Request.Host.Host)
    let! canvasID =
      match domain with
      | None -> Task.FromResult None
      | Some domain ->
        ctx.Items["canvasDomain"] <- domain // store for exception tracking
        Telemetry.addTags [ "canvas.domain", domain ]
        Canvas.canvasIDForDomain domain

    match canvasID with
    | Some canvasID ->
      let traceID = LibExecution.AnalysisTypes.TraceID.create ()
      let requestMethod = ctx.Request.Method
      let requestPath = ctx.Request.Path.Value |> Routing.sanitizeUrlPath
      let desc = ("HTTP", requestPath, requestMethod)

      Telemetry.addTags [ "canvas.id", canvasID; "trace_id", traceID ]

      // redirect HEADs to GET. We pass the actual HEAD method to the engine,
      // and leave it to middleware to say what it wants to do with that
      let searchMethod = if requestMethod = "HEAD" then "GET" else requestMethod

      // Canvas to process request against, with enough loaded to handle this
      // request
      let! canvas = Canvas.loadHttpHandlers canvasID requestPath searchMethod

      let url : string = ctx.Request.GetEncodedUrl() |> canonicalizeURL (isHttps ctx)

      // Filter down canvas' handlers to those (hopefully only one) that match
      let pages =
        Routing.filterMatchingHandlers requestPath (Map.values canvas.handlers)

      match pages with
      // matching handler found - process normally
      | [ { spec = PT.Handler.HTTP(route = route); tlid = tlid } as handler ] ->
        Telemetry.addTags [ "handler.route", route; "handler.tlid", tlid ]

        let routeVars = Routing.routeInputVars route requestPath

        let! reqBody = getBody ctx
        let reqHeaders = getHeadersWithoutMergingKeys ctx

        match routeVars with
        | Some routeVars ->
          Telemetry.addTag "handler.routeVars" routeVars

          // Do request
          use _ = Telemetry.child "executeHandler" []

          let request = LibHttpMiddleware.Request.fromRequest url reqHeaders reqBody
          let inputVars = routeVars |> Map |> Map.add "request" request
          let! (result, _) =
            RealExe.executeHandler
              LibClientTypesToCloudTypes.Pusher.eventSerializer
              (PT2RT.Handler.toRT handler)
              (Canvas.toProgram canvas)
              traceID
              inputVars
              (RealExe.InitialExecution(desc, "request", request))

          let result = LibHttpMiddleware.Response.toHttpResponse result

          do! writeResponseToContext ctx result.statusCode result.headers result.body
          Telemetry.addTag "http.completion_reason" "success"

          return ctx

        | None -> // vars didnt parse

          // TODO: reenable using CloudStorage
          // FireAndForget.fireAndForgetTask "store-event" (fun () ->
          //   let request = LibHttpMiddleware.Request.fromRequest url reqHeaders reqBody
          //   TI.storeEvent canvasID traceID desc request)

          return! unmatchedRouteResponse ctx requestPath route

      | [] when string ctx.Request.Path = "/favicon.ico" ->
        return! faviconResponse ctx

      // no matching route found - store as 404
      | [] ->
        // TODO: reenable using CloudStorage
        // let! reqBody = getBody ctx
        // let reqHeaders = getHeadersWithoutMergingKeys ctx
        // let event = LibHttpMiddleware.Request.fromRequest url reqHeaders reqBody
        // let! timestamp = TI.storeEvent canvasID traceID desc event

        // CLEANUP: move pusher into storeEvent
        // Send to pusher - do not resolve task, send this into the ether
        // Pusher.push
        //   LibClientTypesToCloudTypes.Pusher.eventSerializer
        //   canvasID
        //   (Pusher.New404("HTTP", requestPath, requestMethod, timestamp, traceID))
        //   None

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
      // CLEANUP: replace this with the more traditional approach, if possible
      // HttpHandlerTODO lowercase keys for Http handler responses
      setResponseHeader ctx "Strict-Transport-Security" LibService.HSTS.stringConfig

      setResponseHeader ctx "x-darklang-execution-id" (Telemetry.rootID ())
      setResponseHeader ctx "Server" "darklang"

      try
        return! runDarkHandler ctx
      with
      // These errors are the only ones we want to handle here. We don't want to give
      // GrandUsers any info not intended for them. We want the rest to be caught by
      // the 500 handler, be reported, and then have a small error message printed
      | NotFoundException msg -> return! errorResponse ctx msg 404
      | GrandUserException msg ->
        // Messages caused by user input should be displayed to the user
        return! errorResponse ctx msg 400
      | e ->
        // respond and then reraise to get it to the rollbar middleware
        let! (_ : HttpContext) = internalErrorResponse ctx e
        return Exception.reraise e
    })

  let rollbarCtxToMetadata (ctx : HttpContext) : Rollbar.Person * Metadata =
    let domain =
      try
        Some(ctx.Items["canvasDomain"])
      with _ ->
        None

    let id =
      try
        Some(ctx.Items["canvasOwnerID"] :?> UserID)
      with _ ->
        None

    let metadata =
      domain |> Option.map (fun d -> [ "canvasDomain", d ]) |> Option.defaultValue []

    (id, metadata)

  Rollbar.AspNet.addRollbarToApp app rollbarCtxToMetadata None
  |> fun app -> app.UseRouting()
  // must go after UseRouting
  |> Kubernetes.configureApp healthCheckPort
  |> fun app -> app.Run(RequestDelegate handler)

let configureServices (services : IServiceCollection) : unit =
  services
  |> Kubernetes.configureServices [ Canvas.healthCheck ]
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


// Generally speaking, this should be the same list as QueueWorker's
let initSerializers () =
  // universally-serializable types

  // one-off types used internally
  Json.Vanilla.allow<LibExecution.DvalReprInternalRoundtrippable.FormatV0.Dval>
    "RoundtrippableSerializationFormatV0.Dval"
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.Toplevel.T>>
    "Canvas.loadJsonFromDisk"
  Json.Vanilla.allow<LibExecution.ProgramTypes.Toplevel.T> "Canvas.loadJsonFromDisk"
  Json.Vanilla.allow<LibCloud.Queue.NotificationData> "eventqueue storage"
  Json.Vanilla.allow<LibCloud.TraceCloudStorage.CloudStorageFormat>
    "TraceCloudStorageFormat"
  Json.Vanilla.allow<LibService.Rollbar.HoneycombJson> "Rollbar"

  // for Pusher.com payloads
  Json.Vanilla.allow<CTPusher.Payload.NewTrace> "Pusher"
  Json.Vanilla.allow<CTPusher.Payload.New404> "Pusher"
// Json.Vanilla.allow<CTPusher.Payload.AddOpV1> "Pusher"
// Json.Vanilla.allow<CTPusher.Payload.AddOpV1PayloadTooBig> "Pusher" // this is so-far unused
// Json.Vanilla.allow<CTPusher.Payload.UpdateWorkerStates> "Pusher"



[<EntryPoint>]
let main _ =
  try
    let name = "BwdServer"
    print "Starting BwdServer"
    initSerializers ()
    LibService.Init.init name
    (LibCloud.Init.init LibCloud.Init.WaitForDB name).Result
    (LibCloudExecution.Init.init name).Result

    run ()
    // CLEANUP I suspect this isn't called
    LibService.Init.shutdown name
    0
  with e ->
    Rollbar.lastDitchBlockAndPage "error starting bwdserver" e
