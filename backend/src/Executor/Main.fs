module Executor.Main

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

type StringValues = Microsoft.Extensions.Primitives.StringValues

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

// open Http
// open Middleware

module Config = LibBackend.Config

module FireAndForget = LibService.FireAndForget
module Kubernetes = LibService.Kubernetes
module Rollbar = LibService.Rollbar
module Telemetry = LibService.Telemetry

type Packages = List<LibExecution.ProgramTypes.Package.Fn>

type HttpHandler = HttpContext -> Task

open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Primitives
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

[<Extension>]
type HttpContextExtensions() =

  [<Extension>]
  static member WriteClientJsonAsync<'T>
    (
      ctx : HttpContext,
      value : 'T
    ) : Task<option<HttpContext>> =
    task {
      // use t = startTimer "serialize-json" ctx
      // addTag "json_flavor" "vanilla"
      ctx.Response.ContentType <- "application/json; charset=utf-8"
      // Use a client-specific ApiServer
      let serialized = Json.Vanilla.serialize value
      let bytes = System.ReadOnlyMemory(UTF8.toBytes serialized)
      ctx.Response.ContentLength <- int64 bytes.Length
      // t.next "write-json-async"
      let! (_ : System.IO.Pipelines.FlushResult) =
        ctx.Response.BodyWriter.WriteAsync(bytes)
      return Some ctx
    }

  [<Extension>]
  static member WriteTextAsync
    (
      ctx : HttpContext,
      value : string
    ) : Task<option<HttpContext>> =
    task {
      // use t = startTimer "text-to-bytes" ctx
      ctx.Response.ContentType <- "text/plain; charset=utf-8"
      let bytes = System.ReadOnlyMemory(UTF8.toBytes value)
      ctx.Response.ContentLength <- int64 bytes.Length
      // t.next "write-text-async"
      let! (_ : System.IO.Pipelines.FlushResult) =
        ctx.Response.BodyWriter.WriteAsync(bytes)
      return Some ctx
    }

/// Helper to write a value as serialized JSON response body
let clientJsonHandler (f : HttpContext -> Task<'a>) : HttpHandler =
  (fun ctx ->
    task {
      let! result = f ctx
      return! ctx.WriteClientJsonAsync result
    })


// --------------------
// Handlers
// --------------------
let addRoutes (app : IApplicationBuilder) : IApplicationBuilder =
  let ab = app
  let app = app :?> WebApplication

  let builder = RouteBuilder(ab)

  let addRoute (pattern : string) (handler : HttpHandler) =
    builder.MapPost(pattern, handler) |> ignore<IRouteBuilder>

  let clientJsonApi name version (f : HttpContext -> Task<'a>) =
    let handler = clientJsonHandler f
    let route = $"/api/v{version}/{name}"
    addRoute route handler

  clientJsonApi "execute-text" 0 API.ExecuteText.post
  clientJsonApi "execute-json" 0 API.ExecuteJson.post

  app.UseRouter(builder.Build())


// let rollbarCtxToMetadata (ctx : HttpContext) : (Rollbar.Person * Metadata) =
//   let person =
//     try
//       loadUserInfo ctx
//     with
//     | _ -> None

//   let canvas =
//     try
//       string (loadCanvasInfo ctx).name
//     with
//     | _ -> null

//   let clientVersion =
//     try
//       string ctx.Request.Headers["x-darklang-client-version"] |> String.take 7
//     with
//     | _ -> null

//   (person, [ "canvas", canvas; "client_version", clientVersion ])

let configureApp (appBuilder : WebApplication) =
  appBuilder
  // |> fun app -> app.UseServerTiming() // must go early or this is dropped
  // |> fun app -> Rollbar.AspNet.addRollbarToApp app rollbarCtxToMetadata None
  |> fun app -> app.UseRouting()
  |> fun app -> app.UseDeveloperExceptionPage()
  // must go after UseRouting
  // |> Kubernetes.configureApp LibService.Config.apiServerKubernetesPort
  |> addRoutes
  |> ignore<IApplicationBuilder>

// A service is a value that's added to each request, to be used by some middleware.
// For example, ServerTiming adds a ServerTiming value that is then used by the ServerTiming middleware
let configureServices (services : IServiceCollection) : unit =
  services
  // |> Rollbar.AspNet.addRollbarToServices
  // |> Telemetry.AspNet.addTelemetryToServices "ApiServer" Telemetry.TraceDBQueries
  // |> Kubernetes.configureServices [ LibBackend.Canvas.healthCheck ]
  // |> fun s -> s.AddServerTiming()
  |> ignore<IServiceCollection>

let webserver
  (loggerSetup : ILoggingBuilder -> unit)
  (httpPort : int)
  (healthCheckPort : int)
  : WebApplication =
  let hcUrl = Kubernetes.url healthCheckPort

  let builder = WebApplication.CreateBuilder()
  configureServices builder.Services
  // Kubernetes.registerServerTimeout builder.WebHost

  builder.WebHost
  |> fun wh -> wh.ConfigureLogging(loggerSetup)
  |> fun wh -> wh.UseKestrel(LibService.Kestrel.configureKestrel)
  |> fun wh -> wh.UseUrls(hcUrl, $"http://localhost:{httpPort}")
  |> ignore<IWebHostBuilder>

  let app = builder.Build()
  configureApp app
  app

let runServer (debug : bool) (port : int) (hcPort : int) : unit =
  let logger =
    // LIGHTTODO
    // if debug then LibService.Logging.debugLogger else
    LibService.Logging.noLogger
  System.Console.WriteLine
    $"Starting server on port {port}, health check on {hcPort}"
  (webserver logger port hcPort).Run()

let readFromStdin () : unit =
  let stdin = System.Console.In.ReadToEnd()
  let expr = Parser.parseRTExpr stdin
  let result = Execute.execute expr Map.empty
  let dval = result.Result
  let output = LibExecution.DvalReprLegacyExternal.toEnduserReadableTextV0 dval
  System.Console.Out.WriteLine output

let readFiles (files : string list) : unit =
  let expr =
    files
    |> List.map (fun file -> System.IO.File.ReadAllText file)
    |> String.concat "\n"
    |> Parser.parseRTExpr

  let result = Execute.execute expr Map.empty
  let dval = result.Result
  let output = LibExecution.DvalReprLegacyExternal.toEnduserReadableTextV0 dval
  System.Console.Out.WriteLine output





// Generally speaking, this should be a superset of BwdServer's list.
let initSerializers () =
  // universally-serializable types
  Json.Vanilla.allow<pos> "Prelude"

// one-off types used internally
// Json.Vanilla.allow<LibExecution.ProgramTypes.Oplist> "Canvas.loadJsonFromDisk"
// Json.Vanilla.allow<LibExecution.ProgramTypes.Position> "Canvas.saveTLIDs"
// Json.Vanilla.allow<LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.Dval>
//   "RoundtrippableSerializationFormatV0.Dval"
// Json.Vanilla.allow<LibBackend.Analytics.HeapIOMetadata> "heap.io metadata"
// Json.Vanilla.allow<LibBackend.EventQueueV2.NotificationData> "eventqueue storage"
// Json.Vanilla.allow<LibBackend.PackageManager.ParametersDBFormat> "PackageManager"
// Json.Vanilla.allow<LibBackend.Session.JsonData> "LibBackend session db storage"
// Json.Vanilla.allow<LibBackend.TraceCloudStorage.CloudStorageFormat>
//   "TraceCloudStorageFormat"
// Json.Vanilla.allow<LibService.Rollbar.HoneycombJson> "Rollbar"

// for API request/response payloads
// Json.Vanilla.allow<CTApi.Workers.WorkerStats.Request> "ApiServer.Workers"
// Json.Vanilla.allow<CTApi.Workers.WorkerStats.Response> "ApiServer.Workers"
module Arguments =
  type Mode =
    | Serve of port : int * healthCheckPort : int
    | Execute of List<string>
    | Help

  type Option = | Debug

  let printHelp () : unit =
    System.Console.Out.WriteLine
      "Usage: darklang-executor [serve [--port=3275] [--healthCheckPort=3276]] [--debug] ...files"
    System.Console.Out.WriteLine
      "  serve [--port=3275] [--healthCheckPort=3276]  Run the server, accepting requests on the given port at /execute-text and /execute-json"
    System.Console.Out.WriteLine "  --debug  Enable debug logging"
    System.Console.Out.WriteLine "  --help  Print this help message"


  let parse (cliArgs : List<string>) : (Mode * List<Option>) =
    let result =
      List.fold
        (None, [])
        (fun (state : Option<Mode> * List<Option>) (cliArg : string) ->
          match state, cliArg |> String.split "=" with
          // help
          | (Some Help, _), _ -> (Some Help, [])
          | _, [ "--help" ] -> (Some Help, [])
          | (mode, opts), [ "--debug" ] -> (mode, opts @ [ Debug ])
          // serve
          | (None, opts), [ "serve" ] ->
            (Some(Serve(port = 3275, healthCheckPort = 3276)), opts)
          // server --port
          | (Some (Serve (_, hcPort)), opts), [ "--port"; port ] ->
            (Some(Serve(port = int port, healthCheckPort = hcPort)), opts)
          // server --healthCheckPort
          | (Some (Serve (port, _)), opts), [ "--healthCheckPort"; hcPort ] ->
            (Some(Serve(port = port, healthCheckPort = int hcPort)), opts)
          // file list
          | (None, opts), [ file ] -> (Some(Execute [ file ]), opts)
          | (Some (Execute files), opts), [ file ] ->
            (Some(Execute(files @ [ file ])), opts)
          | _ ->
            print "Invalid argument {{cliArg}}, in state {{state}}"
            (Some Help, []))
        cliArgs
    match result with
    | (None, opts) -> (Execute [], opts)
    | (Some mode, opts) -> (mode, opts)


[<EntryPoint>]
let main (args : string []) =
  try
    initSerializers ()
    let cliArgs = args |> List.fromArray |> Arguments.parse
    let debug = List.contains Arguments.Debug (snd cliArgs)
    match cliArgs with
    | (Arguments.Serve (port, hcPort), _) -> runServer debug port hcPort
    | (Arguments.Execute [], _) -> readFromStdin ()
    | (Arguments.Execute files, _) -> readFiles files
    | (Arguments.Help, _) -> Arguments.printHelp ()

    // LibService.Init.init name
    // (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    // (LibRealExecution.Init.init name).Result

    0
  // LibService.Init.shutdown name 0
  with
  | e ->
    System.Console.WriteLine $"Error starting Executor: {{e}}"
    1
//  LibService.Rollbar.lastDitchBlockAndPage "Error starting ApiServer" e
