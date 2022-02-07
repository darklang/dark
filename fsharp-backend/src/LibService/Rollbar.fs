module LibService.Rollbar

open FSharp.Control.Tasks
open System.Threading.Tasks
open FSharp.Control.Tasks

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions

open Prelude
open Tablecloth
open Exception

let debugRollbarConfig () =
  let config = Rollbar.RollbarInfrastructure.Instance.Config
  let destinationOptions = config.RollbarLoggerConfig.RollbarDestinationOptions
  debuG
    "destinationOptions"
    ((destinationOptions :?> Rollbar.RollbarDestinationOptions).TraceAsString())
  let telemetryOptions = config.RollbarTelemetryOptions
  debuG
    "telemetryOptions"
    ((telemetryOptions :?> Rollbar.RollbarTelemetryOptions).TraceAsString())
  let offlineStoreOptions = config.RollbarOfflineStoreOptions
  debuG
    "offlineStoreOptions"
    ((offlineStoreOptions :?> Rollbar.RollbarOfflineStoreOptions).TraceAsString())
  let infraOptions = config.RollbarInfrastructureOptions
  debuG
    "infraOptions"
    ((infraOptions :?> Rollbar.RollbarInfrastructureOptions).TraceAsString())
  let developerOptions = config.RollbarLoggerConfig.RollbarDeveloperOptions
  debuG
    "developerOptions"
    ((developerOptions :?> Rollbar.RollbarDeveloperOptions).TraceAsString())
  let dataSecurityOptions = config.RollbarLoggerConfig.RollbarDataSecurityOptions
  debuG
    "dataSecurityOptions"
    ((dataSecurityOptions :?> Rollbar.RollbarDataSecurityOptions).TraceAsString())
  let payloadAdditionOptions =
    config.RollbarLoggerConfig.RollbarPayloadAdditionOptions
  debuG
    "payloadAdditionOptions"
    ((payloadAdditionOptions :?> Rollbar.RollbarPayloadAdditionOptions)
      .TraceAsString())
  let payloadManipulationOptions =
    config.RollbarLoggerConfig.RollbarPayloadManipulationOptions
  debuG
    "payloadManipulationOptions"
    ((payloadManipulationOptions :?> Rollbar.RollbarPayloadManipulationOptions)
      .TraceAsString())
  ()


let init (serviceName : string) : unit =
  print "Configuring rollbar"
  let config =
    Rollbar.RollbarInfrastructureConfig(
      Config.rollbarServerAccessToken,
      Config.rollbarEnvironment
    )
  // We don't have any settings here
  // let _destinationOptions = config.RollbarLoggerConfig.RollbarDestinationOptions
  // let _telemetryOptions = config.RollbarTelemetryOptions

  // Offline storage
  let offlineStoreOptions = config.RollbarOfflineStoreOptions
  let osOpts = Rollbar.RollbarOfflineStoreOptions()
  osOpts.EnableLocalPayloadStore <- false
  offlineStoreOptions.Reconfigure osOpts
  |> ignore<Rollbar.IRollbarOfflineStoreOptions>

  // Main options
  let iOpts = Rollbar.RollbarInfrastructureOptions()
  iOpts.CaptureUncaughtExceptions <- true // doesn't seem to work afaict (true in v4, unclear in v5)
  config.RollbarInfrastructureOptions.Reconfigure(iOpts)
  |> ignore<Rollbar.IRollbarInfrastructureOptions>

  let dOpts = Rollbar.RollbarDeveloperOptions()
  dOpts.Enabled <- Config.rollbarEnabled
  dOpts.LogLevel <- Rollbar.ErrorLevel.Info // We use Info for notifications
  dOpts.Transmit <- true
  dOpts.RethrowExceptionsAfterReporting <- false
  dOpts.WrapReportedExceptionWithRollbarException <- false
  config.RollbarLoggerConfig.RollbarDeveloperOptions.Reconfigure(dOpts)
  |> ignore<Rollbar.IRollbarDeveloperOptions>

  // data options
  let dsOpts = Rollbar.RollbarDataSecurityOptions()
  dsOpts.ScrubFields <-
    Array.append
      config.RollbarLoggerConfig.RollbarDataSecurityOptions.ScrubFields
      [| "Set-Cookie"; "Cookie"; "Authorization" |]
  config.RollbarLoggerConfig.RollbarDataSecurityOptions.Reconfigure dsOpts
  |> ignore<Rollbar.IRollbarDataSecurityOptions>

  let paOpts = Rollbar.RollbarPayloadAdditionOptions()

  let (state : Dictionary.T<string, obj>) = Dictionary.empty ()
  state["service"] <- serviceName
  paOpts.Server <- Rollbar.DTOs.Server(state)
  paOpts.Server.Host <- Config.hostName
  paOpts.Server.Root <- Config.rootDir
  paOpts.Server.CodeVersion <- Config.buildHash
  config.RollbarLoggerConfig.RollbarPayloadAdditionOptions.Reconfigure paOpts
  |> ignore<Rollbar.IRollbarPayloadAdditionOptions>


  // Seems we don't have the ability to set Rollbar.DTOs.Data.DefaultLanguage
  let pmOpts = Rollbar.RollbarPayloadManipulationOptions()
  pmOpts.Transform <- (fun payload -> payload.Data.Language <- "f#")
  config.RollbarLoggerConfig.RollbarPayloadManipulationOptions.Reconfigure pmOpts
  |> ignore<Rollbar.IRollbarPayloadManipulationOptions>

  // Initialize
  Rollbar.RollbarInfrastructure.Instance.Init(config)

  // Debug Rollbar internals - when a Rollbar log is made, we lose sight of it.
  // Enabling this callback lets us see what actually happens when it's processed
  // Rollbar.RollbarInfrastructure.Instance.QueueController.InternalEvent.AddHandler
  //   (fun this e -> print $"rollbar internal error: {e.TraceAsString()}")

  // Disable the ConnectivityMonitor: https://github.com/rollbar/Rollbar.NET/issues/615
  // We actually want to call
  // Rollbar.RollbarInfrastructure.Instance.ConnectivityMonitor.Disable() to disable
  // the ConnectivityMonitor. However, ConnectivityMonitor is an internal class, and
  // we're only provided access to an IConnectivityMonitor, which does not have that
  // method.
  // We need to disable this because the ConnectivityMonitor checks for
  // www.rollbar.com:80 instead of api.rollbar.com:443, and we firewall that off in
  // production.
  // let cm = Rollbar.RollbarInfrastructure.Instance.ConnectivityMonitor
  // cm
  //   .GetType()
  //   .InvokeMember(
  //     "Disable",
  //     System.Reflection.BindingFlags.Public
  //     ||| System.Reflection.BindingFlags.InvokeMethod
  //     ||| System.Reflection.BindingFlags.Instance,
  //     null,
  //     cm,
  //     [||]
  //   )
  // |> ignore<obj>

  print " Configured rollbar"
  ()

// "https://ui.honeycomb.io/dark/datasets/kubernetes-bwd-ocaml?query={\"filters\":[{\"column\":\"rollbar\",\"op\":\"exists\"},{\"column\":\"execution_id\",\"op\":\"=\",\"value\":\"44602511168214071\"}],\"limit\":100,\"time_range\":604800}"
type HoneycombFilter = { column : string; op : string; value : string }

type HoneycombJson =
  { filters : List<HoneycombFilter>
    limit : int
    time_range : int }

let honeycombLinkOfExecutionID (executionID : ExecutionID) : string =
  let query =
    { filters =
        [ { column = "trace.trace_id"; op = "="; value = string executionID } ]
      limit = 100
      // 604800 is 7 days
      time_range = 604800 }

  let queryStr = Json.Vanilla.serialize query
  let dataset = Config.honeycombDataset

  let uri =
    System.Uri($"https://ui.honeycomb.io/dark/datasets/{dataset}?query={queryStr}")

  string uri


let createState
  (executionID : ExecutionID)
  (metadata : Metadata)
  : Dictionary.T<string, obj> =

  let (custom : Dictionary.T<string, obj>) = Dictionary.empty ()
  // CLEANUP rollbar has a built-in way to do this called "Service links"
  custom["message.honeycomb"] <- honeycombLinkOfExecutionID executionID
  custom["execution_id"] <- string executionID
  List.iter
    (fun (k, v) ->
      Dictionary.add k (v :> obj) custom |> ignore<Dictionary.T<string, obj>>)
    metadata
  custom

// Not an error, let's just be aware a thing happened
let notify
  (executionID : ExecutionID)
  (message : string)
  (metadata : Metadata)
  : unit =
  print $"rollbar: {message}"
  try
    print (string metadata)
  with
  | _ -> ()
  let stacktraceMetadata = ("stacktrace", System.Environment.StackTrace :> obj)
  let metadata = stacktraceMetadata :: metadata
  Telemetry.notify message metadata
  let custom = createState executionID metadata
  Rollbar.RollbarLocator.RollbarInstance.Info(message, custom)
  |> ignore<Rollbar.ILogger>


// An error but not an exception
let sendError
  (executionID : ExecutionID)
  (message : string)
  (metadata : Metadata)
  : unit =
  print $"rollbar: {message}"
  try
    print (string metadata)
  with
  | _ -> ()
  print System.Environment.StackTrace
  Telemetry.addEvent message metadata
  let custom = createState executionID (("message", message :> obj) :: metadata)
  Rollbar.RollbarLocator.RollbarInstance.Error(message, custom)
  |> ignore<Rollbar.ILogger>

let printMetadata (metadata : Metadata) =
  try
    print (string metadata)
  with
  | _ -> ()

// If there is an exception while processing the exception
let exceptionWhileProcessingException
  (original : exn)
  (processingException : exn)
  : unit =

  // If there's an exception while creating another exception, let's try to report
  // as much as possible without error
  try
    print "Exception when processing exception"
  with
  | _ -> ()

  try
    // Let's first try to get the new exception into the tracker quickly
    Rollbar.RollbarLocator.RollbarInstance.Critical(processingException)
    |> ignore<Rollbar.ILogger>
  with
  | _ -> ()

  try
    // Then try to get the original exception into it
    Rollbar.RollbarLocator.RollbarInstance.Error(original)
    |> ignore<Rollbar.ILogger>
  with
  | _ -> ()

  try
    // Now let's send a pageable exception, as our exception processing being down is
    // a pageable situation
    let e =
      PageableException("Exception when processing exception", processingException)
    Rollbar.RollbarLocator.RollbarInstance.Critical(e) |> ignore<Rollbar.ILogger>
  with
  | _ -> ()

  try
    // Do this after, as these could cause exceptions
    print "Original exception"
    print original.Message
    print original.StackTrace
  with
  | _ ->

    try
      // Do this after, as these could cause exceptions
      print "Processing exception"
      print processingException.Message
      print processingException.StackTrace
    with
    | _ ->

      try
        // Do telemetry later, in case that's the cause
        if Telemetry.Span.current () = null then
          Telemetry.createRoot "LastDitch" |> ignore<Telemetry.Span.T>
        Telemetry.addException original
        Telemetry.addException processingException
      with
      | _ -> ()


let sendException (executionID : ExecutionID) (e : exn) : unit =
  try
    print $"rollbar: {e.Message}"
    print e.StackTrace
    let metadata = Exception.toMetadata e
    printMetadata metadata
    Telemetry.addException e
    let custom = createState executionID metadata
    Rollbar.RollbarLocator.RollbarInstance.Error(e, custom)
    |> ignore<Rollbar.ILogger>
  with
  | processingException -> exceptionWhileProcessingException e processingException


// Wraps the exception in a PageableException, then sends in to rollbar in a blocking
// call to make sure this exception gets sent. Use for startup and other places where
// the process is intended to end after this call. Do not use this in general code,
// as it blocks. Returns an int as the process will exit immediately after.
let lastDitchBlockAndPage (msg : string) (inner : exn) : int =
  try
    let e = PageableException(msg, inner)
    print $"last ditch rollbar: {msg}"
    print inner.Message
    print inner.StackTrace
    // FSTODO: handle SystemInitializationException with a .Inner
    let metadata = Exception.toMetadata e
    Telemetry.addException e
    let custom = createState (ExecutionID "last ditch") metadata
    Rollbar
      .RollbarLocator
      .RollbarInstance
      .AsBlockingLogger(System.TimeSpan.FromSeconds(5))
      .Error(e, custom)
    |> ignore<Rollbar.ILogger>
    Telemetry.addException e
    (-1)
  with
  | processingException ->
    exceptionWhileProcessingException inner processingException
    // Pause so that the exceptions can send
    Task.Delay(10000).Wait()
    (-1)

module AspNet =
  open Microsoft.Extensions.DependencyInjection
  open Rollbar.NetCore.AspNet
  open Microsoft.AspNetCore.Builder


  type Person =
    { id : Option<UserID>
      email : Option<string>
      username : Option<UserName.T> }

  let emptyPerson = { id = None; email = None; username = None }


  // Rollbar's ASP.NET core middleware requires an IHttpContextAccessor, which
  // supposedly costs significant performance (couldn't see a cost in practice
  // though). AFAICT, this allows HTTP vars to be shared across the Task using an
  // AsyncContext. This would make sense for a lot of ways to use Rollbar, but we use
  // telemetry for our context and only want to use rollbar for exception tracking.
  type DarkRollbarMiddleware
    (
      nextRequestProcessor : RequestDelegate,
      ctxMetadataFn : HttpContext -> Person * Metadata
    ) =
    member this._nextRequestProcessor : RequestDelegate = nextRequestProcessor
    member this.Invoke(ctx : HttpContext) : Task =
      task {
        try
          do! this._nextRequestProcessor.Invoke(ctx)
        with
        | e ->
          try
            print $"rollbar in http middleware"
            print e.Message
            print e.StackTrace
            print (ctx.Request.GetEncodedUrl())

            let executionID =
              try
                ctx.Items["executionID"] :?> ExecutionID
              with
              | _ -> ExecutionID "unavailable"

            let person, metadata =
              try
                ctxMetadataFn ctx
              with
              | _ -> emptyPerson, [ "exception calling ctxMetadataFn", true ]
            let metadata = metadata @ Exception.toMetadata e
            let custom = createState executionID metadata

            let package : Rollbar.IRollbarPackage =
              new Rollbar.ExceptionPackage(e, e.Message)
            // decorate the http info
            let package = HttpRequestPackageDecorator(package, ctx.Request, true)
            let package =
              new HttpResponsePackageDecorator(package, ctx.Response, true)
            let package =
              Rollbar.PersonPackageDecorator(
                package,
                person.id |> Option.map string |> Option.defaultValue null,
                person.username |> Option.map string |> Option.defaultValue null,
                person.email |> Option.defaultValue null
              )
            Rollbar.RollbarLocator.RollbarInstance.Error(package, custom)
            |> ignore<Rollbar.ILogger>
            print "Rollbar exception sent"
          // No telemetry call here as it should happen automatically
          with
          | processingException ->
            exceptionWhileProcessingException e processingException
          e.Reraise()
      }



  let addRollbarToServices (services : IServiceCollection) : IServiceCollection =
    // Nothing to do here, as rollbar is initialized above
    services

  let addRollbarToApp
    (
      app : IApplicationBuilder,
      ctxMetadataFn : HttpContext -> Person * Metadata
    ) : IApplicationBuilder =
    app.UseMiddleware<DarkRollbarMiddleware>(ctxMetadataFn)
