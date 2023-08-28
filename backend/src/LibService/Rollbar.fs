module LibService.Rollbar

open FSharp.Control.Tasks
open System.Threading.Tasks
open FSharp.Control.Tasks

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions

open Prelude
open Tablecloth

/// Logs various Rollbar-related configuration options to Rollbar,
/// at the debug level
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

// -------------------------------
// Honeycomb
// -------------------------------

// "https://ui.honeycomb.io/dark/datasets/kubernetes-bwd-ocaml?query={\"filters\":[{\"column\":\"rollbar\",\"op\":\"exists\"},{\"column\":\"execution_id\",\"op\":\"=\",\"value\":\"44602511168214071\"}],\"limit\":100,\"time_range\":604800}"
type HoneycombFilter = { column : string; op : string; value : string }

type HoneycombJson =
  { filters : List<HoneycombFilter>; limit : int; time_range : int }

let honeycombLinkOfTraceID () : string =
  let rootID = Telemetry.rootID ()
  let query =
    { filters = [ { column = "trace.trace_id"; op = "="; value = rootID } ]
      limit = 100
      // 604800 is 7 days
      time_range = 604800 }

  let queryStr = Json.Vanilla.serialize query
  let dataset = Config.honeycombDataset

  let uri =
    System.Uri($"https://ui.honeycomb.io/dark/datasets/{dataset}?query={queryStr}")

  string uri

// -------------------------------
// Include person data
// -------------------------------

/// Optional person data
type Person = Option<UserID>

/// Take a Person and an Exception and create a RollbarPackage that can be reported
let createPackage (exn : exn) (person : Person) : Rollbar.IRollbarPackage =
  let package : Rollbar.IRollbarPackage =
    new Rollbar.ExceptionPackage(exn, exn.Message)
  match person with
  | Some id -> Rollbar.PersonPackageDecorator(package, id |> string, null)
  | None -> package



// -------------------------------
// Custom metadata
// -------------------------------
let createCustom (metadata : Metadata) : Dictionary.T<string, obj> =

  let (custom : Dictionary.T<string, obj>) = Dictionary.empty ()
  // CLEANUP rollbar has a built-in way to do this called "Service links"
  custom["message.honeycomb"] <- honeycombLinkOfTraceID ()

  List.iter (fun (k, v) -> Dictionary.add k (v :> obj) custom) metadata
  custom

// -------------------------------
// Reporting functions
// -------------------------------

/// Notify that a non-error happened
let notify (message : string) (metadata : Metadata) : unit =
  print $"rollbar: {message}"
  try
    print (string metadata)
  with _ ->
    ()
  let stacktraceMetadata = ("stacktrace", System.Environment.StackTrace :> obj)
  let metadata = stacktraceMetadata :: metadata
  Telemetry.notify message metadata
  let custom = createCustom metadata
  Rollbar.RollbarLocator.RollbarInstance.Info(message, custom)
  |> ignore<Rollbar.ILogger>


/// Notify that an error (but not an exception) happened
let sendError (message : string) (metadata : Metadata) : unit =
  print $"rollbar: {message}"
  try
    print (string metadata)
  with _ ->
    ()
  print System.Environment.StackTrace
  Telemetry.addEvent message metadata
  let custom = createCustom (("message", message :> obj) :: metadata)
  Rollbar.RollbarLocator.RollbarInstance.Error(message, custom)
  |> ignore<Rollbar.ILogger>

// If there is an exception while processing the exception
let exceptionWhileProcessingException
  (original : exn)
  (processingException : exn)
  : unit =

  // If there's an exception while creating another exception, let's try to report
  // as much as possible without error
  try
    print "Exception when processing exception"
  with _ ->
    ()

  try
    // Let's first try to get the new exception into the tracker quickly
    Rollbar.RollbarLocator.RollbarInstance.Critical(processingException)
    |> ignore<Rollbar.ILogger>
  with _ ->
    ()

  try
    // Then try to get the original exception into it
    Rollbar.RollbarLocator.RollbarInstance.Error(original)
    |> ignore<Rollbar.ILogger>
  with _ ->
    ()

  try
    // Now let's send a pageable exception, as our exception processing being down is
    // a pageable situation
    let e =
      Exception.PageableException(
        "Exception when processing exception",
        [],
        processingException
      )
    Rollbar.RollbarLocator.RollbarInstance.Critical(e) |> ignore<Rollbar.ILogger>
  with _ ->
    ()

  try
    // Do this after, as these could cause exceptions
    printException "Original exception" [] original
  with _ ->

    try
      // Do this after, as these could cause exceptions
      printException "Processing exception" [] processingException
    with _ ->

      try
        // Do telemetry later, in case that's the cause
        if Telemetry.Span.current () = null then
          Telemetry.createRoot "LastDitch" |> ignore<Telemetry.Span.T>
        Telemetry.addException [] original
        Telemetry.addException [] processingException
      with _ ->
        ()

let personMetadata (person : Person) : Metadata =
  match person with
  | None -> []
  | Some id -> [ "user.id", id ]


/// Sends exception to Rollbar and also to Telemetry (honeycomb) and also prints it.
/// The error is titled after the exception message - to change it wrap it in another
/// exception. However, it's better to keep the existing exception and add extra
/// context via metadata. For nested exceptions: the printException function and
/// Telemetry.addException are recursive and capture nested exceptions.
let rec sendException (person : Person) (metadata : Metadata) (e : exn) : unit =
  try
    // don't include exception metadata, as the other functions do
    let metadata = personMetadata person @ metadata

    // print to console
    printException $"rollbar ({NodaTime.Instant.now ()})" metadata e

    // send to telemetry
    use _span =
      if isNull (Telemetry.Span.current ()) then
        Telemetry.createRoot "sendException"
      else
        null // don't use the current span as we don't want `use` to clean it up
    Telemetry.addException metadata e

    // actually send to rollbar
    let metadata = Exception.nestedMetadata e @ metadata
    let custom = createCustom metadata
    let package = createPackage e person
    Rollbar.RollbarLocator.RollbarInstance.Error(package, custom)
    |> ignore<Rollbar.ILogger>
  // Do the same for the innerException
  with processingException ->
    exceptionWhileProcessingException e processingException


/// Wraps the exception in a PageableException, then sends in to rollbar in a blocking
/// call to make sure this exception gets sent. Use for startup and other places where
/// the process is intended to end after this call. Do not use this in general code,
/// as it blocks. Returns an int as the process will exit immediately after.
let lastDitchBlockAndPage (msg : string) (e : exn) : int =
  try
    printException msg [] e
    let e = Exception.PageableException(msg, [], e)
    Telemetry.addException [] e
    let metadata = Exception.nestedMetadata e
    let custom = createCustom metadata
    Rollbar.RollbarLocator.RollbarInstance
      .AsBlockingLogger(System.TimeSpan.FromSeconds(5))
      .Error(e, custom)
    |> ignore<Rollbar.ILogger>
    Telemetry.addException [] e
    (-1)
  with processingException ->
    exceptionWhileProcessingException e processingException
    Telemetry.flush ()
    LaunchDarkly.flush ()
    // Pause so that the exceptions can send
    Task.Delay(10000).Wait()
    (-1)

module AspNet =
  open Microsoft.Extensions.DependencyInjection
  open Rollbar.NetCore.AspNet
  open Microsoft.AspNetCore.Builder

  // Setup parameters for rollbar middleware
  type RollbarContext =
    {
      // Using the HttpContext, fetch the Person and any useful metadata
      ctxMetadataFn : HttpContext -> Person * Metadata
      // Ignore this path, used to ignore exceptions that happen during the k8s startupProbe
      ignoreStartupPath : Option<string> }

  // Rollbar's ASP.NET core middleware requires an IHttpContextAccessor, which
  // supposedly costs significant performance (couldn't see a cost in practice
  // though). AFAICT, this allows HTTP vars to be shared across the Task using an
  // AsyncContext. This would make sense for a lot of ways to use Rollbar, but we use
  // telemetry for our context and only want to use rollbar for exception tracking.
  type DarkRollbarMiddleware
    (nextRequestProcessor : RequestDelegate, rollbarCtx : RollbarContext) =
    member this._nextRequestProcessor : RequestDelegate = nextRequestProcessor
    member this.Invoke(ctx : HttpContext) : Task =
      task {
        try
          do! this._nextRequestProcessor.Invoke(ctx)
        with e ->
          if Some(string ctx.Request.Path) = rollbarCtx.ignoreStartupPath then
            ()
          else
            try
              let url =
                Exception.catch ctx.Request.GetEncodedUrl
                |> Option.defaultValue "invalid url"
              printException "rollbar in http middleware" [ "url", url ] e

              let person, metadata =
                try
                  rollbarCtx.ctxMetadataFn ctx
                with _ ->
                  None, [ "exception calling ctxMetadataFn", true ]
              let metadata = metadata @ Exception.nestedMetadata e
              let custom = createCustom metadata
              let package = createPackage e person
              let package = HttpRequestPackageDecorator(package, ctx.Request, true)
              let package = HttpResponsePackageDecorator(package, ctx.Response, true)
              Rollbar.RollbarLocator.RollbarInstance.Error(package, custom)
              |> ignore<Rollbar.ILogger>
              print "Rollbar exception sent"
            // No telemetry call here as it should happen automatically
            with processingException ->
              exceptionWhileProcessingException e processingException
          Exception.reraise e
      }


  let addRollbarToServices (services : IServiceCollection) : IServiceCollection =
    // Nothing to do here, as rollbar is initialized above
    services

  let addRollbarToApp
    (app : IApplicationBuilder)
    (ctxMetadataFn : HttpContext -> Person * Metadata)
    (ignoreStartupPath : Option<string>)
    : IApplicationBuilder =
    app.UseMiddleware<DarkRollbarMiddleware>(
      { ctxMetadataFn = ctxMetadataFn; ignoreStartupPath = ignoreStartupPath }
    )


let init (serviceName : string) : unit =
  print "Configuring Rollbar"
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
      [| "Set-Cookie"; "Cookie"; "Authorization"; "x-csrf-token" |]
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
  Rollbar.RollbarInfrastructure.Instance.QueueController.InternalEvent.AddHandler
    (fun this e -> print $"rollbar internal error: {e.TraceAsString()}")

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

  Exception.sendRollbarError <- fun msg metadata -> sendError msg metadata

  print " Configured rollbar"
  ()
