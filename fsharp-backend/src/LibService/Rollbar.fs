module LibService.Rollbar

open FSharp.Control.Tasks
open System.Threading.Tasks
open FSharp.Control.Tasks

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions

open Prelude
open Tablecloth

let mutable initialized = false

let init (serviceName : string) : unit =
  print "Configuring rollbar"
  let config = Rollbar.RollbarConfig(Config.rollbarServerAccessToken)
  config.Enabled <- Config.rollbarEnabled
  config.CaptureUncaughtExceptions <- true // doesn't seem to work afaict
  config.Environment <- Config.rollbarEnvironment
  config.LogLevel <- Rollbar.ErrorLevel.Error
  config.RethrowExceptionsAfterReporting <- false
  config.ScrubFields <-
    Array.append config.ScrubFields [| "Set-Cookie"; "Cookie"; "Authorization" |]
  // Seems we don't have the ability to set Rollbar.DTOs.Data.DefaultLanguage
  config.Transform <- fun payload -> payload.Data.Language <- "f#"

  let (state : Dictionary.T<string, obj>) = Dictionary.empty ()
  state["service"] <- serviceName
  config.Server <- Rollbar.DTOs.Server(state)
  config.Server.Host <- Config.hostName
  config.Server.Root <- Config.rootDir
  config.Server.CodeVersion <- Config.buildHash

  Rollbar.RollbarLocator.RollbarInstance.Configure config
  |> ignore<Rollbar.IRollbar>
  initialized <- true
  ()

// "https://ui.honeycomb.io/dark/datasets/kubernetes-bwd-ocaml?query={\"filters\":[{\"column\":\"rollbar\",\"op\":\"exists\"},{\"column\":\"execution_id\",\"op\":\"=\",\"value\":\"44602511168214071\"}],\"limit\":100,\"time_range\":604800}"
type HoneycombFilter = { column : string; op : string; value : string }

type HoneycombJson =
  { filters : List<HoneycombFilter>
    limit : int
    time_range : int }

let honeycombLinkOfExecutionID (executionID : ExecutionID) : string =
  let query =
    { filters = [ { column = "execution_id"; op = "="; value = string executionID } ]
      limit = 100
      // 604800 is 7 days
      time_range = 604800 }

  let queryStr = Json.Vanilla.serialize query

  let uri =
    System.Uri(
      $"https://ui.honeycomb.io/dark/datasets/kubernetes-bwd-ocaml?query={queryStr}"
    )

  string uri

let createState
  (message : string)
  (executionID : ExecutionID)
  (metadata : List<string * obj>)
  : Dictionary.T<string, obj> =

  let (custom : Dictionary.T<string, obj>) = Dictionary.empty ()
  custom["message"] <- message
  // CLEANUP rollbar has a built-in way to do this called "Service links"
  custom["message.honeycomb"] <- honeycombLinkOfExecutionID executionID
  custom["execution_id"] <- string executionID
  List.iter
    (fun (k, v) ->
      Dictionary.add k (v :> obj) custom |> ignore<Dictionary.T<string, obj>>)
    metadata
  custom


let sendHttpException
  (message : string)
  (executionID : ExecutionID)
  (metadata : List<string * obj>)
  (ctx : HttpContext)
  (e : exn)
  : unit =
  assert initialized
  try
    print $"rollbar: {message}"
    print e.Message
    print e.StackTrace
    let custom = createState message executionID metadata
    Rollbar.RollbarLocator.RollbarInstance.Error(e, custom)
    |> ignore<Rollbar.ILogger>
  with
  | e ->
    print "Exception when calling rollbar"
    print e.Message
    print e.StackTrace


let sendException
  (message : string)
  (executionID : ExecutionID)
  (metadata : List<string * obj>)
  (e : exn)
  : unit =
  assert initialized
  try
    print $"rollbar: {message}"
    print e.Message
    print e.StackTrace
    let custom = createState message executionID metadata
    Rollbar.RollbarLocator.RollbarInstance.Error(e, custom)
    |> ignore<Rollbar.ILogger>
  with
  | e ->
    print "Exception when calling rollbar"
    print e.Message
    print e.StackTrace

// Will block for 5 seconds to make sure this exception gets sent. Use for startup
// and other places where the process is intended to end after this call.
let lastDitchBlocking
  (message : string)
  (executionID : ExecutionID)
  (metadata : List<string * obj>)
  (e : exn)
  : unit =
  assert initialized
  try
    print $"last ditch rollbar: {message}"
    print e.Message
    print e.StackTrace
    let custom = createState message executionID metadata
    Rollbar
      .RollbarLocator
      .RollbarInstance
      .AsBlockingLogger(System.TimeSpan.FromSeconds(5))
      .Error(e, custom)
    |> ignore<Rollbar.ILogger>
  with
  | e ->
    print "Exception when calling rollbar"
    print e.Message
    print e.StackTrace

module AspNet =
  open Microsoft.Extensions.DependencyInjection
  open Rollbar.NetCore.AspNet
  open Microsoft.AspNetCore.Builder
  open Microsoft.AspNetCore.Http.Abstractions

  // Rollbar's ASP.NET core middleware requires an IHttpContextAccessor, which
  // supposedly costs significant performance (couldn't see a cost in practice
  // though). AFAICT, this allows HTTP vars to be shared across the Task using an
  // AsyncContext. This would make sense for a lot of ways to use Rollbar, but we use
  // telemetry for our context and only want to use rollbar for exception tracking.
  type DarkRollbarMiddleware
    (
      nextRequestProcessor : RequestDelegate,
      ctxMetadataFn : HttpContext -> List<string * obj>
    ) =
    member this._nextRequestProcessor : RequestDelegate = nextRequestProcessor
    member this.Invoke(ctx : HttpContext) : Task =
      task {
        try
          do! this._nextRequestProcessor.Invoke(ctx)
        with
        | e ->
          assert initialized
          try
            let uri = ctx.Request.GetEncodedUrl()

            print $"rollbar in http: {uri}"
            print e.Message
            print e.StackTrace

            let executionID =
              try
                ctx.Items["executionID"] :?> ExecutionID
              with
              | _ -> ExecutionID "unavailable"

            let metadata = ctxMetadataFn ctx
            let custom = createState "http" executionID metadata

            let package : Rollbar.IRollbarPackage =
              new Rollbar.ExceptionPackage(
                e,
                $"{nameof (RollbarMiddleware)} processed uncaught exception."
              )
            // decorate tha package http info:
            let package = HttpRequestPackageDecorator(package, ctx.Request, true)
            let package =
              new HttpResponsePackageDecorator(package, ctx.Response, true)

            Rollbar.RollbarLocator.RollbarInstance.Error(package, custom)
            |> ignore<Rollbar.ILogger>
          with
          | e ->
            print "Exception when calling rollbar"
            print e.Message
            print e.StackTrace
          raise e
      }



  let addRollbarToServices (services : IServiceCollection) : IServiceCollection =
    // Nothing to do here, as rollbar is initialized above
    services

  let addRollbarToApp
    (
      app : IApplicationBuilder,
      ctxMetadataFn : HttpContext -> List<string * obj>
    ) : IApplicationBuilder =
    app.UseMiddleware<DarkRollbarMiddleware>(ctxMetadataFn)



// FSTODO enrich this
// let error_to_payload =
//   let context =
//     match ctx with
//     | Remote _ ->
//         `String "server"
//     | EventQueue ->
//         `String "event queue worker"
//     | CronChecker ->
//         `String "cron event emitter"
//     | GarbageCollector ->
//         `String "garbage collector worker"
//     | Push _ ->
//         `String "server push"
//     | Other str ->
//         `String str
//     | Heapio event ->
//         `String (sprintf "heapio: %s" event)
//   in
//   let env = `String Config.rollbar_environment in
//   let language = `String "OCaml" in
//   let framework = `String "Cohttp" in
//   let level = if pageable then `String "critical" else `String "error" in
//   let payload =
//     match ctx with
//     | Remote request_data ->
//         let request =
//           let headers =
//             request_data.headers |> List.Assoc.map ~f:(fun v -> `String v)
//           in
//           [ ("url", `String ("https:" ^ request_data.url))
//           ; ("method", `String request_data.http_method)
//           ; ("headers", `Assoc headers)
//           ; ("execution_id", `String execution_id)
//           ; ("body", `String request_data.body) ]
//           |> fun r -> `Assoc r
//         in
//         [ ("body", message)
//         ; ("level", level)
//         ; ("environment", env)
//         ; ("language", language)
//         ; ("framework", framework)
//         ; ("context", context)
//         ; ("execution_id", `String execution_id)
//         ; ("request", request) ]
//     | EventQueue | CronChecker | GarbageCollector ->
//         [ ("body", message)
//         ; ("level", level)
//         ; ("environment", env)
//         ; ("language", language)
//         ; ("framework", framework)
//         ; ("execution_id", `String execution_id)
//         ; ("context", context) ]
//     | Push event | Heapio event ->
//         [ ("body", message)
//         ; ("level", level)
//         ; ("environment", env)
//         ; ("language", language)
//         ; ("framework", framework)
//         ; ("execution_id", `String execution_id)
//         ; ("context", context)
//         ; ("push_event", `String event) ]
//     | Other str ->
//         [ ("body", message)
//         ; ("level", level)
//         ; ("environment", env)
//         ; ("language", language)
//         ; ("framework", framework)
//         ; ("execution_id", `String execution_id)
//         ; ("context", context) ]
//   in
//   payload |> fun p -> `Assoc p
