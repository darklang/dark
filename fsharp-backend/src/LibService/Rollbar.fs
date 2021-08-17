module LibService.Rollbar

open Prelude
open Tablecloth

let mutable initialized = false

let init (serviceName : string) : unit =
  print "Configuring rollbar"
  // FSTODO: include host, ip address, serviceName
  let config = Rollbar.RollbarConfig(Config.rollbarServerAccessToken)
  config.Environment <- Config.rollbarEnvironment
  config.Enabled <- Config.rollbarEnabled
  config.LogLevel <- Rollbar.ErrorLevel.Error
  // FSTODO add username

  let (_ : Rollbar.IRollbar) =
    Rollbar.RollbarLocator.RollbarInstance.Configure config

  initialized <- true
  ()

// "https://ui.honeycomb.io/dark/datasets/kubernetes-bwd-ocaml?query={\"filters\":[{\"column\":\"rollbar\",\"op\":\"exists\"},{\"column\":\"execution_id\",\"op\":\"=\",\"value\":\"44602511168214071\"}],\"limit\":100,\"time_range\":604800}"
type HoneycombFilter = { column : string; op : string; value : string }

type HoneycombJson =
  { filters : List<HoneycombFilter>
    limit : int
    time_range : int }

let honeycombLinkOfExecutionID (executionID : id) : string =
  let query =
    { filters =
        [ { column = "execution_id"; op = "="; value = toString executionID } ]
      limit = 100
      // 604800 is 7 days
      time_range = 604800 }

  let queryStr = Json.Vanilla.serialize query

  let uri =
    System.Uri(
      $"https://ui.honeycomb.io/dark/datasets/kubernetes-bwd-ocaml?query={queryStr}"
    )

  toString uri

let send (executionID : id) (metadata : List<string * string>) (e : exn) : unit =
  assert initialized

  try
    print "sending exception to rollbar"
    let (state : Dictionary.T<string, obj>) = Dictionary.empty ()
    state.["message.honeycomb"] <- honeycombLinkOfExecutionID executionID
    state.["execution_id"] <- executionID
    List.iter (fun (k, v) -> Dictionary.add k (v :> obj) state |> ignore) metadata

    let (_ : Rollbar.ILogger) =
      Rollbar.RollbarLocator.RollbarInstance.Error(e, state)

    ()
  with
  | e ->
    // FSTODO: log failure
    print "Exception when calling rollbar"

module AspNet =
  open Microsoft.Extensions.DependencyInjection
  open Rollbar.NetCore.AspNet
  open Microsoft.AspNetCore.Builder
  open Microsoft.AspNetCore.Http.Abstractions

  let addRollbarToServices (services : IServiceCollection) : IServiceCollection =
    // https://jsnelders.com/Blog/2989/adding-rollbar-to-asp-net-core-2-some-services-are-not-able-to-be-constructed-and-unable-to-resolve-service-for-type-microsoft-aspnetcore-http-ihttpcontextaccessor/
    services.AddHttpContextAccessor().AddRollbarLogger()

  let addRollbarToApp (app : IApplicationBuilder) : IApplicationBuilder =
    app.UseRollbarMiddleware()



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
