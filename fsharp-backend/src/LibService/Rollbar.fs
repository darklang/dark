module LibService.Rollbar

let mutable initialized = false

let init () : unit =
  printfn "Configuring rollbar"
  // FSTODO: include host, ip address, canvas, person, serviceName, honeycomb url, execution_id, in requests
  let config = Rollbar.RollbarConfig(Config.rollbarServerAccessToken)
  config.Environment <- Config.rollbarEnvironment
  config.Enabled <- Config.rollbarEnabled

  printfn
    $"Starting rollbarConfig: {Config.rollbarServerAccessToken}, {
                                                                    Config.rollbarEnvironment
    }, {Config.rollbarEnabled}"

  let (_ : Rollbar.IRollbar) =
    Rollbar.RollbarLocator.RollbarInstance.Configure config

  initialized <- true
  ()

let send (e : exn) : unit =
  assert initialized

  try
    printfn "sending exception to rollbar"
    let (_ : Rollbar.ILogger) = Rollbar.RollbarLocator.RollbarInstance.Error e
    ()
  with e -> printfn "Exception when calling rollbar"
// FSTODO: log failure

// (* "https://ui.honeycomb.io/dark/datasets/kubernetes-bwd-ocaml?query={\"filters\":[{\"column\":\"rollbar\",\"op\":\"exists\"},{\"column\":\"execution_id\",\"op\":\"=\",\"value\":\"44602511168214071\"}],\"limit\":100,\"time_range\":604800}"
//  *)
// (* The escaping on this is a bit overkill - it'll do a layer of url-escaping
//  * beyond the example above, which we don't need - but the link works *)
// let honeycomb_link_of_execution_id (execution_id : string) : string =
//   let (query : Yojson.Safe.t) =
//     `Assoc
//       [ ( "filters"
//         , `List
//             [ `Assoc
//                 [ ("column", `String "execution_id")
//                 ; ("op", `String "=")
//                 ; ("value", `String execution_id) ] ] )
//       ; ("limit", `Int 100)
//       ; ("time_range", `Int 604800) ]
//     (* 604800 is 7 days *)
//   in
//   Uri.make
//     ~scheme:"https"
//     ~host:"ui.honeycomb.io"
//     ~path:"/dark/datasets/kubernetes-bwd-ocaml"
//     ~query:[("query", [query |> Yojson.Safe.to_string])]
//     ()
//   |> Uri.to_string


// let error_to_payload =
//   let message =
//     let interior =
//       [ ("body", `String (pp e))
//       ; ("raw_trace", `String (Caml.Printexc.raw_backtrace_to_string bt))
//       ; ("honeycomb", `String (honeycomb_link_of_execution_id execution_id))
//       ; ("raw_info", inspect e) ]
//       |> fun b -> `Assoc b
//     in
//     `Assoc [("message", interior)]
//   in
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
