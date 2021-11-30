module LibService.HeapAnalytics

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net.Http
open System.Net.Http.Json
open System.Net.Http.Headers

open Prelude
open Tablecloth

type IdentifyPayload =
  { identity : string
    app_id : string
    properties : Map<string, string> }

type TrackPayload =
  { identity : string
    app_id : string
    event : string
    timestamp : System.DateTime
    properties : Map<string, string> }

type Type =
  | Track
  | Identify


let _payloadForEvent
  (executionID : ExecutionID)
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  (event : string)
  (owner : UserID)
  (properties : Map<string, string>)
  : TrackPayload =
  let timestamp = System.DateTime.Now

  let properties =
    properties
    |> Map.add "canvas" (string canvasName)
    |> Map.add "organization" (string owner)
    |> Map.add "canvas_id" (string canvasID)
    |> Map.add "execution_id" (string executionID)
    |> Map.add "timestamp" (string timestamp)

  { identity = string owner
    timestamp = timestamp
    event = event
    app_id = Config.heapioId
    properties = properties }

// FSTODO
// let _log_params_for_heapio ~canvas ~canvas_id ~event ~(user_id : Uuidm.t) :
//     (string * string) list =
//   [ ("canvas", canvas)
//   ; ("canvas_id", canvas_id |> Option.map ~f:Uuidm.to_string)
//   ; ("event", event)
//   ; ("userid", Some (user_id |> Uuidm.to_string)) ]
//   |> Ply.List.filterMapSequentially ~f:(fun (k, v) ->
//          match v with Some v -> Some (k, v) | _ -> None)

// https://www.stevejgordon.co.uk/httpclient-connection-pooling-in-dotnet-core
let _socketsHandler =
  let socketsHandler = new SocketsHttpHandler()
  socketsHandler.PooledConnectionIdleTimeout <- System.TimeSpan.FromMinutes 5.0
  socketsHandler.PooledConnectionLifetime <- System.TimeSpan.FromMinutes 10.0
  socketsHandler

let httpClient () : HttpClient = new HttpClient(_socketsHandler)


let heapioEvent
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (owner : System.Guid)
  (event : string)
  (msgType : Type)
  (payload : Map<string, string>)
  : Task<unit> =
  // FSTODO: discard the task, don't use it, same as with pusher
  task {
    // FSTODO
    // let log_params = _log_params_for_heapio canvas_id canvas event user_id in
    // Log.infO "pushing heapio event via stroller" log_params ;
    use client = httpClient ()

    // path
    let endpoint =
      match msgType with
      | Track -> "api/track"
      | Identify -> "api/add_user_properties"

    let url = $"https://heapanalytics.com/{endpoint}"
    let requestMessage = new HttpRequestMessage(HttpMethod.Post, url)

    // body
    let payload =
      _payloadForEvent executionID canvasName canvasID event owner payload

    requestMessage.Content <- JsonContent.Create payload

    // auth
    let authenticationString =
      $":{Config.heapioId}" |> UTF8.toBytes |> Base64.defaultEncodeToString

    requestMessage.Headers.Authorization <-
      AuthenticationHeaderValue("Basic", authenticationString)

    let! result = client.SendAsync(requestMessage)

    match result.StatusCode with
    | System.Net.HttpStatusCode.Accepted ->
      // FSTODO
      // Log.infO
      //   "pushed to heapio via stroller"
      //   ~jsonparams:[("status", `Int code)]
      //   ~params:log_params
      ()
    | _ ->
      // FSTODO
      // Log.erroR
      //   "failed to push to heapio via stroller"
      //   ~jsonparams:[("status", `Int code)]
      //   ~params:log_params ) ;
      ()

    return ()
  }

// CLEANUP do bulk track
let track
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (owner : System.Guid)
  (event : string)
  (payload : Map<string, string>)
  : unit =
  heapioEvent executionID canvasID canvasName owner event Track payload
  |> ignore<Task<unit>>

//
// (* We call this in two contexts: DarkInternal:: fns, and
//  * bin/heapio_identify_users.exe. Neither of those is an async/lwt context, so
//  * we use the blocking_curl_post instead of Curl_lwt. *)
// let heapio_identify_user (username : string) : unit =
//   match Account.get_user_and_created_at_and_analytics_metadata username with
//   | None ->
//       let bt = Exception.get_backtrace () in
//       ( match
//           Rollbar.report
//             (Exception.internal
//                "No user found when calling heapio_identify user")
//             bt
//             (Other "heapio_identify_user")
//             "No execution id"
//         with
//       | `Failure ->
//           Log.erroR "Failed to Rollbar.report in heapio_identify_user"
//       | _ ->
//           () )
//   | Some (user_info_and_created_at, heapio_metadata) ->
//       let organization =
//         username
//         |> Authorization.orgs_for
//         (* A user's orgs for this purpose do not include orgs it has
//          * read-only access to *)
//         |> List.filter ~f:(function _, rw -> rw = ReadWrite)
//         (* If you have one org, that's your org! If you have no orgs, or
//          * more than one, then we just use your username. This is because
//          * Heap's properties/traits don't support lists. *)
//         |> function [(org_name, _)] -> org_name | _ -> username
//       in
//       let payload =
//         let payload =
//           `Assoc
//             [ ("username", `String user_info_and_created_at.username)
//             ; ("email", `String user_info_and_created_at.email)
//             ; ("name", `String user_info_and_created_at.name)
//             ; ("admin", `Bool user_info_and_created_at.admin)
//             ; ("handle", `String user_info_and_created_at.username)
//             ; ("organization", `String organization) ]
//         in
//         (* We do zero checking of fields in heapio_metadata, but this is ok
//          * because it's a field we control, going to a service only we see.
//          * If we wanted to harden this later, we could List.filter the
//          * heapio_metadata yojson *)
//         Yojson.Safe.Util.combine payload heapio_metadata
//       in
//       heapio_event_blocking
//         ~user_id:user_info_and_created_at.id
//         Identify
//         payload
