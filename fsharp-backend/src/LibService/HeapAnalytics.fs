module LibService.HeapAnalytics

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net.Http
open System.Net.Http.Json
open System.Net.Http.Headers

open Prelude
open Tablecloth

module FireAndForget = LibService.FireAndForget

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
  : unit =
  FireAndForget.fireAndForgetTask "heapio.track" executionID (fun () ->
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
      assertEq "heapid status" System.Net.HttpStatusCode.Accepted result.StatusCode
      return ()
    })

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

