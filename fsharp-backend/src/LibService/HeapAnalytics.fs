module LibService.HeapAnalytics

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net.Http
open System.Net.Http.Json
open System.Net.Http.Headers

open Prelude
open Tablecloth

module FireAndForget = LibService.FireAndForget

type IdentifyUserPayload =
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
  | IdentifyUser

// https://www.stevejgordon.co.uk/httpclient-connection-pooling-in-dotnet-core
let _socketsHandler =
  let socketsHandler = new SocketsHttpHandler()
  socketsHandler.PooledConnectionIdleTimeout <- System.TimeSpan.FromMinutes 5.0
  socketsHandler.PooledConnectionLifetime <- System.TimeSpan.FromMinutes 10.0
  socketsHandler

let httpClient () : HttpClient = new HttpClient(_socketsHandler)


let heapioEvent
  (executionID : ExecutionID)
  (msgType : Type)
  (body : JsonContent)
  : unit =
  FireAndForget.fireAndForgetTask "heapio.track" executionID (fun () ->
    task {
      let client = httpClient ()

      // path
      let endpoint =
        match msgType with
        | Track -> "api/track"
        | IdentifyUser -> "api/add_user_properties"

      let url = $"https://heapanalytics.com/{endpoint}"
      let requestMessage = new HttpRequestMessage(HttpMethod.Post, url)

      requestMessage.Content <- body

      // auth
      let authenticationString =
        $":{Config.heapioId}" |> UTF8.toBytes |> Base64.defaultEncodeToString

      requestMessage.Headers.Authorization <-
        AuthenticationHeaderValue("Basic", authenticationString)
      // Content-Type added automatically via JsonContent
      requestMessage.Headers.Add("Accept", "application/json")


      let! result = client.SendAsync(requestMessage)
      if result.StatusCode <> System.Net.HttpStatusCode.OK then
        let! body = result.Content.ReadAsStringAsync()
        Rollbar.sendError
          "heapio-apierror"
          executionID
          [ "body", body; "statusCode", result.StatusCode ]
      return ()
    })

let trackBody
  (executionID : ExecutionID)
  (event : string)
  (owner : UserID)
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  (properties : Map<string, string>)
  : TrackPayload =
  let timestamp = System.DateTime.Now
  let properties =
    properties
    |> Map.add "timestamp" (string timestamp)
    |> Map.add "organization" (string owner)
    |> Map.add "execution_id" (string executionID)
    |> Map.add "canvas" (string canvasName)
    |> Map.add "organization" (string owner)
    |> Map.add "canvas_id" (string canvasID)

  { identity = string owner
    timestamp = timestamp
    event = event
    app_id = Config.heapioId
    properties = properties }


// CLEANUP do bulk track
let track
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (owner : System.Guid)
  (event : string)
  (metadata : Map<string, string>)
  : unit =
  let body =
    trackBody executionID event owner canvasName canvasID metadata
    |> JsonContent.Create
  heapioEvent executionID Track body



let identifyUserBody
  (executionID : ExecutionID)
  (owner : UserID)
  (properties : Map<string, string>)
  : IdentifyUserPayload =
  let timestamp = System.DateTime.Now
  let properties =
    properties
    |> Map.add "timestamp" (string timestamp)
    |> Map.add "organization" (string owner)
    |> Map.add "execution_id" (string executionID)

  { identity = string owner; app_id = Config.heapioId; properties = properties }

let identifyUser
  (executionID : ExecutionID)
  (owner : UserID)
  (metadata : Map<string, string>)
  : unit =
  let body = identifyUserBody executionID owner metadata |> JsonContent.Create
  heapioEvent executionID IdentifyUser body
