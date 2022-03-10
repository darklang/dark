/// Provides functions that emit events to https://heap.io
[<RequireQualifiedAccess>]
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
    // Setting this type as a string gives us more control over how this serializes
    // as JSON
    timestamp : string
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


/// Send an event to Heap.io
let emitEvent
  (executionID : ExecutionID)
  (msgType : Type)
  (body : JsonContent)
  : unit =
  FireAndForget.fireAndForgetTask executionID "heapio.track" (fun () ->
    task {
      let client = httpClient ()

      // prepare request
      let url =
        let endpoint =
          match msgType with
          | Track -> "api/track"
          | IdentifyUser -> "api/add_user_properties"

        $"https://heapanalytics.com/{endpoint}"

      let authString =
        $":{Config.heapioId}" |> UTF8.toBytes |> Base64.defaultEncodeToString

      let requestMessage =
        let msg = new HttpRequestMessage(HttpMethod.Post, url)

        msg.Content <- body

        msg.Headers.Authorization <- AuthenticationHeaderValue("Basic", authString)

        // Content-Type added automatically via JsonContent
        msg.Headers.Add("Accept", "application/json")

        msg

      // make request
      let! result = client.SendAsync(requestMessage)

      // handle response
      if result.StatusCode <> System.Net.HttpStatusCode.OK then
        let! responseBody = result.Content.ReadAsStringAsync()
        Rollbar.sendError
          executionID
          "heapio-apierror"
          [ "response", responseBody
            "request", body
            "url", url
            "authentication", authString
            "statusCode", result.StatusCode ]

      return ()
    })

/// Collects data to be sent in a Heap event
let makeTrackEventBody
  (executionID : ExecutionID)
  (event : string)
  (owner : UserID)
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  (properties : Map<string, string>)
  : TrackPayload =
  let timestamp = NodaTime.Instant.now ()
  let properties =
    properties
    |> Map.add "timestamp" (string timestamp)
    |> Map.add "organization" (string owner)
    |> Map.add "execution_id" (string executionID)
    |> Map.add "canvas" (string canvasName)
    |> Map.add "organization" (string owner)
    |> Map.add "canvas_id" (string canvasID)

  { identity = string owner
    // heap API doesn't like submilliseconds, which the json encoding produces.
    // Stringifying here prevents this.
    timestamp = string timestamp
    event = event
    app_id = Config.heapioId
    properties = properties }


// CLEANUP do bulk track

/// Track an event to Heap
let track
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (owner : System.Guid)
  (event : string)
  (metadata : Map<string, string>)
  : unit =
  let body =
    makeTrackEventBody executionID event owner canvasName canvasID metadata
    |> JsonContent.Create

  emitEvent executionID Track body


/// Collects data to be sent in an "identify user" Heap event
let makeIdentifyUserBody
  (executionID : ExecutionID)
  (owner : UserID)
  (properties : Map<string, string>)
  : IdentifyUserPayload =
  let timestamp = NodaTime.Instant.now ()
  let properties =
    properties
    |> Map.add "timestamp" (string timestamp)
    |> Map.add "organization" (string owner)
    |> Map.add "execution_id" (string executionID)

  { identity = string owner; app_id = Config.heapioId; properties = properties }

let emitIdentifyUserEvent
  (executionID : ExecutionID)
  (owner : UserID)
  (metadata : Map<string, string>)
  : unit =
  let body = makeIdentifyUserBody executionID owner metadata |> JsonContent.Create
  emitEvent executionID IdentifyUser body
