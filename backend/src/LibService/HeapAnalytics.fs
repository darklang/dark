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
  { identity : string; app_id : string; properties : Map<string, string> }

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
let emitEvent (msgType : Type) (body : JsonContent) : unit =
  FireAndForget.fireAndForgetTask "heapio.track" (fun () ->
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
  (event : string)
  (owner : UserID)
  (canvasID : CanvasID)
  (properties : Map<string, string>)
  : TrackPayload =
  let timestamp = NodaTime.Instant.now ()
  let properties =
    properties
    |> Map.add "timestamp" (string timestamp)
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
  (canvasID : CanvasID)
  (owner : System.Guid)
  (event : string)
  (metadata : Map<string, string>)
  : unit =
  metadata
  |> makeTrackEventBody event owner canvasID
  |> JsonContent.Create
  |> emitEvent Track


/// Collects data to be sent in an "identify user" Heap event
let makeIdentifyUserBody
  (owner : UserID)
  (properties : Map<string, string>)
  : IdentifyUserPayload =
  let timestamp = NodaTime.Instant.now ()
  let properties =
    properties
    |> Map.add "timestamp" (string timestamp)
    |> Map.add "organization" (string owner)

  { identity = string owner; app_id = Config.heapioId; properties = properties }

let emitIdentifyUserEvent (owner : UserID) (metadata : Map<string, string>) : unit =
  let body = makeIdentifyUserBody owner metadata |> JsonContent.Create
  emitEvent IdentifyUser body
