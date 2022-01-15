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

let standardProperties
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  (owner : UserID)
  : Map<string, string> =
  Map.empty
  |> Map.add "canvas" (string canvasName)
  |> Map.add "organization" (string owner)
  |> Map.add "canvas_id" (string canvasID)


let _payloadForEvent
  (executionID : ExecutionID)
  (event : string)
  (owner : UserID)
  (properties : Map<string, string>)
  : TrackPayload =
  let timestamp = System.DateTime.Now
  let properties =
    properties
    |> Map.add "timestamp" (string timestamp)
    |> Map.add "organization" (string owner)
    |> Map.add "execution_id" (string executionID)

  { identity = string owner
    timestamp = timestamp
    event = event
    app_id = Config.heapioId
    properties = properties }

// https://www.stevejgordon.co.uk/httpclient-connection-pooling-in-dotnet-core
let _socketsHandler =
  let socketsHandler = new SocketsHttpHandler()
  socketsHandler.PooledConnectionIdleTimeout <- System.TimeSpan.FromMinutes 5.0
  socketsHandler.PooledConnectionLifetime <- System.TimeSpan.FromMinutes 10.0
  socketsHandler

let httpClient () : HttpClient = new HttpClient(_socketsHandler)


let heapioEvent
  (executionID : ExecutionID)
  (owner : UserID)
  (event : string)
  (msgType : Type)
  (payload : Map<string, string>)
  : unit =
  FireAndForget.fireAndForgetTask "heapio.track" executionID (fun () ->
    task {
      Telemetry.addEvent
        // CLEANUP rate limit this, it will double our events otherwise
        "pushing heapio event via stroller" // CLEANUP it's not via stroller
        ((Map.toList payload |> List.map (fun (k, v) -> (k, v)))
         @ [ ("event", event); ("userid", string owner) ])

      let client = httpClient ()

      // path
      let endpoint =
        match msgType with
        | Track -> "api/track"
        | Identify -> "api/add_user_properties"

      let url = $"https://heapanalytics.com/{endpoint}"
      let requestMessage = new HttpRequestMessage(HttpMethod.Post, url)

      // body
      let payload = _payloadForEvent executionID event owner payload
      requestMessage.Content <- JsonContent.Create payload

      // auth
      let authenticationString =
        $":{Config.heapioId}" |> UTF8.toBytes |> Base64.defaultEncodeToString

      requestMessage.Headers.Authorization <-
        AuthenticationHeaderValue("Basic", authenticationString)
      // Content-Type added automatically via JsonContent
      requestMessage.Headers.Add("Accept", "application/json")


      let! result = client.SendAsync(requestMessage)
      assertEq "heapid status" System.Net.HttpStatusCode.OK result.StatusCode
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
  let props = standardProperties canvasName canvasID owner
  heapioEvent executionID owner event Track (Map.mergeFavoringRight props payload)
