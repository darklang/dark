/// Module supporting Pusher.com usage
module LibBackend.Pusher

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module AT = LibExecution.AnalysisTypes
module FireAndForget = LibService.FireAndForget

// PusherClient has own internal serializer which matches this interface.
type Serializer() =
  interface PusherServer.ISerializeObjectsToJson with
    member this.Serialize(x : obj) : string = Json.OCamlCompatible.serialize x


let pusherClient : Lazy<PusherServer.Pusher> =
  lazy
    (let options = PusherServer.PusherOptions()
     options.Cluster <- Config.pusherCluster
     options.set_JsonSerializer (Serializer())

     PusherServer.Pusher(
       Config.pusherID,
       Config.pusherKey,
       Config.pusherSecret,
       options
     ))


/// <summary>Send an event to Pusher.com.</summary>
///
/// <remarks>
/// This is fired in the background, and does not take any time from the current thread.
/// You cannot wait for it, by design.
/// </remarks>
let push
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (eventName : string)
  (payload : 'x)
  : unit =
  FireAndForget.fireAndForgetTask executionID $"pusher: {eventName}" (fun () ->
    task {
      use _ = LibService.Telemetry.child "pusher" [ "eventName", eventName ]
      // TODO: handle messages over 10k bytes
      // TODO: make channels private and end-to-end encrypted in order to add public canvases
      let client = Lazy.force pusherClient
      let channel = $"canvas_{canvasID}"

      let! (_ : PusherServer.ITriggerResult) =
        client.TriggerAsync(channel, eventName, payload)

      return ()
    })




let pushNewTraceID
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (tlids : tlid list)
  : unit =
  push executionID canvasID "new_trace" (traceID, tlids)


let pushNew404
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (f404 : TraceInputs.F404)
  =
  push executionID canvasID "new_404" f404


let pushNewStaticDeploy
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (asset : StaticAssets.StaticDeploy)
  =
  push executionID canvasID "new_static_deploy" asset


// For exposure as a DarkInternal function
let pushAddOpEvent
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (event : Op.AddOpEvent)
  =
  push executionID canvasID "add_op" event

let pushWorkerStates
  (executionID : ExecutionID)
  (canvasID : CanvasID)
  (ws : EventQueue.WorkerStates.T)
  : unit =
  push executionID canvasID "worker_state" ws

type JsConfig = { enabled : bool; key : string; cluster : string }

let jsConfigString =
  // CLEANUP use JSON serialization
  $"{{enabled: true, key: '{Config.pusherKey}', cluster: '{Config.pusherCluster}'}}"
