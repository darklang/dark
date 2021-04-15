module LibBackend.Pusher

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module AT = LibExecution.AnalysisTypes

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


// Send an event to pusher. Note: this is fired in the backgroup, and does not
// take any time from the current thread. You cannot wait for it, by design.
let push
  (executionID : id)
  (canvasID : CanvasID)
  (eventName : string)
  (payload : 'x)
  : unit =
  let client = Lazy.force pusherClient

  // TODO: handle messages over 10k
  // TODO: make channels private and end-to-end encrypted in order to add public canvases

  let (_ : Task<unit>) =
    task {
      try
        printfn $"Sending push to Pusher {eventName}: {canvasID}"
        let channel = $"canvas_{canvasID}"
        // FSTODO add executionID

        let! (_ : PusherServer.ITriggerResult) =
          client.TriggerAsync(channel, eventName, payload)

        return ()
      with e ->
        // swallow this error
        printfn
          $"Error Sending push to Pusher {eventName}: {canvasID}: {e.ToString()}"

        LibService.Rollbar.send
          executionID
          [ "canvasID", toString canvasID; "event", eventName; "context", "pusher" ]
          e

      return ()
    }
  // do not wait for the push task to finish, just fire and forget
  ()



let pushNewTraceID
  (executionID : id)
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (tlids : tlid list)
  : unit =
  push executionID canvasID "new_trace" (traceID, tlids)


let pushNew404 (executionID : id) (canvasID : CanvasID) (f404 : TraceInputs.F404) =
  push executionID canvasID "new_404" f404


let pushNewStaticDeploy
  (executionID : id)
  (canvasID : CanvasID)
  (asset : StaticAssets.StaticDeploy)
  =
  push executionID canvasID "new_static_deploy" asset

//
// (* For exposure as a DarkInternal function *)
// let push_new_event
//     ~(execution_id : Types.id)
//     ~(canvas_id : Uuidm.t)
//     ~(event : string)
//     (payload : string) =
//   push ~execution_id ~canvas_id ~event payload

let pushWorkerStates
  (executionID : id)
  (canvasID : CanvasID)
  (ws : EventQueue.WorkerStates.T)
  : unit =
  push executionID canvasID "worker_state" ws

type JsConfig = { enabled : bool; key : string; cluster : string }

let jsConfigString =
  // CLEANUP use JSON serialization
  $"{{enabled: true, key: '{Config.pusherKey}', cluster: '{Config.pusherCluster}'}}"
