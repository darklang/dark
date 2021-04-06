module LibBackend.Pusher

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

let mutable initialized = false

// PusherClient has own internal serializer which matches this interface.
type Serializer() =
  interface PusherServer.ISerializeObjectsToJson with
    member this.Serialize(x : obj) : string = Json.OCamlCompatible.serialize x


let pusherClient : Lazy<PusherServer.Pusher> =
  let create () : PusherServer.Pusher =
    let options = PusherServer.PusherOptions()
    options.Cluster <- Config.pusherCluster
    options.set_JsonSerializer (Serializer())

    let client =
      PusherServer.Pusher(
        Config.pusherID,
        Config.pusherKey,
        Config.pusherSecret,
        options
      )

    initialized <- true
    client
  lazy (create ())

// Send an event to pusher. Note: this is fired in the backgroup, and does not
// take any time from the current thread. You cannot wait for it, by design.
let push (canvasID : CanvasID) (eventName : string) (payload : 'x) : unit =
  let client = Lazy.force pusherClient
  assert initialized

  let (_ : Task<unit>) =
    task {
      try
        printfn $"Sending push to Pusher {eventName}: {canvasID}"
        let channel = $"canvas_{canvasID}"

        let! (_ : PusherServer.ITriggerResult) =
          client.TriggerAsync(channel, eventName, payload)

        return ()
      with e ->
        // swallow this error
        printfn
          $"Error Sending push to Pusher {eventName}: {canvasID}: {e.ToString()}"

        LibService.Rollbar.send e

      return ()
    }
  // do not wait for the push task to finish, just fire and forget
  ()



// let push_new_trace_id
//     ~(execution_id : Types.id)
//     ~(canvas_id : Uuidm.t)
//     (trace_id : Uuidm.t)
//     (tlids : Types.tlid list) =
//   let payload = Analysis.to_new_trace_frontend (trace_id, tlids) in
//   push ~execution_id ~canvas_id ~event:"new_trace" payload
//
//
// let push_new_404
//     ~(execution_id : Types.id)
//     ~(canvas_id : Uuidm.t)
//     (fof : Stored_event.four_oh_four) =
//   let payload = Analysis.to_new_404_frontend fof in
//   push ~execution_id ~canvas_id ~event:"new_404" payload
//
//
// let push_new_static_deploy
//     ~(execution_id : Types.id)
//     ~(canvas_id : Uuidm.t)
//     (asset : Static_assets.static_deploy) =
//   let payload = Analysis.to_new_static_deploy_frontend asset in
//   push ~execution_id ~canvas_id ~event:"new_static_deploy" payload
//
//
// (* For exposure as a DarkInternal function *)
// let push_new_event
//     ~(execution_id : Types.id)
//     ~(canvas_id : Uuidm.t)
//     ~(event : string)
//     (payload : string) =
//   push ~execution_id ~canvas_id ~event payload

let pushWorkerStates (canvasID : CanvasID) (ws : EventQueue.WorkerStates.T) : unit =
  push canvasID "worker_state" ws

type JsConfig = { enabled : bool; key : string; cluster : string }

let jsConfigString =
  // CLEANUP use JSON serialization
  $"{{enabled: true, key: '{Config.pusherKey}', cluster: '{Config.pusherCluster}'}}"
