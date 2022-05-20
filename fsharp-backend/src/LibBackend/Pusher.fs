/// Module supporting Pusher.com usage
module LibBackend.Pusher

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module AT = LibExecution.AnalysisTypes
module FireAndForget = LibService.FireAndForget


let pusherClient : Lazy<PusherServer.Pusher> =
  lazy
    (let options = PusherServer.PusherOptions()
     options.Cluster <- Config.pusherCluster
     options.Encrypted <- true
     // Use the raw serializer and expect everywhere to serialize appropriately
     options.set_JsonSerializer (PusherServer.RawBodySerializer())

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
  (canvasID : CanvasID)
  (eventName : string)
  (payload : string) // The raw string is sent, it's the job of the caller to have it as appropriate json
  : unit =
  FireAndForget.fireAndForgetTask $"pusher: {eventName}" (fun () ->
    task {
      // TODO: handle messages over 10k bytes
      // TODO: make channels private and end-to-end encrypted in order to add public canvases
      let client = Lazy.force pusherClient
      let channel = $"canvas_{canvasID}"

      let! (_ : PusherServer.ITriggerResult) =
        client.TriggerAsync(channel, eventName, payload)

      return ()
    })




let pushNewTraceID
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (tlids : tlid list)
  : unit =
  push canvasID "new_trace" (Json.Vanilla.serialize (traceID, tlids))


let pushNew404 (canvasID : CanvasID) (f404 : TraceInputs.F404) =
  push canvasID "new_404" (Json.Vanilla.serialize f404)


let pushNewStaticDeploy (canvasID : CanvasID) (asset : StaticAssets.StaticDeploy) =
  push canvasID "new_static_deploy" (Json.Vanilla.serialize asset)


// For exposure as a DarkInternal function
let pushAddOpEvent (canvasID : CanvasID) (event : Op.AddOpEvent) =
  push canvasID "add_op" (Json.OCamlCompatible.serialize event)

let pushWorkerStates
  (canvasID : CanvasID)
  (ws : QueueSchedulingRules.WorkerStates.T)
  : unit =
  push canvasID "worker_state" (Json.Vanilla.serialize ws)

type JsConfig = { enabled : bool; key : string; cluster : string }

let jsConfigString =
  // CLEANUP use JSON serialization
  $"{{enabled: true, key: '{Config.pusherKey}', cluster: '{Config.pusherCluster}'}}"
