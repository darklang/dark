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
///
/// Do not send requests over 10240 bytes. Each caller should check their payload,
/// and send a different push if appropriate (eg, instead of sending
/// `TraceData hugePayload`, send `TraceDataTooBig traceID`
/// </remarks>
let push
  (canvasID : CanvasID)
  (eventName : string)
  (payload : string) // The raw string is sent, it's the job of the caller to have it as appropriate json
  : unit =
  FireAndForget.fireAndForgetTask $"pusher: {eventName}" (fun () ->
    task {
      // TODO: make channels private and end-to-end encrypted in order to add public canvases
      let client = Lazy.force pusherClient
      let channel = $"canvas_{canvasID}"

      let! (_ : PusherServer.ITriggerResult) =
        client.TriggerAsync(channel, eventName, payload)

      return ()
    })


type Event =
  | NewTrace of trace : AT.TraceID * tlids : List<tlid>
  | NewStaticDeploy of asset : StaticAssets.StaticDeploy
  | New404 of TraceInputs.F404
  | AddOpV1 of Op.AddOpParamsV1 * Op.AddOpResultV1
  | AddOpPayloadTooBig of List<tlid>

type EventNameAndPayload = { EventName : string; Payload : string }

type PusherEventSerializer = Event -> EventNameAndPayload


/// <summary>Send an event to Pusher.com.</summary>
///
/// <remarks>
/// This is fired in the background, and does not take any time from the current thread.
/// You cannot wait for it, by design.
///
/// Payloads over 10240 bytes will not be sent. If there's a risk of a payload being
/// over this size, include a 'fallback' event to process in such a case. This will
/// probably result in some data being manually fetched/refreshed, rather than
/// "pushing" the data via Pusher.com.
/// </remarks>
let pushNew
  (eventSerializer : PusherEventSerializer)
  (canvasID : CanvasID)
  (event : Event)
  (fallback : Option<Event>)
  : unit =
  let handleEvent (event : EventNameAndPayload) : unit =
    FireAndForget.fireAndForgetTask $"pusher: {event.EventName}" (fun () ->
      task {
        // TODO: make channels private and end-to-end encrypted in order to add public canvases
        let client = Lazy.force pusherClient
        let channel = $"canvas_{canvasID}"

        let! (_ : PusherServer.ITriggerResult) =
          client.TriggerAsync(channel, event.EventName, event.Payload)

        return ()
      })

  let serialized = eventSerializer event

  if String.length serialized.Payload > 10240 then
    // TODO: this sort of functionality was outlined before, but never actually
    // used. We need to test this and update the client to handle the payloads.
    // (note: make sure you remove the payload from the 'ignores' list of TestJsonEncoding.res)

    // match fallback with
    // | None -> printfn "Uh oh!"
    // | Some fallback -> eventSerializer fallback |> handleEvent
    ()
  else
    serialized |> handleEvent

let pushWorkerStates
  (canvasID : CanvasID)
  (ws : QueueSchedulingRules.WorkerStates.T)
  : unit =
  push canvasID "worker_state" (Json.Vanilla.serialize ws)

type JsConfig = { enabled : bool; key : string; cluster : string }

let jsConfigString =
  // CLEANUP use JSON serialization
  $"{{enabled: true, key: '{Config.pusherKey}', cluster: '{Config.pusherCluster}'}}"

let init () =
  do Json.Vanilla.allow<QueueSchedulingRules.WorkerStates.T> "LibBackend.Pusher"

  // although we're init-ing this here, it's currently (not for long!) used in a
  // type serialized for an API endpoint (initial load) so we can't remove this
  // quite yet.
  do Json.Vanilla.allow<StaticAssets.StaticDeploy> "LibBackend.Pusher"
