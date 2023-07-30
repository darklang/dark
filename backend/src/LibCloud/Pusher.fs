/// Module supporting Pusher.com usage
module LibCloud.Pusher

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

type Event =
  | NewTrace of trace : AT.TraceID.T * tlids : List<tlid>
  // | New404 of TraceInputs.F404
  // | AddOpV1 of Op.AddOpParamsV1 * Op.AddOpResultV1
  // | AddOpPayloadTooBig of List<tlid> // this is so-far unused.
  // | UpdateWorkerStates of QueueSchedulingRules.WorkerStates.T
  | CustomEvent of eventName : string * payload : string

type EventNameAndPayload = { EventName : string; Payload : string }

// We sometimes need _only_ the event name, and serialization of the payload
// may be expensive, so include a function that _just_ gets the event name.
type PusherEventSerializer =
  { EventName : Event -> string; Serialize : Event -> EventNameAndPayload }


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
let push
  (eventSerializer : PusherEventSerializer)
  (canvasID : CanvasID)
  (event : Event)
  (_fallback : Option<Event>)
  : unit =
  FireAndForget.fireAndForgetTask
    $"pusher: {eventSerializer.EventName event}"
    (fun () ->
      let handleEvent (event : EventNameAndPayload) =
        task {
          // TODO: make channels private and end-to-end encrypted in order to add public canvases
          let client = Lazy.force pusherClient
          let channel = $"canvas_{canvasID}"

          let! (_ : PusherServer.ITriggerResult) =
            client.TriggerAsync(channel, event.EventName, event.Payload)

          return ()
        }

      let serialized = eventSerializer.Serialize event

      if String.length serialized.Payload > 10240 then
        // TODO: this sort of functionality was outlined before, but never actually
        // used. We need to test this and update the client to handle the payloads.

        // match fallback with
        // | None -> printfn "Uh oh!"
        // | Some fallback -> eventSerializer fallback |> handleEvent
        Task.FromResult()
      else
        handleEvent serialized)

type JsConfig = { enabled : bool; key : string; cluster : string }

let jsConfigString =
  // CLEANUP use JSON serialization
  $"{{enabled: true, key: '{Config.pusherKey}', cluster: '{Config.pusherCluster}'}}"
