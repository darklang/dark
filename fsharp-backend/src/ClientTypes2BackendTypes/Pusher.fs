module ClientTypes2BackendTypes.Pusher

open Prelude

type DomainEvent = LibBackend.Pusher.Event

let eventSerializer : LibBackend.Pusher.PusherEventSerializer =
  fun event ->
    match event with
    | DomainEvent.NewTrace (traceID, tlids) ->
      let payload : ClientTypes.Pusher.Payload.NewTrace = (traceID, tlids)
      { EventName = "new_trace"; Payload = Json.Vanilla.serialize payload }
