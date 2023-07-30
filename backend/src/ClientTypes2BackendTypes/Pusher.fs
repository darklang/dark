module ClientTypes2BackendTypes.Pusher

open Prelude

type DomainEvent = LibCloud.Pusher.Event

let eventName event =
  match event with
  | DomainEvent.NewTrace _ -> "new_trace"
  // | DomainEvent.New404 _ -> "new_404"
  // | DomainEvent.AddOpV1 _ -> "v1/add_op"
  // | DomainEvent.AddOpPayloadTooBig _ -> "addOpTooBig" // This is so-far unused
  // | DomainEvent.UpdateWorkerStates _ -> "worker_state"
  | DomainEvent.CustomEvent(eventName, _) -> eventName

let eventPayload event =
  match event with
  | DomainEvent.NewTrace(traceID, tlids) ->
    let payload : ClientTypes.Pusher.Payload.NewTrace =
      (LibExecution.AnalysisTypes.TraceID.toUUID traceID, tlids)
    Json.Vanilla.serialize payload

  // | DomainEvent.New404(module_, eventName, eventModifier, timestamp, traceID) ->
  //   let payload : ClientTypes.Pusher.Payload.New404 =
  //     (module_,
  //      eventName,
  //      eventModifier,
  //      timestamp,
  //      LibExecution.AnalysisTypes.TraceID.toUUID traceID)
  //   Json.Vanilla.serialize payload

  // | DomainEvent.AddOpV1 (params_, result) ->
  //   let payload : ClientTypes.Pusher.Payload.AddOpV1 =
  //     { ``params`` = params_ |> Ops.AddOpParamsV1.toCT
  //       result = result |> Ops.AddOpResultV1.toCT }
  //   Json.Vanilla.serialize payload

  // This is so-far unused.
  // | DomainEvent.AddOpPayloadTooBig (tlids) ->
  //   let payload : ClientTypes.Pusher.Payload.AddOpV1PayloadTooBig =
  //     { tlids = tlids }
  //   Json.Vanilla.serialize payload

  // | DomainEvent.UpdateWorkerStates (ws) ->
  //   let payload : ClientTypes.Pusher.Payload.UpdateWorkerStates =
  //     Worker.WorkerStates.toCT ws
  //   Json.Vanilla.serialize payload

  | DomainEvent.CustomEvent(_, payload) -> payload


let eventSerializer : LibCloud.Pusher.PusherEventSerializer =
  { EventName = eventName
    Serialize =
      fun event -> { EventName = eventName event; Payload = eventPayload event } }
