module ClientTypes2BackendTypes.Pusher

open Prelude

type DomainEvent = LibBackend.Pusher.Event

let eventSerializer : LibBackend.Pusher.PusherEventSerializer =
  fun event ->
    match event with
    | DomainEvent.NewTrace (traceID, tlids) ->
      let payload : ClientTypes.Pusher.Payload.NewTrace = (traceID, tlids)
      { EventName = "new_trace"; Payload = Json.Vanilla.serialize payload }

    | DomainEvent.NewStaticDeploy (asset) ->
      let payload : ClientTypes.Pusher.Payload.NewStaticDeploy =
        { deployHash = asset.deployHash
          url = asset.url
          lastUpdate = asset.lastUpdate
          status = asset.status |> StaticDeploy.DeployStatus.toCT }
      { EventName = "new_static_deploy"; Payload = Json.Vanilla.serialize payload }

    | DomainEvent.New404 (module_, eventName, eventModifier, timestamp, traceID) ->
      let payload : ClientTypes.Pusher.Payload.New404 =
        (module_, eventName, eventModifier, timestamp, traceID)
      { EventName = "new_404"; Payload = Json.Vanilla.serialize payload }
