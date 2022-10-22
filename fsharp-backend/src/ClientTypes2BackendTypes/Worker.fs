module ClientTypes2BackendTypes.Worker

open Prelude

module WorkerState =
  let toCT
    (ws : LibBackend.QueueSchedulingRules.WorkerStates.State)
    : ClientTypes.Worker.WorkerState =
    match ws with
    | LibBackend.QueueSchedulingRules.WorkerStates.Running ->
      ClientTypes.Worker.Running
    | LibBackend.QueueSchedulingRules.WorkerStates.Blocked ->
      ClientTypes.Worker.Blocked
    | LibBackend.QueueSchedulingRules.WorkerStates.Paused ->
      ClientTypes.Worker.Paused

module WorkerStates =
  let toCT
    (ws : LibBackend.QueueSchedulingRules.WorkerStates.T)
    : ClientTypes.Worker.WorkerStates =
    ws |> Map.map (fun _k v -> WorkerState.toCT v)
