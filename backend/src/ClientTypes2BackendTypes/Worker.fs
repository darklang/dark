module ClientTypes2BackendTypes.Worker

open Prelude

module WorkerStates = LibBackend.QueueSchedulingRules.WorkerStates

module WorkerState =
  let toCT (ws : WorkerStates.State) : ClientTypes.Worker.WorkerState =
    match ws with
    | WorkerStates.Running -> "run"
    | WorkerStates.Blocked -> "block"
    | WorkerStates.Paused -> "pause"

module WorkerStates =
  let toCT (ws : WorkerStates.T) : ClientTypes.Worker.WorkerStates =
    ws |> Map.map (fun _k v -> WorkerState.toCT v)
