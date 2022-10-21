module ClientTypes.Worker

open Prelude

type WorkerState =
  | Running
  | Blocked
  | Paused

  override this.ToString() : string =
    match this with
    | Running -> "run"
    | Blocked -> "block"
    | Paused -> "pause"

let parse (str : string) : WorkerState =
  match str with
  | "run" -> Running
  | "block" -> Blocked
  | "pause" -> Paused
  | _ -> Exception.raiseInternal "invalid WorkerState" [ "workerState", str ]

type T = Map<string, WorkerState>
