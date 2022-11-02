module ClientTypes.Worker

open Prelude

type WorkerState = string

type WorkerStates = Map<string, WorkerState>
