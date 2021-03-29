module LibBackend.Init

open System

open Prelude
open Tablecloth

let init () : unit =
  printfn "Initializing LibBackend"

  Json.OCamlCompatible.registerConverter (
    EventQueue.WorkerStates.JsonConverter.WorkerStateConverter()
  )
  ()
