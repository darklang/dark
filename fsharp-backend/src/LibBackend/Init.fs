module LibBackend.Init

open System

open Prelude
open Tablecloth

let init () : unit =
  printfn "Initializing LibBackend"

  LibService.Rollbar.init ()

  Json.OCamlCompatible.registerConverter (
    EventQueue.WorkerStates.JsonConverter.WorkerStateConverter()
  )

  ()
