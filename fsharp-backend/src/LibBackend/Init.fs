module LibBackend.Init

open System

open Prelude
open Tablecloth

let init () : unit =
  printfn "Initializing LibBackend"
  Json.AutoSerialize.registerConverter (EventQueue.WorkerStates.JsonConverter.WorkerStateConverter())
  OCamlInterop.Binary.init ()
  ()