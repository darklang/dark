module LibBackend.Init

// LibBackend holds the whole framework

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

let init (serviceName : string) : Task<unit> =
  task {

    print $"Initing LibBackend in {serviceName}"

    Json.OCamlCompatible.registerConverter (
      EventQueue.WorkerStates.JsonConverter.WorkerStateConverter()
    )

    do! Account.init serviceName

    print $" Inited LibBackend in {serviceName}"
  }
