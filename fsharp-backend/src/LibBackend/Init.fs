module LibBackend.Init

// LibBackend holds the whole framework

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

let init (serviceName : string) (runSideEffects : bool) : Task<unit> =
  task {

    print $"Initing LibBackend in {serviceName}"

    Json.OCamlCompatible.registerConverter (
      EventQueue.WorkerStates.JsonConverter.WorkerStateConverter()
    )

    if runSideEffects then do! Account.init serviceName

    do! Canvas.checkTierOneHosts ()

    print $" Inited LibBackend in {serviceName}"
  }
