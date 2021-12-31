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

    // FSTODO: disabled for now as prevents startup. I _think_ this doesn't work
    // because the legacyserver isn't ready, but I'm not sure. It's unclear why we're
    // not seeing logs or exceptions associated with this - it seems to just hang.
    // do! Canvas.checkTierOneHosts ()

    print $" Inited LibBackend in {serviceName}"
  }
