module LibBackend.Init

// LibBackend holds the whole framework

open Prelude
open Tablecloth

let init (serviceName : string) : unit =
  print $"Initing LibBackend in {serviceName}"

  LibService.Telemetry.init serviceName
  LibService.Rollbar.init serviceName

  Json.OCamlCompatible.registerConverter (
    EventQueue.WorkerStates.JsonConverter.WorkerStateConverter()
  )
  print $" Inited LibBackend in {serviceName}"
