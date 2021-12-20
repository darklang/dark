module LibBackend.Init

// LibBackend holds the whole framework

open Prelude

let init (serviceName : string) : unit =
  print $"Initing LibBackend in {serviceName}"

  Json.OCamlCompatible.registerConverter (
    EventQueue.WorkerStates.JsonConverter.WorkerStateConverter()
  )
  print $" Inited LibBackend in {serviceName}"
