module LibBackend.Init

open System

open Prelude
open Tablecloth

let init (serviceName : string) : unit =
  print "Initializing LibBackend"


  HttpClient.init ()
  LibService.Telemetry.init serviceName
  LibService.Rollbar.init serviceName

  Json.OCamlCompatible.registerConverter (
    EventQueue.WorkerStates.JsonConverter.WorkerStateConverter()
  )

  ()
