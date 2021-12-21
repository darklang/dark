module LibService.Init

// Initialize LibService

open Prelude

let init (serviceName : string) : unit =
  print $"Initing LibService in {serviceName}"

  Rollbar.init serviceName
  Telemetry.init serviceName

  print $" Inited LibService in {serviceName}"
