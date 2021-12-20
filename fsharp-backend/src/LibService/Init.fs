module LibService.Init

// Initialize LibService

open Prelude

let init (serviceName : string) : unit =
  print $"Initing LibService in {serviceName}"

  Telemetry.init serviceName
  Rollbar.init serviceName

  print $" Inited LibService in {serviceName}"
