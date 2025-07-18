module LibService.Init

open Prelude

let init (serviceName : string) : unit =
  printTime $"Initing LibService in {serviceName}"
  Rollbar.init serviceName
  Telemetry.init serviceName
  printTime $" Inited LibService in {serviceName}"


/// Called when shutting down cloud services. Used to explicitly flush any buffered
/// connections for LaunchDarkly.
let shutdown (serviceName : string) : unit =
  printTime $"Shutting down LibService in {serviceName}"
  LaunchDarkly.flush ()
  Telemetry.flush ()
  printTime $"Shutting down LibService in {serviceName}"
