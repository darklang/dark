/// Initialize LibService
module LibService.Init

open Prelude

let init (serviceName : string) : unit =
  printTime $"Initing LibService in {serviceName}"

  Rollbar.init serviceName
  Telemetry.init serviceName

  printTime $" Inited LibService in {serviceName}"


/// Called when shutting down, probably. Used to explicitly flush any buffered
/// connections.
let shutdown (serviceName : string) : unit =
  printTime $"Shutting down LibService in {serviceName}"
  LaunchDarkly.flush ()
  Telemetry.flush ()
