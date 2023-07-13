/// Initialize LibService
module LibService.Init

open Prelude

let init (serviceName : string) : unit =
  print $"Initing LibService in {serviceName}"

  Rollbar.init serviceName
  Telemetry.init serviceName

  print $" Inited LibService in {serviceName}"

/// Called when shutting down, probably. Used to explicitly flush any buffered
/// connections.
let shutdown (serviceName : string) : unit =
  print $"Shutting down LibService in {serviceName}"
  LaunchDarkly.flush ()
  Telemetry.flush ()
