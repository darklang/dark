module LibService.Logging

// Configure logging (which we basically don't use, as we use Telemetry instead)

open Microsoft.Extensions.Logging

open Prelude

let noLogger (builder : ILoggingBuilder) : unit =
  // We use telemetry instead
  builder.ClearProviders() |> ignore<ILoggingBuilder>
