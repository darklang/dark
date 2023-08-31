module LibService.Logging

// Configure logging (which we basically don't use, as we use Telemetry instead)

open Microsoft.Extensions.Logging

open Prelude

let noLogger (builder : ILoggingBuilder) : unit =
  builder.ClearProviders() |> ignore<ILoggingBuilder>

let consoleLogger (builder : ILoggingBuilder) : unit =
  builder.AddConsole() |> ignore<ILoggingBuilder>

let defaultLogger (builder : ILoggingBuilder) : unit =
  match Config.defaultLogger with
  | Config.NoLogger -> noLogger builder
  | Config.ConsoleLogger -> consoleLogger builder
