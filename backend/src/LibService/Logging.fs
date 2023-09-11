module LibService.Logging

// Configure logging (which we basically don't use, as we use Telemetry instead)

open Microsoft.Extensions.Logging

open Prelude

type Logger = ILoggingBuilder -> unit

let noLogger (builder : ILoggingBuilder) : unit =
  builder.ClearProviders() |> ignore<ILoggingBuilder>

let consoleLogger (builder : ILoggingBuilder) : unit =
  builder.AddConsole() |> ignore<ILoggingBuilder>

let defaultLogger (providedLogger : Option<Logger>) : Logger =
  match providedLogger with
  | Some f -> f
  | None ->
    match Config.defaultLogger with
    | Config.NoLogger -> noLogger
    | Config.ConsoleLogger -> consoleLogger

open Microsoft.AspNetCore.Builder

let addHttpLogging (app : IApplicationBuilder) : IApplicationBuilder =
  match Config.defaultLogger with
  | Config.NoLogger -> app
  // TODO: make this nicer before enabling. Right now it clogs the logs. See
  // https://learn.microsoft.com/en-us/aspnet/core/fundamentals/http-logging/?view=aspnetcore-7.0
  | Config.ConsoleLogger -> app // app.UseHttpLogging()
