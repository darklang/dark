/// Standard libraries related to Time
module Builtins.Time.Libs.Time

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let fns () : List<BuiltInFn> =
  [ { name = fn "timeSleep" 0
      typeParams = []
      parameters = [ Param.make "delayInMs" TFloat "The delay in milliseconds" ]
      returnType = TUnit
      description = "Sleeps for the given <param delayInMs> milliseconds."
      fn =
        (function
        | _, _, _, [| DFloat delay |] ->
          uply {
            let delay = System.TimeSpan.FromMilliseconds delay
            do! Task.Delay(delay)
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = set [ Effect.Clock ]
      deprecated = NotDeprecated }

    { name = fn "timeNowMs" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt
      description =
        "Returns a monotonic timestamp in milliseconds. Useful for measuring "
        + "elapsed time between two calls (subtract start from end). The absolute "
        + "value has no defined epoch — only differences are meaningful."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let ts = System.Diagnostics.Stopwatch.GetTimestamp()
          // Divide the ticks, don't scale them: `ts * 1000L` overflows int64 once the tick count passes
          // ~9.2e15, which on a 1 GHz-tick clocksource is ~106 days of uptime, after which the wrap makes
          // differences meaningless. Same bug class as the one that produced the negative durations in
          // Prelude/Telemetry.fs. Differences stay accurate to 1 ms either way, which is all this promises.
          let ticksPerMs = System.Diagnostics.Stopwatch.Frequency / 1000L
          let ms =
            if ticksPerMs > 0L then
              ts / ticksPerMs
            else
              ts * 1000L / System.Diagnostics.Stopwatch.Frequency
          LibExecution.Dval.int (bigint ms) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = set [ Effect.Clock ]
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
