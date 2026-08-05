/// Standard libraries related to Time
module Builtins.Time.Libs.Time

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

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
        | _, _, _, [ DFloat delay ] ->
          uply {
            let delay = System.TimeSpan.FromMilliseconds delay
            do! Task.Delay(delay)
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.clock
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
        | _, _, _, [ DUnit ] ->
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
      capabilities = LibExecution.Capabilities.Needs.clock
      deprecated = NotDeprecated }

    { name = fn "interpreterStatsReset" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUnit
      description = "Resets interpreter performance counters to zero."
      fn =
        (function
        | _, vm, _, [ DUnit ] ->
          vm.stats.reset ()
          vm.stats.enabled <- true
          DUnit |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "interpreterStatsEnableDetailedTiming" 0
      typeParams = []
      parameters = [ Param.make "enabled" TBool "" ]
      returnType = TUnit
      description =
        "Turns per-builtin and per-package-fn cumulative timing on or off for this VM. "
        + "Off by default even when counting is on, because it costs a Stopwatch read per "
        + "call: that is ~20ns on a TSC host but ~1.27us on an HPET one, where it would "
        + "dominate the calls being measured. Turn it on for a deliberate profiling run, "
        + "never for a run whose wall time you intend to quote."
      fn =
        (function
        | _, vm, _, [ DBool enabled ] ->
          // Counting has to be on for timing to record anything; asking for timing implies it.
          if enabled then vm.stats.enabled <- true
          vm.stats.detailedTiming <- enabled
          DUnit |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "interpreterStatsGet" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Returns interpreter performance counters as a JSON string. "
        + "Includes instruction count, builtin/package call counts, frame pushes. "
        + "When detailed timing is enabled, also includes per-builtin and per-package-fn "
        + "cumulative nanoseconds and call counts."
      fn =
        (function
        | _, vm, _, [ DUnit ] ->
          let s = vm.stats
          // The accumulators hold raw Stopwatch ticks, so that the hot path has no division in it.
          // Convert once, here. Reporting ticks as microseconds would be wrong by `Frequency / 1e6`,
          // which varies by clocksource, so it's worse than being uniformly wrong.
          //
          // Nanoseconds rather than microseconds: individual calls cost tens of nanoseconds, so a
          // microsecond report rounds nearly all of them to zero.
          let nsPerTick =
            1_000_000_000.0 / float System.Diagnostics.Stopwatch.Frequency
          let toNs (ticks : int64) : int64 = int64 (float ticks * nsPerTick)
          let sb = System.Text.StringBuilder()
          sb.Append("{") |> ignore<System.Text.StringBuilder>
          sb.Append($"\"instructions\":{s.instructionCount}")
          |> ignore<System.Text.StringBuilder>
          sb.Append($",\"builtinCalls\":{s.builtinCallCount}")
          |> ignore<System.Text.StringBuilder>
          sb.Append($",\"packageCalls\":{s.packageCallCount}")
          |> ignore<System.Text.StringBuilder>
          sb.Append($",\"framePushes\":{s.framePushCount}")
          |> ignore<System.Text.StringBuilder>
          let dtStr = if s.detailedTiming then "true" else "false"
          sb.Append($",\"detailedTiming\":{dtStr}")
          |> ignore<System.Text.StringBuilder>

          if s.detailedTiming && s.builtinTiming.Count > 0 then
            sb.Append(",\"builtinTiming\":{") |> ignore<System.Text.StringBuilder>
            let mutable first = true
            for kv in s.builtinTiming do
              if not first then sb.Append(",") |> ignore<System.Text.StringBuilder>
              let count =
                match s.builtinCounts.TryGetValue(kv.Key) with
                | true, c -> c
                | _ -> 0L
              sb.Append($"\"{kv.Key}\":{{\"ns\":{toNs kv.Value},\"n\":{count}}}")
              |> ignore<System.Text.StringBuilder>
              first <- false
            sb.Append("}") |> ignore<System.Text.StringBuilder>

          if s.detailedTiming && s.packageFnTiming.Count > 0 then
            sb.Append(",\"packageFnTiming\":{") |> ignore<System.Text.StringBuilder>
            let mutable first = true
            for kv in s.packageFnTiming do
              if not first then sb.Append(",") |> ignore<System.Text.StringBuilder>
              let count =
                match s.packageFnCounts.TryGetValue(kv.Key) with
                | true, c -> c
                | _ -> 0L
              sb.Append($"\"{kv.Key}\":{{\"ns\":{toNs kv.Value},\"n\":{count}}}")
              |> ignore<System.Text.StringBuilder>
              first <- false
            sb.Append("}") |> ignore<System.Text.StringBuilder>

          sb.Append("}") |> ignore<System.Text.StringBuilder>
          DString(sb.ToString()) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
