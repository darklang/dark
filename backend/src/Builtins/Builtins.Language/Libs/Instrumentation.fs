/// Introspection of the running runtime: interpreter counters and process allocation.
///
/// Lives here rather than in Builtins.Time because none of it is about time. It sits alongside
/// Reflection for the same reason that does -- these builtins report on the system executing the
/// code, not on the code's own values.
module Builtins.Language.Libs.Instrumentation

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

module Dval = LibExecution.Dval

let fns () : List<BuiltInFn> =
  [ { name = fn "interpreterStatsReset" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUnit
      description = "Resets interpreter performance counters to zero."
      fn =
        (function
        | _, vm, _, [| DUnit |] ->
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
        | _, vm, _, [| DBool enabled |] ->
          // Counting has to be on for timing to record anything; asking for timing implies it.
          if enabled then vm.stats.enabled <- true
          vm.stats.detailedTiming <- enabled
          DUnit |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "gcAllocatedBytes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description =
        "Total bytes this process has allocated since it started, including memory since freed. "
        + "For measuring what a piece of work costs: read it before and after, and subtract. "
        + "Unlike wall-clock time this repeats exactly, so it's the number to trust when comparing "
        + "two versions of the same code."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          // `precise: true` walks every thread's allocation context. Slower, but the imprecise
          // version rounds to allocation-context refills, which is coarse enough to hide a whole
          // request's worth of work.
          Dval.dint64 (System.GC.GetTotalAllocatedBytes true) |> Ply
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
        | _, vm, _, [| DUnit |] ->
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
