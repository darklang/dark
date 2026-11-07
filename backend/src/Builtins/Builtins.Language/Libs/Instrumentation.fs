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

/// Which build produced the numbers below. Reported with them because Debug and Release are up to
/// 3x apart and AOT and R2R about 5x on startup, so the same workload has three right answers.
/// `IsDynamicCodeSupported` is false under NativeAOT and true wherever a JIT exists.
let private buildMode : string =
#if DEBUG
  "debug"
#else
  if System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported then
    "release-r2r"
  else
    "release-aot"
#endif


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
      previewable = Impure
      callEffects = Set.empty
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
      previewable = Impure
      callEffects = Set.empty
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
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "interpreterStatsGet" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Returns interpreter performance counters as a JSON string. "
        + "Includes instruction count, builtin/package call counts, frame pushes, bytes and "
        + "counts per opcode, and bytes and counts per named region of the Apply path (read the "
        + "fine-grained stages, not the totals -- see the note in the implementation). "
        + "When detailed timing is enabled, also includes per-builtin and per-package-fn "
        + "cumulative nanoseconds and call counts. Also reports `buildMode`, so a number can "
        + "say which build produced it, and `statSets`, which says whether these counters are "
        + "the whole process or one VM's share."
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
          let jsonObject (entries : List<string>) : string =
            "{" + String.concat "," entries + "}"
          let dtStr = if s.detailedTiming then "true" else "false"

          // Which opcode produced the garbage, which a flat allocation profile cannot answer.
          // Non-zero entries only.
          let byOpcode =
            [ for i in 0 .. s.countByOpcode.Length - 1 do
                if s.countByOpcode[i] > 0L then
                  let name =
                    if i < LibExecution.RuntimeTypes.Opcode.names.Length then
                      LibExecution.RuntimeTypes.Opcode.names[i]
                    else
                      string i
                  let syncPart =
                    if s.syncHitByOpcode[i] > 0L || s.syncMissByOpcode[i] > 0L then
                      $",\"syncHit\":{s.syncHitByOpcode[i]},\"syncMiss\":{s.syncMissByOpcode[i]}"
                    else
                      ""
                  $"\"{name}\":{{\"bytes\":{s.allocByOpcode[i]},\"n\":{s.countByOpcode[i]}{syncPart}}}" ]

          // Per-stage bytes and counts for the Apply path. Read the fine-grained stages only: the
          // three totals (`apply.total`, `bi.total`, `lambda.total`) bracket across the builtin's
          // `Ply`, so they measure nested execution and can exceed what the process allocated.
          // `stats.builtinAlloc` has the same defect, so it is collected but not reported.
          let byStage =
            [ for i in 0 .. s.countByStage.Length - 1 do
                if s.countByStage[i] > 0L then
                  let name =
                    if i < LibExecution.RuntimeTypes.ApplyStage.names.Length then
                      LibExecution.RuntimeTypes.ApplyStage.names[i]
                    else
                      string i
                  $"\"{name}\":{{\"bytes\":{s.allocByStage[i]},\"n\":{s.countByStage[i]}}}" ]

          let timings
            (timing : System.Collections.Generic.Dictionary<string, int64>)
            (counts : System.Collections.Generic.Dictionary<string, int64>)
            : List<string> =
            if s.detailedTiming then
              [ for kv in timing do
                  let count =
                    match counts.TryGetValue(kv.Key) with
                    | true, c -> c
                    | _ -> 0L
                  $"\"{kv.Key}\":{{\"ns\":{toNs kv.Value},\"n\":{count}}}" ]
            else
              []

          let section (name : string) (entries : List<string>) : List<string> =
            if List.isEmpty entries then
              []
            else
              [ $"\"{name}\":{jsonObject entries}" ]

          [ $"\"instructions\":{s.instructionCount}"
            $"\"builtinCalls\":{s.builtinCallCount}"
            $"\"packageCalls\":{s.packageCallCount}"
            $"\"framePushes\":{s.framePushCount}"
            $"\"traitDispatches\":{s.traitDispatchCount}"
            $"\"traitDispatchMisses\":{s.traitDispatchMissCount}"
            $"\"detailedTiming\":{dtStr}"
            $"\"buildMode\":\"{buildMode}\""
            // What the numbers above cover. Telemetry off: every VM shares one set, so 0 means
            // the whole process. Telemetry on: one set per VM, so a positive count means this is
            // the calling VM's share and `Cli.fs`'s exit dump is what totals them.
            $"\"statSets\":{InterpreterStatsSink.all.Count}" ]
          @ section "byOpcode" byOpcode
          @ section "byStage" byStage
          @ section "builtinTiming" (timings s.builtinTiming s.builtinCounts)
          @ section "packageFnTiming" (timings s.packageFnTiming s.packageFnCounts)
          |> jsonObject
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
