/// Standard libraries related to Time
module BuiltinCli.Libs.Time

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
      deprecated = NotDeprecated }

    { name = fn "timeNowMs" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description =
        "Returns a monotonic timestamp in milliseconds. Useful for measuring "
        + "elapsed time between two calls (subtract start from end). The absolute "
        + "value has no defined epoch — only differences are meaningful."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let ts = System.Diagnostics.Stopwatch.GetTimestamp()
          let ms = ts * 1000L / System.Diagnostics.Stopwatch.Frequency
          DInt64 ms |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
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
          DUnit |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "interpreterStatsGet" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Returns interpreter performance counters as a JSON string. "
        + "Includes instruction count, builtin/package call counts, frame pushes. "
        + "When detailed timing is enabled, also includes per-builtin and per-package-fn "
        + "cumulative microseconds and call counts."
      fn =
        (function
        | _, vm, _, [ DUnit ] ->
          let s = vm.stats
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
              sb.Append($"\"{kv.Key}\":{{\"us\":{kv.Value},\"n\":{count}}}")
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
              sb.Append($"\"{kv.Key}\":{{\"us\":{kv.Value},\"n\":{count}}}")
              |> ignore<System.Text.StringBuilder>
              first <- false
            sb.Append("}") |> ignore<System.Text.StringBuilder>

          sb.Append("}") |> ignore<System.Text.StringBuilder>
          DString(sb.ToString()) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "interpreterStatsEnableDetailedTiming" 0
      typeParams = []
      parameters = [ Param.make "enabled" TBool "" ]
      returnType = TUnit
      description =
        "Enables or disables per-builtin and per-package-fn timing collection. "
        + "When enabled, adds ~1 Stopwatch call per builtin invocation."
      fn =
        (function
        | _, vm, _, [ DBool enabled ] ->
          vm.stats.detailedTiming <- enabled
          DUnit |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
