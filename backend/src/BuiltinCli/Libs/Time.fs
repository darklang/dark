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
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
