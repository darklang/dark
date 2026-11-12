/// Browser/event-loop clock inputs: raw runtime units, not claimed syscalls.
module Builtins.Time.Libs.RuntimeClock

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts
module Builtin = LibExecution.Builtin
module Dval = LibExecution.Dval

let private make name parameters returnType body : BuiltInFn =
  { name = fn name 0
    typeParams = []
    parameters = parameters
    returnType = returnType
    description = "Raw clock/event-loop runtime input; no Dark clock algorithms"
    fn = body
    previewable = Impure
    callEffects = set [ Effect.Clock ]
    deprecated = NotDeprecated }

let fns () =
  [ make "runtimeRealtimeTicks" [ Param.make "unit" TUnit "" ] TInt64 (function
      | _, _, _, [| DUnit |] ->
        NodaTime.SystemClock.Instance.GetCurrentInstant().ToUnixTimeTicks()
        |> Dval.dint64
        |> Ply
      | _ -> incorrectArgs ())
    make
      "runtimeMonotonicCounter"
      [ Param.make "unit" TUnit "" ]
      (TTuple(TInt64, TInt64, []))
      (function
       | _, _, _, [| DUnit |] ->
         Ply(
           DTuple(
             Dval.dint64 (int64 (System.Diagnostics.Stopwatch.GetTimestamp())),
             Dval.dint64 (int64 System.Diagnostics.Stopwatch.Frequency),
             []
           )
         )
       | _ -> incorrectArgs ())
    // `Task.Delay` throws on a negative delay, or one past about 49 days.
    { (make
        "runtimeDelayTicks"
        [ Param.make "ticks" TInt64 "100ns runtime timer ticks" ]
        TUnit
        (function
         | _, _, _, [| DInt64 ticks |] ->
           uply {
             let delay = System.TimeSpan.FromTicks ticks
             do! System.Threading.Tasks.Task.Delay delay
             return DUnit
           }
         | _ -> incorrectArgs ())) with
        callEffects = set [ Effect.Clock; Effect.Raise ] } ]

let builtins () = Builtin.make [] (fns ())
