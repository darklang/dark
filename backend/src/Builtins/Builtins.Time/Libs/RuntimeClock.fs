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
  [ make "runtimeRealtimeTicks" [ Param.make "unit" TUnit "" ] TInt (function
      | _, _, _, [| DUnit |] ->
        NodaTime.SystemClock.Instance.GetCurrentInstant().ToUnixTimeTicks()
        |> bigint
        |> Dval.int
        |> Ply
      | _ -> incorrectArgs ())
    make
      "runtimeMonotonicCounter"
      [ Param.make "unit" TUnit "" ]
      (TTuple(TInt, TInt, []))
      (function
       | _, _, _, [| DUnit |] ->
         Ply(
           DTuple(
             Dval.int (bigint (System.Diagnostics.Stopwatch.GetTimestamp())),
             Dval.int (bigint System.Diagnostics.Stopwatch.Frequency),
             []
           )
         )
       | _ -> incorrectArgs ())
    make
      "runtimeDelayTicks"
      [ Param.make "ticks" TInt "100ns runtime timer ticks" ]
      TUnit
      (function
       | _, vm, _, [| DInt ticks |] ->
         uply {
           let delay = System.TimeSpan.FromTicks(intToInt64 vm ticks)
           do! System.Threading.Tasks.Task.Delay delay
           return DUnit
         }
       | _ -> incorrectArgs ()) ]

let builtins () = Builtin.make [] (fns ())
