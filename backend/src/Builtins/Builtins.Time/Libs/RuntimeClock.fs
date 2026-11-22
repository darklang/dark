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

// Task.Delay throws on a negative delay or one past ~49 days, so long ones wait a day at a time
let rec private realDelay (ticks : int64) : System.Threading.Tasks.Task<unit> =
  task {
    if ticks > 0L then
      let step = min ticks System.TimeSpan.TicksPerDay
      do! System.Threading.Tasks.Task.Delay(System.TimeSpan.FromTicks step)
      return! realDelay (ticks - step)
  }

let fns () =
  [ make "runtimeRealtimeTicks" [ Param.make "unit" TUnit "" ] TInt64 (function
      | state, _, _, [| DUnit |] ->
        let ticks =
          match state.clock with
          | LibExecution.HostClock.Real ->
            NodaTime.SystemClock.Instance.GetCurrentInstant().ToUnixTimeTicks()
          | LibExecution.HostClock.Virtual v -> v.Now
        Dval.dint64 ticks
      | _ -> incorrectArgs ())
    make
      "runtimeMonotonicCounter"
      [ Param.make "unit" TUnit "" ]
      (TTuple(TInt64, TInt64, []))
      (function
       | state, _, _, [| DUnit |] ->
         let struct (ticks, frequency) =
           match state.clock with
           | LibExecution.HostClock.Real ->
             struct (System.Diagnostics.Stopwatch.GetTimestamp(),
                     System.Diagnostics.Stopwatch.Frequency)
           | LibExecution.HostClock.Virtual v -> struct (v.Monotonic, 10_000_000L)
         DTuple(Dval.dint64 ticks, Dval.dint64 frequency, [])
       | _ -> incorrectArgs ())
    make
      "runtimeDelayTicks"
      [ Param.make "ticks" TInt64 "100ns runtime timer ticks" ]
      TUnit
      (function
       | state, _, _, [| DInt64 ticks |] ->
         task {
           match state.clock with
           | LibExecution.HostClock.Real -> do! realDelay ticks
           | LibExecution.HostClock.Virtual v -> do! v.Delay ticks
           return DUnit
         }
         |> LibExecution.Pending.ofTask
       | _ -> incorrectArgs ()) ]

let builtins () = Builtin.make [] (fns ())
