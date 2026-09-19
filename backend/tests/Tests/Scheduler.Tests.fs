/// The scheduler: processes over the VM, the budget, the event queue and its sources.
module Tests.Scheduler

open System.Threading
open System.Threading.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RTE = RT.RuntimeError
module Scheduler = LibExecution.Scheduler
module HE = LibExecution.HostEvents
module Gates = TestUtils.LibTest.Gates
module Trace = TestUtils.LibTest.Trace

/// Compile a Dark expression to the instructions a process runs.
let private instrsFor (code : string) : Task<RT.Instructions> =
  task {
    let! ptExpr = parsePTExpr code
    return PT2RT.Expr.toRT Map.empty 0 None ptExpr
  }

/// A scheduler with its loop on its own thread, running until `until` is done.
let private runOnThread
  (s : Scheduler.Scheduler)
  (until : Scheduler.Process)
  : Task<RT.ExecutionResult> =
  Task.Run(fun () -> s.RunUntil until)

/// Spawn `code` as a process of `s`.
let private spawn
  (s : Scheduler.Scheduler)
  (state : RT.ExecutionState)
  (code : string)
  : Task<Scheduler.Process> =
  task {
    let! instrs = instrsFor code
    return s.Spawn(state, (None, instrs), Scheduler.EntryExpr, None)
  }

let private expectOk (result : RT.ExecutionResult) (what : string) : RT.Dval =
  match result with
  | Ok dv -> dv
  | Error(rte, _) -> failtest $"{what} failed: {rte}"


let private interleaving =
  testTask "two processes that each await interleave in the order the awaits finish" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor TestValues.pm false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (a : Scheduler.Process) =
      spawn
        s
        state
        """(let _ = Builtin.testTrace "a1"
            let _ = Builtin.testGateWait 1L
            Builtin.testTrace "a2")"""
    let! (b : Scheduler.Process) =
      spawn
        s
        state
        """(let _ = Builtin.testTrace "b1"
            let _ = Builtin.testGateWait 2L
            Builtin.testTrace "b2")"""
    // The loop stops when `a` finishes; `a` is released last, so `b` is done by then too.
    let running = runOnThread s a
    // Both are parked before either gate opens: the trace has both first halves.
    let deadline = System.DateTime.UtcNow.AddSeconds 5.
    while (match a.status, b.status with
           | Scheduler.Parked _, Scheduler.Parked _ -> false
           | _ -> true)
          && System.DateTime.UtcNow < deadline do
      Thread.Sleep 5
    Expect.equal
      (Trace.take ())
      [ "a1"; "b1" ]
      $"both parked after their first half (a: {a.status}, b: {b.status})"
    Gates.release 2L
    Gates.release 1L
    let! result = running
    expectOk result "process a" |> ignore<RT.Dval>
    let! bResult = s.Await b
    expectOk bResult "process b" |> ignore<RT.Dval>
    Expect.equal
      (Trace.take ())
      [ "b2"; "a2" ]
      "resumed in the order the gates opened"
  }


let private budgetYields =
  testTask "a tight loop parks on its budget and another process runs between slices" {
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor TestValues.pm false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    // A Dark-level loop (a self-recursive nested fn runs in this VM's frames), long enough to
    // cost many slices, then a trace at the end.
    let! (loop : Scheduler.Process) =
      spawn
        s
        state
        """(let spin (n: Int64) (acc: Int64) : Int64 =
              if n == 0L then acc else spin (n - 1L) (acc + n)
            let total = spin 200000L 0L
            let _ = Builtin.testTrace "loop-done"
            total)"""
    let! (other : Scheduler.Process) =
      spawn s state """(Builtin.testTrace "other")"""
    let! result = runOnThread s loop
    let total = expectOk result "the loop"
    Expect.equal total (RT.DInt64 20000100000L) "the loop's answer"
    Expect.isGreaterThan loop.slices 1L "the loop was preempted at least once"
    let! _ = s.Await other
    Expect.equal
      (Trace.take ())
      [ "other"; "loop-done" ]
      "the other process ran before the loop, spawned first, finished"
  }


// Sequenced: the tests share the process-wide trace and gates in `LibTest`.
let tests = testSequenced (testList "scheduler" [ interleaving; budgetYields ])
