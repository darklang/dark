/// The scheduler: processes over the VM, the budget, the event queue and its sources.
module Tests.Scheduler

open System.Threading
open System.Threading.Tasks

open Expecto
open Prelude
open Fumble
open LibDB.Sqlite
open TestUtils.TestUtils

open TestUtils.PTShortcuts

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
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
///
/// A dedicated thread, not the pool: the loop blocks on its queue while nothing is runnable,
/// and a test that fails before releasing what it waits for would otherwise hold a pool
/// thread for the rest of the run.
let private runOnThread
  (s : Scheduler.Scheduler)
  (until : Scheduler.Process)
  : Task<RT.ExecutionResult> =
  let done' =
    TaskCompletionSource<RT.ExecutionResult>(
      TaskCreationOptions.RunContinuationsAsynchronously
    )
  let thread =
    Thread(
      (fun () ->
        try
          done'.SetResult(s.RunUntil until)
        with ex ->
          done'.SetException ex),
      IsBackground = true
    )
  thread.Start()
  done'.Task

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
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (a : Scheduler.Process) =
      spawn
        s
        state
        """let _ = Builtin.testTrace "a1"
let _ = Builtin.testGateWait 1L
Builtin.testTrace "a2"
"""
    let! (b : Scheduler.Process) =
      spawn
        s
        state
        """let _ = Builtin.testTrace "b1"
let _ = Builtin.testGateWait 2L
Builtin.testTrace "b2"
"""
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
    // One gate at a time: two releases back to back post their `Completed`s in whichever order
    // the pool runs them, and the loop stops when `a` finishes. The order that is fixed is the
    // order the queue sees, so the test feeds it one event at a time.
    Gates.release 2L
    let! bResult = s.Await b
    expectOk bResult "process b" |> ignore<RT.Dval>
    Gates.release 1L
    let! result = running
    expectOk result "process a" |> ignore<RT.Dval>
    Expect.equal
      (Trace.take ())
      [ "b2"; "a2" ]
      "resumed in the order the gates opened"
  }


let private budgetYields =
  testTask "a tight loop parks on its budget and another process runs between slices" {
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
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
      spawn
        s
        state
        """Builtin.testTrace "other"
"""
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


let private aKey : RT.Dval =
  Builtins.Cli.Libs.Stdin.keyReadToDval
    (System.ConsoleKeyInfo('x', System.ConsoleKey.X, false, false, false))
    None
    1


let private hostAwaitTimerOrKey =
  testTask
    "hostAwait [Key; Timer 10] answers Timer with no key, Key when one was pushed first" {
    let! state = executionStateFor pmPT false Map.empty
    // A fake key source stands in for the terminal: it blocks until a gate opens and then
    // yields one key, which nothing does in this test, so only pushed keys arrive.
    Gates.reset ()
    HE.sources.readKey <-
      Some(fun () ->
        Gates.wait 30L |> fun t -> t.Wait()
        aKey)
    try
      let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
      let program =
        "Stdlib.Host.await [ Stdlib.Host.EventSpec.Key; Stdlib.Host.EventSpec.Timer 10L ]"
      let! (timedOut : Scheduler.Process) = spawn s state program
      let! result = runOnThread s timedOut
      match expectOk result "the timer await" with
      | RT.DEnum(_, _, _, "Timer", []) -> ()
      | other -> failtest $"expected Timer, got {other}"

      // The key is pushed before the process runs, so it is waiting when the subscription
      // is made and wins over the timer.
      let! (keyed : Scheduler.Process) = spawn s state program
      s.PushEvent(HE.HostEvent.Key aKey)
      let! result = runOnThread s keyed
      match expectOk result "the key await" with
      | RT.DEnum(_, _, _, "Key", [ k ]) -> Expect.equal k aKey "the pushed key"
      | other -> failtest $"expected Key, got {other}"
    finally
      HE.sources.readKey <- None
  }


let private readKeyDoesNotBlock =
  testTask "readKey in one process does not stop a sleep in another" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    // The fake terminal: one key, once the test says so.
    HE.sources.readKey <-
      Some(fun () ->
        Gates.wait 40L |> fun t -> t.Wait()
        aKey)
    try
      let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
      let! (reader : Scheduler.Process) =
        spawn
          s
          state
          """let k = Stdlib.Cli.Stdin.readKey ()
let _ = Builtin.testTrace "key"
k"""
      let! (sleeper : Scheduler.Process) =
        spawn
          s
          state
          """let _ = Stdlib.Cli.Posix.sleep 20.0
Builtin.testTrace "slept"
"""
      let running = runOnThread s reader
      let! sleptResult = s.Await sleeper
      expectOk sleptResult "the sleeper" |> ignore<RT.Dval>
      Expect.equal
        (Trace.take ())
        [ "slept" ]
        "the sleeper finished while readKey waited"
      match reader.status with
      | Scheduler.Parked(Scheduler.OnEvent [ HE.EventSpec.Key ]) -> ()
      | other -> failtest $"the reader should be parked on a key, was {other}"
      Gates.release 40L
      let! result = running
      Expect.equal
        (expectOk result "the reader")
        aKey
        "the key the terminal produced"
      Expect.equal (Trace.take ()) [ "key" ] "the reader ran on after its key"
    finally
      HE.sources.readKey <- None
  }


let private accessIsPerProcess =
  testTask
    "a process runs under its state's access and a denial fails that process only" {
    let! state = executionStateFor pmPT false Map.empty
    let denied =
      LibExecution.Execution.restrictRun
        LibExecution.Permissions.Policy.denyAll
        state
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let program = "Stdlib.Host.await [ Stdlib.Host.EventSpec.Timer 1L ]"
    let! (confined : Scheduler.Process) = spawn s denied program
    let! (free : Scheduler.Process) = spawn s state program
    let! freeResult = runOnThread s free
    match expectOk freeResult "the free process" with
    | RT.DEnum(_, _, _, "Timer", []) -> ()
    | other -> failtest $"expected Timer, got {other}"
    let! confinedResult = s.Await confined
    match confinedResult with
    | Error(RTE.UncaughtException(msg, _), _) ->
      Expect.stringContains msg "permission denied" "the denial names itself"
    | other -> failtest $"expected a permission denial, got {other}"
  }


let private killWakesAParkedProcess =
  testTask "kill finishes a process parked on a builtin without waiting for it" {
    Gates.reset ()
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    // Parks on a gate nobody will ever open.
    let! (stuck : Scheduler.Process) = spawn s state "Builtin.testGateWait 50L"
    let! (killer : Scheduler.Process) =
      spawn
        s
        state
        $"match Stdlib.Uuid.parse \"{stuck.id}\" with | Ok id -> Stdlib.Exec.kill id | Error _ -> false"
    // The loop runs until the stuck one is finished, which the kill is what makes happen.
    let running = runOnThread s stuck
    let! killed = s.Await killer
    Expect.equal
      (expectOk killed "the killer")
      (RT.DBool true)
      "kill found the process"
    let! result = running
    match result with
    | Error(RTE.UncaughtException("cancelled", _), _) -> ()
    | other -> failtest $"expected the stuck process to be cancelled, got {other}"
  }


/// Live's H2 as a scheduler rule: a running process keeps the hashes it resolved, an entry
/// resolved after the edit gets the new ones. The rebase relies on it. The "edit" is a
/// package manager whose location points at a different hash; the two fns are both known to
/// both managers, as two versions of one item are in the store.
let private editDoesNotReachAParkedProcess =
  testTask
    "a parked process finishes on the old hash; a fresh one on the same entry gets the new" {
    Gates.reset ()
    let location : PT.PackageLocation =
      { owner = "Tests"; modules = [ "Scheduler" ]; name = "entry" }
    let fnOf (hash : string) (body : PT.Expr) : PT.PackageFn.PackageFn =
      { hash = PT.Hash hash
        typeParams = []
        parameters =
          NEList.singleton { name = "unit"; typ = PT.TUnit; description = "" }
        returnType = PT.TString
        body = body
        description = ""
        permissionCeiling = None }
    // The old version waits on a gate before answering; the new one answers at once.
    let oldFn =
      fnOf
        "0000000000000000000000000000000000000000000000000000000000000001"
        (eLet
          (lpUnit ())
          (eApply (eBuiltinFn "testGateWait" 0) [] [ eInt64 70L ])
          (eStr [ PT.StringText "old" ]))
    let newFn =
      fnOf
        "0000000000000000000000000000000000000000000000000000000000000002"
        (eStr [ PT.StringText "new" ])
    let pmWith (current : PT.PackageFn.PackageFn) : PT.PackageManager =
      // Both versions are gettable by hash; only the name moves.
      TestValues.pm
      |> PT.PackageManager.withExtras
        []
        []
        [ oldFn, { location with name = "entry-old" }
          newFn, { location with name = "entry-new" }
          current, location ]
    let callEntry (pm : PT.PackageManager) : Task<RT.Instructions> =
      task {
        let! hash = pm.findFn location |> Ply.toTask
        match hash with
        | None -> return failtest "the entry did not resolve"
        | Some(PT.Hash hash) ->
          let expr = eApply (ePackageFn hash) [] [ eUnit () ]
          return PT2RT.Expr.toRT Map.empty 0 None expr
      }

    let! before = executionStateFor (pmWith oldFn) false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! oldInstrs = callEntry (pmWith oldFn)
    let parked = s.Spawn(before, (None, oldInstrs), Scheduler.EntryExpr, None)
    let running = runOnThread s parked

    // The edit lands while `parked` waits: the name now points at the new version.
    let! after = executionStateFor (pmWith newFn) false Map.empty
    let! newInstrs = callEntry (pmWith newFn)
    let fresh = s.Spawn(after, (None, newInstrs), Scheduler.EntryExpr, None)
    let! freshResult = s.Await fresh
    Expect.equal
      (expectOk freshResult "the fresh process")
      (RT.DString "new")
      "new code"
    match parked.status with
    | Scheduler.Parked _ -> ()
    | other -> failtest $"the first process should still be parked, was {other}"

    Gates.release 70L
    let! parkedResult = running
    Expect.equal
      (expectOk parkedResult "the parked process")
      (RT.DString "old")
      "old code"
  }


// -- Cores --

/// A root scheduler with `n` workers, for the tests below; `Scheduler.defaultWorkers` is
/// process-wide, so it is set and put back around each.
let private withWorkers
  (n : int)
  (body : Scheduler.Scheduler -> Task<unit>)
  : Task<unit> =
  task {
    let before = Scheduler.defaultWorkers
    Scheduler.defaultWorkers <- n
    let root = Scheduler.Scheduler(Scheduler.defaultQuantum)
    try
      do! body root
    finally
      Scheduler.defaultWorkers <- before
      root.Workers.Stop()
  }

/// A Dark loop that costs about as much as it says: `spin n` runs `n` iterations in this VM's
/// frames (a nested self-recursive fn, so it budget-yields rather than re-entering).
let private spinProgram (n : int64) : string =
  $"""(let spin (n: Int64) (acc: Int64) : Int64 =
        if n == 0L then acc else spin (n - 1L) (acc + n)
      spin {n}L 0L)"""

/// Spawn `count` copies of `code` with `spawn` and wait for all of them; the wall time.
let private timeAll
  (spawn : RT.Instructions -> Scheduler.Process)
  (await : Scheduler.Process -> Task<RT.ExecutionResult>)
  (instrs : RT.Instructions)
  (count : int)
  : Task<System.TimeSpan> =
  task {
    let watch = System.Diagnostics.Stopwatch.StartNew()
    let procs = List.init count (fun _ -> spawn instrs)
    for p in procs do
      let! result = await p
      expectOk result "a spinner" |> ignore<RT.Dval>
    return watch.Elapsed
  }


let private workersUseCores =
  testTask "four CPU-bound processes on four workers finish well ahead of one thread" {
    let! state = executionStateFor pmPT false Map.empty
    // About a second per spinner in Debug (measured: 300k iterations took 3.0 s).
    let! instrs = instrsFor (spinProgram 80_000L)
    do!
      withWorkers 4 (fun root ->
        task {
          // Serial: all four on one worker, one thread.
          let one : Scheduler.Scheduler = root.Workers.Members[0]
          // Warm first: the JIT tiers the interpreter up during the first few hundred
          // milliseconds, and a serial batch measured cold is slower for that reason alone.
          let! _ =
            timeAll
              (fun i -> one.Spawn(state, (None, i), Scheduler.EntryExpr, None))
              one.Await
              instrs
              2
          let! serial =
            timeAll
              (fun i -> one.Spawn(state, (None, i), Scheduler.EntryExpr, None))
              one.Await
              instrs
              4
          // Parallel: placed across the four workers.
          let! spread =
            timeAll
              (fun i -> root.SpawnOn(state, (None, i), Scheduler.EntryExpr, None))
              root.Await
              instrs
              4
          // Measured, published, warm: 0.59 to 0.65 (Debug: 0.34). Not the plan's 1/4: the
          // interpreter allocates per value and this box's allocator gives four threads about
          // 1.6x, which a plain F# allocation loop reproduces with no interpreter at all
          // (`docs/processes.md`). The bound is loose because the box is shared; anything under
          // 0.8 still takes more than one core.
          Expect.isLessThan
            spread.TotalMilliseconds
            (serial.TotalMilliseconds * 0.8)
            $"spread {spread.TotalMilliseconds:F0} ms vs serial {serial.TotalMilliseconds:F0} ms"
          // Every worker took at least one of the four.
          let placed =
            root.Snapshot()
            |> List.filter (fun p -> p.entry = Scheduler.EntryExpr)
            |> List.length
          Expect.equal placed 10 "all ten spinners are in the group's table"
        })
  }


/// `fix-types-cache-race` again, as processes: many of them on the workers, one state, all
/// resolving types, building records and enums, creating and applying lambdas at once. The
/// caches on `ExecutionState` are shared by reference, so this is the test that they can be.
let private sharedStateAcrossWorkers =
  testTask
    "sixteen processes on four workers share one state's caches without corruption" {
    let! state = executionStateFor pmPT false Map.empty
    let! instrs =
      instrsFor
        """(let step (i: Int64) : Int64 =
              let r = Stdlib.Result.Result.Ok i
              let o = Stdlib.Option.Option.Some (Stdlib.Int64.add i 1L)
              let xs = Stdlib.List.map [ 1L; 2L; 3L ] (fun x -> Stdlib.Int64.multiply x i)
              let sum = Stdlib.List.fold xs 0L (fun acc x -> Stdlib.Int64.add acc x)
              match r, o with
              | Ok a, Some b -> Stdlib.Int64.add (Stdlib.Int64.add a b) sum
              | _, _ -> 0L
            let loop (n: Int64) (acc: Int64) : Int64 =
              if n == 0L then acc else loop (n - 1L) (acc + step n)
            loop 2000L 0L)"""
    do!
      withWorkers 4 (fun root ->
        task {
          let procs =
            List.init 16 (fun _ ->
              root.SpawnOn(state, (None, instrs), Scheduler.EntryExpr, None))
          for p in procs do
            let! result = root.Await p
            // step n = n + (n + 1) + 6n = 8n + 1, summed over 1..2000
            Expect.equal
              (expectOk result "a process")
              (RT.DInt64(8L * 2001000L + 2000L))
              "every process computed the same answer"
        })
  }


let private psSeesTheWholeGroup =
  testTask "ps and kill from the root see and reach a process on a worker" {
    Gates.reset ()
    let! state = executionStateFor pmPT false Map.empty
    let! instrs = instrsFor "Builtin.testGateWait 80L"
    do!
      withWorkers 2 (fun root ->
        task {
          let stuck = root.SpawnOn(state, (None, instrs), Scheduler.EntryExpr, None)
          let deadline = System.DateTime.UtcNow.AddSeconds 5.
          while (match stuck.status with
                 | Scheduler.Parked _ -> false
                 | _ -> true)
                && System.DateTime.UtcNow < deadline do
            Thread.Sleep 5
          let seen = root.Snapshot() |> List.tryFind (fun p -> p.id = stuck.id)
          Expect.isSome seen "the root's snapshot lists the worker's process"
          Expect.isTrue (root.Kill stuck.id) "kill found it through the group"
          let! result = root.Await stuck
          match result with
          | Error(RTE.UncaughtException("cancelled", _), _) -> ()
          | other -> failtest $"expected cancelled, got {other}"
        })
  }


/// One trace, two processes on two threads: every row carries its process, and `seq` is one
/// order across both.
let private traceCarriesProcessAndSeq =
  testTask
    "a trace written by two processes on two workers keeps each one's calls apart" {
    let! state = executionStateFor pmPT false Map.empty
    let! instrs =
      instrsFor
        """(let twice (x: Int64) : Int64 = Stdlib.Int64.add x x
            let loop (n: Int64) (acc: Int64) : Int64 =
              if n == 0L then acc else loop (n - 1L) (acc + twice n)
            loop 200L 0L)"""
    LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.On
    try
      let traceId = LibExecution.AnalysisTypes.TraceID.create ()
      let tracer =
        LibDB.Tracing.createCliTracer traceId "scheduler test" "expression" RT.DUnit
      let traced : RT.ExecutionState =
        { state with RT.ExecutionState.tracing = tracer.executionTracing }
      do!
        withWorkers 2 (fun root ->
          task {
            let a = root.SpawnOn(traced, (None, instrs), Scheduler.EntryExpr, None)
            let b = root.SpawnOn(traced, (None, instrs), Scheduler.EntryExpr, None)
            let! ra = root.Await a
            let! rb = root.Await b
            expectOk ra "a" |> ignore<RT.Dval>
            expectOk rb "b" |> ignore<RT.Dval>
            do! tracer.storeTraceResults traced |> Ply.toTask
            let! rows =
              Sql.query
                "SELECT call_id, parent_call_id, process_id, seq
                 FROM trace_fn_calls WHERE trace_id = @t ORDER BY seq"
              |> Sql.parameters [ "t", Sql.string (string traceId) ]
              |> Sql.executeAsync (fun read ->
                read.string "call_id",
                read.stringOrNone "parent_call_id",
                read.string "process_id",
                read.int64 "seq")
            Expect.isGreaterThan (List.length rows) 10 "the trace has rows"
            let pids = rows |> List.map (fun (_, _, pid, _) -> pid) |> List.distinct
            Expect.equal
              (List.sort pids)
              (List.sort [ string a.id; string b.id ])
              "every row belongs to one of the two processes"
            Expect.equal
              (rows |> List.map (fun (_, _, _, seq) -> seq))
              (List.init (List.length rows) int64)
              "seq is 0..n-1 across both"
            // A call's parent is in the same process: the stacks never crossed.
            let byId =
              rows |> List.map (fun (id, _, pid, _) -> id, pid) |> Map.ofList
            for (_, parent, pid, _) in rows do
              match parent with
              | Some parentId ->
                Expect.equal
                  (Map.tryFind parentId byId)
                  (Some pid)
                  "parent in the same process"
              | None -> ()
          })
    finally
      LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
  }


// Sequenced: the tests share the process-wide trace, gates and key source in `LibTest` and
// `HostEvents`.
let tests =
  testSequenced (
    testList
      "scheduler"
      [ interleaving
        budgetYields
        hostAwaitTimerOrKey
        readKeyDoesNotBlock
        accessIsPerProcess
        killWakesAParkedProcess
        editDoesNotReachAParkedProcess
        workersUseCores
        sharedStateAcrossWorkers
        psSeesTheWholeGroup
        traceCarriesProcessAndSeq ]
  )
