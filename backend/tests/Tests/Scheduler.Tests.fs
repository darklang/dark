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
module HostRegistry = LibExecution.HostRegistry
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

/// Poll until `cond`, or give up after five seconds.
let private waitFor (what : string) (cond : unit -> bool) : unit =
  let deadline = System.DateTime.UtcNow.AddSeconds 5.
  while not (cond ()) && System.DateTime.UtcNow < deadline do
    Thread.Sleep 5
  if not (cond ()) then failtest $"gave up waiting for {what}"

/// Poll the trace until it has said exactly `expected`, in that order, or give up. `Trace.take`
/// empties the trace, so entries that arrive between polls are gathered rather than compared
/// one poll at a time.
let private waitForTrace (what : string) (expected : List<string>) : unit =
  let mutable seen = []
  let deadline = System.DateTime.UtcNow.AddSeconds 5.
  while seen <> expected && System.DateTime.UtcNow < deadline do
    seen <- seen @ Trace.take ()
    if seen <> expected then Thread.Sleep 5
  Expect.equal seen expected what




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
    waitFor "both parked" (fun () ->
      match a.status, b.status with
      | Scheduler.Parked _, Scheduler.Parked _ -> true
      | _ -> false)
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
            let total = spin 20000L 0L
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
    Expect.equal total (RT.DInt64 200010000L) "the loop's answer"
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
      // The reader thread is parked on the gate; let it finish rather than leave it for the run.
      Gates.release 30L
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


/// A fake stdin for the stdin tests: each read waits on a gate, then answers a line or that many
/// `x`s, and counts itself, so a test can say how many reads the runtime actually made.
let private fakeStdin (gate : int64) (reads : int ref) =
  fun (request : HE.StdinRequest) ->
    Gates.wait gate |> fun t -> t.Wait()
    Interlocked.Increment(&reads.contents) |> ignore<int>
    match request with
    | HE.StdinRequest.Line -> Ok(Some $"line {reads.Value}")
    | HE.StdinRequest.Bytes n -> Ok(Some(System.String('x', n)))


let private readLineDoesNotBlock =
  testTask
    "readLine in one process parks it, and a sleep in another finishes meanwhile" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let reads = ref 0
    HE.sources.readStdin <- Some(fakeStdin 50L reads)
    try
      let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
      let! (reader : Scheduler.Process) =
        spawn
          s
          state
          """let l = Stdlib.Cli.Stdin.readLine ()
let _ = Builtin.testTrace "line"
l"""
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
        "the sleeper finished while readLine waited"
      match reader.status with
      | Scheduler.Parked(Scheduler.OnEvent [ HE.EventSpec.StdinLine ]) -> ()
      | other -> failtest $"the reader should be parked on a line, was {other}"
      Gates.release 50L
      let! result = running
      Expect.equal (expectOk result "the reader") (RT.DString "line 1") "the line"
    finally
      HE.sources.readStdin <- None
  }


let private stdinTimerLeavesReadOutstanding =
  testTask
    "a stdin wait a timer beat leaves its read for the next wait, which reads nothing more" {
    Gates.reset ()
    let! state = executionStateFor pmPT false Map.empty
    let reads = ref 0
    HE.sources.readStdin <- Some(fakeStdin 51L reads)
    try
      let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
      let! (timedOut : Scheduler.Process) =
        spawn
          s
          state
          "Stdlib.Host.await [ Stdlib.Host.EventSpec.StdinLine; Stdlib.Host.EventSpec.Timer 10L ]"
      let! result = runOnThread s timedOut
      match expectOk result "the timed wait" with
      | RT.DEnum(_, _, _, "Timer", []) -> ()
      | other -> failtest $"expected Timer, got {other}"

      // A body asked for now would take the line already on its way: refused, not misread.
      let! (wrongKind : Scheduler.Process) =
        spawn s state "Builtin.stdinReadExactly 3"
      let! result = runOnThread s wrongKind
      match result with
      | Error(RTE.UncaughtException(msg, _), _) when msg.Contains "outstanding" -> ()
      | other ->
        failtest $"expected a refusal naming the outstanding read, got {other}"

      let! (line : Scheduler.Process) = spawn s state "Stdlib.Cli.Stdin.readLine ()"
      let running = runOnThread s line
      Gates.release 51L
      let! result = running
      Expect.equal
        (expectOk result "the line")
        (RT.DString "line 1")
        "the outstanding read"
      Expect.equal reads.Value 1 "one read, not one per wait"

      let! (body : Scheduler.Process) = spawn s state "Builtin.stdinReadExactly 3"
      let! result = runOnThread s body
      Expect.equal (expectOk result "the body") (RT.DString "xxx") "a byte count"
      Expect.equal reads.Value 2 "the body was a read of its own"
    finally
      HE.sources.readStdin <- None
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
        $"match Stdlib.Uuid.parse \"{stuck.id}\" with | Ok id -> Darklang.Cli.Ps.killById id | Error _ -> false"
    // The loop runs until the stuck one is finished, which the kill is what makes happen.
    let running = runOnThread s stuck
    let! killed = s.Await killer
    Expect.equal
      (expectOk killed "the killer")
      (RT.DBool true)
      "kill found the process"
    let! result = running
    match result with
    | Error(RTE.UncaughtException("killed from ps", _), _) -> ()
    | other -> failtest $"expected the stuck process to be killed, got {other}"
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
        permissionCeiling = None
        bounds = [] }
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
        []
        []
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
    // What this test does and does not prove, because the halves are not equal.
    //
    // It PROVES that the edit does not break a process that is parked across it: the status
    // check below, and the completion after `Gates.release`, both fail if the name moving
    // leaves the parked process unable to finish.
    //
    // It does NOT prove that a parked process cannot pick up the new body. `callEntry`
    // resolves the name to a hash and bakes that hash into the instructions BEFORE the
    // process spawns, so "the parked one answers old" holds by construction and no change to
    // the reload path could make it fail. Testing the redirect case needs a process whose
    // callee is resolved by NAME at call time, which is a different test than this one.
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
  testTask "four CPU-bound processes are spread across four workers" {
    let! state = executionStateFor pmPT false Map.empty
    // Long enough (about half a second each in Debug) that all four are still running when
    // the last is placed, so placement has four busy workers to choose between.
    let! instrs = instrsFor (spinProgram 40_000L)
    do!
      withWorkers 4 (fun root ->
        task {
          let! _ =
            timeAll
              (fun i -> root.SpawnOn(state, (None, i), Scheduler.EntryExpr, None))
              root.Await
              instrs
              4
          // The claim is that the work is spread, and that is asserted structurally: every
          // worker ran one of the four.
          //
          // This test used to assert a speedup too, spread under 0.9 of serial. It was
          // widened once from 0.8 and still failed in full-suite runs on a busy box (641 ms
          // spread against 645 ms serial), while passing alone, including at load average
          // 88 from outside CPU load. A wall-clock ratio measures the box as much as the
          // scheduler. Measured on an idle box it was 0.59 to 0.65 published (Debug 0.34),
          // not 1/4, because the interpreter allocates per value and the allocator gives four
          // threads about 1.6x (`docs/processes.md`).
          for w in root.Workers.Members do
            Expect.isGreaterThan
              (w.SnapshotHere()
               |> List.filter (fun p -> p.entry = Scheduler.EntryExpr)
               |> List.length)
              0
              "each worker ran a spinner"
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
              let o = Stdlib.Option.Option.Some (i + 1L)
              let xs = Stdlib.List.map [ 1L; 2L; 3L ] (fun x -> x * i)
              let sum = Stdlib.List.fold xs 0L (fun acc x -> acc + x)
              match r, o with
              | Ok a, Some b -> (a + b) + sum
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
          waitFor "the worker's process parked" (fun () ->
            match stuck.status with
            | Scheduler.Parked _ -> true
            | _ -> false)
          let seen = root.Snapshot() |> List.tryFind (fun p -> p.id = stuck.id)
          Expect.isSome seen "the root's snapshot lists the worker's process"
          Expect.isTrue (root.Kill stuck.id) "kill found it through the group"
          let! result = root.Await stuck
          match result with
          | Error(RTE.UncaughtException("killed from ps", _), _) -> ()
          | other -> failtest $"expected cancelled, got {other}"
        })
  }


/// One trace, two processes on two threads: every row carries its process, and `seq` is one
/// order across both.
let private traceCarriesProcessAndSeq =
  testTask
    "a trace written by two processes on two workers keeps each one's calls apart" {
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let! instrs =
      instrsFor
        // Impure on purpose: only impure calls are recorded, so a pure loop would write a
        // trace with no rows in it and prove nothing about which process wrote what.
        """(let record (n: Int64) : Unit = Builtin.testTrace (Stdlib.toString n)
            let loop (n: Int64) : Unit =
              if n == 0L then () else (record n
                                       loop (n - 1L))
            loop 20L)"""
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
                "SELECT process_id, seq, ord FROM trace_fn_calls
                 WHERE trace_id = @t ORDER BY seq"
              |> Sql.parameters [ "t", Sql.string (string traceId) ]
              |> Sql.executeAsync (fun read ->
                read.string "process_id", read.int64 "seq", read.int64 "ord")
            Expect.isGreaterThan (List.length rows) 10 "the trace has rows"
            let pids = rows |> List.map (fun (pid, _, _) -> pid) |> List.distinct
            Expect.equal
              (List.sort pids)
              (List.sort [ string a.id; string b.id ])
              "every row belongs to one of the two processes"
            Expect.equal
              (rows |> List.map (fun (_, seq, _) -> seq))
              (List.init (List.length rows) int64)
              "seq is 0..n-1 across both"
            // `seq` interleaves the two processes; `ord` is each process's own log, so within
            // one process it must count 0, 1, 2... with no gaps and no sharing.
            for pid in pids do
              let ords =
                rows
                |> List.filter (fun (p, _, _) -> p = pid)
                |> List.map (fun (_, _, ord) -> ord)
              Expect.equal
                ords
                (List.init (List.length ords) int64)
                "each process's ordinals count from zero, on their own"
          })
    finally
      Trace.take () |> ignore<List<string>>
      LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
  }


// -- Reads are concurrent --

let private readsRunAtOnce =
  testTask "three reads under List.map are all in flight before anything waits" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) =
      spawn
        s
        state
        """let xs = Stdlib.List.map [ 1L; 2L; 3L ] (fun n -> Builtin.testRead n)
let _ = Builtin.testTrace "mapped"
let total = Stdlib.List.fold xs 0L (fun a b -> a + b)
let _ = Builtin.testTrace "forced"
total"""
    let running = runOnThread s p
    // The map returned and the next statement ran while all three reads were still waiting.
    waitFor "the map to return" (fun () -> List.length (Gates.waiting ()) = 3)
    waitFor "the process to park on the fold" (fun () ->
      match p.status with
      | Scheduler.Parked _ -> true
      | _ -> false)
    Expect.equal (Trace.take ()) [ "mapped" ] "the statement after the map ran"
    Expect.equal (Gates.waiting ()) [ 1L; 2L; 3L ] "every read is in flight"
    Gates.release 2L
    Gates.release 3L
    Gates.release 1L
    let! result = running
    Expect.equal (expectOk result "the program") (RT.DInt64 6L) "the sum"
    Expect.equal (Trace.take ()) [ "forced" ] "the fold ran once the reads landed"
  }


let private writesKeepOrder =
  testTask
    "a read in flight does not hold up the writes after it, which stay in order" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) =
      spawn
        s
        state
        """let a = Builtin.testRead 11L
let _ = Builtin.testTrace "w1"
let b = Builtin.testRead 12L
let _ = Builtin.testTrace "w2"
(a + b)"""
    let running = runOnThread s p
    waitFor "both reads in flight" (fun () -> Gates.waiting () = [ 11L; 12L ])
    waitForTrace
      "both writes ran, in order, before either read landed"
      [ "w1"; "w2" ]
    Gates.release 12L
    Gates.release 11L
    let! result = running
    Expect.equal (expectOk result "the program") (RT.DInt64 23L) "the sum"
  }


let private failedReadRaisesAtAwait =
  testTask "a read that fails raises where it is forced, naming the read" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) =
      spawn
        s
        state
        """let a = Builtin.testFailingRead 21L
let _ = Builtin.testTrace "after the call"
Stdlib.await a"""
    let running = runOnThread s p
    waitFor "the read in flight" (fun () -> Gates.waiting () = [ 21L ])
    waitForTrace "the call itself did not raise" [ "after the call" ]
    Gates.release 21L
    let! result = running
    match result with
    | Error(RTE.UncaughtException(msg, _), stack) ->
      Expect.stringContains msg "read 21 failed" "the read's own error"
      Expect.isTrue
        (stack
         |> List.exists (fun ep ->
           match ep with
           | RT.Function(RT.FQFnName.Builtin b) -> b.name = "testFailingRead"
           | _ -> false))
        $"the stack names the read below the force site: {stack}"
    | other -> failtest $"expected the read's failure, got {other}"
  }


let private unlookedReadFailsTheRunAtItsEnd =
  testTask
    "a read nobody looks at is waited for at the end of the run, and its failure fails the run" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) =
      spawn
        s
        state
        """let a = Builtin.testFailingRead 23L
let _ = Builtin.testTrace "after the call"
1L"""
    let running = runOnThread s p
    waitFor "the read in flight" (fun () -> Gates.waiting () = [ 23L ])
    waitForTrace "the program ran past the read" [ "after the call" ]
    // The program is over but the read is not: the run waits for it.
    Expect.isFalse
      running.IsCompleted
      "the run has not ended while the read is in flight"
    Gates.release 23L
    let! result = running
    match result with
    | Error(RTE.UncaughtException(msg, _), stack) ->
      Expect.stringContains msg "read 23 failed" "the read's own error"
      Expect.isTrue
        (stack
         |> List.exists (fun ep ->
           match ep with
           | RT.Function(RT.FQFnName.Builtin b) -> b.name = "testFailingRead"
           | _ -> false))
        $"the stack names the read: {stack}"
    | other ->
      failtest $"expected the read's failure at the end of the run, got {other}"
  }


let private denialRaisesAtTheCall =
  testTask
    "a read the policy denies raises at the call, before anything is in flight" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let denied =
      LibExecution.Execution.restrictRun
        LibExecution.Permissions.Policy.denyAll
        state
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) =
      spawn
        s
        denied
        """let a = Builtin.testRead 31L
let _ = Builtin.testTrace "after the call"
a"""
    let! result = runOnThread s p
    match result with
    | Error(RTE.UncaughtException(msg, _), _) ->
      Expect.stringContains msg "permission denied" "the denial names itself"
    | other -> failtest $"expected a denial, got {other}"
    Expect.equal (Trace.take ()) [] "nothing after the call ran"
    Expect.equal (Gates.waiting ()) [] "no read started"
  }


let private inflightBoundHolds =
  testTask "past the in-flight bound a read is awaited in program order" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let before = LibExecution.Interpreter.Promises.maxInflight
    LibExecution.Interpreter.Promises.maxInflight <- 2
    try
      let! state = executionStateFor pmPT false Map.empty
      let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
      let! (p : Scheduler.Process) =
        spawn
          s
          state
          """let xs = Stdlib.List.map [ 41L; 42L; 43L; 44L ] (fun n -> (let _ = Builtin.testTrace "start" in let r = Builtin.testRead n in let _ = Builtin.testTrace "end" in r))
let _ = Builtin.testTrace "mapped"
Stdlib.List.fold xs 0L (fun a b -> a + b)"""
      let running = runOnThread s p
      // Two in flight, and the third awaited before the fourth is even called.
      waitFor "three reads started" (fun () -> Gates.waiting () = [ 41L; 42L; 43L ])
      Thread.Sleep 50
      Expect.equal
        (Gates.waiting ())
        [ 41L; 42L; 43L ]
        "the fourth waits for the third"
      // The first two lambdas ran to their end with the read still in flight (their `r` is a
      // promise); the third is waiting inline, so its "end" has not come.
      Expect.equal
        (Trace.take ())
        [ "start"; "end"; "start"; "end"; "start" ]
        "the map has not returned"
      Gates.release 43L
      waitFor "the fourth read" (fun () -> List.contains 44L (Gates.waiting ()))
      Gates.release 44L
      waitForTrace "the map returned" [ "end"; "start"; "end"; "mapped" ]
      Gates.release 41L
      Gates.release 42L
      let! result = running
      Expect.equal (expectOk result "the program") (RT.DInt64 170L) "the sum"
    finally
      LibExecution.Interpreter.Promises.maxInflight <- before
  }


let private spawnAwaitSelect =
  testTask "spawn runs on a worker; await and select collect it" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    do!
      withWorkers 2 (fun root ->
        task {
          let! (p : Scheduler.Process) =
            spawn
              root
              state
              """let slow = Stdlib.Exec.spawn (fun () -> (let _ = Builtin.testGateWait 51L in "slow"))
let fast = Stdlib.Exec.spawn (fun () -> "fast")
let (_, first) = Stdlib.Exec.select [ slow; fast ]
let _ = Builtin.testTrace first
Stdlib.Exec.await slow"""
          let running = runOnThread root p
          waitForTrace "select answered with the one that finished" [ "fast" ]
          // The slow one is a process of its own, parked on the gate, on a worker.
          let slowProc =
            root.Snapshot()
            |> List.tryFind (fun q ->
              q.parent = Some p.id
              && (match q.status with
                  | Scheduler.Parked _ -> true
                  | _ -> false))
          Expect.isSome
            slowProc
            "the spawned process is parked in the group's table"
          Gates.release 51L
          let! result = running
          Expect.equal
            (expectOk result "the program")
            (RT.DString "slow")
            "await's answer"
        })
  }


let private spawnedErrorReachesAwait =
  testTask "a spawned process that fails raises at await, under the spawner's access" {
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    // Spawning is allowed and nothing else is, so the spawn itself goes through and the
    // child's read is what gets denied: the child inherits the spawner's access.
    let spawnOnly =
      LibExecution.Execution.restrictRun
        (LibExecution.Permissions.Policy.allowEffects (
          Set.singleton LibExecution.Effects.Effect.Concurrency
        ))
        state
    do!
      withWorkers 2 (fun root ->
        task {
          let! (p : Scheduler.Process) =
            spawn
              root
              spawnOnly
              """let h = Stdlib.Exec.spawn (fun () -> Builtin.testRead 61L)
let _ = Builtin.testTrace "spawned"
Stdlib.Exec.await h"""
          let! result = runOnThread root p
          Expect.equal
            (Trace.take ())
            [ "spawned" ]
            "the program got past the spawn"
          match result with
          | Error(RTE.UncaughtException(msg, _), _) ->
            Expect.stringContains msg "permission denied" "the child's denial"
          | other -> failtest $"expected the child's denial at await, got {other}"
        })
  }


let private execDoneAnswersAFinishedProcess =
  testTask
    "Host.await [ExecDone id] returns for a process already over, and for one that failed" {
    let! state = executionStateFor pmPT false Map.empty
    do!
      withWorkers 2 (fun root ->
        task {
          let! (p : Scheduler.Process) =
            spawn
              root
              state
              """let done_ = Stdlib.Exec.spawn (fun () -> 1L)
let _ = Stdlib.Exec.await done_
let bad = Stdlib.Exec.spawn (fun () -> 1L / 0L)
let first = Stdlib.Host.await [ Stdlib.Host.EventSpec.ExecDone done_.id ]
let second = Stdlib.Host.await [ Stdlib.Host.EventSpec.ExecDone bad.id ]
match (first, second) with
| (ExecDone a, ExecDone b) -> a == done_.id && b == bad.id
| _ -> false"""
          let! result = runOnThread root p
          Expect.equal
            (expectOk result "the program")
            (RT.DBool true)
            "both waits came back with the right id"
        })
  }


let private httpGetIsARead =
  testTask
    "the HTTP client's GET and HEAD builtin is a read, and the request builtin is not" {
    let! (state : RT.ExecutionState) = executionStateFor pmPT false Map.empty
    let readsOnly (name : string) : bool =
      let b = state.fns.builtIn[RT.FQFnName.builtin name 0]
      LibExecution.Effects.readsOnly b.name.name b.callEffects
    Expect.isTrue (readsOnly "httpClientRead") "GET and HEAD are reads"
    Expect.isFalse (readsOnly "httpClientRequest") "the rest keep their order"
  }


// -- No host re-entry: a lambda a builtin applies is a frame on the process's own stack --

/// A lambda a builtin applies is a frame of the process: parked inside it, `ps` shows the
/// lambda's frame, and the builtin finishes once the wait lands.
let private parkedInsideShowsTheLambda
  (name : string)
  (code : string)
  (gate : int64)
  (expected : RT.Dval)
  =
  testTask name {
    Gates.reset ()
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) = spawn s state code
    let running = runOnThread s p
    waitFor "the process to park inside the lambda" (fun () ->
      match p.status with
      | Scheduler.Parked _ -> true
      | _ -> false)
    let frames =
      s.Snapshot()
      |> List.tryFind (fun q -> q.id = p.id)
      |> Option.map (fun q -> q.frames)
      |> Option.defaultValue []
    Expect.isTrue
      (frames
       |> List.exists (fun ep ->
         match ep with
         | RT.Lambda _ -> true
         | _ -> false))
      $"the lambda's frame is on the stack: {frames}"
    Gates.release gate
    let! result = running
    Expect.equal (expectOk result name) expected "finished after the lambda resumed"
  }

let private mapped =
  RT.DList(RT.ValueType.Known RT.KTInt64, [ RT.DInt64 2L; RT.DInt64 3L ])

let private parkedInsideMapShowsTheLambda =
  parkedInsideShowsTheLambda
    "a process parked inside List.map f shows f's frame, and resumes"
    """Stdlib.List.map [ 1L; 2L ] (fun x -> (let _ = Builtin.testGateWait 91L in (x + 1L)))"""
    91L
    mapped


let private budgetYieldInsideMap =
  testTask "a tight loop inside a mapped lambda is preempted and the map finishes" {
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (mapping : Scheduler.Process) =
      spawn
        s
        state
        """(let spin (n: Int64) (acc: Int64) : Int64 =
              if n == 0L then acc else spin (n - 1L) (acc + n)
            Stdlib.List.map [ 20000L; 30000L ] (fun n -> spin n 0L))"""
    let! (other : Scheduler.Process) =
      spawn
        s
        state
        """Builtin.testTrace "other"
"""
    let! result = runOnThread s mapping
    Expect.equal
      (expectOk result "the map")
      (RT.DList(
        RT.ValueType.Known RT.KTInt64,
        [ RT.DInt64 200010000L; RT.DInt64 450015000L ]
      ))
      "both spins summed"
    Expect.isGreaterThan
      mapping.slices
      1L
      "the loops inside the lambda were preempted"
    let! _ = s.Await other
    Expect.equal
      (Trace.take ())
      [ "other" ]
      "the other process ran between the slices"
  }


let private errorInsideMapNamesTheLambda =
  testTask "an error inside a mapped lambda reports the lambda's frame" {
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) =
      spawn s state """Stdlib.List.map [ 1L ] (fun x -> x / 0L)"""
    let! result = runOnThread s p
    match result with
    | Error(_, stack) ->
      Expect.isTrue
        (stack
         |> List.exists (fun ep ->
           match ep with
           | RT.Lambda _ -> true
           | _ -> false))
        $"the stack names the lambda: {stack}"
    | Ok v -> failtest $"expected the division to fail, got {v}"
  }


/// A stream transform's callable is a frame of the pulling process too.
let private parkedInsideStreamMapShowsTheLambda =
  parkedInsideShowsTheLambda
    "a process parked inside a stream transform shows the lambda's frame"
    """Stdlib.Stream.toList (Stdlib.Stream.map (Stdlib.Stream.fromList [ 1L; 2L ]) (fun x -> (let _ = Builtin.testGateWait 93L in (x + 1L))))"""
    93L
    mapped


/// The source waits on the host before every element (a network stream's shape), so the
/// transform is asked for after the pulling builtin's first wait: the frame is pushed from
/// where the wait lands, in the scheduler's step and in a plain run.
let private transformAfterTheSourceWaits =
  testTask "a transform over a stream that waits on the host runs as a frame" {
    let! state = executionStateFor pmPT false Map.empty
    let code =
      """Stdlib.Stream.toList (Stdlib.Stream.filter (Stdlib.Stream.map (Builtin.testSlowStream [ 1L; 2L; 3L; 4L ]) (fun x -> x * 10L)) (fun x -> x > 10L))"""
    let expected =
      RT.DList(
        RT.ValueType.Known RT.KTInt64,
        [ RT.DInt64 20L; RT.DInt64 30L; RT.DInt64 40L ]
      )
    // Scheduled: the wait parks the process; the landing pushes the lambda's frame.
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) = spawn s state code
    let! result = runOnThread s p
    Expect.equal (expectOk result "the scheduled drain") expected "scheduled"
    // Unscheduled (a test's `execute`): the same landing in the task loop.
    let! instrs = instrsFor code
    let! plain = LibExecution.Execution.executeExpr state instrs
    Expect.equal (expectOk plain "the plain drain") expected "unscheduled"
  }


/// Two equal spinners on one scheduler: round robin lands the older first; a Dark policy
/// (`Stdlib.Exec.Policy.youngestFirst`) asked between slices lands the younger first.
let private darkPolicyOrders =
  testTask "a Dark scheduling policy decides which runnable process is stepped" {
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let spinner (tag : string) =
      $"""(let spin (n: Int64) (acc: Int64) : Int64 =
              if n == 0L then acc else spin (n - 1L) (acc + n)
            let total = spin 20000L 0L
            let _ = Builtin.testTrace "{tag}"
            total)"""
    let runBoth () =
      task {
        let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
        let! (older : Scheduler.Process) = spawn s state (spinner "older")
        let! (younger : Scheduler.Process) = spawn s state (spinner "younger")
        // Whichever finishes first ends the loop; a second run until the other one takes it
        // the rest of the way (or returns at once, when it is done already).
        let! result = runOnThread s older
        expectOk result "the older spinner" |> ignore<RT.Dval>
        let! result = runOnThread s younger
        expectOk result "the younger spinner" |> ignore<RT.Dval>
        return Trace.take ()
      }
    let! (plain : List<string>) = runBoth ()
    Expect.equal plain [ "older"; "younger" ] "round robin: the older finishes first"
    let location : PT.PackageLocation =
      { owner = "Darklang"
        modules = [ "Stdlib"; "Exec"; "Policy" ]
        name = "youngestFirst" }
    let! (found : Option<PT.FQFnName.Package>) = Ply.toTask (pmPT.findFn location)
    let fn =
      match found with
      | Some p -> RT.FQFnName.Package(PT2RT.FQFnName.Package.toRT p)
      | None -> failtest "Stdlib.Exec.Policy.youngestFirst is not in the store"
    Scheduler.policy <-
      Scheduler.Chooser(Builtins.Language.Libs.Exec.chooserFor state fn)
    try
      let! (chosen : List<string>) = runBoth ()
      Expect.equal
        chosen
        [ "younger"; "older" ]
        "youngestFirst: the younger runs to the end before the older gets a slice"
    finally
      Scheduler.policy <- Scheduler.RoundRobin
  }


let private parkedOnAHostOperation =
  testTask "a process waiting on the host is parked on the operation, and resumes" {
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    // A process run the host performs for the builtin; `waitFor` looks every 5 ms, so a
    // third of a second is long enough to be seen parked.
    let! (p : Scheduler.Process) =
      spawn s state """(Stdlib.Cli.execute "sleep 0.3").exitCode"""
    let running = runOnThread s p
    waitFor "the process to park on the host" (fun () ->
      match p.status with
      | Scheduler.Parked _ -> true
      | _ -> false)
    match p.status with
    | Scheduler.Parked(Scheduler.OnHost(LibExecution.HostTypes.Operation.ProcessRun(_,
                                                                                    args,
                                                                                    _))) ->
      Expect.equal (List.tryLast args) (Some "sleep 0.3") "parked on the run itself"
    | other ->
      failtest $"expected the process parked on the host operation, got {other}"
    let! result = running
    Expect.equal (expectOk result "the run") (RT.Dval.int 0I) "the run finished"
  }


// -- Cancellation: soft and hard, and the tree --

let private cancelLetsTheWaitLand =
  testTask "cancel lets what a process waits on land, then stops it" {
    Gates.reset ()
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) =
      spawn s state "let _ = Builtin.testGateWait 60L in Builtin.testTrace \"after\""
    let running = runOnThread s p
    waitFor "the process to park on the gate" (fun () ->
      match p.status with
      | Scheduler.Parked _ -> true
      | _ -> false)
    Expect.isTrue (s.Cancel p.id) "cancel found it"
    // Still parked: a soft stop does not abandon the wait.
    Thread.Sleep 100
    match p.status with
    | Scheduler.Parked _ -> ()
    | other -> failtest $"expected the cancelled process still parked, got {other}"
    Gates.release 60L
    let! result = running
    match result with
    | Error(RTE.UncaughtException("cancelled", _), _) -> ()
    | other -> failtest $"expected cancelled, got {other}"
    Expect.equal (Trace.take ()) [] "the code after the wait did not run"
  }


let private childrenDieWithTheParent =
  testTask "a parent's end stops its children, unless spawned detached" {
    Gates.reset ()
    Trace.take () |> ignore<List<string>>
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    // Two children that would run forever; the parent finishes at once without awaiting them.
    let! (parent : Scheduler.Process) =
      spawn
        s
        state
        """(let spin (n: Int64) : Int64 =
              if n == 0L then 1L else spin (n + 1L)
            let attached = Stdlib.Exec.spawn (fun () -> spin 1L)
            let detached = Stdlib.Exec.spawnDetached (fun () -> spin 1L)
            (attached.id, detached.id))"""
    let running = runOnThread s parent
    let! result = running
    let (attachedId, detachedId) =
      match expectOk result "the parent" with
      | RT.DTuple(RT.DUuid a, RT.DUuid d, []) -> a, d
      | other -> failtest $"expected two ids, got {other}"
    let find (id : System.Guid) = s.Find id |> Option.get
    waitFor "the attached child to be stopped" (fun () ->
      match (find attachedId).status with
      | Scheduler.Failed _ -> true
      | _ -> false)
    match (find attachedId).status with
    | Scheduler.Failed(RTE.UncaughtException("its parent finished", _), _) -> ()
    | other ->
      failtest $"expected the child stopped by its parent's end, got {other}"
    Thread.Sleep 100
    match (find detachedId).status with
    | Scheduler.Failed _
    | Scheduler.Done _ -> failtest "the detached child should still be running"
    | _ -> ()
    Expect.isTrue (s.Kill detachedId) "the detached child is stopped by hand"
    let! _ = s.Await(find detachedId)
    ()
  }


let private awaitWithinTimesOut =
  testTask "awaitWithin gives up on a slow child, which then can be cancelled" {
    Gates.reset ()
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (p : Scheduler.Process) =
      spawn
        s
        state
        """(let h = Stdlib.Exec.spawn (fun () -> Builtin.testGateWait 61L)
            let first = Stdlib.Exec.awaitWithin 50L h
            let _ = Stdlib.Exec.cancel h
            (first, h.id))"""
    let running = runOnThread s p
    let! result = running
    let childId =
      match expectOk result "the program" with
      | RT.DTuple(RT.DEnum(_, _, _, "None", []), RT.DUuid id, []) -> id
      | other -> failtest $"expected None and the child's id, got {other}"
    // The child was cancelled softly, so it is still parked on its gate until that lands.
    let child = s.Find childId |> Option.get
    match child.status with
    | Scheduler.Parked _ -> ()
    | other -> failtest $"expected the child still parked, got {other}"
    Gates.release 61L
    let! childResult = s.Await child
    match childResult with
    | Error(RTE.UncaughtException("cancelled", _), _) -> ()
    | other -> failtest $"expected the child cancelled, got {other}"
  }


let private killCascadesHard =
  testTask "ps kill of a parent reaches a child stuck on the host, at once" {
    Gates.reset ()
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (parent : Scheduler.Process) =
      spawn
        s
        state
        """let h = Stdlib.Exec.spawn (fun () -> Builtin.testGateWait 62L)
Stdlib.Exec.await h"""
    let running = runOnThread s parent
    let deadline = System.DateTime.UtcNow.AddSeconds 5.
    while (match parent.status with
           | Scheduler.Parked _ -> false
           | _ -> true)
          && System.DateTime.UtcNow < deadline do
      Thread.Sleep 5
    match parent.status with
    | Scheduler.Parked _ -> ()
    | other -> failtest $"the parent never parked on its child: {other}"
    let child : Scheduler.ProcessSummary =
      s.Snapshot() |> List.find (fun q -> q.parent = Some parent.id) |> Option.get
    Expect.isTrue (s.Kill parent.id) "kill found the parent"
    let! result = running
    match result with
    | Error(RTE.UncaughtException("killed from ps", _), _) -> ()
    | other -> failtest $"expected the parent killed, got {other}"
    let! childResult = s.Await(s.Find child.id |> Option.get)
    match childResult with
    | Error(RTE.UncaughtException("killed from ps", _), _) -> ()
    | other -> failtest $"expected the child killed with it, got {other}"
  }


let private capsEndARunaway =
  testTask
    "a process over exec.maxInstructions ends with the reason; ps shows what it ran" {
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let before = Scheduler.maxInstructions
    // Three slices' worth, so the cap is crossed after three refills.
    Scheduler.maxInstructions <- 3L * Scheduler.defaultQuantum
    try
      let! (loop : Scheduler.Process) =
        spawn
          s
          state
          """(let spin (n: Int64) (acc: Int64) : Int64 =
                if n == 0L then acc else spin (n - 1L) (acc + n)
              spin 2000000L 0L)"""
      let! result = runOnThread s loop
      match result with
      | Ok dv -> failtest $"the runaway finished: {dv}"
      | Error(rte, _) ->
        Expect.stringContains
          (string rte)
          "over 30000 instructions"
          "the reason names the cap"
      Expect.equal loop.slices 3L "it got its three slices"
      let summary = s.SummaryOf loop
      Expect.isGreaterThanOrEqual
        summary.instructionsTaken
        (3L * Scheduler.defaultQuantum)
        "and the instructions it ran are counted"
      Expect.isGreaterThan summary.allocated 0L "allocation was accounted per slice"
    finally
      Scheduler.maxInstructions <- before
  }


let private registryListsAndForgets =
  testTask "the machine registry lists a live pid and drops a dead one" {
    let dir =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-ps-test-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
    HostRegistry.setDirectory dir
    try
      // A stale entry: a pid no process has.
      let psDir = System.IO.Path.Combine(dir, "run", "ps")
      System.IO.Directory.CreateDirectory psDir |> ignore<System.IO.DirectoryInfo>
      System.IO.File.WriteAllText(
        System.IO.Path.Combine(psDir, "999999.json"),
        "{\"pid\":999999,\"title\":\"dark gone\",\"command\":\"dark gone\",\"branch\":\"\",\"started\":\"2026-01-01T00:00:00.0000000Z\"}"
      )
      // A quote in the command line is what a hand-rolled reader trips on.
      HostRegistry.register "dark tests" "dark eval \"1L\"" "main"
      let entries = HostRegistry.list ()
      let me = System.Environment.ProcessId
      Expect.isTrue
        (entries
         |> List.exists (fun e ->
           e.pid = me && e.title = "dark tests" && e.command = "dark eval \"1L\""))
        "this process is listed with its title and command"
      Expect.isFalse
        (entries |> List.exists (fun e -> e.pid = 999999))
        "the dead pid is gone"
      Expect.isFalse
        (System.IO.File.Exists(
          System.IO.Path.Combine(dir, "run", "ps", "999999.json")
        ))
        "and its file was removed"
    finally
      HostRegistry.setDirectory ""
      System.IO.Directory.Delete(dir, true)
  }


let private byteCapEndsARunaway =
  testTask "a process over exec.maxBytes ends with a reason that names the cap" {
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let before = Scheduler.maxBytes
    Scheduler.maxBytes <- 1L
    try
      let! (loop : Scheduler.Process) =
        spawn
          s
          state
          """(let spin (n: Int64) (acc: Int64) : Int64 =
                if n == 0L then acc else spin (n - 1L) (acc + n)
              spin 2000000L 0L)"""
      let! result = runOnThread s loop
      match result with
      | Ok dv -> failtest $"the runaway finished: {dv}"
      | Error(rte, _) ->
        Expect.stringContains
          (string rte)
          "bytes allocated"
          "the reason names the cap"
    finally
      Scheduler.maxBytes <- before
  }


/// The polite stop reaches the children too: a cancelled parent's child waits for what it has
/// in flight, then fails with the parent's reason.
let private cancelCascadesSoftly =
  testTask
    "cancel of a parent lets the child's wait land, then stops it as cancelled" {
    Gates.reset ()
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let! (parent : Scheduler.Process) =
      spawn
        s
        state
        """(let h = Stdlib.Exec.spawn (fun () -> Builtin.testGateWait 63L)
            let _ = Builtin.testGateWait 62L
            Stdlib.Exec.await h)"""
    let running = runOnThread s parent
    waitFor "the child to park on its gate" (fun () ->
      s.Snapshot()
      |> List.exists (fun q ->
        q.parent = Some parent.id
        && (match q.status with
            | Scheduler.Parked _ -> true
            | _ -> false)))
    let (child : Scheduler.ProcessSummary) =
      s.Snapshot()
      |> List.tryFind (fun (q : Scheduler.ProcessSummary) ->
        q.parent = Some parent.id)
      |> Option.get
    Expect.isTrue (s.Cancel parent.id) "the parent was found"
    // Neither has been given its turn yet: both waits are still in flight.
    Gates.release 62L
    let! result = running
    match result with
    | Error(RTE.UncaughtException("cancelled", _), _) -> ()
    | other -> failtest $"expected the parent cancelled, got {other}"
    let childProc = s.Find child.id |> Option.get
    match childProc.status with
    | Scheduler.Parked _ -> ()
    | other -> failtest $"expected the child still parked on its gate, got {other}"
    Gates.release 63L
    let! childResult = s.Await childProc
    match childResult with
    | Error(RTE.UncaughtException("cancelled", _), _) -> ()
    | other ->
      failtest $"expected the child cancelled after its wait landed, got {other}"
  }


// -- Cancelling a process that is busy, not parked --

let private cancelStopsAnEmptyLambdaMap =
  testTask "a cancel stops a map whose lambda runs no instructions" {
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    // `fun x -> x` compiles to no instructions: each element is a frame pushed and returned. Long
    // enough to take seconds if nothing preempts it.
    let! (p : Scheduler.Process) =
      spawn
        s
        state
        "Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 0 5000000) (fun x -> x))"
    let running = runOnThread s p
    // Preempted at least once, so it is mid-map rather than still building the range.
    let deadline = System.DateTime.UtcNow.AddSeconds 60.
    while p.slices < 2L
          && not running.IsCompleted
          && System.DateTime.UtcNow < deadline do
      Thread.Sleep 1
    if running.IsCompleted then
      failtest $"the map finished in {p.slices} slice(s), never preempted"
    Expect.isTrue (s.Cancel p.id) "cancel found it"
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let! result = running
    match result with
    | Error(RTE.UncaughtException("cancelled", _), _) -> ()
    | other -> failtest $"expected cancelled, got {other}"
    Expect.isLessThan sw.ElapsedMilliseconds 1000L "it stopped within a slice or so"
  }


let private cancelDuringOneLongCallIsNotOk =
  testTask "a cancel that lands during one long builtin call does not report success" {
    let! state = executionStateFor pmPT false Map.empty
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    // One instruction that takes seconds: the slice cannot end until the call does.
    let! (p : Scheduler.Process) =
      spawn s state "Stdlib.List.length (Stdlib.List.range 0 15000000)"
    let running = runOnThread s p
    let deadline = System.DateTime.UtcNow.AddSeconds 60.
    while p.slices < 1L
          && not running.IsCompleted
          && System.DateTime.UtcNow < deadline do
      Thread.Sleep 1
    if running.IsCompleted then failtest "the range finished before the cancel"
    Expect.isTrue (s.Cancel p.id) "cancel found it"
    let! result = running
    match result with
    | Error(RTE.UncaughtException("cancelled", _), _) -> ()
    | other -> failtest $"expected cancelled, got {other}"
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
        readLineDoesNotBlock
        stdinTimerLeavesReadOutstanding
        accessIsPerProcess
        killWakesAParkedProcess
        editDoesNotReachAParkedProcess
        workersUseCores
        sharedStateAcrossWorkers
        psSeesTheWholeGroup
        traceCarriesProcessAndSeq
        readsRunAtOnce
        writesKeepOrder
        failedReadRaisesAtAwait
        unlookedReadFailsTheRunAtItsEnd
        denialRaisesAtTheCall
        inflightBoundHolds
        spawnAwaitSelect
        spawnedErrorReachesAwait
        execDoneAnswersAFinishedProcess
        httpGetIsARead
        parkedInsideMapShowsTheLambda
        budgetYieldInsideMap
        errorInsideMapNamesTheLambda
        parkedInsideStreamMapShowsTheLambda
        transformAfterTheSourceWaits
        darkPolicyOrders
        parkedOnAHostOperation
        cancelLetsTheWaitLand
        childrenDieWithTheParent
        awaitWithinTimesOut
        killCascadesHard
        capsEndARunaway
        byteCapEndsARunaway
        cancelCascadesSoftly
        cancelStopsAnEmptyLambdaMap
        cancelDuringOneLongCallIsNotOk
        registryListsAndForgets ]
  )
