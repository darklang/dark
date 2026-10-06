/// List ops spreading across cores by themselves (`LibExecution.Spread`).
///
/// Every test here forces the decision (`crossover = 0`) so that it is about what a spread
/// DOES, and asserts that a spread happened, since every result below is also what a serial run
/// returns: a test that only compared values would pass with spreading broken or off.
module Tests.Spread

open System.Threading
open System.Threading.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Scheduler = LibExecution.Scheduler
module Spread = LibExecution.Spread
module Trace = TestUtils.LibTest.Trace

let private instrsFor (code : string) : Task<RT.Instructions> =
  task {
    let! ptExpr = parsePTExpr code
    return PT2RT.Expr.toRT Map.empty 0 None ptExpr
  }

/// Run `code` as a process on a fresh scheduler, with spreading forced or off.
let private runInstrs
  (state : RT.ExecutionState)
  (spread : bool)
  (instrs : RT.Instructions)
  : Task<RT.ExecutionResult> =
  task {
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = s.Spawn(state, (None, instrs), Scheduler.EntryExpr, None)
    let saved = Spread.crossover
    let savedChunk = Spread.minChunk
    Spread.crossover <- (if spread then 0L else -1L)
    // One element a chunk where there are cores for it, so order and failure are exercised
    // across many chunks rather than within one.
    Spread.minChunk <- 0L
    Spread.forgetFallbacks ()
    try
      let done' =
        TaskCompletionSource<RT.ExecutionResult>(
          TaskCreationOptions.RunContinuationsAsynchronously
        )
      let thread =
        Thread(
          (fun () ->
            try
              done'.SetResult(s.RunUntil p)
            with ex ->
              done'.SetException ex),
          IsBackground = true
        )
      thread.Start()
      return! done'.Task
    finally
      Spread.crossover <- saved
      Spread.minChunk <- savedChunk
  }

/// Compiled once per call: compare two runs of the SAME program, since lambda ids differ between
/// two compilations of one source and a frame names its lambda by id.
let private runWith
  (state : RT.ExecutionState)
  (spread : bool)
  (code : string)
  : Task<RT.ExecutionResult> =
  task {
    let! instrs = instrsFor code
    return! runInstrs state spread instrs
  }

let private run (state : RT.ExecutionState) (code : string) = runWith state true code

/// The same program spread and serial.
let private both (state : RT.ExecutionState) (code : string) =
  task {
    let! instrs = instrsFor code
    let! spread = runInstrs state true instrs
    let! serial = runInstrs state false instrs
    return spread, serial
  }

let private expectOk (result : RT.ExecutionResult) : RT.Dval =
  match result with
  | Ok dv -> dv
  | Error(rte, _) -> failtest $"failed: {rte}"

/// What a counter moved by while `f` ran.
let private delta (read : unit -> int64) (f : unit -> Task<'a>) : Task<'a * int64> =
  task {
    let before = read ()
    let! r = f ()
    return r, read () - before
  }

let private spreads () = Spread.spreads
let private fallbacks () = Spread.fallbacks

/// A body whose cost falls steeply with the element: the first elements cost the most, so the
/// first chunk finishes LAST. A spread that collected chunks as they finished would put the cheap
/// tail first.
let private uneven =
  "(fun i -> (let n = (41 - i) * (41 - i) * 20 in Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 1 n) (fun x -> x)) + i))"

let private inOrder =
  testTask "results come back in input order when the first chunk finishes last" {
    let! state = executionStateFor pmPT false Map.empty
    let code = $"Stdlib.List.map (Stdlib.List.range 1 40) {uneven}"
    let! (spread, moved) = delta spreads (fun () -> run state code)
    let! serial = runWith state false code
    Expect.isGreaterThan moved 0L "the map spread"
    Expect.equal (expectOk spread) (expectOk serial) "the same list as serial"
  }

let private siblings =
  testTask "filter, filterMap and indexedMap spread and agree with serial" {
    let! state = executionStateFor pmPT false Map.empty
    for code in
      [ "Stdlib.List.filter (Stdlib.List.range 1 100) (fun i -> i % 3 == 0)"
        "Stdlib.List.filterMap (Stdlib.List.range 1 100) (fun i -> if i % 4 == 0 then Stdlib.Option.Option.Some (i * 2) else Stdlib.Option.Option.None)"
        "Stdlib.List.indexedMap (Stdlib.List.range 1 100) (fun idx i -> (idx, i * 10))" ] do
      let! (spread, moved) = delta spreads (fun () -> run state code)
      let! serial = runWith state false code
      Expect.isGreaterThan moved 0L $"spread: {code}"
      Expect.equal (expectOk spread) (expectOk serial) code
  }

/// The failing element is in a cheap late chunk, so it fails long before the expensive chunks
/// ahead of it finish. Serial raises at it; so must this, with the same error and frames, and
/// only after everything before it.
let private errorAsSerial =
  testTask "an error is raised by the element that raises it serially, as serially" {
    let! state = executionStateFor pmPT false Map.empty
    let code =
      "Stdlib.List.map (Stdlib.List.range 1 40) (fun i -> if i == 35 then 1 / 0 else ("
      + uneven
      + " i))"
    let! ((spread, serial), fellBack) = delta fallbacks (fun () -> both state code)
    Expect.equal fellBack 1L "the spread fell back"
    match spread, serial with
    | Error(e1, s1), Error(e2, s2) ->
      Expect.equal e1 e2 "the same error"
      Expect.equal s1 s2 "the same frames"
    | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// The earliest error wins even when a later one is found first.
let private earliestErrorWins =
  testTask "of two failing elements the earlier one raises, whichever fails first" {
    let! state = executionStateFor pmPT false Map.empty
    let code =
      "Stdlib.List.map (Stdlib.List.range 1 40) (fun i -> if i == 3 then (let _ = ("
      + uneven
      + " 1) in 1 / 0) else if i == 38 then Builtin.testRuntimeError \"late\" else i)"
    let! spread = run state code
    let! serial = runWith state false code
    match spread, serial with
    | Error(e1, _), Error(e2, _) -> Expect.equal e1 e2 "the same, earlier, error"
    | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// `testTrace` is logged (an effect ordinal), so a chunk reaching it is refused and the rest runs
/// here. The trace is the order the effects happened in.
let private effectsKeepOrder =
  testTask "a body that reaches an effect runs it in order, here" {
    let! state = executionStateFor pmPT false Map.empty
    Trace.take () |> ignore<List<string>>
    let code =
      "Stdlib.List.map (Stdlib.List.range 1 30) (fun i -> if i % 7 == 0 then (let _ = Builtin.testTrace (if i == 7 then \"7\" else if i == 14 then \"14\" else if i == 21 then \"21\" else \"28\") in i) else i * 2)"
    let! (spread, fellBack) = delta fallbacks (fun () -> run state code)
    let spreadTrace = Trace.take ()
    let! serial = runWith state false code
    let serialTrace = Trace.take ()
    expectOk spread |> ignore<RT.Dval>
    Expect.equal fellBack 1L "the spread fell back"
    Expect.equal spreadTrace [ "7"; "14"; "21"; "28" ] "each effect once, in order"
    Expect.equal spreadTrace serialTrace "as serial"
    Expect.equal (expectOk spread) (expectOk serial) "the same list"
  }

/// Inner maps run in the outer map's chunks, where nothing spreads. The outer probe's first answer
/// comes after three elements (one to warm up, two measured), which run in the original process,
/// so THEIR inner maps spread: four in all, the outer and three inner, never one per element.
let private nestingIsBounded =
  testTask "a map inside a spread map does not spread again" {
    let! state = executionStateFor pmPT false Map.empty
    let code =
      "Stdlib.List.map (Stdlib.List.range 1 20) (fun i -> Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 1 50) (fun x -> x + i)))"
    let! (result, moved) = delta spreads (fun () -> run state code)
    Expect.equal
      moved
      4L
      "the outer map and the first three elements' inner maps, and no others"
    Expect.equal
      (expectOk result)
      (RT.DList(
        LibExecution.ValueType.int,
        List.replicate 20 (LibExecution.Dval.int (bigint 50))
      ))
      "twenty lengths of fifty"
  }

let private watchedRunsDoNotSpread =
  testTask "a recorded or viewed run does not spread" {
    let! (state : RT.ExecutionState) = executionStateFor pmPT false Map.empty
    // Watched all the way down: a process asks its tracer for its own copy (`forProcess`).
    let tracing : RT.Tracing.Tracing = state.tracing
    let rec watching () : RT.Tracing.Tracing =
      { tracing with collectFrames = true; forProcess = fun _ -> watching () }
    let watched : RT.ExecutionState = { state with tracing = watching () }
    let! (result, moved) =
      delta spreads (fun () ->
        run watched "Stdlib.List.map (Stdlib.List.range 1 100) (fun i -> i)")
    expectOk result |> ignore<RT.Dval>
    Expect.equal moved 0L "nothing spread"
  }

/// Cancelled mid-spread: the map's process ends cancelled, with no list, and the chunks go with
/// it.
let private cancelledLeavesNothing =
  testTask "a cancelled spread map answers no list and leaves no chunk running" {
    let! state = executionStateFor pmPT false Map.empty
    let! instrs =
      instrsFor
        "Stdlib.List.map (Stdlib.List.range 1 16) (fun i -> Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 1 3000000) (fun x -> x)))"
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = s.Spawn(state, (None, instrs), Scheduler.EntryExpr, None)
    let saved = Spread.crossover
    Spread.crossover <- 0L
    try
      let before = Spread.spreads
      let thread =
        Thread(
          (fun () -> s.RunUntil p |> ignore<RT.ExecutionResult>),
          IsBackground = true
        )
      thread.Start()
      let deadline = System.DateTime.UtcNow.AddSeconds 30.
      while Spread.spreads = before && System.DateTime.UtcNow < deadline do
        Thread.Sleep 5
      Expect.isGreaterThan Spread.spreads before "the map spread"
      let sw = System.Diagnostics.Stopwatch.StartNew()
      s.Cancel p.id |> ignore<bool>
      let! result = p.completion.Task
      // Measured before the stop was checked at the fallback: 26 to 29 s, the whole rest of the
      // program. After: 0.1 to 0.7 s in Debug. Ten seconds is far from both.
      Expect.isLessThan
        sw.ElapsedMilliseconds
        10_000L
        "the cancel took effect promptly"
      match result with
      | Error _ -> ()
      | Ok v -> failtest $"a cancelled map answered {v}"
      let deadline = System.DateTime.UtcNow.AddSeconds 10.
      let live () =
        s.Snapshot()
        |> List.filter (fun c ->
          c.parent = Some p.id
          && (match c.status with
              | Scheduler.Done _
              | Scheduler.Failed _ -> false
              | _ -> true))
      while not (List.isEmpty (live ())) && System.DateTime.UtcNow < deadline do
        Thread.Sleep 10
      Expect.isEmpty (live ()) "no chunk still running"
    finally
      Spread.crossover <- saved
  }

/// After a fallback the rest runs here as an ordinary serial map, in slices like any other
/// work: a fallback must not turn into one long uninterruptible step.
let private fallbackIsPreempted =
  testTask "the serial rest after a fallback is preempted like any map" {
    let! state = executionStateFor pmPT false Map.empty
    let! instrs =
      instrsFor
        "Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 1 400) (fun i -> if i == 10 then (let _ = Builtin.testTrace \"ten\" in i) else Stdlib.List.fold (Stdlib.List.range 1 2000) i (fun a x -> a + x)))"
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = s.Spawn(state, (None, instrs), Scheduler.EntryExpr, None)
    let saved = Spread.crossover
    Spread.crossover <- 0L
    Spread.forgetFallbacks ()
    try
      let before = Spread.fallbacks
      let thread =
        Thread(
          (fun () -> s.RunUntil p |> ignore<RT.ExecutionResult>),
          IsBackground = true
        )
      thread.Start()
      let! result = p.completion.Task
      Trace.take () |> ignore<List<string>>
      expectOk result |> ignore<RT.Dval>
      Expect.equal (Spread.fallbacks - before) 1L "it fell back"
      Expect.isGreaterThan p.slices 20L "the serial rest ran in many slices"
    finally
      Spread.crossover <- saved
  }

/// Every element reaches an effect, so the first chunk fails at once, usually before the spreader
/// has even finished starting the others: the fallback then continues inside the same step.
let private syncFallbackIsPreempted =
  testTask "a fallback that lands at once is preempted like any map" {
    let! state = executionStateFor pmPT false Map.empty
    let! instrs =
      instrsFor
        "Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 1 400) (fun i -> (let _ = Builtin.testTrace \"e\" in Stdlib.List.fold (Stdlib.List.range 1 2000) i (fun a x -> a + x))))"
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = s.Spawn(state, (None, instrs), Scheduler.EntryExpr, None)
    let saved = Spread.crossover
    Spread.crossover <- 0L
    Spread.forgetFallbacks ()
    try
      let before = Spread.fallbacks
      let thread =
        Thread(
          (fun () -> s.RunUntil p |> ignore<RT.ExecutionResult>),
          IsBackground = true
        )
      thread.Start()
      let! result = p.completion.Task
      Trace.take () |> ignore<List<string>>
      expectOk result |> ignore<RT.Dval>
      Expect.equal (Spread.fallbacks - before) 1L "it fell back"
      Expect.isGreaterThan p.slices 20L "the serial rest ran in many slices"
    finally
      Spread.crossover <- saved
  }

let tests =
  testSequenced (
    testList
      "spread"
      [ inOrder
        siblings
        errorAsSerial
        earliestErrorWins
        effectsKeepOrder
        nestingIsBounded
        watchedRunsDoNotSpread
        cancelledLeavesNothing
        fallbackIsPreempted
        syncFallbackIsPreempted ]
  )
