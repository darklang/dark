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

/// Run `code` as a process on a fresh scheduler, at this crossover (0 forces a spread, -1 turns it
/// off), its VM counting into `stats` when given.
let private runInstrsCountingAt
  (stats : Option<RT.InterpreterStats>)
  (state : RT.ExecutionState)
  (crossover : int64)
  (instrs : RT.Instructions)
  : Task<RT.ExecutionResult> =
  task {
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = s.Spawn(state, (None, instrs), Scheduler.EntryExpr, None, ?stats = stats)
    let saved = Spread.crossover
    let savedChunk = Spread.minChunk
    Spread.crossover <- crossover
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

/// Run `code` as a process on a fresh scheduler, with spreading forced or off.
let private runInstrsCounting
  (stats : Option<RT.InterpreterStats>)
  (state : RT.ExecutionState)
  (spread : bool)
  (instrs : RT.Instructions)
  : Task<RT.ExecutionResult> =
  runInstrsCountingAt stats state (if spread then 0L else -1L) instrs

let private runInstrsAt = runInstrsCountingAt None

let private runInstrs = runInstrsCounting None

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
  "(fun i -> (let n = (41 - i) * (41 - i) * 20 in Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, n), fun x -> x)) + i))"

let private inOrder =
  testTask "results come back in input order when the first chunk finishes last" {
    let! state = executionStateFor pmPT false
    let code = $"Stdlib.List.map(Stdlib.List.range(1, 40), {uneven})"
    let! (spread, moved) = delta spreads (fun () -> run state code)
    let! serial = runWith state false code
    Expect.isGreaterThan moved 0L "the map spread"
    Expect.equal (expectOk spread) (expectOk serial) "the same list as serial"
  }

let private siblings =
  testTask "filter, filterMap and indexedMap spread and agree with serial" {
    let! state = executionStateFor pmPT false
    for code in
      [ "Stdlib.List.filter(Stdlib.List.range(1, 100), fun i -> i % 3 == 0)"
        "Stdlib.List.filterMap(Stdlib.List.range(1, 100), fun i -> if i % 4 == 0 then Stdlib.Option.Option.Some(i * 2) else Stdlib.Option.Option.None)"
        "Stdlib.List.indexedMap(Stdlib.List.range(1, 100), fun(idx, i) -> (idx, i * 10))" ] do
      let! (spread, moved) = delta spreads (fun () -> run state code)
      let! serial = runWith state false code
      Expect.isGreaterThan moved 0L $"spread: {code}"
      Expect.equal (expectOk spread) (expectOk serial) code
  }

/// A dict of `n` entries keyed and valued 1 to `n`, built without a list op that could spread.
let private dictOf (n : int) =
  $"Stdlib.Dict.fromEntries(Stdlib.List.zipShortest(Stdlib.List.range(1, {n}), Stdlib.List.range(1, {n})))"

let private dictOps =
  testTask "Dict.map and Dict.filter spread and agree with serial" {
    let! state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.Dict.map({dictOf 40}, fun(k, v) -> {uneven}(k) + v)"
        $"Stdlib.Dict.filter({dictOf 40}, fun(k, v) -> ({uneven}(k) + v) %% 3 == 0)" ] do
      let! (spread, moved) = delta spreads (fun () -> run state code)
      let! serial = runWith state false code
      Expect.isGreaterThan moved 0L $"spread: {code}"
      Expect.equal (expectOk spread) (expectOk serial) code
  }

/// At the shipped crossover the ops after `List.map` spread only a body that costs at least
/// `Spread.minPerElement`: a one-operation body stays serial however long the dict, and an
/// expensive one spreads.
let private cheapDictBodyStaysSerial =
  testTask "at the shipped crossover a cheap Dict.map body stays serial" {
    let! state = executionStateFor pmPT false
    let! cheap =
      instrsFor
        $"Stdlib.Dict.size(Stdlib.Dict.map({dictOf 20000}, fun(k, v) -> v + 1))"
    let! (_, cheapSpreads) =
      delta spreads (fun () -> runInstrsAt state Spread.crossover cheap)
    Expect.equal cheapSpreads 0L "a cheap body did not spread"
    let! costly =
      instrsFor
        $"Stdlib.Dict.size(Stdlib.Dict.map({dictOf 40}, fun(k, v) -> {uneven}(k)))"
    let! (_, costlySpreads) =
      delta spreads (fun () -> runInstrsAt state Spread.crossover costly)
    Expect.isGreaterThan costlySpreads 0L "an expensive one did"
  }

/// `Dict.filter` checks its answers once they are all in, so a later entry's error comes before
/// an earlier entry's non-Bool. `Stdlib.Dict.filter` declares its callback's `Bool`, so a non-Bool
/// raises where it is returned, in a chunk as serially: from half way on, every chunk answers
/// non-Bools. And a map whose answers change type half way raises when the dict is built.
let private dictErrorsAsSerial =
  testTask "Dict.map and Dict.filter raise as serially" {
    let! state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.Dict.map({dictOf 40}, fun(k, v) -> if k == 35 then 1 / 0 else {uneven}(k))"
        $"Stdlib.Dict.filter({dictOf 40}, fun(k, v) -> if k == 3 then 1 else if k == 35 then 1 / 0 else {uneven}(k) > 0)"
        $"Stdlib.Dict.filter({dictOf 40}, fun(k, v) -> if k == 30 then \"no\" else {uneven}(k) > 0)"
        $"Stdlib.Dict.filter({dictOf 40}, fun(k, v) -> if k > 20 then \"no\" else {uneven}(k) > 0)"
        $"Stdlib.Dict.map({dictOf 40}, fun(k, v) -> if k > 20 then \"s\" else {uneven}(k))" ] do
      let! (spread, serial) = both state code
      match spread, serial with
      | Error(e1, s1), Error(e2, s2) ->
        Expect.equal e1 e2 $"the same error: {code}"
        Expect.equal s1 s2 $"the same frames: {code}"
      | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// A callback type an enclosing fn declares (`mapDictInts`'s `f: (Int64, Int64) -> Int64`) is
/// checked in a `Dict.map` chunk as serially.
let private dictCallbackTypeAsSerial =
  testTask "a Dict.map callback type an enclosing fn declares is checked as serially" {
    let! state = executionStateFor pmPT false
    let code =
      $"Darklang.Language.Tests.Lambdas.mapDictInts({dictOf 40}, fun(k, v) -> if k == 35 then \"wrong\" else {uneven}(k) + v)"
    let! ((spread, serial), moved) = delta spreads (fun () -> both state code)
    Expect.isGreaterThan moved 0L "the map spread"
    match spread, serial with
    | Error(e1, s1), Error(e2, s2) ->
      Expect.equal e1 e2 "the same error"
      Expect.equal s1 s2 "the same frames"
    | _ -> failtest $"both should fail: {spread} / {serial}"
  }

let private dictEffectsKeepOrder =
  testTask "a Dict.map body that reaches an effect runs it in order, here" {
    let! state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      $"(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.Dict.map({dictOf 30}, fun(k, v) -> (let (trace, _) = hidden in if k %% 7 == 0 then (let _ = trace(Stdlib.toString(k)) in v) else v * 2)))"
    let! (spread, fellBack) = delta fallbacks (fun () -> run state code)
    let spreadTrace = Trace.take ()
    let! serial = runWith state false code
    let serialTrace = Trace.take ()
    Expect.equal fellBack 1L "the spread fell back"
    Expect.equal spreadTrace [ "7"; "14"; "21"; "28" ] "each effect once, in order"
    Expect.equal spreadTrace serialTrace "as serial"
    Expect.equal (expectOk spread) (expectOk serial) "the same dict"
  }

let private sortByAndMap2 =
  testTask "sortBy and map2Shortest spread and agree with serial" {
    let! state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.List.sortBy(Stdlib.List.range(1, 40), fun i -> {uneven}(i) %% 7)"
        $"Stdlib.List.map2Shortest(Stdlib.List.range(1, 40), Stdlib.List.range(1, 45), fun(a, b) -> {uneven}(a) + b)"
        $"Stdlib.List.map2Shortest(Stdlib.List.range(1, 45), Stdlib.List.range(1, 40), fun(a, b) -> {uneven}(b) + a)" ] do
      let! (spread, moved) = delta spreads (fun () -> run state code)
      let! serial = runWith state false code
      Expect.isGreaterThan moved 0L $"spread: {code}"
      Expect.equal (expectOk spread) (expectOk serial) code
  }

/// Keys of two types raise where the sort compares them, and an error in a key raises there.
let private sortByAndMap2ErrorsAsSerial =
  testTask "sortBy and map2Shortest raise as serially" {
    let! state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.List.sortBy(Stdlib.List.range(1, 40), fun i -> if i == 35 then 1 / 0 else {uneven}(i))"
        $"Stdlib.List.sortBy(Stdlib.List.range(1, 40), fun i -> if i > 20 then \"s\" else {uneven}(i))"
        $"Stdlib.List.map2Shortest(Stdlib.List.range(1, 40), Stdlib.List.range(1, 40), fun(a, b) -> if a == 35 then 1 / 0 else {uneven}(a))"
        $"Stdlib.List.map2Shortest(Stdlib.List.range(1, 40), Stdlib.List.range(1, 40), fun(a, b) -> if a > 20 then \"s\" else {uneven}(a))" ] do
      let! (spread, serial) = both state code
      match spread, serial with
      | Error(e1, s1), Error(e2, s2) ->
        Expect.equal e1 e2 $"the same error: {code}"
        Expect.equal s1 s2 $"the same frames: {code}"
      | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// A search answers its first hit in input order, so the hit is in a late chunk that finishes
/// first and an early chunk that finishes last: the uneven body.
let private searches =
  testTask "any and findFirst spread and answer the first hit in input order" {
    let! state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.List.any(Stdlib.List.range(1, 40), fun i -> {uneven}(i) == 0)"
        $"Stdlib.List.any(Stdlib.List.range(1, 40), fun i -> {uneven}(i) > 0 && i > 25)"
        $"Stdlib.List.findFirst(Stdlib.List.range(1, 40), fun i -> {uneven}(i) > 0 && i %% 9 == 0 && i > 9)"
        $"Stdlib.List.findFirst(Stdlib.List.range(1, 40), fun i -> {uneven}(i) == 0)" ] do
      let! (spread, moved) = delta spreads (fun () -> run state code)
      let! serial = runWith state false code
      Expect.isGreaterThan moved 0L $"spread: {code}"
      Expect.equal (expectOk spread) (expectOk serial) code
  }

/// Serially a search stops at its first hit, so what comes after it is never run: an error after
/// the hit is not raised, and one before it is.
let private searchErrorsAsSerial =
  testTask "any and findFirst raise only what serially they reach" {
    let! state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.List.any(Stdlib.List.range(1, 40), fun i -> if i == 38 then 1 / 0 == 0 else {uneven}(i) > 0 && i == 20)"
        $"Stdlib.List.findFirst(Stdlib.List.range(1, 40), fun i -> if i > 30 then 1 / 0 == 0 else {uneven}(i) > 0 && i == 12)" ] do
      let! (spread, serial) = both state code
      Expect.equal (expectOk spread) (expectOk serial) code
    for code in
      [ $"Stdlib.List.any(Stdlib.List.range(1, 40), fun i -> if i == 20 then 1 / 0 == 0 else {uneven}(i) > 0 && i == 38)"
        $"Stdlib.List.findFirst(Stdlib.List.range(1, 40), fun i -> if i > 20 then \"s\" else {uneven}(i) == 0)" ] do
      let! (spread, serial) = both state code
      match spread, serial with
      | Error(e1, s1), Error(e2, s2) ->
        Expect.equal e1 e2 $"the same error: {code}"
        Expect.equal s1 s2 $"the same frames: {code}"
      | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// An effect after the hit never happens; one before it happens once, in order.
let private searchEffects =
  testTask "a search reaches the effects a serial one does, in order, and no others" {
    let! state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      $"(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.any(Stdlib.List.range(1, 40), fun i -> (let (trace, _) = hidden in if i == 10 || i == 39 then (let _ = trace(Stdlib.toString(i)) in false) else {uneven}(i) > 0 && i == 30)))"
    let! spread = run state code
    let spreadTrace = Trace.take ()
    let! serial = runWith state false code
    let serialTrace = Trace.take ()
    Expect.equal spreadTrace [ "10" ] "the effect before the hit, once"
    Expect.equal spreadTrace serialTrace "as serial"
    Expect.equal (expectOk spread) (expectOk serial) "the same answer"
  }

/// `partition` declares its callback's `Bool`, and maps it with `List.map`. A chunk does not check
/// a type an enclosing fn declared, so from the 36th value down every chunk answers Strings and
/// none fails; serially the first of them raises. `List.map` takes a spread's answers only while
/// their types agree, so the String is met serially, and raises as it does there.
let private enclosingCallbackType =
  testTask "a callback type an enclosing fn declares is checked as serially" {
    let! state = executionStateFor pmPT false
    let code =
      $"Stdlib.List.partition(Stdlib.List.range(1, 40), fun i -> if i < 37 then \"s\" else {uneven}(i) > 0)"
    let! ((spread, serial), moved) = delta spreads (fun () -> both state code)
    Expect.isGreaterThan moved 0L "the map spread"
    match spread, serial with
    | Error(e1, s1), Error(e2, s2) ->
      Expect.equal e1 e2 "the same error"
      Expect.equal s1 s2 "the same frames"
    | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// The failing element is in a cheap late chunk, so it fails long before the expensive chunks
/// ahead of it finish. Serial raises at it; so must this, with the same error and frames, and
/// only after everything before it.
let private errorAsSerial =
  testTask "an error is raised by the element that raises it serially, as serially" {
    let! state = executionStateFor pmPT false
    let code =
      "Stdlib.List.map(Stdlib.List.range(1, 40), fun i -> if i == 35 then 1 / 0 else ("
      + uneven
      + "(i)))"
    let! ((spread, serial), fellBack) = delta fallbacks (fun () -> both state code)
    Expect.equal fellBack 1L "the spread fell back"
    match spread, serial with
    | Error(e1, s1), Error(e2, s2) ->
      Expect.equal e1 e2 "the same error"
      Expect.equal s1 s2 "the same frames"
    | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// A callback whose type an enclosing fn declares (`mapInts`'s `f: Int64 -> Int64`) is checked
/// in a chunk as it is serially. A chunk is another VM, where the check the enclosing fn set up
/// does not apply by itself; without carrying it, the wrong result got through the chunk and
/// failed later with a different error.
let private callbackTypeAsSerial =
  testTask "a callback type an enclosing fn declares is checked as serially" {
    let! state = executionStateFor pmPT false
    let code =
      "Darklang.Language.Tests.Lambdas.mapInts(Stdlib.List.range(1, 40), fun i -> if i == 35 then \"wrong\" else "
      + uneven
      + "(i))"
    let! ((spread, serial), moved) = delta spreads (fun () -> both state code)
    Expect.isGreaterThan moved 0L "the map spread"
    match spread, serial with
    | Error(e1, s1), Error(e2, s2) ->
      Expect.equal e1 e2 "the same error"
      Expect.equal s1 s2 "the same frames"
    | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// A check that learns its type as it goes learns it across chunks as it would serially: `'b`
/// is settled by element 20's `Some(1)`, so element 35's `Some("x")` raises. The chunk holding 35
/// never saw 20, and learns `String` from it on its own; that disagreement sends it back here.
let private checkLearnsAsSerial =
  testTask "a callback check learns its type across chunks as serially" {
    let! state = executionStateFor pmPT false
    let code =
      "Darklang.Language.Tests.Lambdas.mapOptions(Stdlib.List.range(1, 40), fun i -> let _ = "
      + uneven
      + "(i) in if i == 20 then Stdlib.Option.Option.Some(1) else if i == 35 then Stdlib.Option.Option.Some(\"x\") else Stdlib.Option.Option.None)"
    let! ((spread, serial), moved) = delta spreads (fun () -> both state code)
    Expect.isGreaterThan moved 0L "the map spread"
    match spread, serial with
    | Error(e1, s1), Error(e2, s2) ->
      Expect.equal e1 e2 "the same error"
      Expect.equal s1 s2 "the same frames"
    | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// The earliest error wins even when a later one is found first.
let private earliestErrorWins =
  testTask "of two failing elements the earlier one raises, whichever fails first" {
    let! state = executionStateFor pmPT false
    let code =
      "Stdlib.List.map(Stdlib.List.range(1, 40), fun i -> if i == 3 then (let _ = ("
      + uneven
      + "(1)) in 1 / 0) else if i == 38 then i % 0 else i)"
    let! spread = run state code
    let! serial = runWith state false code
    match spread, serial with
    | Error(e1, _), Error(e2, _) -> Expect.equal e1 e2 "the same, earlier, error"
    | _ -> failtest $"both should fail: {spread} / {serial}"
  }

/// `testTrace` is logged (an effect ordinal), so a chunk reaching it is refused and the rest runs
/// here. The trace is the order the effects happened in. The effect is reached through a tuple the
/// body takes apart, which the prediction cannot follow: this is the OBSERVE path, an unknown
/// callable spread and caught by the refusal.
let private effectsKeepOrder =
  testTask "a body that reaches an effect runs it in order, here" {
    let! state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.map(Stdlib.List.range(1, 30), fun i -> (let (trace, _) = hidden in if i % 7 == 0 then (let _ = trace((if i == 7 then \"7\" else if i == 14 then \"14\" else if i == 21 then \"21\" else \"28\")) in i) else i * 2)))"
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

/// A spread remembers a NAMED fn it fell back from, by the name, and does not try it again. The
/// fn is handed over by name rather than wrapped in a lambda, so the second map can only be
/// recognised through the fall-back table's key: a key that stops matching spreads and falls back
/// a second time, which is what this counts.
let private namedFallbackIsRemembered =
  testTask "a named fn that fell back once is not spread again" {
    let! state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      "(let xs = [\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"] in (let _ = Stdlib.List.map(xs, Builtin.testTrace) in Stdlib.List.map(xs, Builtin.testTrace)))"
    let! ((result, started), fellBack) =
      delta fallbacks (fun () -> delta spreads (fun () -> run state code))
    expectOk result |> ignore<RT.Dval>
    Expect.equal started 1L "only the first map spread"
    Expect.equal fellBack 1L "and only it fell back"
    Expect.equal
      (Trace.take ())
      [ "a"; "b"; "c"; "d"; "e"; "f"; "a"; "b"; "c"; "d"; "e"; "f" ]
      "every effect once, in order"
  }

/// A builtin that hands back its callee's failure as a value must let a chunk's refusal through,
/// or the refusal becomes the answer and the spread never falls back (`Spread`).
let private refusalGetsThrough (call : string) =
  testTask $"an effect under {call} inside a spread falls back" {
    let! state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.map(Stdlib.List.range(1, 30), fun i -> (let (trace, _) = hidden in "
      + call
      + "((fun n -> trace(Stdlib.toString(n))), i))))"
    let! ((spread, serial), fellBack) = delta fallbacks (fun () -> both state code)
    let spreadTrace = Trace.take ()
    Expect.equal fellBack 1L "the spread fell back"
    Expect.equal
      (expectOk spread)
      (expectOk serial)
      "the same results as serially, every one Ok"
    Expect.equal
      (List.length spreadTrace)
      60
      "every effect ran once in each of the two runs"
  }

/// Inner maps run in the outer map's chunks, where nothing spreads. The outer probe's first answer
/// comes after three elements (one to warm up, two measured), which run in the original process,
/// so THEIR inner maps spread: four in all, the outer and three inner, never one per element.
let private nestingIsBounded =
  testTask "a map inside a spread map does not spread again" {
    let! state = executionStateFor pmPT false
    let code =
      "Stdlib.List.map(Stdlib.List.range(1, 20), fun i -> Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 50), fun x -> x + i)))"
    let! (result, moved) = delta spreads (fun () -> run state code)
    Expect.equal
      moved
      4L
      "the outer map and the first three elements' inner maps, and no others"
    Expect.equal
      (expectOk result)
      (RT.DList(LibExecution.ValueType.int64, List.replicate 20 (RT.DInt64 50L)))
      "twenty lengths of fifty"
  }

let private watchedRunsDoNotSpread =
  testTask "a recorded or viewed run does not spread" {
    let! (state : RT.ExecutionState) = executionStateFor pmPT false
    // Watched all the way down: a process asks its tracer for its own copy (`forProcess`).
    let tracing : RT.Tracing.Tracing = state.tracing
    let rec watching () : RT.Tracing.Tracing =
      { tracing with collectFrames = true; forProcess = fun _ -> watching () }
    let watched : RT.ExecutionState = { state with tracing = watching () }
    let! (result, moved) =
      delta spreads (fun () ->
        run watched "Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> i)")
    expectOk result |> ignore<RT.Dval>
    Expect.equal moved 0L "nothing spread"
  }

/// Cancelled mid-spread: the map's process ends cancelled, with no list, and the chunks go with
/// it. The stop is the answer: the cancelled chunks are not taken for a fallback, which would run
/// the rest of the map here and only then notice the cancel.
let private cancelledLeavesNothing =
  testTask "a cancelled spread map answers no list and leaves no chunk running" {
    let! state = executionStateFor pmPT false
    let! instrs =
      instrsFor
        "Stdlib.List.map(Stdlib.List.range(1, 16), fun i -> Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 3000000), fun x -> x)))"
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = s.Spawn(state, (None, instrs), Scheduler.EntryExpr, None)
    let saved = Spread.crossover
    Spread.crossover <- 0L
    try
      let before = Spread.spreads
      let fallbacksBefore = Spread.fallbacks
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
      Expect.isTrue (s.Cancel p.id) "the map was still running when it was cancelled"
      let! result = p.completion.Task
      // Not a time limit, which a loaded machine breaks. Before the stop was checked at the
      // fallback, the cancelled chunks were taken for one, and the rest of the map ran here.
      Expect.equal
        (Spread.fallbacks - fallbacksBefore)
        0L
        "the cancel did not fall back to running the rest here"
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
    let! state = executionStateFor pmPT false
    let! instrs =
      instrsFor
        "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 400), fun i -> (let (trace, _) = hidden in if i == 10 then (let _ = trace(\"ten\") in i) else Stdlib.List.fold(Stdlib.List.range(1, 2000), i, fun(a, x) -> a + x)))))"
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
    let! state = executionStateFor pmPT false
    let! instrs =
      instrsFor
        "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 400), fun i -> (let (trace, _) = hidden in (let _ = trace(\"e\") in Stdlib.List.fold(Stdlib.List.range(1, 2000), i, fun(a, x) -> a + x))))))"
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

// -- Prediction --

/// Prediction is off by default (`Spread.predicting`); these tests are about it, so it is on for
/// each of them and put back after.
let private predictingDuring (f : unit -> Task<unit>) : Task<unit> =
  task {
    let saved = Spread.predicting
    Spread.predicting <- true
    try
      do! f ()
    finally
      Spread.predicting <- saved
  }

let private predicted () =
  struct (Spread.predictedPure, Spread.predictedImpure, Spread.predictedUnknown)

/// A state whose package fns are answered by the real analysis over the test store, as the CLI
/// host's are.
let private predictingState () : Task<RT.ExecutionState> =
  task {
    let! (state : RT.ExecutionState) = executionStateFor pmPT false
    return
      { state with
          fnPurity =
            LibDB.PackagePermissions.purity
              LibDB.PackagePermissions.Load.fromStore
              state.fns.builtIn }
  }

/// The effect is in the body for anyone to see, so no chunk is started for it, even though no
/// element ever reaches it. That is the price of predicting: observing would have spread this.
let private visibleEffectStartsNothing =
  testTask "a body that can reach an effect is predicted impure and starts no chunk" {
    do!
      predictingDuring (fun () ->
        task {
          let! state = executionStateFor pmPT false
          let code =
            "Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> (let _ = (if i == 999 then Builtin.testTrace(\"never\") else ()) in i * 2)))"
          let struct (_, impure0, _) = predicted ()
          let! (result, moved) = delta spreads (fun () -> run state code)
          let struct (_, impure1, _) = predicted ()
          expectOk result |> ignore<RT.Dval>
          Expect.equal moved 0L "no spread started"
          Expect.equal (impure1 - impure0) 1L "predicted impure once"
        })
  }

/// The brief's case: a lambda calling a package fn that calls a builtin that writes a file comes
/// back impure, from the analysis over the store, without the write running. And a pure body over
/// package fns and an `eval`'s `+` comes back pure.
let private packageFnsAnsweredByTheStore =
  testTask "a package fn that can write a file is impure; a pure one is pure" {
    do!
      predictingDuring (fun () ->
        task {
          let! state = predictingState ()
          let writes =
            "Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> (let _ = (if i == 999 then Stdlib.Cli.FileSystem.writeFile(\"/tmp/spread-never-written\", Stdlib.Utf8.encode(\"x\")) else Stdlib.Result.Result.Ok()) in i * 2)))"
          let pureBody =
            "Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> Stdlib.List.fold(Stdlib.List.range(1, 50), i, fun(a, x) -> a + x)))"
          let struct (pure0, impure0, _) = predicted ()
          let! (r1, moved1) = delta spreads (fun () -> run state writes)
          let struct (_, impure1, _) = predicted ()
          let! (r2, moved2) = delta spreads (fun () -> run state pureBody)
          let struct (pure2, _, _) = predicted ()
          expectOk r1 |> ignore<RT.Dval>
          expectOk r2 |> ignore<RT.Dval>
          Expect.equal (impure1 - impure0) 1L "the writing body is predicted impure"
          Expect.equal moved1 0L "and starts no chunk"
          Expect.isFalse
            (System.IO.File.Exists "/tmp/spread-never-written")
            "the write never ran"
          Expect.isGreaterThan (pure2 - pure0) 0L "the pure body is predicted pure"
          Expect.isGreaterThan moved2 0L "and spreads"
        })
  }

/// A prediction that is wrong in the optimistic direction costs a spread, not an effect out of
/// order: every package fn is said to be pure, the body reads the clock, and the chunk refuses it
/// and the map falls back.
let private wrongPureFallsBack =
  testTask "a wrong pure prediction falls back like an unknown one" {
    do!
      predictingDuring (fun () ->
        task {
          let! (state : RT.ExecutionState) = executionStateFor pmPT false
          let liar = { state with fnPurity = fun _ -> Ply RT.Purity.Pure }
          let code =
            "Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> (let _ = Stdlib.DateTime.now() in i * 2)))"
          let! (result, fellBack) = delta fallbacks (fun () -> run liar code)
          Expect.equal (expectOk result) (RT.DInt64 100L) "the whole list"
          Expect.equal fellBack 1L "the spread fell back"
        })
  }

/// What one run of `countsAreExact` counted.
type private Counts =
  { spreads : int64; joins : int64; builtinCalls : int64; instructions : int64 }

/// The chunks run at once on other threads. Counting into the spreader's set from all of them
/// lost increments, so the counts moved from run to run: 400 `stringJoin` calls counted as 390
/// to 397. Each chunk counts into a set of its own, added in when it is collected.
let private countsAreExact (op : string) (code : string) =
  testTask $"a spread {op} counts every call and instruction, the same on every run" {
    let! state = executionStateFor pmPT false
    let! instrs = instrsFor code
    let counted (spread : bool) : Task<Counts> =
      task {
        let stats = RT.InterpreterStats.ForOneVM()
        let! (result, moved) =
          delta spreads (fun () ->
            runInstrsCounting (Some stats) state spread instrs)
        expectOk result |> ignore<RT.Dval>
        let calls name =
          match stats.builtinCallsByName.TryGetValue name with
          | true, n -> n
          | false, _ -> 0L
        return
          { spreads = moved
            joins = calls "stringJoin"
            builtinCalls = stats.builtinCallCount
            instructions = stats.instructionCount }
      }
    let! serial = counted false
    Expect.equal serial.spreads 0L "the serial run did not spread"
    Expect.equal serial.joins 400L "serial: one join per element"
    let! first = counted true
    Expect.isGreaterThan first.spreads 0L "the map spread"
    Expect.equal first.joins 400L "one join per element, spread"
    // A chunk is one more builtin call, over its part of the list, applied by a
    // three-instruction program (`Execution.instructionsForApply`). Beyond that, a spread run
    // does exactly the serial run's work.
    let chunks = first.builtinCalls - serial.builtinCalls
    Expect.isGreaterThan chunks 1L "more than one chunk"
    Expect.equal
      first.instructions
      (serial.instructions + 3L * chunks)
      "instructions: serial's, plus each chunk's three"
    for _ in 1..10 do
      let! again = counted true
      Expect.equal again first "the same counts on every run"
  }

let tests =
  testSequenced (
    testList
      "spread"
      [ inOrder
        siblings
        dictOps
        enclosingCallbackType
        cheapDictBodyStaysSerial
        dictErrorsAsSerial
        dictCallbackTypeAsSerial
        dictEffectsKeepOrder
        sortByAndMap2
        sortByAndMap2ErrorsAsSerial
        searches
        searchErrorsAsSerial
        searchEffects
        errorAsSerial
        earliestErrorWins
        callbackTypeAsSerial
        checkLearnsAsSerial
        effectsKeepOrder
        namedFallbackIsRemembered
        refusalGetsThrough "Darklang.LanguageTools.AtRestTypeChecker.guard"
        refusalGetsThrough "Stdlib.Live.apply"
        nestingIsBounded
        watchedRunsDoNotSpread
        cancelledLeavesNothing
        fallbackIsPreempted
        syncFallbackIsPreempted
        visibleEffectStartsNothing
        packageFnsAnsweredByTheStore
        wrongPureFallsBack
        countsAreExact
          "map"
          "Stdlib.List.map(Stdlib.List.range(1, 400), fun i -> Stdlib.String.join(Stdlib.List.map(Stdlib.List.range(1, 20), fun j -> Stdlib.toString(i + j)), \",\"))"
        countsAreExact
          "Dict.map"
          "Stdlib.Dict.map(Stdlib.Dict.fromEntries(Stdlib.List.zipShortest(Stdlib.List.range(1, 400), Stdlib.List.range(1, 400))), fun(k, v) -> Stdlib.String.join(Stdlib.List.map(Stdlib.List.range(1, 20), fun j -> Stdlib.toString(k + j)), \",\"))" ]
  )
