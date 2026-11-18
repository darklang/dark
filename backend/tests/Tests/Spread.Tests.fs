/// List ops spreading across cores by themselves (`LibExecution.Spread`).
///
/// Every test here forces the decision (`crossover = 0`) so that it is about what a spread
/// DOES, and asserts that a spread happened, since every result below is also what a serial run
/// returns: a test that only compared values would pass with spreading broken or off.
module Tests.Spread

open System.Threading
open System.Threading.Tasks

open Expecto
open Fumble
open LibDB.Sqlite
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Scheduler = LibExecution.Scheduler
module Spread = LibExecution.Spread
module Trace = TestUtils.LibTest.Trace

let private instrsFor (code : string) : Task<RT.Instructions> =
  task {
    let ptExpr = parsePTExpr code
    return PT2RT.Expr.toRT Map.empty 0 None ptExpr
  }

/// `state` with spreading forced (crossover 0, and one element a chunk where there are cores for
/// it, so order and failure are exercised across many chunks rather than within one) or off.
let private spreading
  (spread : bool)
  (state : RT.ExecutionState)
  : RT.ExecutionState =
  { state with
      spread = { state.spread with on = spread; crossover = 0L; minChunk = 0L } }

/// Run `instrs` as a process on a fresh scheduler, its VM counting into `stats` when given.
let private runProcess
  (stats : Option<RT.InterpreterStats>)
  (state : RT.ExecutionState)
  (instrs : RT.Instructions)
  : Task<RT.ExecutionResult> =
  task {
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = s.Spawn(state, (None, instrs), Scheduler.EntryExpr, None, ?stats = stats)
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
  }

/// Run `instrs` as a process on a fresh scheduler, with spreading forced or off.
let private runInstrsCounting
  (stats : Option<RT.InterpreterStats>)
  (state : RT.ExecutionState)
  (spread : bool)
  (instrs : RT.Instructions)
  : Task<RT.ExecutionResult> =
  runProcess stats (spreading spread state) instrs


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

let private spreads (state : RT.ExecutionState) () =
  state.spread.counts.Get RT.SpreadCount.Spreads

let private fallbacks (state : RT.ExecutionState) () =
  state.spread.counts.Get RT.SpreadCount.Fallbacks

/// A body whose cost falls steeply with the element: the first elements cost the most, so the
/// first chunk finishes LAST. A spread that collected chunks as they finished would put the cheap
/// tail first.
let private uneven =
  "(fun i -> (let n = (41 - i) * (41 - i) * 20 in Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, n), fun x -> x)) + i))"

let private inOrder =
  testTask "results come back in input order when the first chunk finishes last" {
    let state = executionStateFor pmPT false
    let code = $"Stdlib.List.map(Stdlib.List.range(1, 40), {uneven})"
    let! (spread, moved) = delta (spreads state) (fun () -> run state code)
    let! serial = runWith state false code
    Expect.isGreaterThan moved 0L "the map spread"
    Expect.equal (expectOk spread) (expectOk serial) "the same list as serial"
  }

let private siblings =
  testTask "filter, filterMap and indexedMap spread and agree with serial" {
    let state = executionStateFor pmPT false
    for code in
      [ "Stdlib.List.filter(Stdlib.List.range(1, 100), fun i -> i % 3 == 0)"
        "Stdlib.List.filterMap(Stdlib.List.range(1, 100), fun i -> if i % 4 == 0 then Stdlib.Option.Option.Some(i * 2) else Stdlib.Option.Option.None)"
        "Stdlib.List.indexedMap(Stdlib.List.range(1, 100), fun(idx, i) -> (idx, i * 10))" ] do
      let! (spread, moved) = delta (spreads state) (fun () -> run state code)
      let! serial = runWith state false code
      Expect.isGreaterThan moved 0L $"spread: {code}"
      Expect.equal (expectOk spread) (expectOk serial) code
  }

/// A dict of `n` entries keyed and valued 1 to `n`, built without a list op that could spread.
let private dictOf (n : int) =
  $"Stdlib.Dict.fromEntries(Stdlib.List.zipShortest(Stdlib.List.range(1, {n}), Stdlib.List.range(1, {n})))"

let private dictOps =
  testTask "Dict.map and Dict.filter spread and agree with serial" {
    let state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.Dict.map({dictOf 40}, fun(k, v) -> {uneven}(k) + v)"
        $"Stdlib.Dict.filter({dictOf 40}, fun(k, v) -> ({uneven}(k) + v) %% 3 == 0)" ] do
      let! (spread, moved) = delta (spreads state) (fun () -> run state code)
      let! serial = runWith state false code
      Expect.isGreaterThan moved 0L $"spread: {code}"
      Expect.equal (expectOk spread) (expectOk serial) code
  }

/// At the shipped crossover an op spreads only a body that costs at least
/// `Spread.minPerElement`: a one-operation body stays serial however long the collection, and an
/// expensive one spreads.
let private cheapBodyStaysSerial =
  testTask "at the shipped crossover a cheap body stays serial" {
    let state = executionStateFor pmPT false
    let long = "Stdlib.List.range(1, 20000)"
    for code in
      [ $"Stdlib.List.length(Stdlib.List.map({long}, fun x -> x + 1))"
        $"Stdlib.List.length(Stdlib.List.filter({long}, fun x -> x > 5))"
        $"Stdlib.List.length(Stdlib.List.filterMap({long}, fun x -> Stdlib.Option.Option.Some(x)))"
        $"Stdlib.List.length(Stdlib.List.indexedMap({long}, fun(i, x) -> i + x))"
        $"Stdlib.Dict.size(Stdlib.Dict.map({dictOf 20000}, fun(k, v) -> v + 1))" ] do
      let! cheap = instrsFor code
      let! (_, cheapSpreads) =
        delta (spreads state) (fun () -> runProcess None state cheap)
      Expect.equal cheapSpreads 0L $"a cheap body did not spread: {code}"
    let! costly =
      instrsFor
        $"Stdlib.Dict.size(Stdlib.Dict.map({dictOf 40}, fun(k, v) -> {uneven}(k)))"
    let! (_, costlySpreads) =
      delta (spreads state) (fun () -> runProcess None state costly)
    Expect.isGreaterThan costlySpreads 0L "an expensive one did"
  }

/// `Dict.filter` checks its answers once they are all in, so a later entry's error comes before
/// an earlier entry's non-Bool. `Stdlib.Dict.filter` declares its callback's `Bool`, so a non-Bool
/// raises where it is returned, in a chunk as serially: from half way on, every chunk answers
/// non-Bools. And a map whose answers change type half way raises when the dict is built.
let private dictErrorsAsSerial =
  testTask "Dict.map and Dict.filter raise as serially" {
    let state = executionStateFor pmPT false
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
    let state = executionStateFor pmPT false
    let code =
      $"Darklang.Language.Tests.Lambdas.mapDictInts({dictOf 40}, fun(k, v) -> if k == 35 then \"wrong\" else {uneven}(k) + v)"
    let! ((spread, serial), moved) =
      delta (spreads state) (fun () -> both state code)
    Expect.isGreaterThan moved 0L "the map spread"
    match spread, serial with
    | Error(e1, s1), Error(e2, s2) ->
      Expect.equal e1 e2 "the same error"
      Expect.equal s1 s2 "the same frames"
    | _ -> failtest $"both should fail: {spread} / {serial}"
  }

let private dictEffectsKeepOrder =
  testTask "a Dict.map body that reaches an effect runs it in order, here" {
    let state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      $"(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.Dict.map({dictOf 30}, fun(k, v) -> (let (trace, _) = hidden in if k %% 7 == 0 then (let _ = trace(Stdlib.display(k)) in v) else v * 2)))"
    let! (spread, fellBack) = delta (fallbacks state) (fun () -> run state code)
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
    let state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.List.sortBy(Stdlib.List.range(1, 40), fun i -> {uneven}(i) %% 7)"
        $"Stdlib.List.map2Shortest(Stdlib.List.range(1, 40), Stdlib.List.range(1, 45), fun(a, b) -> {uneven}(a) + b)"
        $"Stdlib.List.map2Shortest(Stdlib.List.range(1, 45), Stdlib.List.range(1, 40), fun(a, b) -> {uneven}(b) + a)" ] do
      let! (spread, moved) = delta (spreads state) (fun () -> run state code)
      let! serial = runWith state false code
      Expect.isGreaterThan moved 0L $"spread: {code}"
      Expect.equal (expectOk spread) (expectOk serial) code
  }

/// Keys of two types raise where the sort compares them, and an error in a key raises there.
let private sortByAndMap2ErrorsAsSerial =
  testTask "sortBy and map2Shortest raise as serially" {
    let state = executionStateFor pmPT false
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

let private filterErrorsAsSerial =
  testTask "filter and filterMap raise as serially" {
    let state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.List.filter(Stdlib.List.range(1, 40), fun i -> if i == 35 then 1 / 0 == 0 else {uneven}(i) > 0)"
        $"Stdlib.List.filter(Stdlib.List.range(1, 40), fun i -> if i > 20 then \"s\" else {uneven}(i) > 0)"
        $"Stdlib.List.filterMap(Stdlib.List.range(1, 40), fun i -> if i == 35 then Stdlib.Option.Option.Some(1 / 0) else Stdlib.Option.Option.Some({uneven}(i)))" ] do
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
    let state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.List.any(Stdlib.List.range(1, 40), fun i -> {uneven}(i) == 0)"
        $"Stdlib.List.any(Stdlib.List.range(1, 40), fun i -> {uneven}(i) > 0 && i > 25)"
        $"Stdlib.List.findFirst(Stdlib.List.range(1, 40), fun i -> {uneven}(i) > 0 && i %% 9 == 0 && i > 9)"
        $"Stdlib.List.findFirst(Stdlib.List.range(1, 40), fun i -> {uneven}(i) == 0)" ] do
      let! (spread, moved) = delta (spreads state) (fun () -> run state code)
      let! serial = runWith state false code
      Expect.isGreaterThan moved 0L $"spread: {code}"
      Expect.equal (expectOk spread) (expectOk serial) code
  }

/// Serially a search stops at its first hit, so what comes after it is never run: an error after
/// the hit is not raised, and one before it is.
let private searchErrorsAsSerial =
  testTask "any and findFirst raise only what serially they reach" {
    let state = executionStateFor pmPT false
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
    let state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      $"(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.any(Stdlib.List.range(1, 40), fun i -> (let (trace, _) = hidden in if i == 10 || i == 39 then (let _ = trace(Stdlib.display(i)) in false) else {uneven}(i) > 0 && i == 30)))"
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
    let state = executionStateFor pmPT false
    let code =
      $"Stdlib.List.partition(Stdlib.List.range(1, 40), fun i -> if i < 37 then \"s\" else {uneven}(i) > 0)"
    let! ((spread, serial), moved) =
      delta (spreads state) (fun () -> both state code)
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
    let state = executionStateFor pmPT false
    let code =
      "Stdlib.List.map(Stdlib.List.range(1, 40), fun i -> if i == 35 then 1 / 0 else ("
      + uneven
      + "(i)))"
    let! ((spread, serial), fellBack) =
      delta (fallbacks state) (fun () -> both state code)
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
    let state = executionStateFor pmPT false
    let code =
      "Darklang.Language.Tests.Lambdas.mapInts(Stdlib.List.range(1, 40), fun i -> if i == 35 then \"wrong\" else "
      + uneven
      + "(i))"
    let! ((spread, serial), moved) =
      delta (spreads state) (fun () -> both state code)
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
    let state = executionStateFor pmPT false
    let code =
      "Darklang.Language.Tests.Lambdas.mapOptions(Stdlib.List.range(1, 40), fun i -> let _ = "
      + uneven
      + "(i) in if i == 20 then Stdlib.Option.Option.Some(1) else if i == 35 then Stdlib.Option.Option.Some(\"x\") else Stdlib.Option.Option.None)"
    let! ((spread, serial), moved) =
      delta (spreads state) (fun () -> both state code)
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
    let state = executionStateFor pmPT false
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
    let state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.map(Stdlib.List.range(1, 30), fun i -> (let (trace, _) = hidden in if i % 7 == 0 then (let _ = trace((if i == 7 then \"7\" else if i == 14 then \"14\" else if i == 21 then \"21\" else \"28\")) in i) else i * 2)))"
    let! (spread, fellBack) = delta (fallbacks state) (fun () -> run state code)
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
    let state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      "(let xs = [\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"] in (let _ = Stdlib.List.map(xs, Builtin.testTrace) in Stdlib.List.map(xs, Builtin.testTrace)))"
    let! ((result, started), fellBack) =
      delta (fallbacks state) (fun () ->
        delta (spreads state) (fun () -> run state code))
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
    let state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.map(Stdlib.List.range(1, 30), fun i -> (let (trace, _) = hidden in "
      + call
      + "((fun n -> trace(Stdlib.display(n))), i))))"
    let! ((spread, serial), fellBack) =
      delta (fallbacks state) (fun () -> both state code)
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
    let state = executionStateFor pmPT false
    let code =
      "Stdlib.List.map(Stdlib.List.range(1, 20), fun i -> Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 50), fun x -> x + i)))"
    let! (result, moved) = delta (spreads state) (fun () -> run state code)
    Expect.equal
      moved
      4L
      "the outer map and the first three elements' inner maps, and no others"
    Expect.equal
      (expectOk result)
      (RT.DList(
        LibExecution.ValueType.int64,
        RT.Items(List.replicate 20 (RT.DInt64 50L))
      ))
      "twenty lengths of fifty"
  }

let private viewedRunsDoNotSpread =
  testTask "a viewed run does not spread" {
    let (state : RT.ExecutionState) = executionStateFor pmPT false
    // Viewed all the way down: a process asks its tracer for its own copy (`forProcess`).
    let tracing : RT.Tracing.Tracing = state.tracing
    let rec viewing () : RT.Tracing.Tracing =
      { tracing with
          collectFrames = true
          collectExprValues = true
          forProcess = fun _ -> viewing () }
    let viewed : RT.ExecutionState = { state with tracing = viewing () }
    let! (result, moved) =
      delta (spreads state) (fun () ->
        run viewed "Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> i)")
    expectOk result |> ignore<RT.Dval>
    Expect.equal moved 0L "nothing spread"
  }

/// Spawned on `s` and run on a thread of its own, with spreading forced.
let private startForced
  (s : Scheduler.Scheduler)
  (state : RT.ExecutionState)
  (instrs : RT.Instructions)
  : Scheduler.Process =
  let p = s.Spawn(spreading true state, (None, instrs), Scheduler.EntryExpr, None)
  let thread =
    Thread(
      (fun () -> s.RunUntil p |> ignore<RT.ExecutionResult>),
      IsBackground = true
    )
  thread.Start()
  p

/// Cancelled mid-spread: the map's process ends cancelled, with no list, and the chunks go with
/// it. The stop is the answer: the cancelled chunks are not taken for a fallback, which would run
/// the rest of the map here and only then notice the cancel.
let private cancelledLeavesNothing =
  testTask "a cancelled spread map answers no list and leaves no chunk running" {
    let state = executionStateFor pmPT false
    let! instrs =
      instrsFor
        "Stdlib.List.map(Stdlib.List.range(1, 16), fun i -> Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 3000000), fun x -> x)))"
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = startForced s state instrs
    let deadline = System.DateTime.UtcNow.AddSeconds 30.
    while spreads state () = 0L && System.DateTime.UtcNow < deadline do
      Thread.Sleep 5
    Expect.isGreaterThan (spreads state ()) 0L "the map spread"
    Expect.isTrue (s.Cancel p.id) "the map was still running when it was cancelled"
    let! result = p.completion.Task
    // Not a time limit, which a loaded machine breaks. The stop is checked at the fallback,
    // so cancelled chunks are not taken for one and the rest of the map does not run here.
    Expect.equal
      (fallbacks state ())
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
  }

/// After a fallback the rest runs here as an ordinary serial map, in slices like any other
/// work: a fallback must not turn into one long uninterruptible step.
let private fallbackIsPreempted =
  testTask "the serial rest after a fallback is preempted like any map" {
    let state = executionStateFor pmPT false
    let! instrs =
      instrsFor
        "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 400), fun i -> (let (trace, _) = hidden in if i == 10 then (let _ = trace(\"ten\") in i) else Stdlib.List.fold(Stdlib.List.range(1, 2000), i, fun(a, x) -> a + x)))))"
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = startForced s state instrs
    let! result = p.completion.Task
    Trace.take () |> ignore<List<string>>
    expectOk result |> ignore<RT.Dval>
    Expect.equal (fallbacks state ()) 1L "it fell back"
    Expect.isGreaterThan p.slices 20L "the serial rest ran in many slices"
  }

/// Every element reaches an effect, so the first chunk fails at once, usually before the spreader
/// has even finished starting the others: the fallback then continues inside the same step.
let private syncFallbackIsPreempted =
  testTask "a fallback that lands at once is preempted like any map" {
    let state = executionStateFor pmPT false
    let! instrs =
      instrsFor
        "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 400), fun i -> (let (trace, _) = hidden in (let _ = trace(\"e\") in Stdlib.List.fold(Stdlib.List.range(1, 2000), i, fun(a, x) -> a + x))))))"
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let p = startForced s state instrs
    let! result = p.completion.Task
    Trace.take () |> ignore<List<string>>
    expectOk result |> ignore<RT.Dval>
    Expect.equal (fallbacks state ()) 1L "it fell back"
    Expect.isGreaterThan p.slices 20L "the serial rest ran in many slices"
  }

// -- Prediction --

/// Prediction is off by default (`SpreadSettings.predict`); these tests are about it.
let private predicting (state : RT.ExecutionState) : RT.ExecutionState =
  { state with spread = { state.spread with predict = true } }

let private predicted (state : RT.ExecutionState) =
  let get = state.spread.counts.Get
  struct (get RT.SpreadCount.PredictedPure,
          get RT.SpreadCount.PredictedImpure,
          get RT.SpreadCount.PredictedUnknown)

/// A state whose package fns are answered by the real analysis over the test store, as the CLI
/// host's are.
let private predictingState () : Task<RT.ExecutionState> =
  task {
    let (state : RT.ExecutionState) = executionStateFor pmPT false
    return
      predicting
        { state with
            fnPurity =
              LibDB.PackagePermissions.purity
                LibDB.PackagePermissions.Load.fromStore
                state.fns.builtIn
                state.spread.counts }
  }

/// The effect is in the body for anyone to see, so no chunk is started for it, even though no
/// element ever reaches it. That is the price of predicting: observing would have spread this.
let private visibleEffectStartsNothing =
  testTask "a body that can reach an effect is predicted impure and starts no chunk" {
    let state = executionStateFor pmPT false
    let state = predicting state
    let code =
      "Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> (let _ = (if i == 999 then Builtin.testTrace(\"never\") else ()) in i * 2)))"
    let struct (_, impure0, _) = predicted state
    let! (result, moved) = delta (spreads state) (fun () -> run state code)
    let struct (_, impure1, _) = predicted state
    expectOk result |> ignore<RT.Dval>
    Expect.equal moved 0L "no spread started"
    Expect.equal (impure1 - impure0) 1L "predicted impure once"
  }

/// A lambda calling a package fn that calls a builtin that writes a file comes back impure, from
/// the analysis over the store, without the write running. And a pure body over package fns
/// comes back pure.
let private packageFnsAnsweredByTheStore =
  testTask "a package fn that can write a file is impure; a pure one is pure" {
    let! state = predictingState ()
    let writes =
      "Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> (let _ = (if i == 999 then Stdlib.Cli.FileSystem.writeFile(\"/tmp/spread-never-written\", Stdlib.Utf8.encode(\"x\")) else Stdlib.Result.Result.Ok()) in i * 2)))"
    let pureBody =
      "Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> Stdlib.List.fold(Stdlib.List.range(1, 50), i, fun(a, x) -> a + x)))"
    let struct (pure0, impure0, _) = predicted state
    let! (r1, moved1) = delta (spreads state) (fun () -> run state writes)
    let struct (_, impure1, _) = predicted state
    let! (r2, moved2) = delta (spreads state) (fun () -> run state pureBody)
    let struct (pure2, _, _) = predicted state
    expectOk r1 |> ignore<RT.Dval>
    expectOk r2 |> ignore<RT.Dval>
    Expect.equal (impure1 - impure0) 1L "the writing body is predicted impure"
    Expect.equal moved1 0L "and starts no chunk"
    Expect.isFalse
      (System.IO.File.Exists "/tmp/spread-never-written")
      "the write never ran"
    Expect.isGreaterThan (pure2 - pure0) 0L "the pure body is predicted pure"
    Expect.isGreaterThan moved2 0L "and spreads"
  }

/// A prediction that is wrong in the optimistic direction costs a spread, not an effect out of
/// order: every package fn is said to be pure, the body reads the clock, and the chunk refuses it
/// and the map falls back.
let private wrongPureFallsBack =
  testTask "a wrong pure prediction falls back like an unknown one" {
    let (state : RT.ExecutionState) = executionStateFor pmPT false
    let liar = predicting { state with fnPurity = fun _ -> RT.Purity.Pure }
    let code =
      "Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 100), fun i -> (let _ = Stdlib.DateTime.now() in i * 2)))"
    let! (result, fellBack) = delta (fallbacks liar) (fun () -> run liar code)
    Expect.equal (expectOk result) (RT.DInt64 100L) "the whole list"
    Expect.equal fellBack 1L "the spread fell back"
  }

/// What one run of `countsAreExact` counted.
type private Counts =
  { spreads : int64; joins : int64; builtinCalls : int64; instructions : int64 }

/// The chunks run at once on other threads, and counting into the spreader's set from all of them
/// would lose increments. Each chunk counts into a set of its own, added in when it is collected.
let private countsAreExact (op : string) (code : string) =
  testTask
    $"a spread {op} counts every call and instruction a serial run does, on any number of workers" {
    let state = executionStateFor pmPT false
    let! instrs = instrsFor code
    let counted (spread : bool) : Task<Counts> =
      task {
        let stats = RT.InterpreterStats.ForOneVM()
        let! (result, moved) =
          delta (spreads state) (fun () ->
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
    // However the work is split, and however many workers there are to split it over, a spread
    // run counts exactly the serial run's work.
    let before = Scheduler.defaultWorkers
    try
      for workers in [ 2; 4; 16; before ] do
        Scheduler.defaultWorkers <- workers
        for _ in 1..3 do
          let! spread = counted true
          Expect.isGreaterThan spread.spreads 0L $"the map spread, {workers} workers"
          Expect.equal
            { spread with spreads = 0L }
            serial
            $"the serial run's counts, {workers} workers"
    finally
      Scheduler.defaultWorkers <- before
  }

// -- Guarantees (`docs/processes.md`, "Computation spreads by itself") --

/// What a tracer is told, in order.
[<RequireQualifiedAccess>]
type private Hook =
  | Frame of id : int * parent : int * at : RT.ExecutionPoint * args : List<RT.Dval>
  | Noted of noted : RT.Hash
  | Entered of entered : RT.Hash
  | Left of left : RT.Hash
  | Call of
    name : RT.FQFnName.FQFnName *
    ord : int64 *
    args : List<RT.Dval> *
    result : RT.Dval

/// Where a frame runs, as a trace keys it: a lambda by its own expression, not by the point it
/// was applied from, which a VM derives from whatever frame it last had (`pushLambdaFrame`'s
/// cache) and no trace stores.
let private site (at : RT.ExecutionPoint) : RT.ExecutionPoint =
  match at with
  | RT.ExecutionPoint.Lambda(_, id, writtenIn) ->
    RT.ExecutionPoint.Lambda(RT.ExecutionPoint.Source, id, writtenIn)
  | other -> other

[<RequireQualifiedAccess>]
type private Seen =
  | Frame of
    id : System.Guid *
    parent : System.Guid *
    at : RT.ExecutionPoint *
    args : List<RT.Dval>
  | Noted of noted : RT.Hash
  | Entered of entered : RT.Hash
  | Left of left : RT.Hash

/// A chunk's trace for `logging` (`Tracing.forChunk`): every hook a pure run calls, kept in order
/// and told to `parent` when taken, each frame with the id the spreading VM gives it and the
/// chunk's outermost frames under the frame that spread.
let private bufferedChunk (parent : RT.Tracing.Tracing) : RT.Tracing.ChunkTrace =
  let seen = ResizeArray<Seen>()
  let rec tracing () : RT.Tracing.Tracing =
    { parent with
        storeFrameEntry =
          fun id parentId at args -> seen.Add(Seen.Frame(id, parentId, at, args))
        noteFunction = fun h -> seen.Add(Seen.Noted h)
        enterFunction = fun h -> seen.Add(Seen.Entered h)
        leaveFunction = fun h -> seen.Add(Seen.Left h)
        forProcess = fun _ -> tracing () }
  let take (nextId : unit -> System.Guid) (spreading : System.Guid) =
    let ids = System.Collections.Generic.Dictionary<System.Guid, System.Guid>()
    for s in seen do
      match s with
      | Seen.Frame(id, parentId, at, args) ->
        let fresh = nextId ()
        ids.Add(id, fresh)
        let under =
          match ids.TryGetValue parentId with
          | true, p -> p
          | false, _ -> spreading
        parent.storeFrameEntry fresh under at args
      | Seen.Noted h -> parent.noteFunction h
      | Seen.Entered h -> parent.enterFunction h
      | Seen.Left h -> parent.leaveFunction h
  { tracing = tracing (); take = take }

/// A recording tracer that keeps every hook it is called with, the frames numbered by first
/// appearance (a root frame's id is random). The process it is first handed to logs; anything
/// that process spawns is recorded too, but not logged.
let private logging () : RT.Tracing.Tracing * (unit -> List<Hook>) =
  let log = ResizeArray<Hook>()
  let ids = System.Collections.Generic.Dictionary<System.Guid, int>()
  let ordinal = [| 0L |]
  let number (id : System.Guid) =
    match ids.TryGetValue id with
    | true, n -> n
    | false, _ ->
      ids.Add(id, ids.Count)
      ids.Count - 1
  let add (h : unit -> Hook) = lock log (fun () -> log.Add(h ()))
  let rec quiet () : RT.Tracing.Tracing =
    { LibExecution.Execution.noTracing with
        collectFrames = true
        forProcess = fun _ -> quiet () }
  let rec logs () : RT.Tracing.Tracing =
    { LibExecution.Execution.noTracing with
        collectFrames = true
        traceEffects = true
        storeFrameEntry =
          fun id parent at args ->
            add (fun () -> Hook.Frame(number id, number parent, site at, args))
        noteFunction = fun h -> add (fun () -> Hook.Noted h)
        enterFunction = fun h -> add (fun () -> Hook.Entered h)
        leaveFunction = fun h -> add (fun () -> Hook.Left h)
        nextEffect =
          fun () ->
            lock log (fun () ->
              let n = ordinal[0]
              ordinal[0] <- n + 1L
              n)
        storeFnResult =
          fun name meta args result ->
            add (fun () -> Hook.Call(name, meta.ord, NEList.toList args, result))
        forProcess = fun _ -> quiet ()
        forChunk = fun () -> bufferedChunk (logs ()) }
  { logs () with forProcess = fun _ -> logs () },
  (fun () -> lock log (fun () -> List.ofSeq log))

/// A run recorded hook by hook, by `runner`: its outcome, what it printed, and what the tracer
/// was told.
let private recordedBy
  (runner : RT.ExecutionState -> bool -> RT.Instructions -> Task<RT.ExecutionResult>)
  (state : RT.ExecutionState)
  (spread : bool)
  (instrs : RT.Instructions)
  : Task<RT.ExecutionResult * List<string> * List<Hook>> =
  task {
    Trace.take () |> ignore<List<string>>
    let tracing, hooks = logging ()
    let! result = runner { state with tracing = tracing } spread instrs
    return result, Trace.take (), hooks ()
  }

let private recorded = recordedBy runInstrs

/// Two recorded runs agree: outcome, effects, and every hook, the first that differs named.
let private expectSameRun
  (what : string)
  (actual : RT.ExecutionResult * List<string> * List<Hook>)
  (expected : RT.ExecutionResult * List<string> * List<Hook>)
  : unit =
  let (r1, p1, h1) = actual
  let (r2, p2, h2) = expected
  Expect.equal r1 r2 $"the same outcome: {what}"
  Expect.equal p1 p2 $"the same effects: {what}"
  match
    List.zip
      (List.truncate (min h1.Length h2.Length) h1)
      (List.truncate (min h1.Length h2.Length) h2)
    |> List.tryFindIndex (fun (a, b) -> a <> b)
  with
  | Some i ->
    let around (h : List<Hook>) = h |> List.skip (max 0 (i - 2)) |> List.truncate 5
    failtest
      $"the same trace: {what}: hook {i} of {h1.Length}/{h2.Length} differs.\nspread: %A{around h1}\nserial: %A{around h2}"
  | None -> Expect.equal h1.Length h2.Length $"the same number of hooks: {what}"

/// A recorded run spreads, and records what a serial run records: frames, numbered and nested
/// as serially, functions entered and left, and effects in order.
let private recordedAsSerial =
  testTask "a recorded run spreads and records what a serial run records" {
    let state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.List.map(Stdlib.List.range(1, 40), {uneven})"
        $"Stdlib.List.findFirst(Stdlib.List.range(1, 40), fun i -> {uneven}(i) > 0 && i %% 9 == 0 && i > 9)"
        $"Stdlib.Dict.map({dictOf 40}, fun(k, v) -> {uneven}(k) + v)"
        $"Stdlib.List.map(Stdlib.List.range(1, 40), fun i -> if i == 35 then 1 / 0 else {uneven}(i))"
        "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.map(Stdlib.List.range(1, 30), fun i -> (let (trace, _) = hidden in if i % 7 == 0 then (let _ = trace(Stdlib.display(i)) in i) else Stdlib.List.fold(Stdlib.List.range(1, 50), i, fun(a, x) -> a + x))))" ] do
      let! instrs = instrsFor code
      let! (spread, moved) =
        delta (spreads state) (fun () -> recorded state true instrs)
      let! serial = recorded state false instrs
      let (_, _, hooks) = spread
      Expect.isGreaterThan moved 0L $"spread while recorded: {code}"
      Expect.isGreaterThan (List.length hooks) 40 $"something recorded: {code}"
      expectSameRun code spread serial
  }

/// The recorder the CLI uses, end to end: what it stores of a spread run is what it stores of a
/// serial one.
let private storedAsSerial =
  testTask "a recorded run that spreads stores the trace a serial run stores" {
    let (state : RT.ExecutionState) = executionStateFor pmPT false
    let! instrs =
      instrsFor
        "(let _ = Builtin.testTrace(\"before\") in (let xs = Stdlib.List.map(Stdlib.List.range(1, 40), fun i -> Stdlib.List.fold(Stdlib.List.range(1, 300), i, fun(a, x) -> a + x)) in (let _ = Builtin.testTrace(\"after\") in xs)))"
    LibDB.Tracing.TraceDetail.set LibDB.Tracing.TraceDetail.On
    try
      let record (spread : bool) =
        task {
          let traceId = LibExecution.AnalysisTypes.TraceID.create ()
          let tracer =
            LibDB.Tracing.createCliTracer
              RT.Tracing.defaultCaps
              traceId
              "spread test"
              "expression"
              RT.DUnit
          let traced = { state with tracing = tracer.executionTracing }
          let! (result, moved) =
            delta (spreads state) (fun () -> runInstrs traced spread instrs)
          tracer.storeTraceResults traced
          let rows (sql : string) =
            Sql.query sql
            |> Sql.parameters [ "t", Sql.string (string traceId) ]
            |> Sql.executeRows (fun read -> read.string "a", read.int64 "b")
          let loops =
            rows
              "SELECT call_site AS a, passes AS b FROM trace_loops WHERE trace_id = @t ORDER BY call_site"
          let fns =
            rows
              "SELECT fn_hash AS a, calls AS b FROM trace_fns WHERE trace_id = @t ORDER BY fn_hash"
          let calls =
            rows
              "SELECT fn_hash AS a, ord AS b FROM trace_fn_calls WHERE trace_id = @t ORDER BY seq"
          return result, moved, loops, fns, calls
        }
      let! (r1, moved1, loops1, fns1, calls1) = record true
      let! (r2, moved2, loops2, fns2, calls2) = record false
      Expect.isGreaterThan moved1 0L "the recorded run spread"
      Expect.equal moved2 0L "the other did not"
      Expect.equal (expectOk r1) (expectOk r2) "the same list"
      Expect.isNonEmpty loops1 "loops were counted"
      Expect.isNonEmpty fns1 "functions were counted"
      Expect.equal (List.length calls1) 2 "both effects were logged"
      Expect.equal loops1 loops2 "the same passes of every loop"
      Expect.equal fns1 fns2 "the same calls of every function"
      Expect.equal calls1 calls2 "the same effects, in order"
    finally
      Trace.take () |> ignore<List<string>>
      LibDB.Tracing.TraceDetail.set LibDB.Tracing.TraceDetail.Off
  }

/// A self tail call and a fold's later steps run in the frame they are in, even while recorded,
/// but the recorder is told of each call as if it had been pushed: a frame and its arguments.
let private inPlaceCallsAreRecorded =
  testTask
    "a recorded run is told of every call that runs in place, with its arguments" {
    let state = executionStateFor pmPT false
    let ints (xs : List<int64>) = xs |> List.map RT.DInt64
    // Frames given two ints: the calls under test, not the operators inside them.
    let pairs (hooks : List<Hook>) =
      hooks
      |> List.choose (fun h ->
        match h with
        | Hook.Frame(_, _, _, ([ RT.DInt64 _; RT.DInt64 _ ] as args)) -> Some args
        | _ -> None)

    let! instrs = instrsFor "Darklang.Language.Tests.TailCalls.count(3, 0)"
    let! (result, _, hooks) = recorded state false instrs
    Expect.equal (expectOk result) (RT.DInt64 6L) "count answers"
    Expect.equal
      (pairs hooks)
      [ ints [ 3L; 0L ]; ints [ 2L; 3L ]; ints [ 1L; 5L ]; ints [ 0L; 6L ] ]
      "each self tail call is a frame with its arguments"

    let! instrs =
      instrsFor "Stdlib.List.fold([10, 20, 30], 0, fun (acc, x) -> acc + x)"
    let! (result, _, hooks) = recorded state false instrs
    Expect.equal (expectOk result) (RT.DInt64 60L) "fold answers"
    Expect.equal
      (pairs hooks)
      [ ints [ 0L; 10L ]; ints [ 10L; 20L ]; ints [ 30L; 30L ] ]
      "each step of the fold is a frame with its accumulator and element"
  }

/// Every List and Dict test in the store, run forced to spread and with spreading off, recorded
/// hook by hook: the same outcome, effects and trace. A test that does not come out the same in
/// two serial runs (it counts live processes, say) says nothing here and is left out. Spread onto
/// busy workers too, so a loaded box spreads the same maps.
let private everyListAndDictTest =
  testTask "every List and Dict test comes out the same spread and serial" {
    let (state : RT.ExecutionState) = executionStateFor pmPT false
    let state : RT.ExecutionState =
      { state with
          test = { state.test with isPackageTest = true }
          spread = { state.spread with idleOnly = false } }
    let named =
      Sql.query
        "SELECT DISTINCT name, item_hash FROM locations
         WHERE owner = 'Darklang' AND modules IN ('Stdlib.List.Tests', 'Stdlib.Dict.Tests')
           AND item_type = 'test' AND live = 1 AND item_hash <> ''"
      |> Sql.executeRows (fun read -> read.string "name", read.string "item_hash")
    // One group of workers for all of them, each run a process on one of its workers.
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let onWorker
      (state : RT.ExecutionState)
      (spread : bool)
      (instrs : RT.Instructions)
      =
      task {
        let p =
          s.SpawnOn(
            spreading spread state,
            (None, instrs),
            Scheduler.EntryExpr,
            None
          )
        let! result = p.completion.Task
        s.TakeResult p.id |> ignore<Option<RT.ExecutionResult>>
        return result
      }
    let recorded = recordedBy onWorker
    try
      let mutable compared = 0
      for (name, h) in named do
        match pmPT.getTest (LibExecution.ProgramTypes.Hash h) with
        | None -> failtest $"no test at {h}"
        | Some(test : LibExecution.ProgramTypes.PackageTest.PackageTest) ->
          let instrs = PT2RT.Expr.toRT Map.empty 0 None test.body
          let! once = recorded state false instrs
          let! twice = recorded state false instrs
          if once = twice then
            let! spread = recorded state true instrs
            expectSameRun name spread once
            compared <- compared + 1
      Expect.isGreaterThan compared 100 "most tests compared"
      let spread = spreads state ()
      let chosen = spread + state.spread.counts.Get RT.SpreadCount.NoChunks
      Expect.isGreaterThan spread 0L "they spread"
      Expect.equal spread chosen "every map the probe chose to spread did"
    finally
      s.Workers.Stop()
  }

/// The decision is the probe's, by instructions; the cores are whoever is idle. With every worker
/// busy, a map that would spread runs here.
let private busyWorkersStaySerial =
  testTask "with no worker idle, a map runs serially" {
    let state = executionStateFor pmPT false
    let! busy =
      instrsFor
        "Stdlib.List.fold(Stdlib.List.range(1, 1000000), 0, fun(a, _) -> a + Stdlib.List.fold(Stdlib.List.range(1, 1000), 0, fun(b, x) -> b + x))"
    let! instrs = instrsFor $"Stdlib.List.map(Stdlib.List.range(1, 40), {uneven})"
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let busy =
      List.init Scheduler.defaultWorkers (fun _ ->
        s.SpawnOn(spreading false state, (None, busy), Scheduler.EntryExpr, None))
    try
      Expect.equal s.Workers.Idle 0 "every worker busy"
      let p =
        s.Spawn(spreading true state, (None, instrs), Scheduler.EntryExpr, None)
      let! (result, moved) =
        delta (spreads state) (fun () ->
          task {
            let t =
              Thread(
                (fun () -> s.RunUntil p |> ignore<RT.ExecutionResult>),
                IsBackground = true
              )
            t.Start()
            return! p.completion.Task
          })
      let! serial =
        runWith state false $"Stdlib.List.map(Stdlib.List.range(1, 40), {uneven})"
      Expect.equal moved 0L "nothing spread"
      Expect.equal (expectOk result) (expectOk serial) "the same list"
    finally
      for b in busy do
        s.Cancel b.id |> ignore<bool>
  }

/// The group holds at most two chunks a worker in flight, across every spread at once.
let private noSlotsStaySerial =
  testTask "with no chunk slot free, a map runs serially" {
    let state = executionStateFor pmPT false
    let! instrs = instrsFor $"Stdlib.List.map(Stdlib.List.range(1, 40), {uneven})"
    let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
    let slots = s.Workers.ChunkSlots
    let rec takeAll n = if slots.Wait 0 then takeAll (n + 1) else n
    let held = takeAll 0
    Expect.equal held (2 * Scheduler.defaultWorkers) "two slots a worker"
    let run () =
      task {
        let p =
          s.Spawn(spreading true state, (None, instrs), Scheduler.EntryExpr, None)
        let t =
          Thread(
            (fun () -> s.RunUntil p |> ignore<RT.ExecutionResult>),
            IsBackground = true
          )
        t.Start()
        return! p.completion.Task
      }
    let! (_, moved) = delta (spreads state) run
    Expect.equal moved 0L "nothing spread while the slots were held"
    slots.Release held |> ignore<int>
  }

/// What a spread charges its process is what the process takes serially, to the instruction and
/// however the work is split, so a cap (`exec.maxInstructions`) gives a serial run's verdict:
/// chunks after a hit or a fallback are not charged, and neither is the chunk that ran again here.
let private chargesAsSerial =
  testTask "a spread charges the instructions a serial run takes" {
    let state = executionStateFor pmPT false
    for code in
      [ $"Stdlib.List.map(Stdlib.List.range(1, 40), {uneven})"
        $"Stdlib.List.any(Stdlib.List.range(1, 40), fun i -> {uneven}(i) > 0 && i == 20)"
        "(let hidden = ((fun s -> Builtin.testTrace(s)), 0) in Stdlib.List.map(Stdlib.List.range(1, 30), fun i -> (let (trace, _) = hidden in if i == 20 then (let _ = trace(\"x\") in i) else Stdlib.List.fold(Stdlib.List.range(1, 500), i, fun(a, x) -> a + x))))" ] do
      let! instrs = instrsFor code
      let taken (spread : bool) =
        task {
          let s = Scheduler.Scheduler(Scheduler.defaultQuantum)
          let p =
            s.Spawn(
              spreading spread state,
              (None, instrs),
              Scheduler.EntryExpr,
              None
            )
          let t =
            Thread(
              (fun () -> s.RunUntil p |> ignore<RT.ExecutionResult>),
              IsBackground = true
            )
          t.Start()
          let! result = p.completion.Task
          expectOk result |> ignore<RT.Dval>
          return p.instructionsTaken
        }
      let! serial = taken false
      let before = Scheduler.defaultWorkers
      try
        for workers in [ 2; 4; 16; before ] do
          Scheduler.defaultWorkers <- workers
          for _ in 1..3 do
            let! (spread, moved) = delta (spreads state) (fun () -> taken true)
            Expect.isGreaterThan moved 0L $"it spread, {workers} workers: {code}"
            Expect.equal
              spread
              serial
              $"charged as serially, {workers} workers: {code}"
      finally
        Scheduler.defaultWorkers <- before
        Trace.take () |> ignore<List<string>>
  }

/// The first elements are cheap and the rest dear, so every chunk passes its own cap, several
/// times what the elements so far cost, and runs again here. That is a cost misjudged, not an
/// effect, so the next map over the same body is tried again.
let private dearChunksRunHere =
  testTask "a chunk past its own cap runs again here, and the body is tried again" {
    let state = executionStateFor pmPT false
    let code =
      "(let f = (fun i -> if i < 4 then i else Stdlib.List.fold(Stdlib.List.range(1, 30000 * i), 0, fun(a, x) -> a + x)) in (let a = Stdlib.List.map(Stdlib.List.range(1, 12), f) in (a, Stdlib.List.map(Stdlib.List.range(1, 12), f))))"
    let! ((spread, fellBack), moved) =
      delta (spreads state) (fun () ->
        delta (fallbacks state) (fun () -> run state code))
    let! serial = runWith state false code
    Expect.equal (expectOk spread) (expectOk serial) "the same lists"
    Expect.equal moved 2L "both maps spread"
    Expect.equal fellBack 2L "and both ran again here"
  }

/// A callable that fell back is remembered by the process it fell back in, not for the rest of
/// the OS process: a later run (a later request, on `serve`) tries it again.
let private fallbackIsPerProcess =
  testTask "a callable that fell back in one process is tried again in the next" {
    let state = executionStateFor pmPT false
    Trace.take () |> ignore<List<string>>
    let code =
      "Stdlib.List.map([\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"], Builtin.testTrace)"
    let! instrs = instrsFor code
    let! ((_, fellBack), moved) =
      delta (spreads state) (fun () ->
        delta (fallbacks state) (fun () ->
          task {
            let! first = runInstrs state true instrs
            let! second = runInstrs state true instrs
            return first, second
          }))
    Trace.take () |> ignore<List<string>>
    Expect.equal moved 2L "each run spread"
    Expect.equal fellBack 2L "and each fell back"
  }


let tests =
  testSequenced (
    testList
      "spread"
      [ inOrder
        siblings
        dictOps
        enclosingCallbackType
        cheapBodyStaysSerial
        dictErrorsAsSerial
        dictCallbackTypeAsSerial
        dictEffectsKeepOrder
        sortByAndMap2
        sortByAndMap2ErrorsAsSerial
        filterErrorsAsSerial
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
        refusalGetsThrough "Darklang.LanguageTools.Live.apply"
        nestingIsBounded
        viewedRunsDoNotSpread
        recordedAsSerial
        storedAsSerial
        inPlaceCallsAreRecorded
        everyListAndDictTest
        busyWorkersStaySerial
        noSlotsStaySerial
        chargesAsSerial
        dearChunksRunHere
        fallbackIsPerProcess
        cancelledLeavesNothing
        fallbackIsPreempted
        syncFallbackIsPreempted
        visibleEffectStartsNothing
        packageFnsAnsweredByTheStore
        wrongPureFallsBack
        countsAreExact
          "map"
          "Stdlib.List.map(Stdlib.List.range(1, 400), fun i -> Stdlib.String.join(Stdlib.List.map(Stdlib.List.range(1, 20), fun j -> Stdlib.display(i + j)), \",\"))"
        countsAreExact
          "Dict.map"
          "Stdlib.Dict.map(Stdlib.Dict.fromEntries(Stdlib.List.zipShortest(Stdlib.List.range(1, 400), Stdlib.List.range(1, 400))), fun(k, v) -> Stdlib.String.join(Stdlib.List.map(Stdlib.List.range(1, 20), fun j -> Stdlib.display(k + j)), \",\"))" ]
  )
