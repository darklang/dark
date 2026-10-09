/// Package tests: `test name = body`, run by `pmExecuteTest`.
///
/// These author real tests into the store and run them through the Dark wrapper
/// (`LanguageTools.PackageManager.Test.execute`), the same path `dark test` takes.
module Tests.PackageTests

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

open TestUtils.TestUtils


let private authorIn (m : string) (decls : string) : Task<List<PT.PackageOp>> =
  authorIntoMain $"module Darklang.{m}\n\n{decls}"

let private cleanup (m : string) : Task<unit> =
  execSqlP
    "DELETE FROM locations WHERE owner = 'Darklang' AND modules = @m"
    [ "m", Sql.string m ]


/// How a test run came out, as a string an assertion can compare.
let private evalAsCli (code : string) : Task<RT.ExecutionResult> =
  task {
    let! state = Tests.CliTestHarness.buildState ()
    let! expr = parsePTExpr code
    return!
      LibExecution.Execution.executeExpr
        state
        (LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT Map.empty 0 None expr)
  }

let private runHash (h : string) : Task<string> =
  task {
    let code =
      "Darklang.LanguageTools.PackageManager.Test.execute "
      + "Darklang.SCM.Branch.mainBranchId "
      + $"(Darklang.LanguageTools.ProgramTypes.Hash.Hash \"{h}\")"
    match! evalAsCli code with
    | Ok(RT.DEnum(_, _, _, "Ok", [ RT.DEnum(_, _, _, "Pass", []) ])) -> return "pass"
    | Ok(RT.DEnum(_, _, _, "Ok", [ RT.DEnum(_, _, _, "Fail", [ RT.DList(_, ms) ]) ])) ->
      let messages =
        ms
        |> List.map (function
          | RT.DString s -> s
          | other -> $"{other}")
      return "fail: " + String.concat "; " messages
    | Ok(RT.DEnum(_, _, _, "Error", [ RT.DEnum(_, _, _, caseName, _) ])) ->
      return $"error: {caseName}"
    | Ok other -> return failtest $"unexpected result shape: {other}"
    | Error(rte, _) -> return failtest $"the Dark call raised: {rte}"
  }

/// Run what <param name> is bound to NOW. Not the hash the authoring batch
/// returned: the WIP refresh after it re-resolves names and rehashes, and the
/// pre-refresh content is still stored under its old hash.
let private run (m : string) (name : string) : Task<string> =
  task {
    let! bound = liveBoundHash { owner = "Darklang"; modules = [ m ]; name = name }
    match bound with
    | Some h -> return! runHash h
    | None -> return failtest $"nothing is bound at {m}.{name}"
  }


let outcomes =
  testTask "a test passes, fails, or errors by what its body returns" {
    let m = "PackageTestOutcomes"
    do! cleanup m

    let! _ =
      authorIn
        m
        """let double (n: Int64) : Int64 = (n + n)
test passes = Darklang.PackageTestOutcomes.double 4L |> Stdlib.Test.equal 8L
test fails = Darklang.PackageTestOutcomes.double 4L |> Stdlib.Test.equal 9L
test collects =
  Stdlib.Test.all [ Stdlib.Test.fail "a", Stdlib.Test.fail "b" ]
test notAResult = 5L
let restrictedPrint () :{} Unit = Stdlib.printLine "should not print"
test effectful =
  let _ = Darklang.PackageTestOutcomes.restrictedPrint ()
  Stdlib.Test.pass ()
test raisesWhatItRaises =
  (1L / 0L)
  => raises "Cannot divide by 0"
test injectsRuntimeError =
  Stdlib.Test.raiseRuntimeError "injected"
  => raises "Uncaught exception: injected"
test raisesButReturned =
  1L
  => raises "Cannot divide by 0"
test raisesSomethingElse =
  (1L / 0L)
  => raises "another message"
test sqlerrorRejectsOrdinaryError =
  (1L / 0L)
  => sqlerror "Cannot divide by 0"
let randomKey () :{Random} String = Stdlib.DB.generateKey ()
let noRandom () :{} String = Stdlib.DB.generateKey ()
let noClock () :{Random} Unit =
  let _ = Stdlib.DateTime.now ()
  ()
test randomGranted =
  (Darklang.PackageTestOutcomes.randomKey ())
  |> Stdlib.String.length
  |> Stdlib.Test.equal 36
test randomInherited =
  (Stdlib.DB.generateKey ())
  |> Stdlib.String.length
  |> Stdlib.Test.equal 36
test randomDenied =
  (Darklang.PackageTestOutcomes.noRandom ())
  |> Stdlib.String.length
  |> Stdlib.Test.equal 36
test clockDenied =
  let _ = Darklang.PackageTestOutcomes.noClock ()
  Stdlib.Test.pass ()
"""

    let! passes = run m "passes"
    Expect.equal passes "pass" "an equal assertion passes"

    let! fails = run m "fails"
    Expect.equal fails "fail: expected 9, got 8" "the failure says why"

    let! collects = run m "collects"
    Expect.equal
      collects
      "fail: assertion 1: a; assertion 2: b"
      "`all` keeps every failure and identifies its assertion"

    // The at-rest checker flags this at authoring time; the runner must not
    // take it for a pass if it gets stored anyway.
    let! notAResult = run m "notAResult"
    Expect.equal
      notAResult
      "error: UncaughtException"
      "a non-Test.Result body errors"

    // The called function's empty ceiling applies inside a test too.
    let! effectful = run m "effectful"
    Expect.stringStarts effectful "error: " "an effect is refused"

    // Declaration-level `raises` catches the whole body: the exact message
    // passes, and a value or a different message fails saying which.
    let! raised = run m "raisesWhatItRaises"
    Expect.equal raised "pass" "the expected error passes"

    let! injected = run m "injectsRuntimeError"
    Expect.equal injected "pass" "a package test may inject an exact runtime error"
    let! outside = evalDarkExpr "Stdlib.Test.raiseRuntimeError \"outside\""
    match outside with
    | Error(RT.RuntimeError.UncaughtException(message, _), _) ->
      Expect.equal
        message
        "Stdlib.Test.raiseRuntimeError requires a running package test"
        "test capability does not escape to the caller after execution"
    | other -> failtest $"expected injection to be refused outside a test: {other}"

    let! returned = run m "raisesButReturned"
    Expect.equal
      returned
      "fail: expected the error `Cannot divide by 0`, got the value 1"
      "a value where an error was expected fails"

    let! other = run m "raisesSomethingElse"
    Expect.equal
      other
      "fail: expected the error `another message`, got `Cannot divide by 0`"
      "a different error fails, naming both"

    let! wrongKind = run m "sqlerrorRejectsOrdinaryError"
    Expect.equal
      wrongKind
      "fail: expected SQL compiler error `Cannot divide by 0`, got `Cannot divide by 0`"
      "sqlerror requires a SQL compiler error, not just matching text"

    let! randomGranted = run m "randomGranted"
    Expect.equal
      randomGranted
      "pass"
      "the called function may use instance-granted Random"
    let! randomInherited = run m "randomInherited"
    Expect.equal randomInherited "pass" "an unannotated test inherits Random"
    let! randomDenied = run m "randomDenied"
    Expect.stringStarts
      randomDenied
      "error: "
      "the function's empty row denies Random"
    let! clockDenied = run m "clockDenied"
    Expect.stringStarts clockDenied "error: " "Random does not grant Clock"

    do! cleanup m
  }


let unknownHash =
  testTask "running a hash that names no test is an error, not a pass" {
    let! result = runHash (String.replicate 64 "0")
    Expect.equal result "error: VariableNotFound" "missing test"
  }

let isolatedDBs =
  testTask "package test DBs are isolated and leave no user rows" {
    let m = "PackageTestDBs"
    do! cleanup m
    let! authored =
      authorIn
        m
        """[<DB>]
type Items = { name: String }
test writes =
  let _ = Stdlib.DB.set (Items { name = "first" }) "key" Items
  (Stdlib.DB.get "key" Items)
  |> Stdlib.Test.equal (Some (Items { name = "first" }))
test startsEmpty =
  (Stdlib.DB.count Items) |> Stdlib.Test.equal 0
test errorsAfterWrite =
  let _ = Stdlib.DB.set (Items { name = "temporary" }) "key" Items
  (1L / 0L)
  => raises "Cannot divide by 0"
"""

    let tests =
      authored
      |> List.choose (function
        | PT.PackageOp.AddTest test -> Some test
        | _ -> None)
    let types =
      authored
      |> List.choose (function
        | PT.PackageOp.AddType typ -> Some typ
        | _ -> None)
    Expect.equal (List.length types) 1 "inline DB record creates a row type"
    Expect.equal (List.length tests) 3 "three DB tests were authored"
    for test in tests do
      Expect.isNonEmpty test.testDBs "private schemas are stored on the test"
      Expect.equal
        test.permissionCeiling
        None
        "new tests have no independent permission ceiling"

    let! before = countSql "SELECT count(*) AS n FROM user_data_v0" []
    let! writes = run m "writes"
    let! startsEmpty = run m "startsEmpty"
    let! again = run m "writes"
    let! errorsAfterWrite = run m "errorsAfterWrite"
    let! after = countSql "SELECT count(*) AS n FROM user_data_v0" []
    Expect.equal writes "pass" "DB write and read work"
    Expect.equal startsEmpty "pass" "the next test starts with an empty DB"
    Expect.equal again "pass" "the same test can run again"
    Expect.equal errorsAfterWrite "pass" "runtime errors still clean up test rows"
    Expect.equal after before "test rows are removed"
    do! cleanup m
  }


let isolatedObservations =
  testTask "package test observations are local, ordered, and never cached" {
    let m = "PackageTestObservations"
    do! cleanup m
    let! authored =
      authorIn
        m
        """let observed (n: Int64) : Int64 = Stdlib.Test.incrementCounter n
test counts =
  let value = Darklang.PackageTestObservations.observed 7L
  let _ = Stdlib.Test.incrementCounter "another type"
  (value, Stdlib.Test.counterValue ()) |> Stdlib.Test.equal (7L, 2L)
test startsEmpty =
  Stdlib.Test.counterValue () |> Stdlib.Test.equal 0L
test callsStayOrdered =
  Stdlib.List.map (Stdlib.List.range 1 200) (fun _ ->
    let _ = Stdlib.Test.incrementCounter ()
    Stdlib.toString (Stdlib.Test.counterValue ()))
  |> Stdlib.Test.equal (Stdlib.List.map (Stdlib.List.range 1 200) (fun n -> Stdlib.toString n))
test sharesWithChildren =
  let _ = Stdlib.List.parallelMap (Stdlib.List.range 1 8) (fun _ ->
    Stdlib.List.iter (Stdlib.List.range 1 200) (fun _ -> Stdlib.Test.incrementCounter ()))
  Stdlib.Test.counterValue () |> Stdlib.Test.equal 1600L
test stopsAfterError =
  let _ = Stdlib.Test.incrementCounter ()
  (1L / 0L)
  => raises "Cannot divide by 0"
test failsAfterObservation =
  let _ = Stdlib.Test.incrementCounter ()
  Stdlib.Test.fail "deliberate"
"""
    for op in authored do
      match op with
      | PT.PackageOp.AddTest test ->
        Expect.isEmpty test.testDBs "observations need no private DB schema"
      | _ -> ()

    let! counts = run m "counts"
    Expect.equal
      counts
      "pass"
      "helper calls share the counter and values pass through"
    let! failed = run m "failsAfterObservation"
    Expect.equal failed "fail: deliberate" "a failed test still finishes normally"
    let! stopped = run m "stopsAfterError"
    Expect.equal stopped "pass" "a runtime error after an observation is handled"
    let! ordered = run m "callsStayOrdered"
    Expect.equal ordered "pass" "observations prevent automatic parallel spreading"
    let! children = run m "sharesWithChildren"
    Expect.equal
      children
      "pass"
      "explicit children share their test's atomic counter"

    // Execute several tests inside one caller state, so creating a new caller
    // for every test cannot mask leaking counters between test executions.
    let! countHash =
      liveBoundHash { owner = "Darklang"; modules = [ m ]; name = "counts" }
    let! emptyHash =
      liveBoundHash { owner = "Darklang"; modules = [ m ]; name = "startsEmpty" }
    let! errorHash =
      liveBoundHash { owner = "Darklang"; modules = [ m ]; name = "stopsAfterError" }
    let! failHash =
      liveBoundHash
        { owner = "Darklang"; modules = [ m ]; name = "failsAfterObservation" }
    let execute h =
      "Darklang.LanguageTools.PackageManager.Test.execute "
      + "Darklang.SCM.Branch.mainBranchId "
      + $"(Darklang.LanguageTools.ProgramTypes.Hash.Hash \"{h}\")"
    let check h = $"({execute h}) |> Stdlib.Test.equal (Ok (Stdlib.Test.pass ()))"
    let countHash = Option.get countHash
    let emptyHash = Option.get emptyHash
    let errorHash = Option.get errorHash
    let failHash = Option.get failHash
    let failCheck =
      $"({execute failHash}) |> Stdlib.Test.equal (Ok (Stdlib.Test.fail \"deliberate\"))"
    let code =
      $"Stdlib.Test.all [{check countHash}, {check emptyHash}, {check errorHash}, {check emptyHash}, {failCheck}, {check emptyHash}, {check countHash}]"
    let! repeated = evalAsCli code
    match repeated with
    | Ok(RT.DEnum(_, _, _, "Pass", [])) -> ()
    | other -> failtest $"test counters leaked in a shared caller: {other}"

    let! concurrentResults = [| for _ in 1..8 -> runHash countHash |] |> Task.WhenAll
    for result in concurrentResults do
      Expect.equal result "pass" "concurrent executions have separate counters"

    for name in [ "incrementCounter ()"; "counterValue ()" ] do
      let! outside = evalDarkExpr $"Stdlib.Test.{name}"
      match outside with
      | Error(RT.RuntimeError.UncaughtException(message, _), _) ->
        Expect.stringContains
          message
          "requires a running package test"
          "test-only capability"
      | other ->
        failtest $"expected an observation to be refused outside a test: {other}"

    for hash in [ countHash; emptyHash ] do
      let! safe =
        evalDarkExpr (
          "Darklang.LanguageTools.PackageManager.Test.isCacheSafe "
          + $"(Darklang.LanguageTools.ProgramTypes.Hash.Hash \"{hash}\")"
        )
      match safe with
      | Ok(RT.DBool false) -> ()
      | other ->
        failtest
          $"test-local observation calls must not reuse cached passes: {other}"
    do! cleanup m
  }


// Authoring and WIP refresh mutate the shared package store, so these must not
// race other store-mutating backend tests.
let tests =
  testSequenced (
    testList
      "PackageTests"
      [ outcomes; unknownHash; isolatedDBs; isolatedObservations ]
  )
