/// Tests of `dark test`: what it finds, what it reports, and its exit code.
///
/// A test is any function that takes Unit and returns `Stdlib.Test.T`, so each case here authors
/// ordinary functions and then runs the command on them. Each case has its own module under
/// `Tests.DT*` and scopes to it, because a wider scope would also run the other cases' tests.
module Tests.CliTest

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


/// Runs `dark <args>`. Returns what it printed, without colour codes, and its exit code.
let private ran
  (state : RT.ExecutionState)
  (args : List<string>)
  : Task<string * int64> =
  task {
    let! (out, code) = runCliWithExit state args
    return (plain out, code)
  }

let private passing = "() : Stdlib.Test.T = (1 + 1) |> Stdlib.Test.eq 2"
let private failing = "() : Stdlib.Test.T = (1 + 1) |> Stdlib.Test.eq 3"
let private raising =
  "() : Stdlib.Test.T = (Stdlib.List.head [] |> Builtin.unwrap) |> Stdlib.Test.eq 1"


/// Each test prints on one line: its short name, padding, then its verdict. The padding
/// depends on the longest name in the run, so this matches the shape and not the spacing.
let private hasVerdict (out : string) (name : string) (verdict : string) : bool =
  System.Text.RegularExpressions.Regex.IsMatch(
    out,
    $@"(?m)^  {System.Text.RegularExpressions.Regex.Escape name}\s+{verdict}\b"
  )

let allPassingIsZero =
  cliTest "test: everything passing exits 0" (fun state ->
    task {
      do! start state
      do! author state "Tests.DTPass.one" passing
      do! author state "Tests.DTPass.two" passing

      let! (out, code) = ran state [ "test"; "Tests.DTPass" ]
      Expect.equal code 0L $"all passed, got: {out}"
      Expect.stringContains
        out
        "Tests.DTPass\n"
        "the module is a heading, said once"
      Expect.isTrue
        (hasVerdict out "one" "PASS")
        $"each test has its own line: {out}"
      Expect.stringContains out "1 check" "which says how many checks it made"
      Expect.isFalse
        (out.Contains "Did not pass")
        "and nothing to recap when all passed"
      Expect.stringContains out "2 passed" "and counts them"
      do! discardAll state
    })


/// PASS, FAIL and ERROR are reported separately. A failing or crashing test does not stop the
/// next one. Any FAIL or ERROR makes the run exit 1.
let failAndErrorAreDifferent =
  cliTest
    "test: FAIL and ERROR are reported apart, and the run carries on"
    (fun state ->
      task {
        do! start state
        do! author state "Tests.DTMixed.aRaises" raising
        do! author state "Tests.DTMixed.bFails" failing
        do! author state "Tests.DTMixed.cPasses" passing

        let! (out, code) = ran state [ "test"; "Tests.DTMixed" ]
        Expect.equal code 1L $"a failure is exit 1, got: {out}"
        Expect.isTrue
          (hasVerdict out "aRaises" "ERROR")
          $"a raise is an ERROR: {out}"
        Expect.stringContains out "Cannot unwrap None" "with what was raised"
        Expect.isTrue
          (hasVerdict out "bFails" "FAIL")
          $"a returned failure is a FAIL: {out}"
        Expect.stringContains out "Expected: 3" "with what was expected"
        Expect.stringContains out "Actual:   2" "and what came back"
        Expect.isTrue
          (hasVerdict out "cPasses" "PASS")
          $"the test after an ERROR and a FAIL still ran: {out}"
        Expect.stringContains
          out
          "1 passed, 1 failed, 1 errored"
          "and the count says all three"
        // The recap lists full names, to paste into `test <name>`.
        let recap = out.Substring(out.IndexOf "Did not pass:")
        Expect.stringContains
          recap
          "  Tests.DTMixed.aRaises"
          "the recap names the ERROR"
        Expect.stringContains recap "  Tests.DTMixed.bFails" "and the FAIL"
        Expect.isFalse (recap.Contains "cPasses") "and not what passed"
        do! discardAll state
      })


let casesNamesTheFailingRow =
  cliTest
    "test: `actualExpected` runs every row and names only the ones that failed"
    (fun state ->
      task {
        do! start state
        do!
          author
            state
            "Tests.DTCases.rows"
            "() : Stdlib.Test.T = Stdlib.Test.actualExpected [ (1 + 1, 2), (2 + 2, 5), (3 + 3, 6), (4 + 4, 9) ]"

        let! (out, code) = ran state [ "test"; "Tests.DTCases" ]
        Expect.equal code 1L $"a failing row fails the test, got: {out}"
        Expect.stringContains out "case 2:" "the second row failed"
        Expect.stringContains out "case 4:" "and so did the fourth, after it"
        Expect.isFalse
          (out.Contains "case 1:")
          "the first row passed and says nothing"
        Expect.isFalse (out.Contains "case 3:") "so did the third"
        Expect.stringContains
          out
          "0 passed, 1 failed (4 checks)"
          "and it is ONE test, however many rows, with every row counted as a check"
        do! discardAll state
      })


/// Tests are found by signature, and the return type is matched by identity. A helper that
/// takes an argument is not a test. Neither is a function returning a different type that
/// happens to look the same.
let onlyTheSignatureCounts =
  cliTest "test: a helper and a lookalike type are not tests" (fun state ->
    task {
      do! start state
      do! author state "Tests.DTShape.real" passing
      do!
        author
          state
          "Tests.DTShape.helper"
          "(n: Int) : Stdlib.Test.T = n |> Stdlib.Test.eq n"
      do! run state [ "type"; "Tests.DTShape.T"; "= | Pass | Fail of List<String>" ]
      do!
        author
          state
          "Tests.DTShape.lookalike"
          "() : Tests.DTShape.T = Tests.DTShape.T.Pass"

      let! (out, code) = ran state [ "test"; "list"; "Tests.DTShape" ]
      Expect.equal code 0L $"a nonempty listing is exit 0, got: {out}"
      Expect.stringContains out "Tests.DTShape.real" "the real one is found"
      Expect.isFalse
        (out.Contains "helper")
        "a function with an argument is a helper"
      Expect.isFalse
        (out.Contains "lookalike")
        "a type of the same shape elsewhere is not it"
      do! discardAll state
    })


/// A scope matches whole path segments. `Tests.DTSeg` must not match `Tests.DTSegment`, and
/// must match modules nested under it.
let scopeIsBySegment =
  cliTest
    "test: a module scope matches segments and includes what is nested"
    (fun state ->
      task {
        do! start state
        do! author state "Tests.DTSeg.mine" passing
        do! author state "Tests.DTSeg.Nested.deep" passing
        do! author state "Tests.DTSegment.notMine" passing

        let! (out, _) = ran state [ "test"; "list"; "Tests.DTSeg" ]
        Expect.stringContains out "Tests.DTSeg.mine" "its own"
        Expect.stringContains out "Tests.DTSeg.Nested.deep" "and what is under it"
        Expect.isFalse
          (out.Contains "DTSegment")
          "but not a module that only starts the same"

        let! (filtered, _) =
          ran state [ "test"; "list"; "Tests.DTSeg"; "--filter"; "deep" ]
        Expect.stringContains filtered "Nested.deep" "--filter keeps what matches"
        Expect.isFalse (filtered.Contains "DTSeg.mine") "and drops what does not"
        do! discardAll state
      })


/// `test list` only discovers tests. A test that would raise is listed and not run: there is no
/// verdict and no summary, and the exit code is 0.
let listRunsNothing =
  cliTest "test list: names tests without running them" (fun state ->
    task {
      do! start state
      do! author state "Tests.DTList.wouldRaise" raising

      let! (out, code) = ran state [ "test"; "list"; "Tests.DTList" ]
      Expect.equal code 0L $"listing is not running, got: {out}"
      Expect.stringContains out "Tests.DTList.wouldRaise" "it is named"
      Expect.isFalse (out.Contains "ERROR") "and was not run"
      Expect.isFalse (out.Contains "passed") "so there is no summary either"
      do! discardAll state
    })


/// A run that selects no tests never passes. The message says whether the scope was empty or
/// the filter matched nothing.
let emptyIsNeverGreen =
  cliTest "test: nothing selected is exit 2, and says why" (fun state ->
    task {
      do! start state
      do! author state "Tests.DTEmpty.one" passing

      let! (noScope, c1) = ran state [ "test"; "Tests.DTNothingHere" ]
      Expect.equal c1 2L $"an empty scope, got: {noScope}"
      Expect.stringContains
        noScope
        "No tests in Tests.DTNothingHere"
        "names the scope"

      let! (noMatch, c2) =
        ran state [ "test"; "Tests.DTEmpty"; "--filter"; "zzznope" ]
      Expect.equal c2 2L $"a filter that matched nothing, got: {noMatch}"
      Expect.stringContains
        noMatch
        "zzznope"
        "names the filter, since the scope was not empty"

      let! (badFlag, c3) = ran state [ "test"; "--bogus" ]
      Expect.equal c3 2L $"an unknown flag, got: {badFlag}"
      Expect.stringContains badFlag "Usage: test" "and shows the usage"

      let! (twoScopes, c4) = ran state [ "test"; "A"; "B" ]
      Expect.equal c4 2L $"two modules, got: {twoScopes}"
      do! discardAll state
    })


/// Tests are ordinary functions, so a branch runs its own version of a test. Here main keeps
/// failing while the branch that fixed the function passes. No test-specific code is involved.
let aBranchRunsItsOwnVersion =
  cliTest "test: main and a branch run their own versions" (fun state ->
    task {
      do! start state
      do! author state "Tests.DTBranch.double" "(n: Int) : Int = n + 2"
      do!
        author
          state
          "Tests.DTBranch.doubles"
          "() : Stdlib.Test.T = Tests.DTBranch.double 4 |> Stdlib.Test.eq 8"
      do! commit state "a wrong double and its test"

      do! run state [ "branch"; "create"; "dt-fix-double" ]
      do! author state "Tests.DTBranch.double" "(n: Int) : Int = n * 2"

      let! (onBranch, branchCode) = ran state [ "test"; "Tests.DTBranch" ]
      Expect.equal
        branchCode
        0L
        $"the branch fixed it, draft included, got: {onBranch}"

      do! onMain state
      let! (onMainOut, mainCode) = ran state [ "test"; "Tests.DTBranch" ]
      Expect.equal mainCode 1L $"main still has the wrong one, got: {onMainOut}"
      Expect.stringContains onMainOut "Actual:   6" "and says what it got"

      do! archiveBranches state [ "dt-fix-double" ]
    })


/// The only link between the two test systems. Tests written in Dark are run by `dark test`, not
/// by this suite, so without this case CI would not notice one of them failing. Tests are found
/// by signature, so a Dark test added anywhere under `Darklang` is covered as soon as it lands.
let theRealDarkTestsPass =
  cliTest "test: every Dark test under Darklang passes" (fun state ->
    task {
      do! start state
      let! (out, code) = ran state [ "test"; "Darklang" ]
      Expect.equal code 0L $"`dark test Darklang` should be green, got:\n{out}"
      Expect.stringContains
        out
        "Darklang.Stdlib.Bool.Tests\n"
        "and it really ran them: an empty run would be exit 2, but say so by name as well"
      Expect.isTrue (hasVerdict out "xor" "PASS") out
    })


let exactSelectionAndProgress =
  cliTest
    "test: a listed full name runs only that test, named before its verdict"
    (fun state ->
      task {
        do! start state
        do! author state "Tests.DTExact.one" passing
        do! author state "Tests.DTExact.other" failing
        let! (listed, _) = ran state [ "test"; "list"; "/Tests.DTExact.one" ]
        Expect.equal listed "Tests.DTExact.one" "exact listing is directly reusable"
        let! (out, code) = ran state [ "test"; listed ]
        Expect.equal code 0L out
        Expect.isFalse (out.Contains "other") "siblings are not run"
        // The name is printed before the test runs and the verdict after it returns, on
        // the same line. A test that hangs shows as a name with nothing after it.
        Expect.isTrue (hasVerdict out "one" "PASS") out
        do! discardAll state
      })

let tableKeepsFailuresAndErrors =
  cliTest
    "test: table errors retain both surrounding failures and labels"
    (fun state ->
      task {
        do! start state
        do!
          author
            state
            "Tests.DTRows.mixed"
            "() : Stdlib.Test.T = (Stdlib.Test.table (fun n -> 10 / n) [ (2, 99), (0, 0), (5, 99) ]) |> Stdlib.Test.withMessage \"division\""
        let! (out, code) = ran state [ "test"; "Tests.DTRows" ]
        Expect.equal code 1L out
        for part in
          [ "Tests.DTRows\n"
            "  Tests.DTRows.mixed"
            "division: case 1:"
            "division: case 2:"
            "division: case 3:"
            "Cannot divide by 0"
            "Actual:   5"
            "Actual:   2"
            "0 passed, 1 errored" ] do
          Expect.stringContains out part out
        Expect.isTrue (hasVerdict out "mixed" "ERROR") out
        do! discardAll state
      })

let emptyRowsFail =
  cliTest "test: an empty table and actualExpected cannot be green" (fun state ->
    task {
      do! start state
      do!
        author
          state
          "Tests.DTEmptyRows.table"
          "() : Stdlib.Test.T = Stdlib.Test.table Stdlib.Bool.not []"
      do!
        author
          state
          "Tests.DTEmptyRows.cases"
          "() : Stdlib.Test.T = Stdlib.Test.actualExpected []"
      let! (out, code) = ran state [ "test"; "Tests.DTEmptyRows" ]
      Expect.equal code 1L out
      Expect.stringContains out "No cases supplied" out
      Expect.stringContains out "2 failed" out
      do! discardAll state
    })

let createTest =
  cliTest
    "test: new creates an editable failing test and refuses overwrites"
    (fun state ->
      task {
        do! start state
        do!
          author
            state
            "Tests.DTNew.add"
            "(left: Int) (right: Int) : Int = left + right"
        let name = "Tests.DTNew.Tests.adds"
        let! (created, code) =
          ran
            state
            [ "test"; "new"; name; "--for"; "Tests.DTNew.add"; "--no-editor" ]
        Expect.equal code 0L created
        let! (source, _) = ran state [ "view"; name; "--raw" ]
        // A row is the inputs, then the expected result. Several inputs are a tuple.
        Expect.stringContains source "((left, right), expected)" source
        let! (out, failed) = ran state [ "test"; name ]
        Expect.equal failed 1L out
        Expect.stringContains out "TODO: write this test" out
        let! (collision, refused) = ran state [ "test"; "new"; name; "--no-editor" ]
        Expect.equal refused 2L collision
        Expect.stringContains collision "already exists" collision
        let! (after, _) = ran state [ "view"; name; "--raw" ]
        Expect.equal after source "collision preserves the original declaration"
        for args in
          [ [ "new" ]
            [ "new"; "Tests.DTNew.bad"; "--for" ]
            [ "new"; "Tests.DTNew.bad"; "--for"; "Tests.No.such" ]
            [ "new"; "Tests.DTNew.bad"; "--bogus" ] ] do
          let! (out, invalid) = ran state ("test" :: args)
          Expect.equal invalid 2L out
        do! discardAll state
      })

let completionAndDocs =
  cliTest
    "test: completion suggests runnable names and testing docs are available"
    (fun state ->
      task {
        do! start state
        do! author state "Tests.DTComplete.one" passing
        let expression =
          "Darklang.Cli.Test.completeSelection Darklang.SCM.Branch.mainBranchId [\"Tests.DTComplete.o\"] |> Stdlib.List.map (fun item -> item.value)"
        let! (out, code) = ran state [ "eval"; expression ]
        Expect.equal code 0L out
        Expect.stringContains out "Tests.DTComplete.one" out
        let! (docs, docsCode) = ran state [ "docs"; "testing" ]
        Expect.equal docsCode 0L docs
        Expect.stringContains docs "test new" docs
        Expect.stringContains docs "Stdlib.Test.all" docs
        do! discardAll state
      })

let tests : List<Test> =
  [ allPassingIsZero
    failAndErrorAreDifferent
    casesNamesTheFailingRow
    onlyTheSignatureCounts
    scopeIsBySegment
    listRunsNothing
    emptyIsNeverGreen
    aBranchRunsItsOwnVersion
    theRealDarkTestsPass
    exactSelectionAndProgress
    tableKeepsFailuresAndErrors
    emptyRowsFail
    createTest
    completionAndDocs ]
