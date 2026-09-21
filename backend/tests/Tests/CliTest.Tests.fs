/// Test discovery, reporting and exit codes through the CLI.
/// Each case uses its own Tests.DT* module to avoid running other fixtures.
module Tests.CliTest

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Tests.CliTestHarness
open Tests.CliDsl


/// Runs `dark <args>`. Returns what it printed, without colour codes, and its exit code.
let private ran (state : Target) (args : List<string>) : Task<string * int64> =
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


/// Run Darklang's native tests in CI, including newly added tests.
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

let approvedCallbacksRespectPermissions =
  cliTest
    "test: approved table callbacks respect allowed and denied stdout"
    (fun state ->
      task {
        do! start state
        do!
          author
            state
            "Tests.DTCallback.prints"
            "() : Stdlib.Test.T = Stdlib.Test.table Stdlib.printLine [ (\"callback output\", ()) ]"
        let! (approval, approvalCode) =
          ran state [ "permissions"; "approve"; "Tests.DTCallback.prints" ]
        Expect.equal approvalCode 0L approval
        Expect.stringContains approval "approved Tests.DTCallback.prints" approval
        let! (allowed, allowedCode) =
          ran state [ "test"; "Tests.DTCallback.prints" ]
        Expect.equal allowedCode 0L allowed
        Expect.stringContains allowed "callback output" allowed
        Expect.stringContains allowed "1 passed (1 check)" allowed
        let original = LibDB.PolicyStore.instancePolicy ()
        try
          let allow, deny = LibExecution.Permissions.Policy.rules original
          LibDB.PolicyStore.setInstancePolicy (
            LibExecution.Permissions.Policy.create
              allow
              (LibExecution.Permissions.Rule.Effect
                LibExecution.Effects.Effect.Stdout
               :: deny)
          )
          let! (denied, deniedCode) =
            ran state [ "test"; "Tests.DTCallback.prints" ]
          Expect.equal deniedCode 1L denied
          Expect.isTrue (hasVerdict denied "prints" "ERROR") denied
          Expect.stringContains denied "permission denied by instance policy" denied
        finally
          LibDB.PolicyStore.setInstancePolicy original
        do! discardAll state
      })


let caughtCallbackDenialDoesNotClassifyLaterRaise =
  cliTest
    "test: a caught callback denial cannot reclassify a later unrelated raise"
    (fun state ->
      task {
        do! start state
        do!
          author
            state
            "Tests.DTCaughtDenial.raiseLater"
            "(_ignored: Int) : Stdlib.Test.T = Stdlib.List.head [] |> Builtin.unwrap"
        do!
          author
            state
            "Tests.DTCaughtDenial.unrelated"
            """() : Stdlib.Test.T =
  Stdlib.Test.all
    [ Stdlib.Test.table Stdlib.printLine [ ("caught denial", ()) ],
      Tests.DTCaughtDenial.raiseLater 0 ]"""
        let! (approval, approvalCode) =
          ran state [ "permissions"; "approve"; "Tests.DTCaughtDenial.unrelated" ]
        Expect.equal approvalCode 0L approval
        let original = LibDB.PolicyStore.instancePolicy ()
        try
          let allow, deny = LibExecution.Permissions.Policy.rules original
          LibDB.PolicyStore.setInstancePolicy (
            LibExecution.Permissions.Policy.create
              allow
              (LibExecution.Permissions.Rule.Effect
                LibExecution.Effects.Effect.Stdout
               :: deny)
          )
          let! (out, code) = ran state [ "test"; "Tests.DTCaughtDenial.unrelated" ]
          Expect.equal code 1L out
          Expect.stringContains out "Cannot unwrap None" out
          Expect.stringContains
            out
            "Package Function DTCaughtDenial.raiseLater"
            "the unrelated error keeps its call stack"
          Expect.isFalse
            (out.Contains "permission denied")
            "the caught callback denial must not classify the later raise"
        finally
          LibDB.PolicyStore.setInstancePolicy original
        do! discardAll state
      })

let dictionaryErrorsSurviveReporting =
  cliTest
    "test: dictionary-bearing errors preserve surrounding checks and row details"
    (fun state ->
      task {
        do! start state
        do!
          author
            state
            "Tests.DTDictionaryError.mixed"
            """() : Stdlib.Test.T =
  Stdlib.Test.all
    [ Stdlib.Test.fail "before the error",
      Stdlib.Test.table Stdlib.Int.toString
        [ (Stdlib.Dict.singleton "x" 1, "1") ],
      Stdlib.Test.eq 2 2,
      Stdlib.Test.fail "after the error" ]"""
        let! (out, code) = ran state [ "test"; "Tests.DTDictionaryError" ]
        Expect.equal code 1L out
        Expect.isTrue (hasVerdict out "mixed" "ERROR") out
        for part in
          [ "before the error"
            "after the error"
            "case 1: for input"
            "expects Int"
            "Dict"
            "0 passed, 1 errored (4 checks)" ] do
          Expect.stringContains out part out
        Expect.isFalse (out.Contains "could not be read back") out
        do! discardAll state
      })

let loggedOutRequiresScope =
  cliTest "test: a missing account requires an explicit scope" (fun state ->
    task {
      do! start state
      for args in [ "[]"; "[\"list\"]"; "[\"--filter\", \"one\"]" ] do
        let expression = $"(Darklang.Cli.Test.parseArgs \"\" {args}).problem"
        let! (out, code) = ran state [ "eval"; expression ]
        Expect.equal code 0L out
        Expect.stringContains
          out
          "No active account. Specify a module or test name"
          out
      for account, args in
        [ "", "[\"Tests.Example\"]"
          "", "[\"list\", \"Tests.Example\"]"
          "Tests", "[]" ] do
        let expression =
          $"(Darklang.Cli.Test.parseArgs \"{account}\" {args}).problem"
        let! (out, code) = ran state [ "eval"; expression ]
        Expect.equal code 0L out
        Expect.equal out "None" out
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
    "test: create makes an editable failing test and refuses overwrites"
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
            [ "test"; "create"; name; "--for"; "Tests.DTNew.add"; "--no-editor" ]
        Expect.equal code 0L created
        let! (source, _) = ran state [ "view"; name; "--raw" ]
        // A row is the inputs, then the expected result. Several inputs are a tuple.
        Expect.stringContains source "((left, right), expected)" source
        let! (out, failed) = ran state [ "test"; name ]
        Expect.equal failed 1L out
        Expect.stringContains out "TODO: write this test" out
        // `new` remains a compatibility alias for `create`.
        let! (collision, refused) = ran state [ "test"; "new"; name; "--no-editor" ]
        Expect.equal refused 2L collision
        Expect.stringContains collision "already exists" collision
        let! (after, _) = ran state [ "view"; name; "--raw" ]
        Expect.equal after source "collision preserves the original declaration"
        for args in
          [ [ "create" ]
            [ "create"; "Tests.DTNew.bad"; "--for" ]
            [ "create"; "Tests.DTNew.bad"; "--for"; "Tests.No.such" ]
            [ "create"; "Tests.DTNew.bad"; "--bogus" ] ] do
          let! (out, invalid) = ran state ("test" :: args)
          Expect.equal invalid 2L out
        do! discardAll state
      })

let createTestTerminalHandoff =
  cliTest
    "test: workbench hands off the terminal only when create opens an editor"
    (fun state ->
      task {
        let expression =
          """let commands = Darklang.Cli.Registry.allCommands ()
[ ["create", "Tests.Example.check"],
  ["new", "Tests.Example.check"],
  ["create", "Tests.Example.check", "--for", "Stdlib.Bool.not"],
  ["create", "Tests.Example.check", "--no-editor"],
  ["new", "--no-editor", "Tests.Example.check"],
  ["Tests.Example"],
  ["list", "Tests.Example"] ]
|> Stdlib.List.map (fun args -> Darklang.Cli.Registry.needsTerminal commands "test" args)"""
        let! (out, code) = ran state [ "eval"; expression ]
        Expect.equal code 0L out
        Expect.equal out "[true, true, true, false, false, false, false]" out
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
        let commandExpression =
          """let values = Darklang.Cli.Test.completeSelection Darklang.SCM.Branch.mainBranchId [""] |> Stdlib.List.map (fun item -> item.value)
[Stdlib.List.member values "create", Stdlib.List.member values "new"]"""
        let! (commands, commandsCode) = ran state [ "eval"; commandExpression ]
        Expect.equal commandsCode 0L commands
        Expect.equal commands "[true, false]" commands
        let! (docs, docsCode) = ran state [ "docs"; "testing" ]
        Expect.equal docsCode 0L docs
        Expect.stringContains docs "test create" docs
        Expect.stringContains docs "Stdlib.Test.all" docs
        do! discardAll state
      })

let testRunWarnings =
  instanceTest
    "test: discarded checks warn only for selected branch tests"
    (fun state ->
      task {
        do! start state
        let name = "Tests.DTRunLint.discarded"
        let declaration =
          "() : Stdlib.Test.T =\n  let ignoredCheck = Stdlib.Test.eq 1 2\n  Stdlib.Test.eq 1 1"
        do! author state name declaration
        do! author state "Tests.DTRunLint.clean" passing
        // Identical bodies share a hash, but each named test needs its own warning.
        do! author state "Tests.DTRunLint.sameBody" declaration
        do! commit state "tests with a discarded check"

        let! (out, code) = ran state [ "test"; "Tests.DTRunLint" ]
        Expect.equal code 0L $"warnings do not change the exit code: {out}"
        Expect.isTrue (hasVerdict out "discarded" "PASS") out
        Expect.stringContains out "3 passed (3 checks)" out
        Expect.stringContains out "Warnings:" out
        Expect.stringContains out "ignoredCheck" out
        Expect.equal
          (System.Text.RegularExpressions.Regex
            .Matches(out, "UnusedTestResult")
            .Count)
          2
          $"both locations of the shared declaration warn: {out}"

        let! (filtered, filteredCode) =
          ran state [ "test"; "Tests.DTRunLint"; "--filter"; "clean" ]
        Expect.equal filteredCode 0L filtered
        Expect.isFalse (filtered.Contains "UnusedTestResult") filtered

        let! (listed, listCode) = ran state [ "test"; "list"; "Tests.DTRunLint" ]
        Expect.equal listCode 0L listed
        Expect.isFalse (listed.Contains "Warnings:") listed

        do! run state [ "branch"; "create"; "dt-run-lint" ]
        do! author state name passing
        let! (onBranch, branchCode) = ran state [ "test"; name ]
        Expect.equal branchCode 0L onBranch
        Expect.isTrue (hasVerdict onBranch "discarded" "PASS") onBranch
        Expect.isFalse (onBranch.Contains "UnusedTestResult") onBranch

        do! onMain state
        let! (onMainOut, mainCode) = ran state [ "test"; name ]
        Expect.equal mainCode 0L onMainOut
        Expect.stringContains onMainOut "UnusedTestResult" onMainOut
      })


let workbenchSaveReportsLint =
  instanceTest "lint: a workbench save shows its warning" (fun state ->
    task {
      do! start state
      let expression =
        String.concat
          "\n"
          [ "let initial = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId Stdlib.Option.Option.None \"\" \"\" []"
            "let editing ="
            "  Darklang.Cli.Workbench.EditingState"
            "    { kind = \"fn\""
            "      nameStr = \"workbench lint fixture\""
            "      targetModule = [ \"Tests\", \"DTWorkbenchLint\" ]"
            "      buf = Stdlib.Cli.UI.Editor.fromText \"let unusedParameter (unused: Int) : Int = 1\""
            "      err = \"\" }"
            "match Darklang.Cli.Workbench.saveEditing initial editing with"
            "| Continue saved -> saved.message"
            "| Exit _ -> \"unexpected exit\""
            "| Launch(_, _) -> \"unexpected launch\""
            "| ToPrompt _ -> \"unexpected prompt\"" ]
      let! (saved, code) = ran state [ "eval"; expression ]
      Expect.equal code 0L saved
      Expect.stringContains saved "UnusedBinding" saved
      Expect.stringContains saved "unused" saved
      do! discardAll state
    })


let lintReporting =
  instanceTest
    "lint: save, JSON, LSP and commit consume separate results"
    (fun state ->
      task {
        do! start state
        let name = "Tests.DTLint.check"
        let declaration =
          "() : Stdlib.Test.T =\n  let _ignoredCheck = Stdlib.Test.eq 1 2\n  Stdlib.Test.eq 1 1"
        let! (saved, saveCode) = ran state [ "fn"; "/" + name; declaration ]
        Expect.equal saveCode 0L saved
        Expect.stringContains saved "UnusedTestResult" saved
        Expect.stringContains saved "_ignoredCheck" saved

        let ordinaryName = "Tests.DTLint.unusedParameter"
        let! (ordinarySaved, ordinarySaveCode) =
          ran state [ "fn"; "/" + ordinaryName; "(unused: Int) : Int = 1" ]
        Expect.equal ordinarySaveCode 0L ordinarySaved
        Expect.stringContains ordinarySaved "UnusedBinding" ordinarySaved
        Expect.stringContains ordinarySaved "unused" ordinarySaved

        let! (json, jsonCode) = ran state [ "typecheck"; "--json" ]
        Expect.equal jsonCode 0L json
        use document = System.Text.Json.JsonDocument.Parse json
        let item =
          document.RootElement.GetProperty("items").EnumerateArray()
          |> Seq.find (fun item -> item.GetProperty("name").GetString() = name)
        Expect.equal (item.GetProperty("verdict").GetString()) "checked" json
        Expect.equal (item.GetProperty("issues").GetArrayLength()) 0 json
        Expect.equal (item.GetProperty("warnings").GetArrayLength()) 1 json
        let ordinaryItem =
          document.RootElement.GetProperty("items").EnumerateArray()
          |> Seq.find (fun item ->
            item.GetProperty("name").GetString() = ordinaryName)
        Expect.equal (ordinaryItem.GetProperty("warnings").GetArrayLength()) 1 json

        let lspExpression =
          """let (_report, lint) = Darklang.LanguageTools.PackageAnalysis.analyzeBranch Darklang.SCM.Branch.mainBranchId
let point = Darklang.LanguageTools.Parser.Point { row = 1; column = 0 }
let range = Darklang.LanguageTools.Parser.Range { start = point; end_ = point }
Darklang.LanguageTools.LspServer.Diagnostics.lintDiagnostics range [] lint
|> Stdlib.List.filter (fun diagnostic -> Stdlib.String.contains diagnostic.message "ignoredCheck")
|> Stdlib.List.map (fun diagnostic -> diagnostic.severity == Stdlib.Option.Option.Some Darklang.LanguageServerProtocol.DiagnosticSeverity.DiagnosticSeverity.Warning)"""
        let! (lsp, lspCode) = ran state [ "eval"; lspExpression ]
        Expect.equal lspCode 0L lsp
        Expect.equal lsp "[true]" lsp

        let! (committed, commitCode) = ran state [ "commit"; "lint fixture"; "-y" ]
        Expect.equal commitCode 0L committed
        Expect.isFalse (committed.Contains "cannot commit") committed

        do! run state [ "branch"; "create"; "lint-report" ]
        let! (onBranch, branchCode) = ran state [ "typecheck"; "--json" ]
        Expect.equal branchCode 0L onBranch
        Expect.stringContains onBranch "ignoredCheck" onBranch

        let modulePath =
          match state with
          | Instance instance -> System.IO.Path.Combine(instance.dir, "lint.dark")
          | _ -> failtest "this fixture requires an isolated instance"
        System.IO.File.WriteAllText(
          modulePath,
          "let check () : Stdlib.Test.T =\n  let ignoredModuleCheck = Stdlib.Test.eq 1 2\n  Stdlib.Test.eq 1 1"
        )
        let! (moduleSaved, moduleCode) =
          ran state [ "module"; "/Tests.DTLintModule"; modulePath ]
        Expect.equal moduleCode 0L moduleSaved
        Expect.stringContains moduleSaved "UnusedTestResult" moduleSaved
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
    approvedCallbacksRespectPermissions
    caughtCallbackDenialDoesNotClassifyLaterRaise
    dictionaryErrorsSurviveReporting
    loggedOutRequiresScope
    emptyRowsFail
    createTest
    createTestTerminalHandoff
    completionAndDocs
    testRunWarnings
    workbenchSaveReportsLint
    lintReporting ]
