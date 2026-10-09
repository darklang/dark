/// The reading half of the CLI: navigating the package tree, and looking at what is
/// in it.
///
/// Nothing here is about SCM. These are the commands a person runs before they
/// change anything -- `ls`, `view`, `tree`, `search`, `deps`, `hash` -- plus the
/// authoring verbs that pair with them (`fn`, `val`, `delete`, `restore`,
/// `deprecate`). They had almost no coverage, and Dark resolves names when the line
/// RUNS, so a rename in `packages/` leaves holes here that a green suite cannot see.
/// Two of these tests are regressions for exactly that: `hash` and `find-values`
/// each parsed a name with their own rule and answered "not found" about items
/// `view` printed happily.
module Tests.CliPackages

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


// ─── looking around ───────────────────────────────────────────────────────

let lsNamesWhatIsThere =
  instanceTest "ls names what a module holds" (fun state ->
    task {
      do!
        showsAll
          state
          [ "ls"; "Darklang.Stdlib.Option" ]
          [ "Contents of"; "Option" ]
          "ls lists the module it was given"
      do!
        shows
          state
          [ "ls"; "Darklang.Nope.Nothing" ]
          "ot found"
          "ls says so when the module isn't there"
    })

let treeShowsDescendants =
  instanceTest "tree shows a module's descendants" (fun state ->
    task {
      do!
        showsAll
          state
          [ "tree"; "Darklang.Stdlib.Option" ]
          [ "Package tree"; "Option" ]
          "tree roots at what it was given"
    })

let viewPrintsSource =
  instanceTest "view prints an item's source" (fun state ->
    task {
      do!
        shows
          state
          [ "view"; "Darklang.Stdlib.List.head" ]
          "let head"
          "view prints the declaration"
      do!
        shows
          state
          [ "view"; "Darklang.Stdlib.List.head"; "--raw" ]
          "let head"
          "and --raw prints it without the trimmings"
    })

let viewPrintsTestDefaultsAndSqlErrors =
  instanceTest "view prints inherited test access and short SQL errors" (fun state ->
    task {
      do!
        shows
          state
          [ "view"
            "Darklang.Stdlib.DB.Tests.generatedKeyHas36Characters"
            "--raw" ]
          "test generatedKeyHas36Characters ="
          "an unannotated test has no printed effect row"
      do!
        shows
          state
          [ "view"; "Darklang.Stdlib.DB.Tests.FindAll.rejectsInt8Query"; "--raw" ]
          "=> sqlerror \"Only Int64 integer fields"
          "SQL errors retain their short source form"
      do!
        lacks
          state
          [ "view"; "Darklang.Stdlib.DB.Tests.FindAll.rejectsInt8Query"; "--raw" ]
          "You're using our new experimental Datastore query compiler"
          "the display-only SQL preamble is absent from source"
    })

let viewRefusesWhatIsNotThere =
  instanceTest "view refuses a name that holds nothing" (fun state ->
    task {
      do!
        shows
          state
          [ "view"; "Darklang.Nope.nope" ]
          "Not found"
          "view names what it looked for"
      // Nonzero, because `dark view X || echo missing` is a reasonable thing to
      // write.
      do!
        exits
          state
          [ "view"; "Darklang.Nope.nope" ]
          1L
          "a failed view is a failed command"
    })

/// `view --include-tests` ends a function's view with the tests that call it, read from the result
/// cache and never run. `Stdlib.Float.sqrt` has a package test in every store (`Stdlib.Float.Tests.sqrt`),
/// and a fresh instance has never run `dark test`, so this is also the path where the cache table does
/// not exist yet -- which `view` must answer without creating it. Without the flag there is no section.
let viewListsAFunctionsTests =
  instanceTest
    "view --include-tests lists the tests that call a function"
    (fun state ->
      task {
        do!
          showsAll
            state
            [ "view"; "Darklang.Stdlib.Float.sqrt"; "--include-tests" ]
            [ "Tests:"
              "Darklang.Stdlib.Float.Tests.sqrt"
              "no result for this build"
              "`dark test` runs them" ]
            "view names the function's tests, with no result before any run"
        do!
          lacks
            state
            [ "view"; "Darklang.Stdlib.Float.sqrt" ]
            "Tests:"
            "without the flag, view shows no tests"
        do!
          showsAll
            state
            [ "view"; "Darklang.Stdlib.List.head"; "--include-tests" ]
            [ "Tests:"; "Darklang.Stdlib.List.Tests.head" ]
            "view includes the ported tests for List.head"
        // A stdlib function can gain tests at any time. Use a local fixture
        // with a distinct body so it cannot share a tested function's hash.
        do!
          fn
            state
            "Tests.ViewTests.unreferenced"
            "() : String = \"view --include-tests unreferenced fixture\""
        let! withoutTests =
          runCliPlain
            state
            [ "view"; "Tests.ViewTests.unreferenced"; "--include-tests" ]
        Expect.stringContains withoutTests "let unreferenced" "the fixture exists"
        Expect.isFalse
          (withoutTests.Contains "Tests:")
          $"a function no test calls has no tests section, got: {withoutTests}"
        do!
          exits
            state
            [ "view"; "Darklang.Stdlib.Float.sqrt"; "--raw"; "--include-tests" ]
            1L
            "--include-tests is refused alongside --raw"
      })

/// Package tests follow production edits across owners, just like other callers.
let testsFollowEditsAcrossOwners =
  instanceTest "tests follow production edits across owners" (fun state ->
    task {
      do! start state
      do! fn state "Vendor.TestFollow.half" "(n: Int64) : Int64 = (n / 2L)"
      do!
        run
          state
          [ "test"
            "add"
            "Tests.TestFollow.halfOf8"
            "Vendor.TestFollow.half 8L |> Stdlib.Test.equal 4L" ]
      do!
        shows
          state
          [ "test"; "Tests.TestFollow" ]
          "PASS Tests.TestFollow.halfOf8"
          "the test starts against the current function"
      do! fn state "Vendor.TestFollow.half" "(n: Int64) : Int64 = (n / 3L)"
      do!
        showsAll
          state
          [ "test"; "Tests.TestFollow" ]
          [ "FAIL Tests.TestFollow.halfOf8"; "expected 4, got 2" ]
          "the test follows the edited function across owners"
      do!
        exits
          state
          [ "test"; "Tests.TestFollow" ]
          1L
          "the changed result fails the command"
      do! discardAll state
    })

/// The cache key includes the test's hash, but not a list of what the test uses.
/// Propagation must therefore give it a new hash when a dependency changes,
/// however deep. An edit two calls away must rerun it, not reuse the old pass.
let cachedTestsRerunWhenWhatTheyUseChanges =
  instanceTest "a cached test reruns when anything it uses changes" (fun state ->
    task {
      do! start state
      let file =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          $"dark-cache-test-{System.Guid.NewGuid():N}.dark"
        )
      System.IO.File.WriteAllText(
        file,
        "let inner (n: Int64) : Int64 = (n + 1L)\n\n"
        + "let outer (n: Int64) : Int64 = Tests.CacheT.inner n\n\n"
        + "test outerOf1 =\n"
        + "  Stdlib.Test.expect (Tests.CacheT.outer 1L == 2L) \"expected 2, got 3\"\n"
      )
      do! run state [ "module"; "Tests.CacheT"; file ]
      System.IO.File.Delete file

      do!
        shows
          state
          [ "view"; "Tests.CacheT.outerOf1"; "--raw" ]
          "test outerOf1 ="
          "a test needs no effect annotation to be cached"

      do!
        shows
          state
          [ "test"; "Tests.CacheT" ]
          "1 ran, 0 cached"
          "the first run runs it"
      do!
        shows
          state
          [ "test"; "Tests.CacheT" ]
          "0 ran, 1 cached"
          "an unchanged test is answered from the cache"

      // Two calls away: the test names `outer`, and only `inner` changes.
      do! fn state "Tests.CacheT.inner" "(n: Int64) : Int64 = (n + 2L)"
      do!
        showsAll
          state
          [ "test"; "Tests.CacheT" ]
          [ "FAIL Tests.CacheT.outerOf1"; "expected 2, got 3"; "1 ran, 0 cached" ]
          "a change to something the test reaches indirectly reruns it"
      do!
        shows
          state
          [ "view"; "Tests.CacheT.outer"; "--include-tests" ]
          "expected 2, got 3"
          "views still show the last failure"
      do!
        shows
          state
          [ "test"; "Tests.CacheT" ]
          "1 ran, 0 cached"
          "a failed result is shown but never reused"

      // And directly: `outer 1` is now `inner 0`, which is 2 again, so the
      // stored FAIL must not be handed back.
      do!
        fn
          state
          "Tests.CacheT.outer"
          "(n: Int64) : Int64 = Tests.CacheT.inner (n - 1L)"
      do!
        showsAll
          state
          [ "test"; "Tests.CacheT" ]
          [ "PASS Tests.CacheT.outerOf1"; "1 ran, 0 cached" ]
          "a change to something the test calls directly reruns it"
      do! discardAll state
    })

/// The same content hash can be visible on two branches, but the result was
/// judged under one branch's bindings. Never borrow that pass for another.
let cachedPassesStayOnTheirBranch =
  instanceTest "cached passes are scoped to the current branch" (fun state ->
    task {
      do! start state
      let file =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          $"dark-cache-branch-{System.Guid.NewGuid():N}.dark"
        )
      System.IO.File.WriteAllText(file, "test passes = Stdlib.Test.pass ()\n")
      do! run state [ "module"; "Tests.CacheBranch"; file ]
      System.IO.File.Delete file
      do! commit state "cacheable branch test"

      do!
        shows state [ "test"; "Tests.CacheBranch" ] "1 ran, 0 cached" "main runs it"
      do!
        shows
          state
          [ "test"; "Tests.CacheBranch" ]
          "0 ran, 1 cached"
          "main reuses it"
      do! switch state "cachebranch"
      do!
        shows
          state
          [ "test"; "Tests.CacheBranch" ]
          "1 ran, 0 cached"
          "the branch runs its own copy"
      do!
        shows
          state
          [ "test"; "Tests.CacheBranch" ]
          "0 ran, 1 cached"
          "the branch can reuse its own pass"
      do! onMain state
      do!
        shows
          state
          [ "test"; "Tests.CacheBranch" ]
          "0 ran, 1 cached"
          "main's pass remains"
      do! discardAll state
    })

/// Effectful and dynamically-called code cannot be proved pure, even when a
/// particular run happens to return the same result twice.
let testsWithoutPurityProofAlwaysRun =
  instanceTest "test caching fails closed on effects and dynamic calls" (fun state ->
    task {
      do! start state
      let file =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          $"dark-cache-safety-{System.Guid.NewGuid():N}.dark"
        )
      System.IO.File.WriteAllText(
        file,
        "test randomKey = Stdlib.DB.generateKey () |> Stdlib.String.length |> Stdlib.Test.equal 36\n\n"
        + "test dynamicCall =\n"
        + "  (let fn = (+)\n"
        + "   fn 1L 1L) |> Stdlib.Test.equal 2L\n"
        + "\ntest renderedValue =\n"
        + "  let text = Stdlib.toRepr 1L\n"
        + "  Stdlib.Test.equal text text\n"
        + "\ntest expectedError =\n"
        + "  (1L / 0L)\n"
        + "  => raises \"Cannot divide by 0\"\n"
      )
      do! run state [ "module"; "Tests.CacheSafety"; file ]
      System.IO.File.Delete file

      do!
        shows
          state
          [ "test"; "Tests.CacheSafety" ]
          "4 ran, 0 cached"
          "none of the tests is cached initially"
      do!
        shows
          state
          [ "test"; "Tests.CacheSafety" ]
          "4 ran, 0 cached"
          "effectful, incomplete, rendered, and expected-error tests run again"
      do! discardAll state
    })

let assertionFailureMessagesAlwaysRun =
  instanceTest
    "assertion failure messages are never cached across a type rename"
    (fun state ->
      task {
        do! start state
        let file =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            $"dark-assertion-cache-{System.Guid.NewGuid():N}.dark"
          )
        System.IO.File.WriteAllText(
          file,
          """type First = { cacheSafetyField: Int64 }
test equalMessage =
  match
    Stdlib.Test.equal
      (First { cacheSafetyField = 1L })
      (First { cacheSafetyField = 2L })
  with
  | Pass -> Stdlib.Test.fail "expected an assertion failure"
  | Fail messages ->
    let text = Stdlib.String.join messages " "
    Stdlib.Test.expect (Stdlib.String.contains text "First") "type name changed"
test notEqualMessage =
  match
    Stdlib.Test.notEqual
      (First { cacheSafetyField = 1L })
      (First { cacheSafetyField = 1L })
  with
  | Pass -> Stdlib.Test.fail "expected an assertion failure"
  | Fail messages ->
    let text = Stdlib.String.join messages " "
    Stdlib.Test.expect (Stdlib.String.contains text "First") "type name changed"
"""
        )
        do! run state [ "module"; "Tests.AssertionCache"; file ]
        System.IO.File.Delete file
        for _ in 1..2 do
          do!
            showsAll
              state
              [ "test"; "Tests.AssertionCache" ]
              [ "2 passed, 0 failed"; "2 ran, 0 cached" ]
              "a passing test can have inspected a failed assertion"
        do!
          run
            state
            [ "rename"
              "Tests.AssertionCache.First"
              "Tests.AssertionCache.Second" ]
        do!
          showsAll
            state
            [ "test"; "Tests.AssertionCache" ]
            [ "0 passed, 2 failed"; "type name changed"; "2 ran, 0 cached" ]
            "the renamed type must change both assertion messages"
        do!
          exits
            state
            [ "test"; "Tests.AssertionCache" ]
            1L
            "the failures exit nonzero"
        do! discardAll state
      })

let cachedPassesRequireTheSameBuildAndJudge =
  instanceTest "cached passes from another build or judge rerun" (fun state ->
    task {
      do! start state
      do!
        run
          state
          [ "test"; "add"; "Tests.CacheIdentity.passes"; "Stdlib.Test.pass ()" ]
      do!
        shows state [ "test"; "Tests.CacheIdentity" ] "1 ran, 0 cached" "first run"
      do!
        shows
          state
          [ "test"; "Tests.CacheIdentity" ]
          "0 ran, 1 cached"
          "same build and judge"
      let! key =
        runCliPlain state [ "eval"; "Darklang.Cli.Packages.Test.cacheRuntime ()" ]
      let parts = key.Trim().Split(':')
      Expect.equal parts.Length 2 $"build and judge identities: {key}"
      let validId, _ = System.Guid.TryParse parts[0]
      Expect.isTrue validId "the runtime identity is a build UUID, not a git commit"
      let! again =
        runCliPlain state [ "eval"; "Darklang.Cli.Packages.Test.cacheRuntime ()" ]
      Expect.equal again key "the build identity survives a new CLI process"
      // Keep a real stored pass, but associate it with another runtime or judge.
      for oldKey in [ "old-build:" + parts[1]; parts[0] + ":old-judge" ] do
        let sql =
          "UPDATE package_test_results_v1 "
          + $"SET runtime_hash = '{oldKey}' "
          + $"WHERE runtime_hash = '{key.Trim()}'"
        do!
          exits
            state
            [ "eval"
              "Stdlib.Sqlite.mustExec (Stdlib.LocalStore.path ()) "
              + $"\"{sql}\" []" ]
            0L
            "move the stored pass to an older cache identity"
        do!
          shows
            state
            [ "test"; "Tests.CacheIdentity" ]
            "1 ran, 0 cached"
            "the old pass cannot be reused"
      do! discardAll state
    })

/// `test add` authors one test the way `fn` authors one function: a bare body or a whole
/// declaration, into the draft, and it refuses a name that disagrees with its target rather
/// than saving the test under the wrong one.
let testAddAuthorsATest =
  instanceTest "test add authors a test into the draft" (fun state ->
    task {
      do! start state
      do! fn state "Tests.AddT.double" "(n: Int64) : Int64 = (n + n)"
      do!
        shows
          state
          [ "test"
            "add"
            "Tests.AddT.doubles"
            "Tests.AddT.double 4L |> Stdlib.Test.equal 8L" ]
          "Created test Tests.AddT.doubles"
          "a bare body becomes `test doubles = ...`"
      do!
        showsAll
          state
          [ "test"; "Tests.AddT" ]
          [ "PASS Tests.AddT.doubles"; "1 passed" ]
          "and it runs"
      do!
        shows
          state
          [ "test"; "Tests.AddT" ]
          "1 ran, 0 cached"
          "assertion formatting is conservatively rerun"
      do!
        exits
          state
          [ "test"
            "add"
            "Tests.AddT.doubles"
            "test doubles :{} = Tests.AddT.double 4L |> Stdlib.Test.equal 8L" ]
          1L
          "a test cannot declare its own permission ceiling"
      do!
        exits
          state
          [ "test"
            "add"
            "Tests.AddT.doubles"
            "test doubles = Tests.AddT.double 4L |> Stdlib.Test.equal 8L" ]
          0L
          "a complete test declaration is accepted"
      do!
        lacks
          state
          [ "view"; "Tests.AddT.doubles"; "--raw" ]
          ":{}"
          "tests are displayed without independent permission rows"
      do!
        exits
          state
          [ "test"
            "add"
            "Tests.AddT.other"
            "test doubles = Stdlib.Test.pass ()" ]
          1L
          "a declaration named differently from its target is refused"
      do! exits state [ "test"; "add" ] 1L "and so is a bare `test add`"
      do! discardAll state
    })

let testRunUsesOneStartingStore =
  instanceTest
    "a test run shares its starting store but never its writes"
    (fun state ->
      task {
        do! start state
        do! run state [ "permissions"; "allow"; "native" ]
        let definition =
          """test startingStore =
  let before = Stdlib.LocalStore.configGet "baseline-witness"
  let _ = Stdlib.LocalStore.configSet "baseline-witness" "child"
  Stdlib.Test.expect (before == "before") "starting store changed"
"""
        do! run state [ "test"; "add"; "Tests.Baseline.startingStore"; definition ]
        let! hashOutput =
          runCli state [ "hash"; "Tests.Baseline.startingStore"; "--full" ]
        let hash = hashOutput.Trim().Split(' ') |> Array.last
        let expression =
          $"""let hash = Darklang.LanguageTools.ProgramTypes.Hash.Hash "{hash}"
let branch = Darklang.SCM.Branch.mainBranchId
let _ = Stdlib.LocalStore.configSet "baseline-witness" "before"
let (first, second) = Darklang.LanguageTools.PackageManager.Test.withSnapshot (fun () ->
  let first = Darklang.LanguageTools.PackageManager.Test.execute branch hash
  let _ = Stdlib.LocalStore.configSet "baseline-witness" "after"
  let second = Darklang.LanguageTools.PackageManager.Test.execute branch hash
  (first, second))
let later = Darklang.LanguageTools.PackageManager.Test.withSnapshot (fun () ->
  Darklang.LanguageTools.PackageManager.Test.execute branch hash)
first == Ok (Stdlib.Test.pass ())
&& second == Ok (Stdlib.Test.pass ())
&& later == Ok (Stdlib.Test.fail "starting store changed")
&& Stdlib.LocalStore.configGet "baseline-witness" == "after"
"""
        do!
          evals
            state
            expression
            "true"
            "one frozen baseline per callback, private writes per test"
        do! discardAll state
      })

let isolatedTestDeclarations =
  instanceTest
    "package tests run in fresh stores by default and retain safe caching"
    (fun state ->
      task {
        do! start state
        let add name body =
          run state [ "test"; "add"; $"Tests.IsolatedDeclaration.{name}"; body ]
        do!
          add
            "writes"
            """test writes =
  let before = Stdlib.LocalStore.configGet "isolated-declaration-witness"
  let _ = Stdlib.LocalStore.configSet "isolated-declaration-witness" "child"
  Stdlib.Test.equal before ""
"""
        do!
          add
            "expected"
            """test expected =
  1L / 0L => raises "Cannot divide by 0"
"""
        do!
          add
            "fails"
            """test fails = Stdlib.Test.fail "isolated assertion detail"
"""
        do! add "pure" """test pure = Stdlib.Test.pass ()"""
        do!
          shows
            state
            [ "view"; "Tests.IsolatedDeclaration.expected"; "--raw" ]
            "test expected"
            "isolation needs no extra syntax"
        for counts in [ "4 ran, 0 cached"; "3 ran, 1 cached" ] do
          do!
            showsAll
              state
              [ "test"; "Tests.IsolatedDeclaration" ]
              [ "3 passed, 1 failed"; counts; "isolated assertion detail" ]
              "stateful tests rerun in fresh stores while pure passes may be cached"
        do!
          evals
            state
            "Stdlib.LocalStore.configGet \"isolated-declaration-witness\" == \"\""
            "true"
            "the child's config did not leak into the parent"
        do! discardAll state
      })

let isolatedTestsRejectUnsuccessfulWorkers =
  instanceTest
    "isolated tests reject worker failures and missing results"
    (fun state ->
      task {
        if LibExecution.HostLibc.isPosix then
          do! start state
          do! run state [ "permissions"; "allow"; "native" ]
          let instance =
            match state with
            | Instance i -> i
            | _ -> failtest "requires a disposable CLI instance"
          let file = System.IO.Path.Combine(instance.dir, "worker-tests.dark")
          System.IO.File.WriteAllText(
            file,
            """test passes =
  Stdlib.Cli.Stdin.isInteractive () |> Stdlib.Test.equal false
test fails =
  Stdlib.printLine "{\"Pass\":[]}"
  Stdlib.Test.fail "intentional isolation failure"
test expected = 1L / 0L => raises "Cannot divide by 0"
"""
          )
          do! run state [ "module"; "Tests.WorkerExit"; file ]
          do!
            exits
              state
              [ "test"; "--force"; "Tests.WorkerExit.passes" ]
              0L
              "the test passes when its worker exits normally"
          let wrapper = System.IO.Path.Combine(instance.dir, "worker-exits-17")
          let witness = System.IO.Path.Combine(instance.dir, "worker-exits")
          let quote (value : string) = "'" + value.Replace("'", "'\"'\"'") + "'"
          System.IO.File.WriteAllText(
            wrapper,
            "#!/bin/sh\n"
            + "export DARK_CLI_UNDER_TEST="
            + quote wrapper
            + "\n"
            + quote (System.IO.Path.GetFullPath instance.cli)
            + " \"$@\"\n"
            + "rc=$?\n"
            + "for arg in \"$@\"; do\n"
            + "  if [ \"$arg\" = --test-worker ] && [ \"$rc\" -eq 0 ]; then\n"
            + "    echo 17 >> "
            + quote witness
            + "\n"
            + "    exit 17\n  fi\ndone\nexit \"$rc\"\n"
          )
          System.IO.File.SetUnixFileMode(
            wrapper,
            System.IO.UnixFileMode.UserRead ||| System.IO.UnixFileMode.UserExecute
          )
          let wrapped = Instance { instance with cli = wrapper }
          let! (output, code) =
            runCliWithStatus wrapped [ "test"; "--force"; "Tests.WorkerExit" ]
          Expect.equal code 1 "a worker failure must fail the CLI command"
          Expect.stringContains
            output
            "0 passed, 3 failed"
            "passing, failing, and expected-error bodies cannot hide the worker exit"
          Expect.stringContains
            output
            "Isolated test worker exited 17"
            "exit is diagnosed"
          Expect.stringContains
            output
            "intentional isolation failure"
            "assertion failure is preserved"
          Expect.equal
            (System.IO.File.ReadAllLines witness)
            [| "17"; "17"; "17" |]
            "all workers produced a result and then exited unsuccessfully"
          // A worker may exit before writing its result, even with exit code 0.
          // Dark must reject both cases and preserve the diagnostic output.
          for workerExit in [ 0; 23 ] do
            System.IO.File.WriteAllText(
              wrapper,
              "#!/bin/sh\n"
              + "export DARK_CLI_UNDER_TEST="
              + quote wrapper
              + "\nfor arg in \"$@\"; do\n"
              + "  if [ \"$arg\" = --test-worker ]; then\n"
              + "    echo 'worker stdout'\n    echo 'worker stderr' >&2\n"
              + $"    exit {workerExit}\n  fi\ndone\n"
              + "exec "
              + quote (System.IO.Path.GetFullPath instance.cli)
              + " \"$@\"\n"
            )
            for target in [ "passes"; "expected" ] do
              let! (output, code) =
                runCliWithStatus
                  wrapped
                  [ "test"; "--force"; "Tests.WorkerExit." + target ]
              Expect.equal code 1 "a missing result must fail the CLI command"
              for message in
                [ "0 passed, 1 failed"; "worker stdout"; "worker stderr" ] do
                Expect.stringContains output message "missing-result diagnostics"

      })


let testsUseInstanceAndFunctionPolicies =
  instanceTest
    "tests allow effects by default while preserving package and function policies"
    (fun state ->
      task {
        do! start state
        let source =
          """let clock () : Bool =
  let _ = Stdlib.DateTime.now ()
  true
let restricted () :{} Bool =
  let _ = Stdlib.DateTime.now ()
  true
let caller () :{Native} Stdlib.Test.Result =
  match Stdlib.Test.Process.run (Stdlib.Test.Process.defaults ()) Tests.PermissionUse.clock () with
  | Error message -> Stdlib.Test.fail message
  | Ok output ->
    match output.result with
    | Ok _ -> Stdlib.Test.pass ()
    | Error message -> Stdlib.Test.fail message
test direct = Tests.PermissionUse.clock () |> Stdlib.Test.equal true
test isolated =
  match Stdlib.Test.Process.run (Stdlib.Test.Process.defaults ()) Tests.PermissionUse.clock () with
  | Error message -> Stdlib.Test.fail message
  | Ok output ->
    match output.result with
    | Ok value -> value |> Stdlib.Test.equal true
    | Error message -> Stdlib.Test.fail message
test functionCeiling = Tests.PermissionUse.restricted () |> Stdlib.Test.equal true
test callerCeiling = Tests.PermissionUse.caller ()
"""
        let sourceFile =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            $"dark-test-permissions-{System.Guid.NewGuid():N}.dark"
          )
        try
          System.IO.File.WriteAllText(sourceFile, source)
          do!
            exits
              state
              [ "module"; "Tests.PermissionUse"; sourceFile ]
              0L
              "author functions and their tests"
        finally
          System.IO.File.Delete sourceFile
        let selected =
          [ "test"
            "--force"
            "Tests.PermissionUse.direct"
            "Tests.PermissionUse.isolated" ]
        do!
          shows
            state
            selected
            "package policy"
            "unapproved functions are denied in either execution mode"
        do! exits state selected 1L "denials fail the test run"
        do!
          run
            state
            [ "permissions"; "approve"; "Tests.PermissionUse.clock"; "--yes" ]
        do!
          shows
            state
            selected
            "2 passed, 0 failed"
            "the same approval permits ordinary and isolated calls"
        do!
          exits
            state
            selected
            0L
            "approved functions run under the test instance policy"
        do! run state [ "permissions"; "deny"; "clock" ]
        let! savedPolicy = runCliPlain state [ "permissions"; "list" ]
        do!
          shows
            state
            selected
            "2 passed, 0 failed"
            "ordinary and isolated tests allow clock even when the installation denies it"
        do! exits state selected 0L "tests have an allow-all instance boundary"
        let! afterTests = runCliPlain state [ "permissions"; "list" ]
        Expect.equal afterTests savedPolicy "tests do not rewrite the saved policy"
        do!
          shows
            state
            [ "eval"; "Tests.PermissionUse.clock ()" ]
            "instance policy"
            "ordinary eval still uses the installation's denial"
        do!
          exits
            state
            [ "eval"; "Tests.PermissionUse.clock ()" ]
            1L
            "eval stays denied"
        for name in [ "restricted"; "caller" ] do
          do!
            run
              state
              [ "permissions"; "approve"; "Tests.PermissionUse." + name; "--yes" ]
        do!
          shows
            state
            [ "test"; "--force"; "Tests.PermissionUse.functionCeiling" ]
            "function policy"
            "the tested function's ceiling applies"
        do!
          exits
            state
            [ "test"; "--force"; "Tests.PermissionUse.functionCeiling" ]
            1L
            "an instance grant cannot widen a function"
        do!
          shows
            state
            [ "test"; "--force"; "Tests.PermissionUse.callerCeiling" ]
            "function policy"
            "captured caller restrictions survive isolation"
        do!
          exits
            state
            [ "test"; "--force"; "Tests.PermissionUse.callerCeiling" ]
            1L
            "isolating a callback cannot widen its caller"
        do! discardAll state
      })

/// Search leaves tests out unless asked: a test is usually named after what it tests, so
/// every search for a function would list its tests too. Left out is not hidden, though:
/// the output says how many matched, so a search with only test hits never reads as empty.
let searchLeavesTestsOutUnlessAsked =
  instanceTest "search leaves tests out unless asked" (fun state ->
    task {
      do!
        lacks
          state
          [ "search"; "ceiling" ]
          "Float.Tests.ceiling"
          "a plain search lists no tests"
      do!
        shows
          state
          [ "search"; "ceilingRejectsNan" ]
          "1 test matched; --include-tests shows them"
          "and says what it left out"
      do!
        shows
          state
          [ "search"; "ceiling"; "--include-tests" ]
          "Darklang.Stdlib.Float.Tests.ceilingRejectsNan"
          "--include-tests lists them"
      do!
        shows
          state
          [ "search"; "ceiling"; "--test" ]
          "Darklang.Stdlib.Float.Tests.ceilingRejectsNan"
          "and --test asks for them by itself"
    })

/// A Code-view workbench state standing in <paramref name="modules"/>, with the cursor on the row named
/// <paramref name="row"/>, as one line of Dark for `eval`. Built from `initialState`, so no terminal.
let private workbenchAt (modules : List<string>) (row : string) : string =
  let path = modules |> List.map (fun m -> $"\"{m}\"") |> String.concat ", "
  "let st = Darklang.Cli.Workbench.initialState (Darklang.SCM.PackageOps.currentBranch ()) (Stdlib.Option.Option.None) \"T\" \"i\" [] false in "
  + $"let s0 = {{ st with activeView = Darklang.Cli.Workbench.vMatter; location = Darklang.Cli.Packages.PackageLocation.Module [ {path} ] }} in "
  + "let s1 = { s0 with items = Darklang.Cli.Workbench.reloadItems s0 } in "
  + $"let s = {{ s1 with selected = (Stdlib.List.indexedMap s1.items (fun i it -> (i, it.name)) |> Stdlib.List.findFirst (fun (_, nm) -> nm == \"{row}\") |> Stdlib.Option.map (fun (i, _) -> i) |> Stdlib.Option.withDefault 0) }} in "

/// The Code view lists a module's tests as rows of their own, and the Inspect pane shows a test's
/// source and result, and a function's tests. Every row kind here used to fall through to "value",
/// so a test row would have looked itself up as a value and shown "(not found)".
let workbenchShowsTests =
  instanceTest "the workbench lists and inspects tests" (fun state ->
    task {
      let floatTests = [ "Darklang"; "Stdlib"; "Float"; "Tests" ]
      do!
        evals
          state
          ((workbenchAt floatTests "ceiling")
           + "Stdlib.List.map s.items (fun it -> it.kind + \":\" + it.name)")
          "test:ceiling"
          "a module's tests are rows of kind test"
      do!
        evals
          state
          ((workbenchAt floatTests "ceiling")
           + "Stdlib.String.join (Darklang.Cli.Workbench.detailLines s) \"\\n\"")
          "test ceiling ="
          "the Inspect pane shows a test's source"
      do!
        evals
          state
          ((workbenchAt floatTests "ceiling")
           + "match Stdlib.List.getAt s.items s.selected with "
           + "| Some item -> Darklang.Cli.Workbench.itemMeta s item "
           + "(Darklang.Cli.Packages.Query.searchExactMatch s.branchId "
           + "(Darklang.Cli.Packages.modulePathOf s.location) item.name) "
           + "| None -> \"\"")
          "test ·"
          "and the meta line calls it a test"
      do!
        evals
          state
          ((workbenchAt [ "Darklang"; "Stdlib"; "Float" ] "ceiling")
           + "Stdlib.String.join (Darklang.Cli.Workbench.inspectPageLines s) \"\\n\"")
          "Darklang.Stdlib.Float.Tests.ceiling"
          "a function's Inspect pane lists the tests that call it"
    })

/// Renaming from the workbench ends the old name. It used to build the old location from the
/// owner-first module path, so it unbound a name that did not exist and both names stayed bound.
let workbenchRenameEndsTheOldName =
  instanceTest "a workbench rename leaves one name, not two" (fun state ->
    task {
      do! start state
      do! fn state "Tests.WbRename.before" "() : Int64 = 7L"
      do!
        evals
          state
          ((workbenchAt [ "Tests"; "WbRename" ] "before")
           + "match Darklang.Cli.Workbench.performInputAction s (Darklang.Cli.Workbench.InputState { prompt = \"\"; field = Stdlib.Cli.UI.TextField.fromText \"after\"; action = \"rename\" }) with | Continue s2 -> s2.message | _ -> \"no\"")
          "renamed to after"
          "the workbench says it renamed"
      do! evals state "Tests.WbRename.after ()" "7" "the new name resolves"
      do! notFound state "Tests.WbRename.before ()" "and the old name is gone"
      do! discardAll state
    })

let searchFindsByText =
  instanceTest "search finds items by text" (fun state ->
    task {
      do!
        showsAll
          state
          [ "search"; "head" ]
          [ "Search results for"; "head" ]
          "search reports what it matched"
    })

let depsNamesWhatAnItemUses =
  instanceTest "deps names what an item uses" (fun state ->
    task {
      do!
        shows
          state
          [ "deps"; "Darklang.Stdlib.List.head" ]
          "Option"
          "head returns an Option, so it depends on one"
      do!
        shows
          state
          [ "deps"; "Darklang.Nope.nope" ]
          "Not found"
          "deps says so when there's nothing to walk"
    })

/// Regression: `hash` split the name itself and read the FIRST segment as the owner,
/// so `hash Stdlib.List.head` looked for an owner called Stdlib and said "Not found"
/// about a name `view` prints. It traverses now, like everything else that takes a
/// name.
let hashResolvesNamesLikeViewDoes =
  instanceTest "hash takes the same names view takes" (fun state ->
    task {
      do!
        shows
          state
          [ "view"; "Stdlib.List.head" ]
          "let head"
          "view takes the un-owner-qualified name"
      do! shows state [ "hash"; "Stdlib.List.head" ] "fn " "and so does hash"
      do!
        shows
          state
          [ "hash"; "Darklang.Stdlib.List.head" ]
          "fn "
          "the fully qualified one too"
      do! lacks state [ "hash"; "Stdlib.List.head" ] "Not found" "the name resolves"
    })

let hashLongIsTheShortOneSpelledOut =
  instanceTest "hash --long extends the short hash" (fun state ->
    task {
      let! short = runCli state [ "hash"; "Stdlib.List.head" ]
      let! long = runCli state [ "hash"; "--long"; "Stdlib.List.head" ]
      let trim (s : string) = s.Replace("fn ", "").Trim()
      Expect.isTrue
        ((trim long).StartsWith(trim short))
        $"the short hash is a prefix of the long one, got short={short} long={long}"
    })

let hashOfAModuleSaysSo =
  instanceTest "hash of a module explains itself" (fun state ->
    task {
      do!
        shows
          state
          [ "hash"; "Stdlib.Option" ]
          "module"
          "a module is a path, not content"
      do!
        shows
          state
          [ "hash"; "Darklang.Nope.nope" ]
          "Not found"
          "and a missing name is missing"
    })

/// `dark nav X` moves for the length of that one command, like a shell's cwd inside a subshell.
/// The move has to SAY so, or the next `dark ls` disagreeing with it is how you find out.
let navIsHonestAboutHowLongItLasts =
  instanceTest "nav moves, and says how long the move lasts" (fun state ->
    task {
      do!
        showsAll
          state
          [ "nav"; "Darklang.Stdlib.List" ]
          [ "Changed to"; "Stdlib.List" ]
          "nav reports the move"
      do!
        shows
          state
          [ "nav"; "Darklang.Stdlib.List" ]
          "doesn't carry between"
          "and says it does not persist"
      do!
        shows
          state
          [ "nav"; "Darklang.Nope.Nowhere" ]
          "failed"
          "nav says so when the path isn't there"
      // Reading elsewhere without moving is what actually works from a shell.
      do!
        shows
          state
          [ "ls"; "Darklang.Stdlib.List" ]
          "Contents of"
          "ls reads anywhere by path"
    })


/// Regression, same family as `hash`: find-values split the type name itself.
let findValuesTakesTheSameNames =
  instanceTest "find-values takes the same names view takes" (fun state ->
    task {
      do!
        lacks
          state
          [ "find-values"; "Stdlib.Option.Option" ]
          "Type not found"
          "the type resolves un-owner-qualified"
      do!
        sane
          state
          [ "find-values"; "Darklang.Stdlib.Option.Option" ]
          "and fully qualified"
    })

let referenceCommandsAnswer =
  instanceTest "the reference commands answer" (fun state ->
    task {
      do! shows state [ "builtins" ] "Int:" "builtins lists them by module"
      do! shows state [ "commands" ] "nav" "commands lists the registry"
      do! shows state [ "docs" ] "for-ai" "docs lists its topics"
      do! sane state [ "docs"; "syntax" ] "and prints one"
      do!
        shows
          state
          [ "docs"; "nosuchtopic" ]
          "nosuchtopic"
          "an unknown topic names itself back"
    })


// ─── authoring ────────────────────────────────────────────────────────────

let authoringRoundTrips =
  instanceTest "what you author is what you read back" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Round.answer" "() : Int64 = 42L"
      do!
        shows
          state
          [ "view"; "Tests.Round.answer" ]
          "42"
          "view shows what was just authored"
      do! evals state "Tests.Round.answer ()" "42" "and it runs"
      do! shows state [ "hash"; "Tests.Round.answer" ] "fn " "and it has a hash"
      do! discardAll state
    })

let valuesRoundTrip =
  instanceTest "a value round-trips too" (fun state ->
    task {
      do! start state
      do! value state "Tests.Round.eleven" "11L"
      do! evals state "Tests.Round.eleven" "11" "the value is readable by name"
      do! discardAll state
    })

/// `delete` retires an item rather than unbinding the name: the log is append-only,
/// and something that called it yesterday still has to resolve. So the observable
/// effect is the deprecation badge, and `restore` takes it off again.
let deleteAndRestore =
  instanceTest "delete retires an item, restore brings it back" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Retire.f" "() : Int64 = 50505L"
      do! commit state "add f"
      do! evals state "Tests.Retire.f ()" "50505" "it runs before the delete"

      do!
        showsAnyCase
          state
          [ "delete"; "Tests.Retire.f"; "-y" ]
          "deprecated"
          "delete says what it did"
      do!
        showsAnyCase
          state
          [ "view"; "Tests.Retire.f" ]
          "deprecated"
          "the item is marked retired"
      do!
        evals
          state
          "Tests.Retire.f ()"
          "50505"
          "and it still runs, because callers still resolve it"

      // A Deprecate binds no name, so a `status` that counts only bindings calls this clean while
      // the op sits in the draft, waiting to be swept into the next commit.
      do!
        lacks
          state
          [ "status" ]
          "clean"
          "an uncommitted deprecation is not a clean tree"
      do!
        shows
          state
          [ "status" ]
          "no new version"
          "and status says what kind of op it is"

      do!
        showsAnyCase
          state
          [ "restore"; "Tests.Retire.f"; "-y" ]
          "undeprecated"
          "restore takes it back"
      do!
        lacksAnyCase
          state
          [ "view"; "Tests.Retire.f" ]
          "deprecated"
          "and the mark is gone"
      do! discardAll state
    })

let deleteRefusesWhatIsNotThere =
  instanceTest "delete refuses a name that holds nothing" (fun state ->
    task {
      do!
        refuses
          state
          [ "delete"; "Tests.Nope.nope"; "-y" ]
          "live item"
          "deleted"
          "delete says what it looked for"
    })

let deprecateAndUndeprecate =
  instanceTest "deprecate marks an item, undeprecate clears it" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Dep.old" "() : Int64 = 1L"
      do! commit state "add old"

      do! deprecate state "Tests.Dep.old"
      do!
        showsAnyCase
          state
          [ "view"; "Tests.Dep.old" ]
          "deprecated"
          "the badge shows on the item"
      do! evals state "Tests.Dep.old ()" "1" "a deprecated item still runs"

      do! run state [ "undeprecate"; "Tests.Dep.old" ]
      do!
        lacksAnyCase
          state
          [ "view"; "Tests.Dep.old" ]
          "deprecated"
          "and the badge goes away again"
      do! discardAll state
    })

/// Ocean's 3, and the shape it settled into. A doc comment is not behaviour, so editing one leaves
/// the item's hash alone -- which means the edit cannot ride on an `AddFn` (ops are
/// content-addressed, so that op IS the earlier one and folds to nothing) and rides on an `UpdateDoc`
/// instead.
let aDocOnlyEditKeepsTheVersionAndStillLands =
  instanceTest
    "editing only the docs changes the docs and nothing else"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Docs.f" "() : Int64 = 7L"
        do! fn state "Tests.Docs.caller" "() : Int64 = Tests.Docs.f () + 1L"
        do! commit state "add f and its caller"

        let! before = runCliPlain state [ "hash"; "Tests.Docs.f" ]

        do! fn state "Tests.Docs.f" "/// Returns seven.\nlet f (): Int64 =\n  7L"

        let! after = runCliPlain state [ "hash"; "Tests.Docs.f" ]
        Expect.equal
          after
          before
          $"the version did not move, got {after} from {before}"

        do!
          shows
            state
            [ "view"; "Tests.Docs.f" ]
            "Returns seven"
            "and the new text is what you read"
        do! shows state [ "ops" ] "UpdateDoc" "the op log says what happened"
        do! dirty state "an uncommitted doc edit is not a clean tree"
        do! evals state "Tests.Docs.caller ()" "8" "its caller is untouched"

        do! commit state "document f"
        do!
          shows
            state
            [ "view"; "Tests.Docs.f" ]
            "Returns seven"
            "and it survives the commit"
        do! discardAll state
      })

/// The other half: a doc edit made on a branch is the branch's opinion until it merges.
let aBranchesDocEditStaysOnTheBranch =
  instanceTest
    "a doc edit on a branch is invisible to main until it merges"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.DocsBr.f" "() : Int64 = 3L"
        do! fn state "Tests.DocsBr.f" "/// Main's wording.\nlet f (): Int64 =\n  3L"
        do! commit state "add f, documented"

        do! switch state "docsbr"
        do!
          fn
            state
            "Tests.DocsBr.f"
            "/// The branch's wording.\nlet f (): Int64 =\n  3L"
        do!
          shows
            state
            [ "view"; "Tests.DocsBr.f" ]
            "branch's wording"
            "the branch reads its own"
        do! commit state "reword"

        do! onMain state
        do!
          shows
            state
            [ "view"; "Tests.DocsBr.f" ]
            "Main's wording"
            "main still reads main's"
        do!
          lacks
            state
            [ "view"; "Tests.DocsBr.f" ]
            "branch's wording"
            "and not the branch's"

        do! merge state "docsbr"
        do!
          shows
            state
            [ "view"; "Tests.DocsBr.f" ]
            "branch's wording"
            "the merge brings it over"
        do! discardAll state
      })

/// The nested targets, which are the ones that used to be dropped on the floor.
///
/// A record field's, an enum case's and a parameter's doc are all outside the identity hash, so an
/// edit to one alone leaves the item's version exactly where it was -- and before `UpdateDoc` there
/// was no op that could carry it, so the CLI said "unchanged: nothing saved" and the words went
/// nowhere. Each is asserted separately because each reaches a different part of the declaration.
let aFieldsDocEditLands =
  instanceTest "editing only a record field's doc saves it" (fun state ->
    task {
      do! start state
      do!
        run
          state
          [ "type"
            "Tests.FieldDocs.Coord"
            "{\n  /// how far along\n  alongwards: Int64\n}" ]
      do! commit state "add Coord"

      let! before = runCliPlain state [ "hash"; "Tests.FieldDocs.Coord" ]

      do!
        run
          state
          [ "type"
            "Tests.FieldDocs.Coord"
            "{\n  /// distance along the axis\n  alongwards: Int64\n}" ]

      let! after = runCliPlain state [ "hash"; "Tests.FieldDocs.Coord" ]
      Expect.equal
        after
        before
        $"the version did not move, got {after} from {before}"

      do!
        shows
          state
          [ "view"; "Tests.FieldDocs.Coord" ]
          "distance along the axis"
          "the field's new wording is what you read"
      do!
        shows
          state
          [ "ops" ]
          "UpdateDoc Tests.FieldDocs.Coord field"
          "and the op names the NAME and WHICH part of it"
      do! dirty state "an uncommitted field-doc edit is not a clean tree"
      do! commit state "reword the field"
      do!
        shows
          state
          [ "view"; "Tests.FieldDocs.Coord" ]
          "distance along the axis"
          "and it survives the commit"
      do! discardAll state
    })

let anEnumCasesDocEditLands =
  instanceTest "editing only an enum case's doc saves it" (fun state ->
    task {
      do! start state
      do!
        run
          state
          [ "type"
            "Tests.CaseDocs.Signal"
            "| /// stop here\n  Halting\n| /// carry on\n  Proceeding" ]
      do! commit state "add Signal"

      let! before = runCliPlain state [ "hash"; "Tests.CaseDocs.Signal" ]

      do!
        run
          state
          [ "type"
            "Tests.CaseDocs.Signal"
            "| /// come to a full stop\n  Halting\n| /// carry on\n  Proceeding" ]

      let! after = runCliPlain state [ "hash"; "Tests.CaseDocs.Signal" ]
      Expect.equal
        after
        before
        $"the version did not move, got {after} from {before}"

      do!
        shows
          state
          [ "view"; "Tests.CaseDocs.Signal" ]
          "come to a full stop"
          "the case's new wording is what you read"
      do!
        shows
          state
          [ "ops" ]
          "UpdateDoc Tests.CaseDocs.Signal case"
          "and the op names the NAME and WHICH case"
      do! discardAll state
    })

/// A parameter's doc, which is the one addressed by POSITION rather than by name: a parameter name
/// is not in the identity hash, so two functions differing only in their parameter names are one
/// item and a name would not identify anything.
let aParametersDocEditLands =
  instanceTest "editing only a parameter's doc saves it" (fun state ->
    task {
      do! start state
      do!
        fn
          state
          "Tests.ParamDocs.scale"
          "/// scales\nlet scale (/// the multiplier\n           factorly: Int64) : Int64 =\n  factorly"
      do! commit state "add scale"

      let! before = runCliPlain state [ "hash"; "Tests.ParamDocs.scale" ]

      do!
        fn
          state
          "Tests.ParamDocs.scale"
          "/// scales\nlet scale (/// what to multiply by\n           factorly: Int64) : Int64 =\n  factorly"

      let! after = runCliPlain state [ "hash"; "Tests.ParamDocs.scale" ]
      Expect.equal
        after
        before
        $"the version did not move, got {after} from {before}"

      do!
        shows
          state
          [ "view"; "Tests.ParamDocs.scale" ]
          "what to multiply by"
          "the parameter's new wording is what you read"
      do!
        shows
          state
          [ "ops" ]
          "parameter 1"
          "and the op names the parameter by position, since its name is not identity"
      do! discardAll state
    })


/// Two people writing different prose for one doc, neither having seen the other's.
///
/// `previous` is what makes this detectable: an incoming doc op that names a text this store never
/// held was not written on top of what we hold. The newer text still wins, so the store stays
/// usable, and the record is what keeps the loser's words findable instead of gone.
///
/// The op is authored here rather than synced because the sync gates cover the transport; what
/// needs asserting is the FOLD's answer, which is the same either way.
/// The reason docs are scoped to a NAME rather than to content.
///
/// Ten `ParseError` types in the stdlib are literally one item: same declaration, so one hash. What
/// `Int64.ParseError` means is not what `UInt64.ParseError` means, so one doc for the item cannot
/// be right. A doc edit at one name must not be visible at the other.
let twoNamesHoldingOneItemHaveTheirOwnDocs =
  instanceTest "two names holding one item document it separately" (fun state ->
    task {
      do! start state
      do! fn state "Tests.DocOne.f" "() : Int64 = 5301L"
      do! fn state "Tests.DocTwo.f" "() : Int64 = 5301L"
      do! commit state "one body, two names"

      // Same body, so ONE item: this is the premise, not an accident of the fixture.
      let! one = runCliPlain state [ "hash"; "Tests.DocOne.f" ]
      let! two = runCliPlain state [ "hash"; "Tests.DocTwo.f" ]
      Expect.equal two one $"the two names hold one item, got {one} and {two}"

      do!
        fn
          state
          "Tests.DocOne.f"
          "/// what the first name means\nlet f (): Int64 =\n  5301L"

      do!
        shows
          state
          [ "view"; "Tests.DocOne.f" ]
          "what the first name means"
          "the name that was edited says the new thing"
      do!
        lacks
          state
          [ "view"; "Tests.DocTwo.f" ]
          "what the first name means"
          "and the other name holding the same item does not"

      do! commit state "document the first name"
      do!
        lacks
          state
          [ "view"; "Tests.DocTwo.f" ]
          "what the first name means"
          "still not, once committed"
      do! discardAll state
    })


let twoWordingsForOneDocRecordAConflict =
  instanceTest
    "a doc edit made against a text this store never had is a conflict"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.DocClash.f"
            "/// ours, written here\nlet f (): Int64 =\n  5101L"
        do! commit state "add f, documented"

        // What a peer's op looks like on arrival: it names a predecessor nobody here ever wrote.
        let theirs =
          "Darklang.SCM.PackageOps.add (Builtin.scmCurrentBranch ()) "
          + "[Darklang.LanguageTools.ProgramTypes.PackageOp.UpdateDoc("
          + "Darklang.LanguageTools.ProgramTypes.PackageLocation "
          + "{ owner = \"Tests\"; modules = [\"DocClash\"]; name = \"f\" }, "
          + "Darklang.LanguageTools.ProgramTypes.DocPart.WholeItem, "
          + "\"theirs, written somewhere else\", "
          + "Stdlib.Option.Option.Some (Darklang.LanguageTools.ProgramTypes.hashOfText "
          + "\"a text this store never had\"), Stdlib.Option.Option.None)]"

        do! run state [ "eval"; theirs ]

        do!
          shows
            state
            [ "view"; "Tests.DocClash.f" ]
            "theirs, written somewhere else"
            "the newer wording wins, so the store stays usable"
        do!
          showsAll
            state
            [ "conflicts" ]
            [ "Tests.DocClash.f"; "neither made from the other" ]
            "and the divergence is recorded rather than swallowed"

        // A doc divergence has no side to take -- both candidates are TEXT -- so `override` says so
        // instead of trying to bind a name to the hash of a sentence.
        // THIS conflict's id, not the first one listed: every CLI test shares one store, and the
        // doc tests above leave their own divergences in it.
        let! listed = runCliPlain state [ "conflicts" ]

        let id =
          listed.Split('\n')
          |> Array.filter (fun line -> line.Contains "Tests.DocClash.f")
          |> Array.tryHead
          |> Option.defaultWith (fun () ->
            Tests.failtestf
              "no conflict listed for Tests.DocClash.f, got: %s"
              listed)
          |> fun line -> line.Split('#') |> Array.item 1
          |> fun rest -> rest.Split(' ') |> Array.head

        // The review screen shows the two SENTENCES. A hash of a sentence tells a reader nothing
        // they can choose between, and choosing is the point of the screen.
        do!
          showsAll
            state
            [ "conflicts"; "show"; id ]
            [ "two wordings"
              "ours, written here"
              "theirs, written somewhere else" ]
            "both wordings are on the screen you choose from"

        // Taking a side SAYS the chosen wording: it authors the op, so the choice syncs and holds.
        do!
          shows
            state
            [ "conflicts"; "override"; id; "A" ]
            "now says"
            "override takes one of the two wordings"
        do!
          shows
            state
            [ "view"; "Tests.DocClash.f" ]
            "ours, written here"
            "and the name says what was chosen"

        // Settled, and ASSERTED settled: this test deliberately puts a divergence in a store every
        // other CLI test shares, so leaving one behind is leaving a landmine.
        do! lacks state [ "conflicts" ] id "and it is gone from the pending list"
        do! discardAll state
      })


/// Writing a declaration without a `///` does not wipe the doc that is there.
///
/// Content is shared: two functions with the same body are ONE item, and a doc is said about the
/// item. So omitting a doc comment cannot mean "nobody's words apply any more" -- it means this
/// author did not write any.
let authoringWithoutADocDoesNotClearOne =
  instanceTest
    "saving a declaration with no doc comment leaves the existing one alone"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.DocKeep.f"
            "/// what it is for\nlet f (): Int64 =\n  5201L"
        do! commit state "add f, documented"

        do! fn state "Tests.DocKeep.f" "() : Int64 = 5201L"
        do!
          shows
            state
            [ "view"; "Tests.DocKeep.f" ]
            "what it is for"
            "the doc survives a save that did not mention it"
        do! discardAll state
      })


/// Re-authoring the same source before committing, which is what an editor's save button does.
///
/// A rebind to the hash a name already holds folds to nothing -- no new `locations` row -- so the
/// draft ends with two namings and only the FIRST wrote anything. Collapse keeps one naming per
/// name at commit, and keeping the wrong one of those two deleted the only row: the commit
/// reported success and the function stopped existing.
let reAuthoringTheSameSourceSurvivesTheCommit =
  instanceTest
    "authoring the same source twice, then committing, keeps the name"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Twice.f" "() : Int64 = 4901L"
        do! fn state "Tests.Twice.f" "() : Int64 = 4901L"
        do! commit state "add f, said twice"

        do!
          shows
            state
            [ "view"; "Tests.Twice.f" ]
            "4901"
            "the name still holds what was authored"
        do! evals state "Tests.Twice.f ()" "4901" "and it still runs"
        do! discardAll state
      })

/// The other half of the same rule: when the hash really does move and move back inside one draft,
/// the LAST naming is the one that wrote the live row, and it is the one to keep.
let aVersionMovedAndMovedBackKeepsTheLastNaming =
  instanceTest
    "editing a function and putting it back, then committing, keeps the version put back"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.ThereAndBack.f" "() : Int64 = 4902L"
        do! fn state "Tests.ThereAndBack.f" "() : Int64 = 4903L"
        do! fn state "Tests.ThereAndBack.f" "() : Int64 = 4902L"
        do! commit state "there and back"

        do!
          evals
            state
            "Tests.ThereAndBack.f ()"
            "4902"
            "the name holds the version it was put back to"
        do! discardAll state
      })


let renameIsVisibleToEverythingThatReads =
  instanceTest
    "a renamed item is readable at its new name, by every reader"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Ren.before" "() : Int64 = 3L"
        do! commit state "add before"
        do! run state [ "rename"; "Tests.Ren.before"; "Tests.Ren.after" ]

        do!
          shows
            state
            [ "view"; "Tests.Ren.after" ]
            "3"
            "view finds it at the new name"
        do! shows state [ "hash"; "Tests.Ren.after" ] "fn " "hash does too"
        do! evals state "Tests.Ren.after ()" "3" "and it runs there"
        do!
          shows
            state
            [ "view"; "Tests.Ren.before" ]
            "Not found"
            "the old name holds nothing"
        do! discardAll state
      })


let tests : List<Test> =
  [ lsNamesWhatIsThere
    treeShowsDescendants
    viewPrintsSource
    viewPrintsTestDefaultsAndSqlErrors
    viewRefusesWhatIsNotThere
    viewListsAFunctionsTests
    testsFollowEditsAcrossOwners
    cachedTestsRerunWhenWhatTheyUseChanges
    cachedPassesStayOnTheirBranch
    testsWithoutPurityProofAlwaysRun
    assertionFailureMessagesAlwaysRun
    cachedPassesRequireTheSameBuildAndJudge
    testAddAuthorsATest
    testRunUsesOneStartingStore
    isolatedTestDeclarations
    isolatedTestsRejectUnsuccessfulWorkers
    testsUseInstanceAndFunctionPolicies
    workbenchShowsTests
    workbenchRenameEndsTheOldName
    searchFindsByText
    searchLeavesTestsOutUnlessAsked
    depsNamesWhatAnItemUses
    hashResolvesNamesLikeViewDoes
    hashLongIsTheShortOneSpelledOut
    hashOfAModuleSaysSo
    navIsHonestAboutHowLongItLasts
    findValuesTakesTheSameNames
    referenceCommandsAnswer
    authoringRoundTrips
    valuesRoundTrip
    deleteAndRestore
    deleteRefusesWhatIsNotThere
    deprecateAndUndeprecate
    renameIsVisibleToEverythingThatReads
    aDocOnlyEditKeepsTheVersionAndStillLands
    aFieldsDocEditLands
    anEnumCasesDocEditLands
    aParametersDocEditLands
    aBranchesDocEditStaysOnTheBranch
    reAuthoringTheSameSourceSurvivesTheCommit
    aVersionMovedAndMovedBackKeepsTheLastNaming
    twoNamesHoldingOneItemHaveTheirOwnDocs
    twoWordingsForOneDocRecordAConflict
    authoringWithoutADocDoesNotClearOne ]
