module Tests.All

// Main entry point for tests being run

open Expecto
open System.Threading.Tasks

open Prelude

[<EntryPoint>]
let main (args : string array) : int =
  try
    // Most tests don't need trace data on disk; tests that DO check
    // trace contents (CliTraces) flip this to Detailed at their entry.
    LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off

    // Grow the DB from seed if needed. Builtins are deferred (constructed after
    // hashes are generated) because builtin construction triggers hash lookups.
    (LibDB.Seed.growIfNeeded
      (fun () -> TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
      TestUtils.TestUtils.pmRT
      (fun msg -> System.Console.Error.WriteLine msg))
      .Result
    |> ignore<bool>

    let tests =
      [ // core
        Tests.Prelude.tests
        Tests.ProgramTypesToRuntimeTypes.tests
        Tests.Interpreter.tests
        Tests.AnalysisTypes.tests
        Tests.Builtin.tests
        Tests.DvalReprInternalQueryable.tests
        Tests.LibParserRoundTrip.tests
        Tests.LibParser.tests
        Tests.WrittenTypesLoweringParity.tests
        Tests.HttpClient.tests
        Tests.Terminal.tests

        // package manager
        Tests.Propagation.tests
        Tests.Hashing.tests
        Tests.BranchOps.tests

        // serialization
        Tests.BinarySerialization.tests
        Tests.DarkTypesSerialization.tests

        // http server
        Tests.HttpServer.tests
        // CliTraces -- not in the default suite. The cases are sequenced (forced
        // by `Console.SetOut` capture), and `testVersionCommand` runs
        // `dark version`, which fetches the latest release from api.github.com.
        // Enabling it once killed a CI job on a 10-minute no-output timeout,
        // since Expecto prints nothing between "Starting sequenced tests" and
        // the summary.
        //
        // An earlier version of this comment blamed a missing timeout on that
        // fetch. That was wrong: the HTTP client already caps a request at 30s
        // and a connect at 10s, so the stall can't account for ten minutes and
        // the real cause is still unidentified. Reproducing it needs a runner
        // without egress. Don't re-enable this on a guess -- it's been tried.
        //
        // Run them on demand:
        //   scripts/run-backend-tests --filter-test-list CliTraces
        // Tests.CliTraces.tests
        Tests.Toplevels.tests

        // cross-cutting
        Tests.LibExecution.tests.Force()

        Tests.Blob.tests
        Tests.OpsProjections.tests
        Tests.SyncScenarios.tests
        Tests.MultiInstance.tests
        Tests.SyncE2E.tests
        Tests.Releases.tests
        Tests.Stream.tests
        Tests.Capabilities.tests ]

    let cancelationTokenSource = new System.Threading.CancellationTokenSource()
    let httpClientTestsTask = Tests.HttpClient.init cancelationTokenSource.Token

    // Generate this so that we can see if the format has changed in a git diff
    BinarySerialization.generateTestFiles ()

    // this does async stuff within it, so do not run it from a task/async
    // context or it may hang
    //
    // JoinWith Slash because `--filter`'s own help says "a hierarchy that's slash (/)
    // separated" while Expecto's default separator is a dot. So the filter you write
    // after reading the help matches nothing, and Expecto reports that as
    // "0 tests run - Success!". Slashes also make the hierarchy machine-readable,
    // which dots don't: case names contain dots of their own
    // (`Map.mergeFavoringRight`), so nesting and naming were indistinguishable.
    // `--join-with .` gets the old behaviour back.
    let exitCode =
      runTestsWithCLIArgs
        [ Allow_Duplicate_Names; JoinWith "/" ]
        args
        (testList "tests" tests)

    NonBlockingConsole.wait () // flush stdout
    cancelationTokenSource.Cancel()
    httpClientTestsTask.Wait()
    exitCode
  with e ->
    printException "Outer exception" [] e
    NonBlockingConsole.wait () // flush stdout
    1
