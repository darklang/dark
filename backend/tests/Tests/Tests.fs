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
        Tests.AtRestTypeChecker.tests

        // package manager
        Tests.Propagation.tests
        Tests.Hashing.tests
        Tests.BranchOps.tests

        // serialization
        Tests.BinarySerialization.tests
        Tests.DarkTypesSerialization.tests

        // http server
        Tests.HttpServer.tests
        // CliTraces is excluded: it hangs CI, and why is not yet known. The cases
        // are sequenced (`Console.SetOut` capture forces it) so Expecto prints
        // nothing until the summary, and `testVersionCommand` runs `dark version`,
        // which fetches from api.github.com. Request and connect timeouts are
        // already 30s and 10s, so the fetch alone cannot explain a ten-minute
        // stall. Re-enabling needs a repro on a runner without egress, not a guess.
        //
        // Uncomment the line below to run them; no filter reaches them while it is commented out,
        // so nothing currently exercises the tracer end to end.
        // Tests.CliTraces.tests
        Tests.CliScriptLowering.tests
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
