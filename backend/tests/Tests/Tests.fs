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
        Tests.Draft.tests
        Tests.Purge.tests
        Tests.TypeSurface.tests
        Tests.SyncTransport.tests
        Tests.Hashing.tests
        Tests.Config.tests

        // serialization
        Tests.BinarySerialization.tests
        Tests.DarkTypesSerialization.tests

        // http server
        Tests.HttpServer.tests
        // CliTraces — not in the default suite. Two reasons. The cases are
        // sequenced (forced by `Console.SetOut` capture), and more importantly
        // `testVersionCommand` runs `dark version`, which fetches the latest
        // release from api.github.com with no timeout. On a runner without
        // egress that blocks, and since Expecto prints nothing between
        // "Starting sequenced tests" and the summary, CI sees no output and
        // kills the job. Re-enable once `version`'s network check is stubbed
        // and the capture moves to per-call buffers.
        //
        // Run them on demand:
        //   scripts/run-backend-tests --filter-test-list CliTraces
        // Tests.CliTraces.tests
        Tests.Toplevels.tests

        // cross-cutting
        Tests.LibExecution.tests.Force()

        Tests.Blob.tests
        Tests.OpTransport.tests
        Tests.PTConformance.tests
        Tests.BranchOverlay.tests
        Tests.OpsProjections.tests
        Tests.MultiInstance.tests
        // Tests.Releases.tests // REMOVED(kernel-substrate)
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
