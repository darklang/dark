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
        Tests.Draft.tests
        Tests.Purge.tests
        Tests.SyncTransport.tests
        Tests.Hashing.tests
        Tests.Config.tests

        // serialization
        Tests.BinarySerialization.tests
        Tests.DarkTypesSerialization.tests

        // http server
        Tests.HttpServer.tests
        // CliTraces runs again. It was excluded for a suspected CI hang nobody had reproduced, on top
        // of 16 stale cases; both reasons are gone. The cases were never "stale assertions" -- this
        // branch's own conversions had simply never been applied to the file (the pre-uuid main sentinel
        // `initialState ""`, a moved module `Darklang.Cli.Tui.*` -> `Darklang.Stdlib.Cli.Tui.*`, the
        // workbench's flat `scmSection`/`aiSection`/`matterLens` becoming records, `InputState.text`
        // becoming a `TextField`, and `Conflicts.record` taking a Uuid), and fixing them found a product
        // bug nothing else covered: `dark review import` threw on every invocation.
        //
        // The hang suspect was `dark version` making a live GitHub round trip. Measured rather than
        // argued: against an address that DROPS, which is the shape of a runner with no egress, the
        // request gives up after ~11s and the command prints "unable to check for updates". Slow, not a
        // stall. The suspect is gone anyway -- `version --local` skips the check and the test uses it.
        Tests.CliTraces.tests
        Tests.CliScriptLowering.tests
        Tests.Toplevels.tests

        // cross-cutting
        Tests.LibExecution.tests.Force()

        Tests.Blob.tests
        Tests.OpTransport.tests
        Tests.Lww.tests
        Tests.PTConformance.tests
        Tests.BranchOverlay.tests
        Tests.OpsProjections.tests
        Tests.MultiInstance.tests
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
