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
        Tests.TreeSitter.tests
        Tests.ProgramTypesToRuntimeTypes.tests
        Tests.Interpreter.tests
        Tests.AnalysisTypes.tests
        Tests.Builtin.tests
        Tests.DvalReprInternalQueryable.tests
        Tests.LibParser.tests
        Tests.NewParser.tests
        Tests.HttpClient.tests

        // package manager
        Tests.Propagation.tests
        Tests.Hashing.tests
        Tests.BranchOps.tests

        // serialization
        Tests.BinarySerialization.tests
        Tests.DarkTypesSerialization.tests

        // http server
        Tests.HttpServer.tests
        Tests.CliTraces.tests
        Tests.Toplevels.tests

        // cross-cutting
        Tests.LibExecution.tests.Force()

        Tests.Blob.tests
        Tests.Stream.tests ]

    let cancelationTokenSource = new System.Threading.CancellationTokenSource()
    let httpClientTestsTask = Tests.HttpClient.init cancelationTokenSource.Token

    // Generate this so that we can see if the format has changed in a git diff
    BinarySerialization.generateTestFiles ()

    // this does async stuff within it, so do not run it from a task/async
    // context or it may hang
    let exitCode =
      runTestsWithCLIArgs [ Allow_Duplicate_Names ] args (testList "tests" tests)

    NonBlockingConsole.wait () // flush stdout
    cancelationTokenSource.Cancel()
    httpClientTestsTask.Wait()
    exitCode
  with e ->
    printException "Outer exception" [] e
    NonBlockingConsole.wait () // flush stdout
    1
