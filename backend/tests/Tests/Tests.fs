module Tests.All

// Main entry point for tests being run

open Expecto
open System.Threading.Tasks

open Prelude

module PT = LibExecution.ProgramTypes

let initSerializers () =
  // Universally-serializable types — kept here so the test process can
  // (de)serialize the same shapes the older HTTP-handler harness did.
  Json.Vanilla.allow<LibExecution.DvalReprInternalRoundtrippable.FormatV0.Dval>
    "RoundtrippableSerializationFormatV0.Dval"
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.Toplevel.T>>
    "Canvas.loadJsonFromDisk"
  Json.Vanilla.allow<LibExecution.ProgramTypes.Toplevel.T> "Canvas.loadJsonFromDisk"

  // These are serializers used in the tests that are not used in the main program
  Json.Vanilla.allow<Map<string, string>> "tests"
  Json.Vanilla.allow<LibExecution.AnalysisTypes.TraceData> "testTraceData"
  Json.Vanilla.allow<PT.PackageType.PackageType> "Canvas.loadJsonFromDisk"
  Json.Vanilla.allow<PT.PackageValue.PackageValue> "Canvas.loadJsonFromDisk"
  Json.Vanilla.allow<PT.PackageFn.PackageFn> "Canvas.loadJsonFromDisk"


[<EntryPoint>]
let main (args : string array) : int =
  try
    let name = "Tests"
    printTime $"Initing {name}"

    initSerializers ()

    // Grow the DB from seed if needed. Builtins are deferred (constructed after
    // hashes are generated) because builtin construction triggers hash lookups.
    (LibPackageManager.Seed.growIfNeeded
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
        Tests.Execution.tests
        Tests.Builtin.tests
        Tests.DvalRepr.tests
        Tests.LibParser.tests
        Tests.NewParser.tests
        Tests.HttpClient.tests

        // package manager
        Tests.Propagation.tests
        Tests.Hashing.tests
        Tests.BranchOps.tests

        (*
        TODO backfill the following tests we neglected to write during a big refactor:
        - op playback
        - package search
        - branch-specific stuff

        (agaist both in-mem and sql-bound PMs)
        *)

        // cloud
        Tests.HttpServer.tests
        Tests.Canvas.tests
        Tests.Routing.tests
        Tests.BinarySerialization.tests
        Tests.VanillaSerialization.tests
        Tests.DarkTypesSerialization.tests

        // cross-cutting
        Tests.LibExecution.tests.Force()

        Tests.Blob.tests
        Tests.Stream.tests ]

    let cancelationTokenSource = new System.Threading.CancellationTokenSource()
    let httpClientTestsTask = Tests.HttpClient.init cancelationTokenSource.Token

    // Generate this so that we can see if the format has changed in a git diff
    BinarySerialization.generateTestFiles ()
    VanillaSerialization.PersistedSerializations.generateTestFiles ()

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
