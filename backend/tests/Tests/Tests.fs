module Tests.All

// Main entry point for tests being run

open Expecto
open System.Threading.Tasks

open Prelude

module PT = LibExecution.ProgramTypes

let initSerializers () =
  BwdServer.Server.initSerializers ()

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
    LibService.Init.init name
    (LibCloud.Init.init name).Result
    (LibCloudExecution.Init.init name).Result

    initSerializers ()

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

        (*
        TODO backfill the following tests we neglected to write during a big refactor:
        - op playback
        - package search
        - branch-specific stuff

        (agaist both in-mem and sql-bound PMs)
        *)

        // cloud
        Tests.BwdServer.tests
        Tests.Canvas.tests
        Tests.Routing.tests
        Tests.BinarySerialization.tests
        Tests.VanillaSerialization.tests
        Tests.DarkTypesSerialization.tests

        // cross-cutting
        Tests.LibExecution.tests.Force() ]

    let cancelationTokenSource = new System.Threading.CancellationTokenSource()
    let bwdServerTestsTask = Tests.BwdServer.init cancelationTokenSource.Token
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
    bwdServerTestsTask.Wait()
    httpClientTestsTask.Wait()
    exitCode
  with e ->
    printException "Outer exception" [] e
    NonBlockingConsole.wait () // flush stdout
    1
