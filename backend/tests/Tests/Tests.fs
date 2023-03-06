module Tests.All

// Main entry point for tests being run

open Expecto
open System.Threading.Tasks

open Prelude

module Telemetry = LibService.Telemetry

module CTApi = ClientTypes.Api
module CTPusher = ClientTypes.Pusher

let initSerializers () =
  BwdServer.Server.initSerializers ()

  // These are serializers used in the tests that are not used in the main program
  Json.Vanilla.allow<Map<string, string>> "tests"
  Json.Vanilla.allow<ClientTypes.Runtime.Dval.T> "dvalrepr tests"
  Json.Vanilla.allow<ClientTypes.Program.Handler.T> "canvasClone"
  Json.Vanilla.allow<LibExecution.AnalysisTypes.TraceData> "testTraceData"

[<EntryPoint>]
let main (args : string array) : int =
  try
    let name = "Tests"
    LibService.Init.init name
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    (LibRealExecution.Init.init name).Result
    (LibBackend.Account.initializeDevelopmentAccounts name).Result

    LibAnalysis.initSerializers ()
    initSerializers ()

    let tests =
      [ Tests.Account.tests
        Tests.AnalysisTypes.tests
        Tests.Authorization.tests
        Tests.BwdServer.tests
        Tests.Canvas.tests
        Tests.Cron.tests
        Tests.DvalRepr.tests
        Tests.EventQueue.tests
        Tests.EventQueueV2.tests
        Tests.Execution.tests
        Tests.Parser.tests
        Tests.HttpClient.tests
        Tests.LibExecution.tests.Force()
        Tests.Prelude.tests
        Tests.ProgramTypes.tests
        Tests.Routing.tests
        Tests.RuntimeTypes.tests
        Tests.BinarySerialization.tests
        Tests.VanillaSerialization.tests
        Tests.SqlCompiler.tests
        Tests.StdLib.tests
        Tests.Traces.tests
        Tests.StorageTraces.tests
        Tests.TypeChecker.tests
        Tests.Undo.tests ]

    let cancelationTokenSource = new System.Threading.CancellationTokenSource()
    let bwdServerTestsTask = Tests.BwdServer.init cancelationTokenSource.Token
    let httpClientTestsTask = Tests.HttpClient.init cancelationTokenSource.Token
    Telemetry.Console.loadTelemetry "tests" Telemetry.TraceDBQueries

    // Generate this so that we can see if the format has changed in a git diff
    // BinarySerialization.generateTestFiles ()
    // VanillaSerialization.PersistedSerializations.generateTestFiles ()

    // this does async stuff within it, so do not run it from a task/async
    // context or it may hang
    let exitCode = runTestsWithCLIArgs [] args (testList "tests" tests)

    NonBlockingConsole.wait () // flush stdout
    cancelationTokenSource.Cancel()
    bwdServerTestsTask.Wait()
    httpClientTestsTask.Wait()
    QueueWorker.shouldShutdown <- true
    exitCode
  with
  | e ->
    printException "Outer exception" [] e
    NonBlockingConsole.wait () // flush stdout
    1
