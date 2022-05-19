module Tests.All

// Main entry point for tests being run

open Expecto
open System.Threading.Tasks

open Prelude

module Telemetry = LibService.Telemetry

[<EntryPoint>]
let main (args : string array) : int =
  try
    let name = "Tests"
    LibService.Init.init name
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    (LibRealExecution.Init.init name).Result
    (LibBackend.Account.initializeDevelopmentAccounts name).Result

    let tests =
      [ Tests.Account.tests
        Tests.ApiServer.tests
        Tests.Authorization.tests
        Tests.BinarySerialization.tests
        Tests.BwdServer.tests
        Tests.Canvas.tests
        Tests.Cron.tests
        Tests.DvalReprExternal.tests
        Tests.EventQueue.tests
        Tests.EventQueueV2.tests
        Tests.Execution.tests
        Tests.FSharpToExpr.tests
        Tests.HttpClient.tests
        Tests.LibExecution.tests.Force()
        Tests.OCamlInterop.tests
        Tests.Prelude.tests
        Tests.ProgramTypes.tests
        Tests.Routing.tests
        Tests.SqlCompiler.tests
        Tests.StdLib.tests
        Tests.Traces.tests
        Tests.TypeChecker.tests
        Tests.Undo.tests
        Tests.UserDB.tests ]

    let cancelationTokenSource = new System.Threading.CancellationTokenSource()
    let bwdServerTestsTask = Tests.BwdServer.init cancelationTokenSource.Token
    let httpClientTestsTask = Tests.HttpClient.init cancelationTokenSource.Token
    Telemetry.Console.loadTelemetry "tests" Telemetry.TraceDBQueries
    (LibBackend.Account.initTestAccounts ()).Wait()

    // Generate this so that we can see if the format has changed in a git diff
    BinarySerialization.generateBinarySerializationTestFiles ()

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
    print e.Message
    printMetadata (Exception.toMetadata e)
    print e.StackTrace
    NonBlockingConsole.wait () // flush stdout
    1
