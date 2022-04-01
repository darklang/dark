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

    if args.Length = 1 && args[0] = "--regenerate-test-files" then
      BinarySerialization.generateBinarySerializationTestFiles ()
      print "Serialized to backend/serialization"
      0
    else
      let cancelationTokenSource = new System.Threading.CancellationTokenSource()
      let bwdServerTestsTask = Tests.BwdServer.init cancelationTokenSource.Token
      let httpClientTestsTask = Tests.HttpClient.init cancelationTokenSource.Token
      Telemetry.Console.loadTelemetry "tests" Telemetry.TraceDBQueries
      (LibBackend.Account.initTestAccounts ()).Wait()

      // this does async stuff within it, so do not run it from a task/async
      // context or it may hang
      let exitCode = runTestsWithCLIArgs [] args (testList "tests" tests)

      Prelude.NonBlockingConsole.wait () // flush stdout
      cancelationTokenSource.Cancel()
      bwdServerTestsTask.Wait()
      httpClientTestsTask.Wait()
      exitCode
  with
  | e ->
    print e.Message
    let metadata = Exception.toMetadata e
    LibService.Rollbar.printMetadata metadata
    print e.StackTrace
    1
