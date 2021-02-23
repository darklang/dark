module Tests.All

// Main entry point for tests being run

open Expecto
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks

[<EntryPoint>]
let main args =
  let (_ : Task) = Tests.BwdServer.init ()
  LibBackend.Migrations.init ()
  (LibBackend.Account.initTestAccounts ()).Wait()

  let tests =
      [ Tests.LibExecution.tests.Force()
        Tests.ProgramTypes.tests
        Tests.OCamlInterop.tests
        Tests.DvalRepr.tests
        Tests.SqlCompiler.tests
        Tests.FSharpToExpr.tests
        Tests.Account.tests
        Tests.BwdServer.tests
        Tests.ApiServer.tests ]

  // this does async stuff within it, so do not run it from a task/async
  // context or it may hang
  let result = runTestsWithCLIArgs [] args (testList "tests" tests)
  if result <> 0 then failwith "Tests have non-zero exit code"
  0
