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
    [ Tests.Account.tests
      Tests.Analysis.tests
      Tests.ApiServer.tests
      Tests.Authorization.tests
      Tests.BwdServer.tests
      Tests.Canvas.tests
      Tests.DvalRepr.tests
      Tests.FSharpToExpr.tests
      Tests.LibExecution.tests.Force()
      Tests.OCamlInterop.tests
      Tests.ProgramTypes.tests
      Tests.SqlCompiler.tests
      Tests.TypeChecker.tests
      Tests.Undo.tests ]

  // this does async stuff within it, so do not run it from a task/async
  // context or it may hang
  let result = runTestsWithCLIArgs [] args (testList "tests" tests)
  if result <> 0 then failwith "Tests have non-zero exit code"
  0
