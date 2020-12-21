module Tests.All

// Main entry point for tests being run

open Expecto
open System.Threading.Tasks
open FSharp.Control.Tasks

let tests =
  testList
    "tests"
    [ Tests.LibExecution.tests
      Tests.LibBackend.tests
      Tests.BwdServer.tests
      Tests.ApiServer.tests ]

[<EntryPoint>]
let main args =

  LibBackend.ProgramSerialization.OCamlInterop.Binary.init ()

  // Run but don't wait for it
  let (_task : Task) = Tests.BwdServer.init ()

  let t =
    task {

      LibBackend.Migrations.init ()
      do! LibBackend.Account.initTestAccounts ()

      return runTestsWithCLIArgs [] args tests
    }

  t.Wait()
  t.Result
