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
  Tests.BwdServer.init ()
  Task.WaitAll [| LibBackend.Account.initTestAccounts () |]

  runTestsWithCLIArgs [] args tests
