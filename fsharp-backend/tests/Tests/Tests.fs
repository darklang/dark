module Tests.All

// Main entry point for tests being run

open Expecto

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
  runTestsWithCLIArgs [] args tests
