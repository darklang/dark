module Tests.All

// Main entry point for tests being run

open Expecto

let tests =
  testList
    "tests"
    [ Tests.LibExecution.tests; Tests.BwdServer.tests; Tests.ApiServer.tests ]

[<EntryPoint>]
let main _ =
  LibBackend.Serialization.OCamlInterop.Binary.init ()
  runTests defaultConfig tests
