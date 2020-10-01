module Server.Tests

open Expecto

open LibExecution
open Server

let server =
  testList
    "Server"
    [ testCase "Adding valid Todo"
      <| fun _ ->

        Expect.equal 1 1 "Result should be ok" ]

let all =
  testList
    "All"
    [ (* LibExecution.Tests.libExecution *)
    server ]

[<EntryPoint>]
let main _ = runTests defaultConfig all
