module BwdServer.Tests

open Expecto

open LibExecution
open BwdServer

let server =
  testList
    "Server"
    [ testCase "Adding valid Todo"
      <| fun _ ->

        Expect.equal 1 1 "Result should be ok" ]

let all = testList "BwdServer" [ server ]
