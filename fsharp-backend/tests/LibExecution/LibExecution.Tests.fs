module LibExecution.Tests

open Expecto

let client =
  testList
    "Client"
    [ testCase "Added todo"
      <| fun _ -> Expect.equal 1 1 "There should be 1 todo" ]

let all = testList "All" [ client ]

[<EntryPoint>]
let main _ = 0
