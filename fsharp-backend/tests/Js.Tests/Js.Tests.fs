module Js.Tests

open Fable.Mocha

let client =
  testList
    "Js"
    [ testCase "Added todo"
      <| fun _ -> Expect.equal 1 1 "There should be 1 todo" ]

let all =
  testList
    "All"
    [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
    LibExecution.Tests.libExecution
#endif
    client ]

[<EntryPoint>]
let main _ = Mocha.runTests all
