module Execution.Tests

open Expecto

open LibExecution.Runtime.Shortcuts


let t name prog expected =
  testTask name {
    let! result = LibExecution.Execution.run prog
    Expect.equal result expected name
  }
