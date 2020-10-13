module ExecUtils

open Expecto

let t (name: string) (prog: LibExecution.Runtime.Expr) (expected: LibExecution.Runtime.Dval) =
  testTask name {
    let! result = LibExecution.Execution.run prog
    Expect.equal result expected name
  }
