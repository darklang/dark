module Execution

open Runtime

let run (e : Expr) : Task<Dval> =
  let env = Environment.envWith (StdLib.functions)
  (eval env Symtable.empty e).toTask()


let runString (e : Expr) : Task<string> =
  task {
    let! result = run e
    return result.ToString()
  }


let runJSON (e : Expr) : Task<string> =
  task {
    let! result = run e
    return result.toJSON().ToString()
  }
