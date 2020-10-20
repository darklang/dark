module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks
open Runtime


let run (e : Expr) : Task<Dval> =
  let functions = StdLib.fns |> List.map (fun fn -> (fn.name, fn)) |> Map

  let state = { functions = functions; tlid = (int64 7) }
  (Interpreter.eval state Symtable.empty e).toTask()


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
