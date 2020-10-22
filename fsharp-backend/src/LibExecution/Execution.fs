module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks
open Runtime

let run (e : Expr) (fns : List<BuiltInFn>) : Task<Dval> =
  let functions = fns |> List.map (fun fn -> (fn.name, fn)) |> Map

  let state = { functions = functions; tlid = (int64 7) }
  (Interpreter.eval state Symtable.empty e).toTask()
