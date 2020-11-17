module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks
open Runtime

let run (vars : List<string * Dval>) (fns : List<BuiltInFn>) (e : Expr) : Task<Dval> =
  task {
    let functions = fns |> List.map (fun fn -> (fn.name, fn)) |> Map
    let state = { functions = functions; tlid = (int64 7) }
    let symtable = Map.ofList vars

    let result = Interpreter.eval state symtable e

    match result with
    | Prelude.Task t -> return! t
    | Prelude.Value v -> return v
  }
