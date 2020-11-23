module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks
open Runtime

let run (tlid : tlid)
        (vars : List<string * Dval>)
        (fns : List<BuiltInFn>)
        (e : Expr)
        : Task<Dval> =
  task {
    let functions = fns |> List.map (fun fn -> (fn.name, fn)) |> Map
    let state = { functions = functions; tlid = tlid }
    let symtable = Map.ofList vars

    let result = Interpreter.eval state symtable e

    match result with
    | Prelude.Task t -> return! t
    | Prelude.Value v -> return v
  }

open Shortcuts

let runHttp (tlid : tlid)
            (url : string)
            (body : byte array)
            (fns : List<BuiltInFn>)
            (e : Expr)
            : Task<Dval> =
  task {
    let functions = fns |> List.map (fun fn -> (fn.name, fn)) |> Map
    let state = { functions = functions; tlid = tlid }

    let result =
      Interpreter.callFn
        state
        (gid ())
        (Runtime.FnDesc.stdFnDesc "Http" "middleware" 0)
        [ DStr url
          DBytes body
          DObj Map.empty
          DLambda
            { parameters = [ gid (), "request" ]; symtable = Map.empty; body = e } ]
        NoRail

    match result with
    | Prelude.Task t -> return! t
    | Prelude.Value v -> return v
  }
