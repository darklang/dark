module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open RuntimeTypes

let run
  (tlid : tlid)
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

let runHttp
  (tlid : tlid)
  (url : string)
  (vars : DvalMap)
  (body : byte array)
  (fns : List<BuiltInFn>)
  (e : Expr)
  : Task<Dval> =
  task {
    let functions = fns |> List.map (fun fn -> (fn.name, fn)) |> Map
    let state = { functions = functions; tlid = tlid }

    let result =
      Interpreter.applyFnVal
        state
        (FQFnName(FQFnName.stdlibName "Http" "middleware" 0))
        [ DStr url
          DBytes body
          DObj Map.empty
          DFnVal(
            Lambda { parameters = [ gid (), "request" ]; symtable = vars; body = e }
          ) ]
        NotInPipe
        NoRail


    match result with
    | Prelude.Task t ->
        let! t = t
        printfn $"result in runHttp is a task {t}"
        return t

    | Prelude.Value v ->
        printfn $"result is runHttp is a value {v}"
        return v
  }
