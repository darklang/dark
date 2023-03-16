module Executor.Execute

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module Exe = LibExecution.Execution


let stdlibTypes : Map<RT.FQTypeName.T, RT.BuiltInType> =
  LibExecutionStdLib.StdLib.types
  |> Map.fromListBy (fun typ -> RT.FQTypeName.Stdlib typ.name)

let stdlibFns : Map<RT.FQFnName.T, RT.BuiltInFn> =
  LibExecutionStdLib.StdLib.fns
  |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)


let libraries : RT.Libraries =
  { stdlibTypes = stdlibTypes; stdlibFns = stdlibFns; packageFns = Map.empty }


let execute (expr : RT.Expr) (symtable : Map<string, RT.Dval>) : Task<RT.Dval> =

  task {
    let program : RT.ProgramContext =
      { accountID = System.Guid.NewGuid()
        canvasID = System.Guid.NewGuid()
        canvasName = CanvasName.createExn "todo"
        userFns = Map.empty
        userTypes = Map.empty
        dbs = Map.empty
        secrets = [] }

    let tracing = Exe.noTracing RT.Real

    let notify (_state : RT.ExecutionState) (_msg : string) (_metadata : Metadata) =
      // let metadata = extraMetadata state @ metadata
      // LibService.Rollbar.notify msg metadata
      ()

    let sendException
      (_state : RT.ExecutionState)
      (_metadata : Metadata)
      (_exn : exn)
      =
      // let metadata = extraMetadata state @ metadata
      // let person : LibService.Rollbar.Person =
      //   Some { id = program.accountID; username = Some(username ()) }
      // LibService.Rollbar.sendException person metadata exn
      ()


    let state = Exe.createState libraries tracing sendException notify 7UL program

    return! Exe.executeExpr state symtable expr
  }
