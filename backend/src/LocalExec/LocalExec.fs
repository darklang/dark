/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution

let stdlibTypes : Map<RT.FQTypeName.T, RT.BuiltInType> =
  LibExecutionStdLib.StdLib.types
  |> List.map (fun typ -> PT2RT.BuiltInType.toRT typ)
  |> Map.fromListBy (fun typ -> RT.FQTypeName.Stdlib typ.name)

let stdlibFns : Map<RT.FQFnName.T, RT.BuiltInFn> =
  LibExecutionStdLib.StdLib.fns
  |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)


let libraries : RT.Libraries =
  { stdlibTypes = stdlibTypes; stdlibFns = stdlibFns; packageFns = Map.empty }


let execute
  (mod' : Parser.CanvasV2.CanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Task<RT.Dval> =

  task {
    let program : RT.ProgramContext =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        userFns =
          mod'.fns
          |> List.map (fun fn -> PT2RT.UserFunction.toRT fn)
          |> Map.fromListBy (fun fn -> fn.name)
        userTypes =
          mod'.types
          |> List.map (fun typ -> PT2RT.UserType.toRT typ)
          |> Map.fromListBy (fun typ -> typ.name)
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

    return! Exe.executeExpr state symtable (PT2RT.Expr.toRT mod'.exprs[0])
  }




let initSerializers () = Json.Vanilla.allow<pos> "Prelude"

[<EntryPoint>]
let main (args : string []) : int =
  let name = "LocalExec"
  try
    initSerializers ()
    // (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    let mainFile = "/home/dark/app/backend/src/LocalExec/main.dark"
    let mod' = Parser.CanvasV2.parseFromFile Map.empty mainFile
    let args = args |> Array.toList |> List.map RT.DString |> RT.DList
    let result = execute mod' (Map [ "args", args ])
    NonBlockingConsole.wait ()
    match result.Result with
    | RT.DError (_, msg) ->
      System.Console.WriteLine $"Error: {msg}"
      1
    | RT.DInt i -> (int i)
    | dval ->
      let output = LibExecution.DvalReprDeveloper.toRepr dval
      System.Console.WriteLine
        $"Error: main function must return an int, not {output}"
      1
  with
  | e ->
    // Don't reraise or report as ProdExec is only run interactively
    printException "" [] e
    // LibService.Init.shutdown name
    1
