/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module StdLibCli = StdLibCli.StdLib

let (stdlibFns, stdlibTypes) =
  LibExecution.StdLib.combine
    [ StdLibExecution.StdLib.contents; StdLibCli.StdLib.contents; StdLib.contents ]
    []
    []


let libraries : RT.Libraries =
  { stdlibTypes =
      stdlibTypes |> Map.fromListBy (fun typ -> RT.FQTypeName.Stdlib typ.name)
    stdlibFns = stdlibFns |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)
    packageFns = Map.empty }


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

    let extraMetadata (state : RT.ExecutionState) : Metadata =
      [ "executing_fn_name", state.executingFnName; "callstack", state.callstack ]

    let notify (state : RT.ExecutionState) (msg : string) (metadata : Metadata) =
      let metadata = extraMetadata state @ metadata
      let metadata =
        metadata |> List.map (fun (k, v) -> $"  {k}: {v}") |> String.concat ", "
      print $"Notification: {msg}, {metadata}"

    let sendException (state : RT.ExecutionState) (metadata : Metadata) (exn : exn) =
      let metadata = extraMetadata state @ metadata @ Exception.toMetadata exn
      let metadata =
        metadata |> List.map (fun (k, v) -> $"  {k}: {v}") |> String.concat "\n"
      print
        $"Exception: {exn.Message}\nMetadata:\n{metadata}\nStacktrace:\n{exn.StackTrace}"

    let state = Exe.createState libraries tracing sendException notify 7UL program

    return! Exe.executeExpr state symtable (PT2RT.Expr.toRT mod'.exprs[0])
  }




let initSerializers () = ()

[<EntryPoint>]
let main (args : string []) : int =
  let name = "LocalExec"
  try
    initSerializers ()
    LibService.Init.init name
    LibService.Telemetry.Console.loadTelemetry
      name
      LibService.Telemetry.DontTraceDBQueries
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    let mainFile = "/home/dark/app/backend/src/LocalExec/main.dark"
    let mod' = Parser.CanvasV2.parseFromFile mainFile
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
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    // LibService.Init.shutdown name
    NonBlockingConsole.wait ()
    1
