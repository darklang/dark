module LibCLI.Main

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module StdLib = LibExecution.StdLib
module StdLibCLI = StdLibCLI.StdLib

let defaultTLID = 7UL

type DarkCLIConfig =
  {
    /// The name of the specific Dark CLI
    /// (likely either "DarklangCLI" or "Internal Dark CLI")
    name : string

    /// Many StdLib types/functions are accessible to user programs in the CLI.
    /// In some cases (i.e. for our "internal" use) additional stdlib may be availed.
    extraStdlibForUserPrograms : StdLib.Contents

    /// Should `DarkInternal.[x]` functions be callable?
    allowInternalDarkFunctions : bool
  }


let libraries (config : DarkCLIConfig) : RT.Libraries =
  let (builtInFns, builtInTypes) =
    LibExecution.StdLib.combine
      [ StdLibExecution.StdLib.contents
        StdLibCLI.StdLib.contents
        StdLibCLIHost.StdLib.contents
          config.extraStdlibForUserPrograms
          config.allowInternalDarkFunctions ]
      []
      []

  { builtInTypes = builtInTypes |> Map.fromListBy (fun typ -> typ.name)
    builtInFns = builtInFns |> Map.fromListBy (fun fn -> fn.name)
    packageFns = Map.empty
    packageTypes = Map.empty }

let execute
  (cliConfig : DarkCLIConfig)
  (mod' : Parser.CanvasV2.CanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Task<RT.Dval> =

  let config : RT.Config =
    { allowLocalHttpAccess = true; httpclientTimeoutInMs = 30000 }

  task {
    let config : RT.Config =
      { allowLocalHttpAccess = true; httpclientTimeoutInMs = 30000 }

    let program : RT.Program =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        fns =
          mod'.fns
          |> List.map (fun fn -> PT2RT.UserFunction.toRT fn)
          |> Map.fromListBy (fun fn -> fn.name)
        types =
          mod'.types
          |> List.map (fun typ -> PT2RT.UserType.toRT typ)
          |> Map.fromListBy (fun typ -> typ.name)
        dbs = Map.empty
        secrets = [] }

    let libraries = libraries cliConfig

    let tracing = Exe.noTracing RT.Real

    let extraMetadata (state : RT.ExecutionState) : Metadata =
      [ "executing_fn_name", state.executingFnName; "callstack", state.callstack ]

    let notify (state : RT.ExecutionState) (msg : string) (metadata : Metadata) =
      let metadata = extraMetadata state @ metadata
      // LibService.Rollbar.notify msg metadata
      let metadata =
        metadata |> List.map (fun (k, v) -> $"  {k}: {v}") |> String.concat ", "
      print $"Notification: {msg}, {metadata}"

    let sendException (state : RT.ExecutionState) (metadata : Metadata) (exn : exn) =
      let metadata = extraMetadata state @ metadata @ Exception.toMetadata exn
      //printException "Internal error" metadata exn
      let metadata =
        metadata |> List.map (fun (k, v) -> $"  {k}: {v}") |> String.concat "\n"
      print
        $"Exception: {exn.Message}\nMetadata:\n{metadata}\nStacktrace:\n{exn.StackTrace}"

    let state =
      Exe.createState
        libraries
        tracing
        sendException
        notify
        defaultTLID
        program
        config

    if mod'.exprs.Length = 1 then
      return! Exe.executeExpr state symtable (PT2RT.Expr.toRT mod'.exprs[0])
    else if mod'.exprs.Length = 0 then
      return RT.DError(RT.SourceNone, "No expressions to execute")
    else // mod'.exprs.Length > 1
      return RT.DError(RT.SourceNone, "Multiple expressions to execute")
  }

let sourceOf
  (tlid : tlid)
  (id : id)
  (modul : Parser.CanvasV2.CanvasModule)
  : string =
  let ast =
    if tlid = defaultTLID then
      Some modul.exprs[0]
    else
      modul.fns
      |> List.find (fun fn -> fn.tlid = tlid)
      |> Option.map (fun fn -> fn.body)

  let mutable result = "unknown"

  ast
  |> Option.tap (fun e ->
    LibExecution.ProgramTypesAst.preTraversal
      (fun expr ->
        if PT.Expr.toID expr = id then result <- string expr
        expr)
      (fun pipeExpr ->
        if PT.PipeExpr.toID pipeExpr = id then result <- string pipeExpr
        pipeExpr)
      identity
      identity
      identity
      identity
      identity
      e
    |> ignore<PT.Expr>)

  result


let initSerializers () = ()

let main (config : DarkCLIConfig) (args : string[]) =
  try
    initSerializers ()

    // load the host script
    let mainFile = "/home/dark/app/backend/src/LibCLI/cli-host.dark"
    let mod' = Parser.CanvasV2.parseFromFile mainFile

    // prepare args
    let args = args |> Array.toList |> List.map RT.DString |> RT.DList

    // eval
    let result = execute config mod' (Map [ "args", args ])
    let result = result.Result

    NonBlockingConsole.wait ()

    // handle result (ran successfully = `DInt 0`)
    match result with
    | RT.DError(RT.SourceNone, msg) ->
      System.Console.WriteLine $"Error: {msg}"
      1
    | RT.DError(RT.SourceID(tlid, id), msg) ->
      System.Console.WriteLine $"Error ({tlid}, {id}): {msg}"
      System.Console.WriteLine $"Failure at: {sourceOf tlid id mod'}"
      1
    | RT.DInt i -> (int i)
    | dval ->
      let output = LibExecution.DvalReprDeveloper.toRepr dval
      System.Console.WriteLine
        $"Error: main function must return an int (returned {output})"
      1
  with e ->
    printException $"Error starting {config.name}" [] e
    1
