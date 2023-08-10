/// StdLib functions for building the CLI
/// (as opposed to functions needed by CLI programs, which are in StdLibCli)
module LocalExec.Libs.Cli

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution

let builtIns : RT.BuiltIns =
  let (fns, types, constants) =
    LibExecution.StdLib.combine
      [ StdLibExecution.StdLib.contents
        StdLibCli.StdLib.contents
        StdLibDarkInternal.StdLib.contents
        StdLibCliHost.StdLib.contents ]
      []
      []
  { types = types |> Tablecloth.Map.fromListBy (fun typ -> typ.name)
    fns = fns |> Tablecloth.Map.fromListBy (fun fn -> fn.name)
    constants = constants |> Tablecloth.Map.fromListBy (fun c -> c.name) }

let packageManager : RT.PackageManager = RT.PackageManager.Empty


let execute
  (parentState : RT.ExecutionState)
  (mod' : LibParser.Canvas.PTCanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Task<RT.Dval> =

  task {
    let config : Config =
      { allowLocalHttpAccess = true; httpclientTimeoutInMs = 30000 }
    let program : Program =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = true
        fns =
          mod'.fns
          |> List.map (fun fn -> PT2RT.UserFunction.toRT fn)
          |> Tablecloth.Map.fromListBy (fun fn -> fn.name)
        types =
          mod'.types
          |> List.map (fun typ -> PT2RT.UserType.toRT typ)
          |> Tablecloth.Map.fromListBy (fun typ -> typ.name)
        constants =
          mod'.constants
          |> List.map (fun c -> PT2RT.UserConstant.toRT c)
          |> Tablecloth.Map.fromListBy (fun c -> c.name)

        dbs = Map.empty
        secrets = [] }

    let tracing = Exe.noTracing RT.Real
    let notify = parentState.notify
    let sendException = parentState.reportException
    let state =
      Exe.createState
        builtIns
        packageManager
        tracing
        sendException
        notify
        7UL
        program
        config

    if mod'.exprs.Length = 1 then
      return! Exe.executeExpr state symtable (PT2RT.Expr.toRT mod'.exprs[0])
    else if mod'.exprs.Length = 0 then
      return DError(SourceNone, RuntimeError.oldError "No expressions to execute")
    else // mod'.exprs.Length > 1
      return
        DError(SourceNone, RuntimeError.oldError "Multiple expressions to execute")
  }

let constants : List<BuiltInConstant> = []

let types : List<BuiltInType> = []


let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec"; "File" ] "read" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TBytes
      description =
        "Reads the contents of a file specified by <param path> asynchronously and returns its contents as Bytes. This function exists as File.read uses a result, which isn't yet available in LocalExec"
      fn =
        (function
        | _, _, [ DString path ] ->
          uply {
            try
              let! contents = System.IO.File.ReadAllBytesAsync path
              return DBytes contents
            with e ->
              return
                DError(
                  SourceNone,
                  RuntimeError.oldError $"Error reading file: {e.Message}"
                )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
