/// Builtin functions for building the CLI
/// (as opposed to functions needed by CLI programs, which are in BuiltinCli)
module LocalExec.Libs.Cli

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Dval = LibExecution.Dval


let builtIns : RT.BuiltIns =
  let (fns, constants) =
    LibExecution.Builtin.combine
      [ BuiltinExecution.Builtin.contents
          BuiltinExecution.Libs.HttpClient.defaultConfig
        BuiltinCli.Builtin.contents
        BuiltinDarkInternal.Builtin.contents
        BuiltinCliHost.Builtin.contents ]
      []
  { fns = fns |> Map.fromListBy _.name
    constants = constants |> Map.fromListBy _.name }

let packageManager : RT.PackageManager = RT.PackageManager.Empty


let execute
  (parentState : RT.ExecutionState)
  (mod' : LibParser.Canvas.PTCanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Ply<RT.ExecutionResult> =

  uply {
    let program : Program =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = true
        fns = mod'.fns |> List.map PT2RT.UserFunction.toRT |> Map.fromListBy _.name
        types = mod'.types |> List.map PT2RT.UserType.toRT |> Map.fromListBy _.name
        constants =
          mod'.constants |> List.map PT2RT.UserConstant.toRT |> Map.fromListBy _.name
        dbs = Map.empty
        secrets = [] }

    let notify = parentState.notify
    let sendException = parentState.reportException
    let state =
      Exe.createState
        builtIns
        packageManager
        Exe.noTracing
        sendException
        notify
        program

    if mod'.exprs.Length = 1 then
      let expr = PT2RT.Expr.toRT mod'.exprs[0]
      return! Exe.executeExpr state 7777772347523UL symtable expr
    else if mod'.exprs.Length = 0 then
      return Error(None, RuntimeError.oldError "No expressions to execute")
    else // mod'.exprs.Length > 1
      return Error(None, RuntimeError.oldError "Multiple expressions to execute")
  }

let constants : List<BuiltInConstant> = []



let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec"; "File" ] "read" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TList TUInt8
      description =
        "Reads the contents of a file specified by <param path> asynchronously and returns its contents as Bytes. This function exists as Builtin.File.read uses a result, which isn't yet available in LocalExec"
      fn =
        (function
        | _, _, [ DString path ] ->
          uply {
            try
              let! contents = System.IO.File.ReadAllBytesAsync path
              return Dval.byteArrayToDvalList contents
            with e ->
              return
                raiseUntargetedRTE (
                  RuntimeError.oldError $"Error reading file: {e.Message}"
                )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, constants)
