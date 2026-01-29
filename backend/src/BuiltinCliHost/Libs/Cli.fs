/// Builtin functions for building the CLI
/// (as opposed to functions needed by CLI programs, which are in StdLibCli)
module BuiltinCliHost.Libs.Cli

open System.Threading.Tasks
open FSharp.Control.Tasks


open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Exe = LibExecution.Execution
module PackageIDs = LibExecution.PackageIDs
module Json = BuiltinExecution.Libs.Json
module C2DT = LibExecution.CommonToDarkTypes
module D = LibExecution.DvalDecoder

module Utils = BuiltinCliHost.Utils


// Get PackageManager instance with error handling
let executePM (exeState : RT.ExecutionState) : Ply<Dval> =
  uply {
    let getPmFnName = FQFnName.Package PackageIDs.Fn.LanguageTools.PackageManager.pm

    let! execResult =
      Exe.executeFunction exeState getPmFnName [] (NEList.singleton DUnit)

    match execResult with
    | Ok dval -> return dval
    | Error(rte, _cs) ->
      let! rteString = Exe.runtimeErrorToString exeState rte
      match rteString with
      | Ok rte ->
        return Exception.raiseInternal "Error executing pm function" [ "rte", rte ]
      | Error(nestedRte, _cs) ->
        return
          Exception.raiseInternal
            "Error running runtimeErrorToString"
            [ "original rte", rte; "nested rte", nestedRte ]
  }

// Create OnMissing.Allow enum value
let createOnMissingAllow () : Dval =
  let onMissingType =
    FQTypeName.Package
      PackageIDs.Type.LanguageTools.NameResolver.nameResolverOnMissing
  DEnum(onMissingType, onMissingType, [], "Allow", [])

// Create exception error from exn
let createExceptionError (e : exn) : RuntimeError.Error =
  RuntimeError.UncaughtException(
    Exception.getMessages e |> String.concat "\n",
    Exception.toMetadata e |> List.map (fun (k, v) -> (k, DString(string v)))
  )

// Parse CLI script code
let parseCliScript
  (exeState : RT.ExecutionState)
  (pm : Dval)
  (owner : string)
  (scriptName : string)
  (code : string)
  : Ply<Result<Utils.CliScript.PTCliScriptModule, RuntimeError.Error>> =
  uply {
    let onMissingAllow = createOnMissingAllow ()

    let args =
      NEList.ofList
        (DString owner)
        [ DString scriptName; onMissingAllow; pm; DString scriptName; DString code ]

    let parseCliScriptFnName =
      FQFnName.Package PackageIDs.Fn.LanguageTools.Parser.CliScript.parseCliScript

    let! execResult = Exe.executeFunction exeState parseCliScriptFnName [] args

    match execResult with
    | Ok dval ->
      match C2DT.Result.fromDT identity dval identity with
      | Ok parsedModuleAndUnresolvedNames ->
        return (Utils.CliScript.fromDT parsedModuleAndUnresolvedNames) |> Ok
      | Error(DString errMsg) ->
        return Error(RuntimeError.UncaughtException(errMsg, []))
      | Error _ ->
        return
          Exception.raiseInternal
            "Invalid error format from parseCliScript"
            [ "dval", dval ]
    | Error(rte, _cs) ->
      let! rteString = Exe.runtimeErrorToString exeState rte
      match rteString with
      | Ok errorDval ->
        return
          Exception.raiseInternal
            "Error executing parseCliScript function"
            [ "rte", errorDval ]
      | Error(nestedRte, _cs) ->
        return
          Exception.raiseInternal
            "Error running runtimeErrorToString"
            [ "original rte", rte; "nested rte", nestedRte ]
  }


module ExecutionError =
  let fqTypeName = FQTypeName.fqPackage PackageIDs.Type.Cli.executionError
  let typeRef = TCustomType(Ok fqTypeName, [])


let pmRT = LibPackageManager.PackageManager.rt
let ptPM = LibPackageManager.PackageManager.pt

let builtinsToUse : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.builtins
        BuiltinExecution.Libs.HttpClient.defaultConfig
      BuiltinCli.Builtin.builtins
      BuiltinPM.Builtin.builtins ptPM ]
    []


let execute
  (parentState : RT.ExecutionState)
  (mod' : Utils.CliScript.PTCliScriptModule)
  (_args : List<Dval>) // CLEANUP update to List<String>, and extract in builtin
  : Ply<RT.ExecutionResult> =
  uply {
    let (program : Program) =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        secrets = []
        dbs = Map.empty }

    let types =
      List.concat
        [ mod'.types |> List.map PT2RT.PackageType.toRT
          mod'.submodules.types |> List.map PT2RT.PackageType.toRT ]

    let values =
      List.concat
        [ mod'.values |> List.map PT2RT.PackageValue.toRT
          mod'.submodules.values |> List.map PT2RT.PackageValue.toRT ]

    let fns =
      List.concat
        [ mod'.fns |> List.map PT2RT.PackageFn.toRT
          mod'.submodules.fns |> List.map PT2RT.PackageFn.toRT ]

    // TODO we should probably use LibPM's in-memory grafting thing instead of this
    // (no need for RT.PM.withExtras to exist, I think)
    let pm = pmRT |> PackageManager.withExtras types values fns

    let tracing = Exe.noTracing

    let state =
      Exe.createState
        builtinsToUse
        pm
        tracing
        parentState.reportException
        parentState.notify
        program

    match mod'.exprs with
    | [] ->
      return
        RuntimeError.CLIs.NoExpressionsToExecute
        |> RuntimeError.CLI
        |> raiseUntargetedRTE
    | exprs ->
      let exprInsrts = exprs |> List.map (PT2RT.Expr.toRT Map.empty 0 None)
      let results = exprInsrts |> List.map (Exe.executeExpr state)
      match List.tryLast results with
      | Some lastResult -> return! lastResult
      | None ->
        return
          Exception.raiseInternal
            "No results from executing expressions (which should be impossible..)"
            []
  }

let fns : List<BuiltInFn> =
  [ { name = fn "cliParseAndExecuteScript" 0
      typeParams = []
      parameters =
        [ Param.make "filename" TString ""
          Param.make "code" TString ""
          Param.make "args" (TList TString) "" ]
      returnType = TypeReference.result TInt64 ExecutionError.typeRef
      description =
        "Parses Dark code as a script, and and executes it, returning an exit code"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName, [])
        let resultOk = Dval.resultOk KTInt64 errType
        let resultError = Dval.resultError KTInt64 errType
        (function
        | exeState,
          _,
          [],
          [ DString filename; DString code; DList(_vtTODO, scriptArgs) ] ->
          uply {
            let! pm = executePM exeState
            let! parsedScript = parseCliScript exeState pm "CliScript" filename code

            try
              match parsedScript with
              | Ok mod' ->
                match! execute exeState mod' scriptArgs with
                | Ok(DInt64 i) -> return resultOk (DInt64 i)
                | Ok result ->
                  return
                    RuntimeError.CLIs.NonIntReturned result
                    |> RuntimeError.CLI
                    |> RT2DT.RuntimeError.toDT
                    |> resultError
                | Error(e, callStack) ->
                  let! csString = Exe.callStackString exeState callStack
                  print $"Error when executing Script. Call-stack:\n{csString}\n"
                  return e |> RT2DT.RuntimeError.toDT |> resultError
              | Error e -> return e |> RT2DT.RuntimeError.toDT |> resultError
            with e ->
              return createExceptionError e |> RT2DT.RuntimeError.toDT |> resultError
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliEvaluateExpression" 0
      typeParams = []
      parameters = [ Param.make "expression" TString "" ]
      returnType = TypeReference.result TString ExecutionError.typeRef
      description = "Evaluates a Dark expression and returns the result as a String"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName, [])
        let resultOk = Dval.resultOk KTString errType
        let resultError = Dval.resultError KTString errType
        (function
        | exeState, _, [], [ DString expression ] ->
          uply {
            let! pm = executePM exeState
            let! parsedScript =
              parseCliScript exeState pm "CliScript" "exprWrapper" expression

            try
              match parsedScript with
              | Ok mod' ->
                match! execute exeState mod' [] with
                | Ok result ->
                  match result with
                  | DString s -> return resultOk (DString s)
                  | _ ->
                    let asString = DvalReprDeveloper.toRepr result
                    return resultOk (DString asString)
                | Error(e, callStack) ->
                  let! csString = Exe.callStackString exeState callStack
                  print $"Error when executing expression. Call-stack:\n{csString}\n"
                  return e |> RT2DT.RuntimeError.toDT |> resultError
              | Error e -> return e |> RT2DT.RuntimeError.toDT |> resultError
            with e ->
              return createExceptionError e |> RT2DT.RuntimeError.toDT |> resultError
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
