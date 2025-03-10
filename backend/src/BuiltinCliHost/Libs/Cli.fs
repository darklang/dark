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

module Utils = BuiltinCliHost.Utils


module ExecutionError =
  let fqTypeName = FQTypeName.fqPackage PackageIDs.Type.Cli.executionError
  let typeRef = TCustomType(Ok fqTypeName, [])


module Config =
  let pmBaseUrl =
    match
      System.Environment.GetEnvironmentVariable
        "DARK_CONFIG_PACKAGE_MANAGER_BASE_URL"
    with
    | null -> "https://packages.darklang.com"
    | var -> var

  let initializePackageManagers () =
    task {
      let packageManagerRT = LibPackageManager.PackageManager.rt pmBaseUrl
      let packageManagerPT = LibPackageManager.PackageManager.pt pmBaseUrl

      do! packageManagerRT.init

      return packageManagerRT, packageManagerPT
    }

  let packageManagerRT, packageManagerPT = initializePackageManagers().Result

  let builtinsToUse : RT.Builtins =
    LibExecution.Builtin.combine
      [ BuiltinExecution.Builtin.builtins
          BuiltinExecution.Libs.HttpClient.defaultConfig
          packageManagerPT
        BuiltinCli.Builtin.builtins ]
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

    let constants =
      List.concat
        [ mod'.constants |> List.map PT2RT.PackageConstant.toRT
          mod'.submodules.constants |> List.map PT2RT.PackageConstant.toRT ]

    let fns =
      List.concat
        [ mod'.fns |> List.map PT2RT.PackageFn.toRT
          mod'.submodules.fns |> List.map PT2RT.PackageFn.toRT ]

    let packageManager =
      Config.packageManagerRT |> PackageManager.withExtras types constants fns

    let tracing = Exe.noTracing

    let state =
      Exe.createState
        Config.builtinsToUse
        packageManager
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
      let exprInsrts = exprs |> List.map (PT2RT.Expr.toRT Map.empty 0)
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
            let exnError (e : exn) : RuntimeError.Error =
              RuntimeError.UncaughtException(
                Exception.getMessages e |> String.concat "\n",
                Exception.toMetadata e
                |> List.map (fun (k, v) -> (k, DString(string v)))
              )

            let onMissingType =
              FQTypeName.Package
                PackageIDs.Type.LanguageTools.NameResolver.nameResolverOnMissing
            let onMissingAllow = DEnum(onMissingType, onMissingType, [], "Allow", [])

            let getPmFnName =
              FQFnName.Package PackageIDs.Fn.LanguageTools.PackageManager.pm

            let! execResult =
              Exe.executeFunction exeState getPmFnName [] (NEList.singleton DUnit)

            let! pm =
              uply {
                match execResult with
                | Ok dval -> return dval
                | Error(rte, _cs) ->
                  let! rteString = Exe.runtimeErrorToString exeState rte
                  return
                    Exception.raiseInternal
                      "Error executing pm function"
                      [ "rte", rteString ]
              }
            let args =
              NEList.ofList
                (DString "CliScript")
                [ DString "ScriptName"
                  onMissingAllow
                  pm
                  DString filename
                  DString code ]

            let parseCliScriptFnName =
              FQFnName.Package
                PackageIDs.Fn.LanguageTools.Parser.CliScript.parseCliScript

            let! execResult =
              Exe.executeFunction exeState parseCliScriptFnName [] args

            let! (parsedScript :
              Result<Utils.CliScript.PTCliScriptModule, RuntimeError.Error>) =
              uply {
                match execResult with
                | Ok dval -> return (Utils.CliScript.fromDT dval) |> Ok
                | Error(rte, _cs) ->
                  let! rteString = Exe.runtimeErrorToString exeState rte
                  return
                    Exception.raiseInternal
                      "Error executing parseCanvas function"
                      [ "error", rteString ]
              }

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
                  // TODO: do this, some better way
                  // (probably pass it back in a structured way)
                  let! csString = Exe.callStackString exeState callStack
                  print $"Error when executing Script. Call-stack:\n{csString}\n"

                  return e |> RT2DT.RuntimeError.toDT |> resultError
              | Error e -> return e |> RT2DT.RuntimeError.toDT |> resultError
            with e ->
              return exnError e |> RT2DT.RuntimeError.toDT |> resultError
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // TODO: better handling for functions that takes string args
    { name = fn "cliExecuteFunction" 0
      typeParams = []
      parameters =
        [ Param.make "functionName" TString ""
          Param.make "args" (TList(TCustomType(Ok PT2DT.Expr.typeName, []))) "" ]
      returnType = TypeReference.result TString ExecutionError.typeRef
      description =
        "Executes an arbitrary Dark package function using the new darklang parser"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName, [])
        let resultOk = Dval.resultOk KTString errType
        let resultError = Dval.resultError KTString errType

        function
        | exeState, _, [], [ DString functionName; DList(_vtTODO, args) ] ->
          uply {
            let err (msg : string) (metadata : List<string * string>) : Dval =
              let fields =
                [ ("msg", DString msg)
                  ("metadata",
                   DDict(
                     VT.string,
                     metadata |> List.map (Tuple2.mapSecond DString) |> Map
                   )) ]

              DRecord(
                ExecutionError.fqTypeName,
                ExecutionError.fqTypeName,
                [],
                Map fields
              )

            let exnError (e : exn) : Dval =
              let msg = Exception.getMessages e |> String.concat "\n"
              let metadata =
                Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)
              err msg metadata

            try
              let resolveFn =
                FQFnName.Package
                  PackageIDs.Fn.LanguageTools.NameResolver.FnName.resolve

              let onMissingType =
                FQTypeName.Package
                  PackageIDs.Type.LanguageTools.NameResolver.nameResolverOnMissing
              let onMissingAllow =
                DEnum(onMissingType, onMissingType, [], "Allow", [])

              let parserRangeType =
                FQTypeName.Package PackageIDs.Type.LanguageTools.Parser.range
              let pointType =
                FQTypeName.Package PackageIDs.Type.LanguageTools.Parser.point
              let pointFields = [ ("row", DInt64 0); ("column", DInt64 0) ]
              let fields =
                [ ("start", DRecord(pointType, pointType, [], Map pointFields))
                  ("end_", DRecord(pointType, pointType, [], Map pointFields)) ]

              let rangeParser =
                DRecord(parserRangeType, parserRangeType, [], Map fields)
              let writtenTypesNameType =
                FQTypeName.Package PackageIDs.Type.LanguageTools.WrittenTypes.name

              let parts = functionName.Split('.') |> List.ofArray
              let currentModule = DList(VT.string, [])
              let nameArg =
                DEnum(
                  writtenTypesNameType,
                  writtenTypesNameType,
                  [],
                  "Unresolved",
                  [ rangeParser; DList(VT.string, parts |> List.map DString) ]
                )

              let pm = FQFnName.Package PackageIDs.Fn.LanguageTools.PackageManager.pm
              let! execResult =
                Exe.executeFunction exeState pm [] (NEList.singleton RT.Dval.DUnit)
              let! pm =
                uply {
                  match execResult with
                  | Ok dval -> return dval
                  | Error(rte, _cs) ->
                    let! rteString =
                      (Exe.rteToString RT2DT.RuntimeError.toDT exeState rte)
                    return
                      Exception.raiseInternal
                        "Error executing pm function"
                        [ "rte", rteString ]
                }

              let resolveFnArgs =
                NEList.ofList
                  onMissingAllow
                  [ pm; RT.DString "Cli"; currentModule; nameArg ]

              let! execResult =
                Exe.executeFunction exeState resolveFn [] resolveFnArgs

              let! fnName =
                uply {
                  match execResult with
                  | Ok dval ->
                    match C2DT.Result.fromDT PT2DT.FQFnName.fromDT dval identity with
                    | Ok fnName -> return Ok fnName
                    | Error _ ->
                      return
                        Exception.raiseInternal "Error converting Dval to FQName" []
                  | Error(rte) ->
                    return
                      Exception.raiseInternal
                        "Error executing resolve function"
                        [ "rte", rte ]
                }

              match fnName with
              | Ok fnName ->
                let! fn =
                  match PT2RT.FQFnName.toRT fnName with
                  | FQFnName.Package pkg -> exeState.fns.package pkg
                  | _ ->
                    Exception.raiseInternal
                      "Error constructing package function name"
                      [ "fn", fn ]

                match fn with
                | None -> return DString "fn not found"
                | Some f ->
                  let newArgs =
                    args
                    |> List.collect (fun dval ->
                      match dval with
                      | DEnum(_, _, _, _, fields) -> fields |> List.tail
                      | e -> Exception.raiseInternal "Invalid Expr" [ "e", e ])

                  let newArgs = if newArgs = [] then [ RT.DUnit ] else newArgs

                  let! result =
                    Exe.executeFunction
                      exeState
                      (FQFnName.Package f.id)
                      []
                      (NEList.ofList newArgs.Head newArgs.Tail)

                  match result with
                  | Error(rte, _cs) ->
                    // TODO we should probably return the error here as-is, and handle by calling the
                    // toSegments on the error within the CLI
                    match! Exe.runtimeErrorToString exeState rte with
                    | Ok(DString s) -> return s |> DString |> resultError
                    | _ ->
                      let rte =
                        rte |> RT2DT.RuntimeError.toDT |> DvalReprDeveloper.toRepr
                      return
                        $"An error occured trying to print a runtime error:\n\noriginal error:\n{rte}"
                        |> DString
                        |> resultError

                  | Ok value ->
                    match value with
                    | DString s -> return resultOk (DString s)
                    | _ ->
                      let asString = DvalReprDeveloper.toRepr value
                      return resultOk (DString asString)
              | _ -> return incorrectArgs ()
            with e ->
              return exnError e
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
