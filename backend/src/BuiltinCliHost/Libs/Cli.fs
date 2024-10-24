/// Builtin functions for building the CLI
/// (as opposed to functions needed by CLI programs, which are in StdLibCli)
module BuiltinCliHost.Libs.Cli

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
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


module CliRuntimeError =
  open Prelude

  type Error =
    | NoExpressionsToExecute
    | UncaughtException of string * metadata : List<string * string>
    | NonIntReturned of actuallyReturned : Dval

  /// to RuntimeError
  module RTE =
    module Error =
      let toDT (et : Error) : RT.Dval =
        let (caseName, fields) =
          match et with
          | NoExpressionsToExecute -> "NoExpressionsToExecute", []

          | UncaughtException(msg, metadata) ->
            let metadata =
              metadata
              |> List.map (fun (k, v) -> DTuple(DString k, DString v, []))
              |> Dval.list (KTTuple(VT.string, VT.string, []))

            "UncaughtException", [ DString msg; metadata ]

          | NonIntReturned actuallyReturned ->
            "NonIntReturned", [ RT2DT.Dval.toDT actuallyReturned ]

        let typeName =
          FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeError.Cli.error
        DEnum(typeName, typeName, [], caseName, fields)


    let toRuntimeError (e : Error) : RT.RuntimeError =
      Error.toDT e |> RT.RuntimeError.fromDT



// TODO: de-dupe with _other_ Cli.fs
let pmBaseUrl =
  match
    System.Environment.GetEnvironmentVariable "DARK_CONFIG_PACKAGE_MANAGER_BASE_URL"
  with
  | null -> "https://packages.darklang.com"
  | var -> var
let packageManagerRT = LibPackageManager.PackageManager.rt pmBaseUrl
let packageManagerPT = LibPackageManager.PackageManager.pt pmBaseUrl


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
  (symtable : Map<string, RT.Dval>)
  : Ply<Result<RT.Dval, Option<CallStack> * RuntimeError>> =
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
      PackageManager.withExtras packageManagerRT types constants fns

    let tracing = Exe.noTracing (CallStack.fromEntryPoint Script)

    let state =
      Exe.createState
        builtinsToUse
        packageManager
        tracing
        parentState.reportException
        parentState.notify
        program

    if mod'.exprs.Length = 0 then
      let rte =
        CliRuntimeError.NoExpressionsToExecute |> CliRuntimeError.RTE.toRuntimeError
      return Error((None, rte))
    else // mod'.exprs.Length > 1
      let exprs = List.map PT2RT.Expr.toRT mod'.exprs
      let results = List.map (Exe.executeExpr state symtable) exprs
      match List.tryLast results with
      | Some lastResult -> return! lastResult
      | None ->
        let rte =
          CliRuntimeError.NoExpressionsToExecute
          |> CliRuntimeError.RTE.toRuntimeError
        return Error((None, rte))
  }


let fns : List<BuiltInFn> =
  [ { name = fn "cliParseAndExecuteScript" 0
      typeParams = []
      parameters =
        [ Param.make "filename" TString ""
          Param.make "code" TString ""
          Param.make "symtable" (TDict TString) "" ]
      returnType = TypeReference.result TInt64 ExecutionError.typeRef
      description =
        "Parses Dark code as a script, and and executes it, returning an exit code"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName, [])
        let resultOk = Dval.resultOk KTInt64 errType
        let resultError = Dval.resultError KTInt64 errType
        (function
        | state, [], [ DString filename; DString code; DDict(_vtTODO, symtable) ] ->
          uply {
            let exnError (e : exn) : RuntimeError =
              let msg = Exception.getMessages e |> String.concat "\n"
              let metadata =
                Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)
              CliRuntimeError.UncaughtException(msg, metadata)
              |> CliRuntimeError.RTE.toRuntimeError

            let onMissingType =
              RT.FQTypeName.FQTypeName.Package
                PackageIDs.Type.LanguageTools.NameResolver.nameResolverOnMissing
            let onMissingAllow =
              RT.Dval.DEnum(onMissingType, onMissingType, [], "Allow", [])

            let getPmFnName =
              RT.FQFnName.FQFnName.Package
                PackageIDs.Fn.LanguageTools.PackageManager.pm

            let! execResult =
              Exe.executeFunction
                state
                getPmFnName
                []
                (NEList.singleton RT.Dval.DUnit)

            let! pm =
              uply {
                match execResult with
                | Ok dval -> return dval
                | Error(_callStack, rte) ->
                  let! rteString = (Exe.rteToString state rte)
                  return
                    Exception.raiseInternal
                      "Error executing pm function"
                      [ "rte", rteString ]
              }
            let args =
              NEList.ofList
                (RT.Dval.DString "CliScript")
                [ RT.Dval.DString "ScriptName"
                  onMissingAllow
                  pm
                  RT.Dval.DString filename
                  RT.Dval.DString code ]

            let parseCliScriptFnName =
              RT.FQFnName.FQFnName.Package
                PackageIDs.Fn.LanguageTools.Parser.CliScript.parseCliScript

            let! execResult = Exe.executeFunction state parseCliScriptFnName [] args

            let! parsedScript =
              uply {
                match execResult with
                | Ok dval -> return (Utils.CliScript.fromDT dval) |> Ok
                | Error(_callStack, rte) ->
                  let! rteString = Exe.rteToString state rte
                  return
                    Exception.raiseInternal
                      "Error executing parseCanvas function"
                      [ "error", rteString ]
              }

            try
              match parsedScript with
              | Ok mod' ->
                match! execute state mod' symtable with
                | Ok(DInt64 i) -> return resultOk (DInt64 i)
                | Ok result ->
                  return
                    CliRuntimeError.NonIntReturned result
                    |> CliRuntimeError.RTE.toRuntimeError
                    |> RuntimeError.toDT
                    |> resultError
                | Error(_callStack, e) ->
                  // TODO: do this, some better way
                  // (probably pass it back in a structured way)
                  // let! csString = Exe.callStackString state callStack
                  // print $"Error when executing Script. Call-stack:\n{csString}\n"

                  return e |> RuntimeError.toDT |> resultError
              | Error e -> return e |> RuntimeError.toDT |> resultError
            with e ->
              return exnError e |> RuntimeError.toDT |> resultError
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
        | state, [], [ DString functionName; DList(_vtTODO, args) ] ->
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
                RT.FQFnName.FQFnName.Package
                  PackageIDs.Fn.LanguageTools.NameResolver.FnName.resolve

              let onMissingType =
                RT.FQTypeName.FQTypeName.Package
                  PackageIDs.Type.LanguageTools.NameResolver.nameResolverOnMissing
              let onMissingAllow =
                RT.Dval.DEnum(onMissingType, onMissingType, [], "Allow", [])

              let parserRangeType =
                RT.FQTypeName.FQTypeName.Package
                  PackageIDs.Type.LanguageTools.Parser.range
              let pointType =
                RT.FQTypeName.FQTypeName.Package
                  PackageIDs.Type.LanguageTools.Parser.point
              let pointFields =
                [ ("row", RT.Dval.DInt64 0); ("column", RT.Dval.DInt64 0) ]
              let fields =
                [ ("start",
                   RT.Dval.DRecord(pointType, pointType, [], Map pointFields))
                  ("end_", RT.Dval.DRecord(pointType, pointType, [], Map pointFields)) ]

              let rangeParser =
                RT.Dval.DRecord(parserRangeType, parserRangeType, [], Map fields)
              let writtenTypesNameType =
                RT.FQTypeName.FQTypeName.Package
                  PackageIDs.Type.LanguageTools.WrittenTypes.name

              let parts = functionName.Split('.') |> List.ofArray
              let currentModule = RT.Dval.DList(VT.string, [])
              let nameArg =
                RT.Dval.DEnum(
                  writtenTypesNameType,
                  writtenTypesNameType,
                  [],
                  "Unresolved",
                  [ rangeParser
                    RT.Dval.DList(VT.string, parts |> List.map RT.Dval.DString) ]
                )

              let pm =
                RT.FQFnName.FQFnName.Package
                  PackageIDs.Fn.LanguageTools.PackageManager.pm
              let! execResult =
                Exe.executeFunction state pm [] (NEList.singleton RT.Dval.DUnit)
              let! pm =
                uply {
                  match execResult with
                  | Ok dval -> return dval
                  | Error(_, rte) ->
                    let! rteString = (Exe.rteToString state rte)
                    return
                      Exception.raiseInternal
                        "Error executing pm function"
                        [ "rte", rteString ]
                }

              let resolveFnArgs =
                NEList.ofList
                  onMissingAllow
                  [ pm; RT.DString "Cli"; currentModule; nameArg ]

              let! execResult = Exe.executeFunction state resolveFn [] resolveFnArgs

              let! fnName =
                uply {
                  match execResult with
                  | Ok dval ->
                    match C2DT.Result.fromDT PT2DT.FQFnName.fromDT dval identity with
                    | Ok fnName -> return Ok fnName
                    | Error _ ->
                      return
                        Exception.raiseInternal "Error converting Dval to FQName" []
                  | Error(_, rte) ->
                    return
                      Exception.raiseInternal
                        "Error executing resolve function"
                        [ "rte", rte ]
                }

              match fnName with
              | Ok fnName ->
                let! fn =
                  match PT2RT.FQFnName.toRT fnName with
                  | FQFnName.Package pkg ->
                    uply {
                      let! fn = state.packageManager.getFn pkg
                      return Option.map packageFnToFn fn
                    }
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
                      state
                      f.name
                      []
                      (NEList.ofList newArgs.Head newArgs.Tail)

                  match result with
                  | Error(_, e) ->
                    // TODO we should probably return the error here as-is, and handle by calling the
                    // toSegments on the error within the CLI
                    return
                      e
                      |> RuntimeError.toDT
                      |> LibExecution.DvalReprDeveloper.toRepr
                      |> DString
                      |> resultError
                  | Ok value ->
                    match value with
                    | DString s -> return resultOk (DString s)
                    | _ ->
                      let asString = LibExecution.DvalReprDeveloper.toRepr value
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
