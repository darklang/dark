/// StdLib functions for building the CLI
/// (as opposed to functions needed by CLI programs, which are in StdLibCli)
module BuiltinCliHost.Libs.Cli

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module DvalUtils = LibExecution.DvalUtils
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module Exe = LibExecution.Execution
module WT = LibParser.WrittenTypes
module Json = BuiltinExecution.Libs.Json


module CliRuntimeError =
  open Prelude

  type Error =
    | NoExpressionsToExecute
    | UncaughtException of string * metadata : List<string * string>
    | MultipleExpressionsToExecute of List<string>
    | NonIntReturned of actuallyReturned : Dval

  /// to RuntimeError
  module RTE =
    module Error =
      let toDT (et : Error) : RT.Dval =
        let nameTypeName = RT.RuntimeError.name [ "Cli" ] "Error" 0
        let caseName, fields =
          match et with
          | NoExpressionsToExecute -> "NoExpressionsToExecute", []

          | UncaughtException(msg, metadata) ->
            let metadata =
              metadata
              |> List.map (fun (k, v) -> DTuple(DString k, DString v, []))
              |> DvalUtils.list (
                ValueType.Known(
                  KTTuple(ValueType.Known KTString, ValueType.Known KTString, [])
                )
              )

            "UncaughtException", [ "msg", DString msg; "metadata", metadata ]

          | MultipleExpressionsToExecute exprs ->
            "MultipleExpressionsToExecute",
            [ "exprs", DvalUtils.list valueTypeTODO (List.map DString exprs) ]

          | NonIntReturned actuallyReturned ->
            "NonIntReturned",
            [ "actuallyReturned", RT2DT.Dval.toDT actuallyReturned ]

        DvalUtils.enum nameTypeName caseName []


    let toRuntimeError (e : Error) : RT.RuntimeError =
      Error.toDT e |> RT.RuntimeError.fromDT

let libExecutionContents =
  BuiltinExecution.Builtin.contents BuiltinExecution.Libs.HttpClient.defaultConfig

let builtIns : RT.BuiltIns =
  let (fns, types, constants) =
    LibExecution.Builtin.combine
      [ libExecutionContents; BuiltinCli.Builtin.contents ]
      []
      []
  { types = types |> Map.fromListBy (fun typ -> typ.name)
    fns = fns |> Map.fromListBy (fun fn -> fn.name)
    constants = constants |> Map.fromListBy (fun c -> c.name) }

let packageManager = LibCliExecution.PackageManager.packageManager

let execute
  (parentState : RT.ExecutionState)
  (mod' : LibParser.Canvas.PTCanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Task<RT.Dval> =

  task {
    let program : Program =
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
        constants =
          mod'.constants
          |> List.map (fun c -> PT2RT.UserConstant.toRT c)
          |> Map.fromListBy (fun c -> c.name)
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

    if mod'.exprs.Length = 1 then
      return! Exe.executeExpr state symtable (PT2RT.Expr.toRT mod'.exprs[0])
    else if mod'.exprs.Length = 0 then
      return
        DError(
          SourceNone,
          CliRuntimeError.NoExpressionsToExecute
          |> CliRuntimeError.RTE.toRuntimeError
        )
    else // mod'.exprs.Length > 1
      return
        DError(
          SourceNone,
          CliRuntimeError.MultipleExpressionsToExecute(mod'.exprs |> List.map string)
          |> CliRuntimeError.RTE.toRuntimeError
        )
  }

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn [ "Cli" ] "parseAndExecuteScript" 0
      typeParams = []
      parameters =
        [ Param.make "filename" TString ""
          Param.make "code" TString ""
          Param.make "symtable" (TDict TString) "" ]
      returnType =
        TypeReference.result
          TInt
          (TCustomType(Ok(FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0)), []))
      description = "Parses and executes arbitrary Dark code"
      fn =
        function
        | state, [], [ DString filename; DString code; DDict symtable ] ->
          uply {
            let exnError (e : exn) : RuntimeError =
              let msg = Exception.getMessages e |> String.concat "\n"
              let metadata =
                Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)
              CliRuntimeError.UncaughtException(msg, metadata)
              |> CliRuntimeError.RTE.toRuntimeError

            let nameResolver = LibParser.NameResolver.fromExecutionState state

            let! parsedScript =
              uply {
                try
                  return!
                    LibParser.Canvas.parse nameResolver filename code |> Ply.map Ok
                with e ->
                  return Error(exnError e)
              }

            try
              match parsedScript with
              | Ok mod' ->
                match! execute state mod' symtable with
                | DInt i -> return DvalUtils.resultOk (DInt i)
                | DError(_, e) ->
                  return e |> RuntimeError.toDT |> DvalUtils.resultError
                | result ->
                  return
                    CliRuntimeError.NonIntReturned result
                    |> CliRuntimeError.RTE.toRuntimeError
                    |> RuntimeError.toDT
                    |> DvalUtils.resultError
              | Error e -> return e |> RuntimeError.toDT |> DvalUtils.resultError
            with e ->
              return exnError e |> RuntimeError.toDT |> DvalUtils.resultError
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "Cli" ] "executeFunction" 0
      typeParams = []
      parameters =
        [ Param.make "functionName" TString ""
          Param.make "args" (TList TString) "" ]
      returnType =
        TypeReference.result
          TString
          (TCustomType(Ok(FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0)), []))
      description = "Executes an arbitrary Dark function"
      fn =
        function
        | state, [], [ DString functionName; DList(_vtTODO, args) ] ->
          uply {
            let err (msg : string) (metadata : List<string * string>) =
              let metadata = metadata |> List.map (fun (k, v) -> k, DString v)
              DvalUtils.resultError (
                DvalUtils.record
                  (FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0))
                  [ "msg", DString msg; "metadata", DvalUtils.dict metadata ]
              )

            let exnError (e : exn) : Dval =
              let msg = Exception.getMessages e |> String.concat "\n"
              let metadata =
                Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)
              err msg metadata

            try
              let parts = functionName.Split('.') |> List.ofArray
              let name = NEList.ofList "PACKAGE" parts
              let resolver = LibParser.NameResolver.fromExecutionState state
              let! fnName =
                LibParser.NameResolver.FnName.resolve
                  resolver
                  []
                  (WT.Unresolved name)

              match fnName, args with
              | Ok fnName, firstArg :: additionalArgs ->
                match
                  NEList.find Dval.isFake (NEList.ofList firstArg additionalArgs)
                with
                | Some fakeArg -> return fakeArg
                | None ->
                  let desc = fnName |> PT2RT.FnName.toRT
                  let! fn =
                    match desc with
                    | FQName.Package pkg ->
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
                    let types = RT.ExecutionState.availableTypes state

                    let expectedTypes =
                      (NEList.toList f.parameters) |> List.map (fun p -> p.typ)

                    let stringArgs =
                      args
                      |> List.map (fun arg ->
                        match arg with
                        | DString s -> s
                        | e ->
                          Exception.raiseInternal "Expected string" [ "arg", e ])

                    let! args =
                      Ply.List.mapSequentially
                        (fun (typ, (str : string)) ->
                          uply {
                            // Quote the string only if it's of type String and isn't already quoted.
                            // Leave it unquoted for other types.
                            let str =
                              if str.StartsWith("\"") && str.EndsWith("\"") then str
                              else if typ = TString then $"\"{str}\""
                              else str

                            match! Json.parse types typ str with
                            | Ok v -> return v
                            | Error e -> return (DString e)
                          })
                        (List.zip expectedTypes stringArgs)

                    let! result =
                      Exe.executeFunction
                        state
                        (gid ())
                        (f.name)
                        []
                        (NEList.ofList args.Head args.Tail)

                    match result with
                    | DError(_, e) ->
                      // TODO we should probably return the error here as-is, and handle by calling the
                      // toSegments on the error within the CLI
                      return
                        e
                        |> RuntimeError.toDT
                        |> LibExecution.DvalReprDeveloper.toRepr
                        |> DString
                        |> DvalUtils.resultError
                    | value ->
                      let asString = LibExecution.DvalReprDeveloper.toRepr value
                      return DvalUtils.resultOk (DString asString)
              | _ -> return incorrectArgs ()
            with e ->
              return exnError e
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents = (fns, types, constants)
