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
      let toDT (et : Error) : Ply<RT.Dval> =
        uply {
          let! caseName, fields =
            uply {
              match et with
              | NoExpressionsToExecute -> return "NoExpressionsToExecute", []

              | UncaughtException(msg, metadata) ->
                let metadata =
                  metadata
                  |> List.map (fun (k, v) -> DTuple(DString k, DString v, []))
                  |> Dval.list (
                    ValueType.Known(
                      KTTuple(ValueType.Known KTString, ValueType.Known KTString, [])
                    )
                  )

                return "UncaughtException", [ DString msg; metadata ]

              | MultipleExpressionsToExecute exprs ->
                return
                  "MultipleExpressionsToExecute",
                  [ Dval.list VT.unknownTODO (List.map DString exprs) ]

              | NonIntReturned actuallyReturned ->
                let! actuallyReturned = RT2DT.Dval.toDT actuallyReturned
                return "NonIntReturned", [ actuallyReturned ]
            }

          let typeName = RT.RuntimeError.name [ "Cli" ] "Error" 0
          return! Dval.enum typeName typeName (Some []) caseName fields
        }


    let toRuntimeError (e : Error) : Ply<RT.RuntimeError> =
      Error.toDT e |> Ply.map RT.RuntimeError.fromDT

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
  : Ply<Result<RT.Dval, DvalSource * RuntimeError>> =

  uply {
    let! (program : Program) =
      uply {
        let! fns =
          mod'.fns
          |> Ply.List.mapSequentially PT2RT.UserFunction.toRT
          |> Ply.map (Map.fromListBy (fun fn -> fn.name))

        let! types =
          mod'.types
          |> Ply.List.mapSequentially PT2RT.UserType.toRT
          |> Ply.map (Map.fromListBy (fun typ -> typ.name))

        let! constants =
          mod'.constants
          |> Ply.List.mapSequentially PT2RT.UserConstant.toRT
          |> Ply.map (Map.fromListBy (fun c -> c.name))

        return
          { canvasID = System.Guid.NewGuid()
            internalFnsAllowed = false
            fns = fns
            types = types
            constants = constants
            dbs = Map.empty
            secrets = [] }
      }

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
      let! expr = PT2RT.Expr.toRT mod'.exprs[0]
      return! Exe.executeExpr state symtable expr
    else if mod'.exprs.Length = 0 then
      let! rte =
        CliRuntimeError.NoExpressionsToExecute |> CliRuntimeError.RTE.toRuntimeError
      return Error((SourceNone, rte))
    else // mod'.exprs.Length > 1
      let! rte =
        CliRuntimeError.MultipleExpressionsToExecute(mod'.exprs |> List.map string)
        |> CliRuntimeError.RTE.toRuntimeError
      return Error((SourceNone, rte))
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
        let errType = VT.unknownTODO
        let resultOk = Dval.resultOk VT.int errType
        let resultError = Dval.resultError VT.int errType
        (function
        | state, [], [ DString filename; DString code; DDict(_vtTODO, symtable) ] ->
          uply {
            let exnError (e : exn) : Ply<RuntimeError> =
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
                  let! err = exnError e
                  return Error err
              }

            try
              match parsedScript with
              | Ok mod' ->
                match! execute state mod' symtable with
                | Ok(DInt i) -> return resultOk (DInt i)
                | Ok result ->
                  return!
                    CliRuntimeError.NonIntReturned result
                    |> CliRuntimeError.RTE.toRuntimeError
                    |> Ply.map (RuntimeError.toDT >> resultError)
                | Error(_, e) -> return e |> RuntimeError.toDT |> resultError
              | Error e -> return e |> RuntimeError.toDT |> resultError
            with e ->
              return! exnError e |> Ply.map (RuntimeError.toDT >> resultError)
          }
        | _ -> incorrectArgs ())
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
        let errType = VT.unknownTODO
        let resultOk = Dval.resultOk VT.string errType
        let resultError = Dval.resultError VT.string errType

        function
        | state, [], [ DString functionName; DList(_vtTODO, args) ] ->
          uply {
            let err (msg : string) (metadata : List<string * string>) =
              let metadata = metadata |> List.map (fun (k, v) -> k, DString v)
              Dval.record
                (FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0))
                (Some [])
                [ "msg", DString msg; "metadata", Dval.dict VT.unknownTODO metadata ]
              |> Ply.map resultError

            let exnError (e : exn) : Ply<Dval> =
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
                      | e -> Exception.raiseInternal "Expected string" [ "arg", e ])

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
                    let asString = LibExecution.DvalReprDeveloper.toRepr value
                    return resultOk (DString asString)
              | _ -> return incorrectArgs ()
            with e ->
              return! exnError e
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents = (fns, types, constants)
