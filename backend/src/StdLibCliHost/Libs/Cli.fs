/// StdLib functions for building the CLI
/// (as opposed to functions needed by CLI programs, which are in StdLibCli)
module StdLibCliHost.Libs.Cli

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module WT = LibParser.WrittenTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Json = StdLibExecution.Libs.Json


let libExecutionContents =
  StdLibExecution.StdLib.contents StdLibExecution.Libs.HttpClient.unconstrainedConfig

let builtIns : RT.BuiltIns =
  let (fns, types, constants) =
    LibExecution.StdLib.combine
      [ libExecutionContents; StdLibCli.StdLib.contents ]
      []
      []
  { types = types |> Tablecloth.Map.fromListBy (fun typ -> typ.name)
    fns = fns |> Tablecloth.Map.fromListBy (fun fn -> fn.name)
    constants = constants |> Tablecloth.Map.fromListBy (fun c -> c.name) }

let packageManager = LibCliExecution.PackageManager.packageManager

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
        internalFnsAllowed = false
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
      return DError(SourceNone, "No expressions to execute")
    else // mod'.exprs.Length > 1
      return DError(SourceNone, "Multiple expressions to execute")
  }

let types : List<BuiltInType> =
  [ { name = typ [ "Cli" ] "ExecutionError" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Definition.Record(
              NEList.ofList
                { name = "msg"; typ = TString }
                [ { name = "metadata"; typ = TDict TString } ]
            ) }
      description = "Result of Execution"
      deprecated = NotDeprecated } ]


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
          (TCustomType(FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0), []))
      description = "Parses and executes arbitrary Dark code"
      fn =
        function
        | state, [], [ DString filename; DString code; DDict symtable ] ->
          uply {
            let err (msg : string) (metadata : List<string * string>) =
              let metadata = metadata |> List.map (fun (k, v) -> k, DString v) |> Map
              Dval.resultError (
                Dval.record
                  (FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0))
                  [ "msg", DString msg; "metadata", DDict metadata ]
              )

            let exnError (e : exn) : Dval =
              let msg = Exception.getMessages e |> String.concat "\n"
              let metadata =
                Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)
              err msg metadata

            let parsedScript =
              try
                let resolver = LibParser.NameResolver.fromExecutionState state
                LibParser.Canvas.parse resolver filename code |> Ok
              with e ->
                Error(exnError e)

            try
              match parsedScript with
              | Ok mod' ->
                match! execute state mod' symtable with
                | DInt i -> return Dval.resultOk (DInt i)
                | DError(_, e) -> return err e []
                | result ->
                  let asString = LibExecution.DvalReprDeveloper.toRepr result
                  return err $"Expected an integer" [ "actualValue", asString ]
              | Error e -> return e
            with e ->
              return exnError e
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
          (TCustomType(FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0), []))
      description = "Executes an arbitrary Dark function"
      fn =
        function
        | state, [], [ DString functionName; DList args ] ->
          uply {
            let err (msg : string) (metadata : List<string * string>) =
              let metadata = metadata |> List.map (fun (k, v) -> k, DString v) |> Map
              Dval.resultError (
                Dval.record
                  (FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0))
                  [ "msg", DString msg; "metadata", DDict metadata ]
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
              let fnName =
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
                    | DError(_, e) -> return Dval.resultError (DString e)
                    | value ->
                      let asString = LibExecution.DvalReprDeveloper.toRepr value
                      return Dval.resultOk (DString asString)
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
