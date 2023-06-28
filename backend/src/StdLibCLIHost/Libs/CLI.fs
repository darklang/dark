/// StdLib functions for building the CLI
/// (as opposed to functions needed by Cli programs, which are in StdLibCLI)
module StdLibCLIHost.Libs.CLI

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module StdLib = LibExecution.StdLib



let libraries (extraStdlibForUserPrograms : StdLib.Contents) : RT.Libraries =
  let (builtInFns, builtInTypes) =
    LibExecution.StdLib.combine
      [ StdLibExecution.StdLib.contents
        StdLibCLI.StdLib.contents
        extraStdlibForUserPrograms ]
      []
      []

  { builtInTypes = builtInTypes |> Tablecloth.Map.fromListBy (fun typ -> typ.name)
    builtInFns = builtInFns |> Tablecloth.Map.fromListBy (fun fn -> fn.name)
    packageFns = Map.empty
    packageTypes = Map.empty }


let execute
  (extraStdlibForUserPrograms : StdLib.Contents)
  (allowInternalDarkFunctions : bool)
  (parentState : RT.ExecutionState)
  (mod' : Parser.CanvasV2.CanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Task<RT.Dval> =

  task {
    let config : Config =
      { allowLocalHttpAccess = true; httpclientTimeoutInMs = 10000 }

    let program : Program =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = allowInternalDarkFunctions
        fns =
          mod'.fns
          |> List.map (fun fn -> PT2RT.UserFunction.toRT fn)
          |> Tablecloth.Map.fromListBy (fun fn -> fn.name)
        types =
          mod'.types
          |> List.map (fun typ -> PT2RT.UserType.toRT typ)
          |> Tablecloth.Map.fromListBy (fun typ -> typ.name)
        dbs = Map.empty
        secrets = [] }

    let tracing = Exe.noTracing RT.Real
    let notify = parentState.notify
    let sendException = parentState.reportException
    let state =
      Exe.createState
        (libraries extraStdlibForUserPrograms)
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
  [ { name = typ [ "CLI" ] "ExecutionError" 0
      description = "Result of Execution"
      typeParams = []
      definition =
        CustomType.Record(
          { name = "msg"; typ = TString; description = "The error message" },
          [ { name = "metadata"
              typ = TDict TString
              description = "List of metadata as strings" } ]
        )
      deprecated = NotDeprecated } ]


let fns
  (extraStdlibForUserPrograms : StdLib.Contents)
  (allowInternalDarkFunctions : bool)
  : List<BuiltInFn> =
  [ { name = fn [ "CLI" ] "parseAndExecuteScript" 0
      typeParams = []
      parameters =
        [ Param.make "filename" TString ""
          Param.make "code" TString ""
          Param.make "symtable" (TDict TString) "" ]
      returnType =
        TResult(
          TInt,
          TCustomType(FQName.BuiltIn(typ [ "CLI" ] "ExecutionError" 0), [])
        )
      description = "Parses and executes arbitrary Dark code"
      fn =
        function
        | state, [], [ DString filename; DString code; DDict symtable ] ->
          uply {
            let err (msg : string) (metadata : List<string * string>) =
              let metadata = metadata |> List.map (fun (k, v) -> k, DString v) |> Map
              let fields = [ "msg", DString msg; "metadata", DDict metadata ]

              DResult(
                Error(
                  DRecord(
                    FQName.BuiltIn(typ [ "CLI" ] "ExecutionError" 0),
                    Map fields
                  )
                )
              )

            let exnError (e : exn) : Dval =
              let msg = Exception.getMessages e |> String.concat "\n"
              let metadata =
                Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)
              err msg metadata

            let parsedScript =
              try
                Parser.CanvasV2.parse filename code |> Ok
              with e ->
                Error(exnError e)

            try
              match parsedScript with
              | Ok mod' ->
                match!
                  execute
                    extraStdlibForUserPrograms
                    allowInternalDarkFunctions
                    state
                    mod'
                    symtable
                with
                | DInt i -> return DResult(Ok(DInt i))
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
      deprecated = NotDeprecated } ]

let contents
  (extraStdlibForUserPrograms : StdLib.Contents)
  (allowInternalDarkFunctions : bool)
  : StdLib.Contents =
  (fns extraStdlibForUserPrograms allowInternalDarkFunctions, types)
