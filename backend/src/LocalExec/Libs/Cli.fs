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
  let (fns, types) =
    LibExecution.StdLib.combine
      [ StdLibExecution.StdLib.contents
        StdLibCli.StdLib.contents
        StdLibDarkInternal.StdLib.contents ]
      []
      []
  { types = types |> Tablecloth.Map.fromListBy (fun typ -> typ.name)
    fns = fns |> Tablecloth.Map.fromListBy (fun fn -> fn.name) }

let packageManager : RT.PackageManager = RT.PackageManager.Empty


let execute
  (parentState : RT.ExecutionState)
  (mod' : Parser.CanvasV2.PTCanvasModule)
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
  [ { name = typ [ "LocalExec" ] "ExecutionError" 0
      description = "Result of Execution"
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              { name = "msg"; typ = TString; description = "The error message" },
              [ { name = "metadata"
                  typ = TDict TString
                  description = "List of metadata as strings" } ]
            ) }
      deprecated = NotDeprecated } ]


let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec" ] "parseAndExecuteScript" 0
      typeParams = []
      parameters =
        [ Param.make "filename" TString ""
          Param.make "code" TString ""
          Param.make "symtable" (TList TString) "" ]
      returnType =
        TypeReference.result
          TInt
          (TCustomType(FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0), []))

      description = "Parses and executes arbitrary Dark code"
      fn =
        function
        | state, [], [ DString filename; DString code; args ] ->
          uply {
            let err (msg : string) (metadata : List<string * string>) =
              let metadata = metadata |> List.map (fun (k, v) -> k, DString v) |> Map
              let fields = [ "msg", DString msg; "metadata", DDict metadata ]
              Dval.resultError (
                DRecord(FQName.BuiltIn(typ [ "Cli" ] "ExecutionError" 0), Map fields)
              )


            let exnError (e : exn) : Dval =
              let msg = Exception.getMessages e |> String.concat "\n"
              let metadata =
                Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)
              err msg metadata

            let parsedScript =
              try
                let resolver =
                  Parser.NameResolver.fromBuiltins (Map.values builtIns.fns |> Seq.toList, Map.values builtIns.types |> Seq.toList)
                Parser.CanvasV2.parse resolver filename code |> Ok
              with e ->
                Error(exnError e)

            try
              match parsedScript with
              | Ok mod' ->
                let symtable = [ ("args", args) ] |> Map.ofList

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


    { name = fn [ "LocalExec"; "File" ] "read" 0
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
              return DError(SourceNone, $"Error reading file: {e.Message}")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
