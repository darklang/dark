/// StdLib functions for building the CLI
/// (as opposed to functions needed by Cli programs, which are in StdLibCli)
module StdLibCliInternal.Libs.Cli

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution

let typ = FQTypeName.stdlibTypeName'

let fn = FQFnName.stdlibFnName'

let (stdlibFns, stdlibTypes) =
  LibExecution.StdLib.combine
    [ StdLibExecution.StdLib.contents; StdLibCli.StdLib.contents ]
    []
    []


let libraries : RT.Libraries =
  { stdlibTypes = stdlibTypes |> Tablecloth.Map.fromListBy (fun typ -> typ.name)
    stdlibFns = stdlibFns |> Tablecloth.Map.fromListBy (fun fn -> fn.name)
    packageFns = Map.empty
    packageTypes = Map.empty }



let execute
  (mod' : Parser.CanvasV2.CanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Task<RT.Dval> =

  task {
    let program : ProgramContext =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        allowLocalHttpAccess = true
        userFns =
          mod'.fns
          |> List.map (fun fn -> PT2RT.UserFunction.toRT fn)
          |> Tablecloth.Map.fromListBy (fun fn -> fn.name)
        userTypes =
          mod'.types
          |> List.map (fun typ -> PT2RT.UserType.toRT typ)
          |> Tablecloth.Map.fromListBy (fun typ -> typ.name)
        dbs = Map.empty
        secrets = [] }

    let tracing = Exe.noTracing RT.Real

    let notify (_state : RT.ExecutionState) (_msg : string) (_metadata : Metadata) =
      // let metadata = extraMetadata state @ metadata
      // LibService.Rollbar.notify msg metadata
      ()

    let sendException
      (_state : RT.ExecutionState)
      (_metadata : Metadata)
      (_exn : exn)
      =
      // let metadata = extraMetadata state @ metadata
      // let person : LibService.Rollbar.Person =
      //   Some { id = program.accountID; username = Some(username ()) }
      // LibService.Rollbar.sendException person metadata exn
      ()

    let state = Exe.createState libraries tracing sendException notify 7UL program

    return! Exe.executeExpr state symtable (PT2RT.Expr.toRT mod'.exprs[0])
  }

let types : List<BuiltInType> =
  [ { name = typ' [ "Cli" ] "ExecutionError" 0
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


let fns : List<BuiltInFn> =
  [ { name = fn [ "Cli" ] "parseAndExecuteScript" 0
      typeParams = []
      parameters = [ Param.make "filename" TString ""; Param.make "code" TString "" ]
      returnType =
        TResult(
          TInt,
          TCustomType(FQTypeName.Stdlib(typ [ "Cli" ] "ExecutionError" 0), [])
        )
      description = "Parses and executes arbitrary Dark code"
      fn =
        function
        | state, _, [ DString filename; DString code ] ->
          uply {

            let err (msg : string) (metadata : List<string * string>) =
              let metadata = metadata |> List.map (fun (k, v) -> k, DString v) |> Map
              let fields = [ "msg", DString msg; "metadata", DDict metadata ]
              DResult(
                Error(
                  DRecord(
                    FQTypeName.Stdlib(typ [ "Cli" ] "ExecutionError" 0),
                    Map fields
                  )
                )
              )
            let exnError (e : exn) : Dval =
              let msg = Exception.getMessages e |> String.concat "\n"
              let metadata =
                Exception.toMetadata e |> List.map (fun (k, v) -> k, string v)
              err msg metadata
            let parsed =
              try
                Parser.CanvasV2.parse filename code |> Ok
              with e ->
                Error(exnError e)
            try
              match parsed with
              | Ok mod' ->
                let args = [||] |> Array.toList |> List.map DString |> DList
                let! result = execute mod' (Map [ "args", args ])
                match result with
                | DInt i -> return DResult(Ok(DInt i))
                | DError(_, e) -> return err e []
                | _ ->
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

let contents = (fns, types)
