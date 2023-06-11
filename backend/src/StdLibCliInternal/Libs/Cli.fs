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

let types : List<BuiltInType> = []

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


let fns : List<BuiltInFn> =
  [ { name = fn [ "Cli" ] "parseAndExecuteScript" 0
      typeParams = []
      parameters = [ Param.make "filename" TString ""; Param.make "code" TString "" ]
      returnType = TResult(TInt, TString)
      description = "Parses and executes arbitrary Dark code"
      fn =
        function
        | state, _, [ DString filename; DString code ] ->
          uply {
            let err msg = DResult(Error(DString msg))
            let parsed =
              try
                Parser.CanvasV2.parse filename code |> Ok
              with e ->
                let msg = Exception.getMessages e |> String.concat "\n"
                Error $"Parse error:\n{msg}"
            try
              match parsed with
              | Ok mod' ->
                let args = [||] |> Array.toList |> List.map RT.DString |> RT.DList
                let! result = execute mod' (Map [ "args", args ])
                match result with
                | DInt i -> return DResult(Ok(DInt i))
                | DError(_, e) -> return err e
                | _ -> return LibExecution.DvalReprDeveloper.toRepr result |> err
              | Error e -> return err e
            with e ->
              return Exception.getMessages e |> String.concat "\n" |> err
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
