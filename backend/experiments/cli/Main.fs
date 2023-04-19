module Cli.Main

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module StdLibCli = StdLibCli.StdLib

// ---------------------
// Version information
// ---------------------

type VersionInfo = { hash : string; buildDate : string; inDevelopment : bool }

#if DEBUG
let inDevelopment : bool = true
#else
let inDevelopment : bool = false
#endif

open System.Reflection

let info () =
  let buildAttributes =
    Assembly.GetEntryAssembly().GetCustomAttribute<AssemblyMetadataAttribute>()
  // This reads values created during the build in Cli.fsproj
  // It doesn't feel like this is how it's supposed to be used, but it works. But
  // what if we wanted more than two parameters?
  let buildDate = buildAttributes.Key
  let gitHash = buildAttributes.Value
  { hash = gitHash; buildDate = buildDate; inDevelopment = inDevelopment }


// ---------------------
// Execution
// ---------------------

let stdlibTypes : Map<RT.FQTypeName.T, RT.BuiltInType> =
  StdLibExecution.StdLib.types
  |> Map.fromListBy (fun typ -> RT.FQTypeName.Stdlib typ.name)

let stdlibFns : Map<RT.FQFnName.T, RT.BuiltInFn> =
  StdLibExecution.StdLib.fns @ StdLibCli.fns
  |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)


let libraries : RT.Libraries =
  { stdlibTypes = stdlibTypes; stdlibFns = stdlibFns; packageFns = Map.empty }


let execute
  (mod' : Parser.CanvasV2.CanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Task<RT.Dval> =

  task {
    let program : RT.ProgramContext =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        userFns =
          mod'.fns
          |> List.map (fun fn -> PT2RT.UserFunction.toRT fn)
          |> Map.fromListBy (fun fn -> fn.name)
        userTypes =
          mod'.types
          |> List.map (fun typ -> PT2RT.UserType.toRT typ)
          |> Map.fromListBy (fun typ -> typ.name)
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

let initSerializers () = Json.Vanilla.allow<pos> "Prelude"

[<EntryPoint>]
let main (args : string []) =
  try
    initSerializers ()
    let mainFile = "/home/dark/app/backend/experiments/cli/program.dark"
    let mod' = Parser.CanvasV2.parseFromFile mainFile
    let args = args |> Array.toList |> List.map RT.DString |> RT.DList
    let result = execute mod' (Map [ "args", args ])
    NonBlockingConsole.wait ()
    match result.Result with
    | RT.DError (_, msg) ->
      System.Console.WriteLine $"Error: {msg}"
      1
    | RT.DInt i -> (int i)
    | dval ->
      let output = LibExecution.DvalReprDeveloper.toRepr dval
      System.Console.WriteLine "Error: main function must return an int"
      1
  with
  | e ->
    printException "Error starting cli" [] e
    1
