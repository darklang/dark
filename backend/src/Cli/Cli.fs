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

let builtIns : RT.BuiltIns =
  let (fns, types) =
    LibExecution.StdLib.combine
      [ StdLibExecution.StdLib.contents
        StdLibCli.StdLib.contents
        Cli.StdLib.contents ]
      []
      []
  { types = types |> Map.fromListBy (fun typ -> typ.name)
    fns = fns |> Map.fromListBy (fun fn -> fn.name) }

let packageManager : RT.PackageManager = RT.PackageManager.Empty


let execute
  (mod' : Parser.CanvasV2.PTCanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Task<RT.Dval> =

  task {
    let config : RT.Config =
      { allowLocalHttpAccess = true; httpclientTimeoutInMs = 30000 }

    let program : RT.Program =
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
        dbs = Map.empty
        secrets = [] }

    let tracing = Exe.noTracing RT.Real

    let notify (_state : RT.ExecutionState) (_msg : string) (_metadata : Metadata) =
      // let metadata = extraMetadata state @ metadata
      // LibService.Rollbar.notify msg metadata
      ()

    let sendException (_ : RT.ExecutionState) (metadata : Metadata) (exn : exn) =
      printException "Internal error" metadata exn

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
      return RT.DError(RT.SourceNone, "No expressions to execute")
    else // mod'.exprs.Length > 1
      return RT.DError(RT.SourceNone, "Multiple expressions to execute")
  }

let initSerializers () = ()

[<EntryPoint>]
let main (args : string[]) =
  try
    initSerializers ()

    let resolver =
      // TODO: this may need more builtins, and packages
      Parser.NameResolver.fromBuiltins (
        Map.values builtIns.fns,
        Map.values builtIns.types
      )

    let hostScript =
      Parser.CanvasV2.parseFromFile
        resolver
        "/home/dark/app/backend/src/Cli/cli-host.dark"

    let args = args |> Array.toList |> List.map RT.DString |> RT.DList

    let result = execute hostScript (Map [ "args", args ])
    let result = result.Result

    NonBlockingConsole.wait ()

    match result with
    | RT.DError(RT.SourceNone, msg) ->
      System.Console.WriteLine $"Error: {msg}"
      1
    | RT.DError(RT.SourceID(tlid, id), msg) ->
      System.Console.WriteLine $"Error ({tlid}, {id}): {msg}"
      1
    | RT.DInt i -> (int i)
    | dval ->
      let output = LibExecution.DvalReprDeveloper.toRepr dval
      System.Console.WriteLine
        $"Error: main function must return an int (returned {output})"
      1
  with e ->
    printException "Error starting Darklang CLI" [] e
    1
