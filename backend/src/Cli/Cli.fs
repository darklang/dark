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
  let (fns, types, constants) =
    LibExecution.StdLib.combine
      [ StdLibExecution.StdLib.contents
        StdLibCli.StdLib.contents
        StdLibCliHost.StdLib.contents ]
      []
      []
  { types = types |> Map.fromListBy (fun typ -> typ.name)
    fns = fns |> Map.fromListBy (fun fn -> fn.name)
    constants = constants |> Map.fromListBy (fun c -> c.name) }

let packageManager = LibCliExecution.PackageManager.packageManager


let execute (symtable : Map<string, RT.Dval>) (args : PT.Expr) : Task<RT.Dval> =

  task {
    let config : RT.Config =
      { allowLocalHttpAccess = true; httpclientTimeoutInMs = 30000 }

    let program : RT.Program =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        fns = Map.empty
        types = Map.empty
        constants = Map.empty
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

    let callFn =
      PT.EApply(
        gid (),
        PT.EFnName(
          gid (),
          Ok(
            PT.FQName.Package(
              { owner = "Darklang"
                modules = [ "Cli" ]
                name = PT.FnName.FnName "executeCliCommand"
                version = 0 }
            )
          )
        ),
        [],
        NEList.singleton args
      )
    return! Exe.executeExpr state symtable (PT2RT.Expr.toRT callFn)
  }

let initSerializers () =
  Json.Vanilla.allow<List<LibCliExecution.PackageManager.LanguageToolsTypesFork.ProgramTypes.PackageType.T>>
    "PackageManager"
  Json.Vanilla.allow<List<LibCliExecution.PackageManager.LanguageToolsTypesFork.ProgramTypes.PackageFn.T>>
    "PackageManager"

[<EntryPoint>]
let main (args : string[]) =
  try
    initSerializers ()

    // CLEANUP
    packageManager.init |> Ply.toTask |> Async.AwaitTask |> Async.RunSynchronously

    let resolver =
      // TODO: this may need more builtins, and packages
      LibParser.NameResolver.fromBuiltins (
        Map.values builtIns.fns,
        Map.values builtIns.types,
        Map.values builtIns.constants
      )
    let args =
      args
      |> Array.toList
      |> List.map (fun arg -> PT.EString(gid (), [ PT.StringText arg ]))
    let args = PT.EList(gid (), args)

    let result = execute (Map.empty) args
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
