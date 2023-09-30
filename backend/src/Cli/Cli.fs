module Cli.Main

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module BuiltinCli = BuiltinCli.Builtin

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
    LibExecution.Builtin.combine
      [ BuiltinExecution.Builtin.contents
          BuiltinExecution.Libs.HttpClient.defaultConfig
        BuiltinCli.Builtin.contents
        BuiltinCliHost.Builtin.contents ]
      []
      []
  { types = types |> Map.fromListBy (fun typ -> typ.name)
    fns = fns |> Map.fromListBy (fun fn -> fn.name)
    constants = constants |> Map.fromListBy (fun c -> c.name) }

let packageManager = LibCliExecution.PackageManager.packageManager

let state () =
  let program : RT.Program =
    { canvasID = System.Guid.NewGuid()
      internalFnsAllowed = false
      fns = Map.empty
      types = Map.empty
      constants = Map.empty
      dbs = Map.empty
      secrets = [] }

  let tracing = Exe.noTracing

  let notify (_state : RT.ExecutionState) (_msg : string) (_metadata : Metadata) =
    // let metadata = extraMetadata state @ metadata
    // LibService.Rollbar.notify msg metadata
    ()

  let sendException (_ : RT.ExecutionState) (metadata : Metadata) (exn : exn) =
    printException "Internal error" metadata exn

  Exe.createState builtIns packageManager tracing sendException notify 7UL program




let execute
  (args : List<string>)
  : Task<Result<RT.Dval, RT.DvalSource * RT.RuntimeError>> =
  task {
    let state = state ()
    let fnName = RT.FnName.fqPackage "Darklang" [ "Cli" ] "executeCliCommand" 0
    let args =
      args
      |> List.map RT.DString
      |> Dval.list (RT.ValueType.Known RT.KTString)
      |> NEList.singleton
    return! Exe.executeFunction state 7UL fnName [] args
  }

let initSerializers () =
  Json.Vanilla.allow<List<LibCliExecution.PackageManager.ProgramTypes.PackageType>>
    "PackageManager"
  Json.Vanilla.allow<List<LibCliExecution.PackageManager.ProgramTypes.PackageFn.PackageFn>>
    "PackageManager"
  Json.Vanilla.allow<List<LibCliExecution.PackageManager.ProgramTypes.PackageConstant>>
    "PackageManager"

[<EntryPoint>]
let main (args : string[]) =
  try
    initSerializers ()

    packageManager.init.Result

    let result = execute (Array.toList args)
    let result = result.Result

    NonBlockingConsole.wait ()

    match result with
    | Error(source, rte) ->
      let state = state ()
      let source =
        match source with
        | RT.SourceID(tlid, id) -> $"(source: {tlid}, {id})"
        | RT.SourceNone -> "(source unknown)"
      match (LibExecution.Execution.runtimeErrorToString state rte).Result with
      | Ok(RT.DString s) -> System.Console.WriteLine $"Error {source}:\n  {s}"
      | Ok otherVal ->
        System.Console.WriteLine
          $"Unexpected value while stringifying error {source}\n"
        System.Console.WriteLine $"Original Error: {rte}"
        System.Console.WriteLine $"Value is:\n{otherVal}"
      | Error(_, newErr) ->
        System.Console.WriteLine $"Error while stringifying error {source}\n"
        System.Console.WriteLine $"Original Error: {rte}"
        System.Console.WriteLine $"New Error is:\n{newErr}"
      1
    | Ok(RT.DInt i) -> (int i)
    | Ok dval ->
      let output = LibExecution.DvalReprDeveloper.toRepr dval
      System.Console.WriteLine
        $"Error: main function must return an int (returned {output})"
      1
  with e ->
    printException "Error starting Darklang CLI" [] e
    1
