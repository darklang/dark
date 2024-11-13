module Cli.Main

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module PackageIDs = LibExecution.PackageIDs
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

// TODO: de-dupe with _other_ Cli.fs
let pmBaseUrl =
  match
    System.Environment.GetEnvironmentVariable "DARK_CONFIG_PACKAGE_MANAGER_BASE_URL"
  with
  | null -> "https://packages.darklang.com"
  | var -> var
let packageManagerRT = LibPackageManager.PackageManager.rt pmBaseUrl
let packageManagerPT = LibPackageManager.PackageManager.pt pmBaseUrl


let builtins : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.builtins
        BuiltinExecution.Libs.HttpClient.defaultConfig
        packageManagerPT
      BuiltinCli.Builtin.builtins
      BuiltinCliHost.Builtin.builtins ]
    []



let state () =
  let program : RT.Program =
    { canvasID = System.Guid.NewGuid()
      internalFnsAllowed = false
      dbs = Map.empty
      secrets = [] }

  let notify (_state : RT.ExecutionState) (_msg : string) (_metadata : Metadata) =
    // let metadata = extraMetadata state @ metadata
    // LibService.Rollbar.notify msg metadata
    ()

  let sendException (_ : RT.ExecutionState) (metadata : Metadata) (exn : exn) =
    printException "Internal error" metadata exn

  Exe.createState
    builtins
    packageManagerRT
    Exe.noTracing
    sendException
    notify
    program




let execute (args : List<string>) : Task<RT.ExecutionResult> =
  task {
    let state = state ()
    let fnName = RT.FQFnName.fqPackage PackageIDs.Fn.Cli.executeCliCommand
    let args =
      args |> List.map RT.DString |> Dval.list RT.KTString |> NEList.singleton
    return! Exe.executeFunction state fnName [] args
  }

let initSerializers () =
  Json.Vanilla.allow<List<LibPackageManager.Types.ProgramTypes.PackageType.PackageType>>
    "PackageManager"
  Json.Vanilla.allow<List<LibPackageManager.Types.ProgramTypes.PackageFn.PackageFn>>
    "PackageManager"
  Json.Vanilla.allow<List<LibPackageManager.Types.ProgramTypes.PackageConstant.PackageConstant>>
    "PackageManager"
  ()

[<EntryPoint>]
let main (args : string[]) =
  try
    initSerializers ()

    packageManagerRT.init.Result

    let result = execute (Array.toList args)
    let result = result.Result

    NonBlockingConsole.wait ()

    match result with
    | Error(rte, callStack) ->
      let state = state ()

      let errorCallStackStr = LibExecution.Execution.callStackString state callStack

      match (LibExecution.Execution.runtimeErrorToString state rte).Result with
      | Ok(RT.DString s) ->
        System.Console.WriteLine $"Error source: {errorCallStackStr}\n  {s}"

      | Ok otherVal ->
        System.Console.WriteLine
          $"Unexpected value while stringifying error.\nCallStack: {errorCallStackStr}\n"
        System.Console.WriteLine $"Original Error: {rte}"
        System.Console.WriteLine $"Value is:\n{otherVal}"

      | Error(newErr) ->
        System.Console.WriteLine
          $"Error while stringifying error.\n CallStack: {errorCallStackStr}\n"
        System.Console.WriteLine $"Original Error: {rte}"
        System.Console.WriteLine $"New Error is:\n{newErr}"

      1
    | Ok(RT.DInt64 i) -> (int i)
    | Ok dval ->
      let output = DvalReprDeveloper.toRepr dval
      System.Console.WriteLine
        $"Error: main function must return an int (returned {output})"
      1

  with e ->
    printException "Error starting Darklang CLI" [] e
    1
