module Cli.Main

open System
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
module BuiltinCliHostConfig = BuiltinCliHost.Libs.Cli.Config

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

let builtins : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinCliHostConfig.builtinsToUse
      LibExecution.Builtin.combine [ BuiltinCliHost.Builtin.builtins ] [] ]
    []



let state () =
  let program : RT.Program =
    { canvasID = System.Guid.NewGuid()
      internalFnsAllowed = false
      dbs = Map.empty
      secrets = [] }

  let notify
    (_state : RT.ExecutionState)
    (_vm : RT.VMState)
    (_msg : string)
    (_metadata : Metadata)
    =
    // let metadata = extraMetadata state @ metadata
    // LibService.Rollbar.notify msg metadata
    uply { return () }

  let sendException
    (_ : RT.ExecutionState)
    (_ : RT.VMState)
    (metadata : Metadata)
    (exn : exn)
    =
    uply { printException "Internal error" metadata exn }

  Exe.createState
    builtins
    BuiltinCliHostConfig.packageManagerRT
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

    BuiltinCliHostConfig.packageManagerRT.init.Result

    let result = execute (Array.toList args)
    let result = result.Result

    NonBlockingConsole.wait ()

    match result with
    | Error(rte, callStack) ->
      let state = state ()

      let errorCallStackStr =
        (LibExecution.Execution.callStackString state callStack).Result

      match (LibExecution.Execution.runtimeErrorToString state rte).Result with
      | Ok(RT.DString s) ->
        Console.WriteLine
          $"Encountered a Runtime Error:\n{s}\n\n{errorCallStackStr}\n  "

      | Ok otherVal ->
        Console.WriteLine
          $"Encountered a Runtime Error, stringified it, but somehow a non-string was returned.\n"
        Console.WriteLine $"Runtime Error: {rte}"
        Console.WriteLine $"'Stringified':\n{otherVal}"
        Console.WriteLine $"{errorCallStackStr}"

      | Error(newErr) ->
        Console.WriteLine
          $"Encountered a Runtime Error, tried to stringify it, and then _that_ failed."
        Console.WriteLine $"Original Error: {rte}"
        Console.WriteLine $"{errorCallStackStr}"
        Console.WriteLine $"\nError encountered when trying to stringify:\n{newErr}"

      1
    | Ok(RT.DInt64 i) -> (int i)
    | Ok dval ->
      let output = DvalReprDeveloper.toRepr dval
      Console.WriteLine
        $"Error: main function must return an int (returned {output})"
      1

  with e ->
    printException "Error starting Darklang CLI" [] e
    1
