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

// Dual logging (console + cli.log file)
let private logError (message : string) : unit =
  // Always write to stderr for immediate feedback
  System.Console.Error.WriteLine message

  // Also try to log to file (best effort - don't fail if we can't)
  try
    let logPath = System.IO.Path.Combine(LibConfig.Config.logDir, "cli.log")
    let logDir = System.IO.Path.GetDirectoryName(logPath)
    if not (IO.Directory.Exists logDir) then
      System.IO.Directory.CreateDirectory logDir |> ignore<IO.DirectoryInfo>

    let timestamp = System.DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss"
    let logEntry = $"[{timestamp}] {message}\n"
    System.IO.File.AppendAllText(logPath, logEntry)
  with _ ->
    () // Silently ignore logging errors - don't make things worse

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
      BuiltinCliHost.Builtin.builtins
      BuiltinCli.builtins ]
    []



let state (packageManager : RT.PackageManager) =
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

  Exe.createState builtins packageManager Exe.noTracing sendException notify program




let execute
  (packageManager : RT.PackageManager)
  (args : List<string>)
  : Task<RT.ExecutionResult> =
  task {
    let state = state packageManager
    let fnName = RT.FQFnName.fqPackage PackageIDs.Fn.Cli.executeCliCommand
    let args =
      args |> List.map RT.DString |> Dval.list RT.KTString |> NEList.singleton
    let! result = Exe.executeFunction state fnName [] args
    return result
  }

let initSerializers () = ()

[<EntryPoint>]
let main (args : string[]) =
  try
    EmbeddedResources.extract ()
    initSerializers ()

    let cliPackageManager = LibPackageManager.PackageManager.rt
    cliPackageManager.init.Result

    let result = execute cliPackageManager (Array.toList args)
    let result = result.Result

    NonBlockingConsole.wait ()

    match result with
    | Error(rte, callStack) ->
      let state = state cliPackageManager

      let errorCallStackStr =
        (LibExecution.Execution.callStackString state callStack).Result

      match (LibExecution.Execution.runtimeErrorToString state rte).Result with
      | Ok(RT.DString s) ->
        logError $"Encountered a Runtime Error:\n{s}\n\n{errorCallStackStr}\n  "

      | Ok otherVal ->
        logError
          $"Encountered a Runtime Error, stringified it, but somehow a non-string was returned.\nRuntime Error: {rte}\n'Stringified':\n{otherVal}\n{errorCallStackStr}"

      | Error newErr ->
        logError
          $"Encountered a Runtime Error, tried to stringify it, and then _that_ failed.\nOriginal Error: {rte}\n{errorCallStackStr}\n\nError encountered when trying to stringify:\n{newErr}"

      1
    | Ok(RT.DInt64 i) -> (int i)
    | Ok dval ->
      let output = DvalReprDeveloper.toRepr dval
      logError $"Error: main function must return an int (returned {output})"
      1


  with e ->
    System.Console.Error.WriteLine
      $"Error starting Darklang CLI: {e.Message}\nStack trace:\n{e.StackTrace}"
    1
