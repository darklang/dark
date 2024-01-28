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
  let (fns, constants) =
    LibExecution.Builtin.combine
      [ BuiltinExecution.Builtin.contents
          BuiltinExecution.Libs.HttpClient.defaultConfig
        BuiltinCli.Builtin.contents
        BuiltinCliHost.Builtin.contents ]
      []
  { fns = fns |> Map.fromListBy _.name
    constants = constants |> Map.fromListBy _.name }

// TODO: de-dupe with _other_ Cli.fs
let packageManagerBaseUrl =
  match
    System.Environment.GetEnvironmentVariable "DARK_CONFIG_PACKAGE_MANAGER_BASE_URL"
  with
  | null -> "https://packages.darklang.com"
  | var -> var

let packageManager =
  LibPackageManager.PackageManager.packageManager packageManagerBaseUrl


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

  Exe.createState builtIns packageManager tracing sendException notify program




let execute
  (args : List<string>)
  : Task<Result<RT.Dval, RT.Source * RT.RuntimeError>> =
  task {
    let state = state ()
    let fnName = RT.FQFnName.fqPackage "Darklang" [ "Cli" ] "executeCliCommand" 0
    let args =
      args |> List.map RT.DString |> Dval.list RT.KTString |> NEList.singleton
    return! Exe.executeFunction state None fnName [] args
  }

let initSerializers () =
  // Json.Vanilla.allow<List<LibCliExecution.PackageManager.ProgramTypes.PackageType>>
  //   "PackageManager"
  // Json.Vanilla.allow<List<LibCliExecution.PackageManager.ProgramTypes.PackageFn.PackageFn>>
  //   "PackageManager"
  // Json.Vanilla.allow<List<LibCliExecution.PackageManager.ProgramTypes.PackageConstant>>
  //   "PackageManager"
  ()

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

      // Ideally, this would be done in Dark rather than F#
      //
      // TODO: pretty-print the source the expr.
      let errorSourceStr =
        match source with
        | Some(tlid, id) ->
          let foundProgramTL =
            state.program.fns.Values |> Seq.tryFind (fun fn -> fn.tlid = tlid)

          let foundPackageTL =
            state.packageManager.getFnByTLID tlid
            // TODO don't do this hacky stuff
            |> Ply.toTask
            |> Async.AwaitTask
            |> Async.RunSynchronously

          match foundProgramTL, foundPackageTL with
          | Some programFn, _ ->
            $"user fn {RT.FQFnName.userProgramToString programFn.name}, expr {id}"

          | None, Some packageFn ->
            $"package fn {RT.FQFnName.packageToString packageFn.name}, expr {id}"

          | None, None -> $"tlid {tlid}, expr {id}"

        | None -> "(unknown)"


      match (LibExecution.Execution.runtimeErrorToString state rte).Result with
      | Ok(RT.DString s) ->
        System.Console.WriteLine $"Error source: {errorSourceStr}\n  {s}"

      | Ok otherVal ->
        System.Console.WriteLine
          $"Unexpected value while stringifying error.\nSource: {errorSourceStr}\n"
        System.Console.WriteLine $"Original Error: {rte}"
        System.Console.WriteLine $"Value is:\n{otherVal}"

      | Error(_, newErr) ->
        System.Console.WriteLine
          $"Error while stringifying error.\n Source: {errorSourceStr}\n"
        System.Console.WriteLine $"Original Error: {rte}"
        System.Console.WriteLine $"New Error is:\n{newErr}"

      1
    | Ok(RT.DInt64 i) -> (int i)
    | Ok dval ->
      let output = LibExecution.DvalReprDeveloper.toRepr dval
      System.Console.WriteLine
        $"Error: main function must return an int (returned {output})"
      1
  with e ->
    printException "Error starting Darklang CLI" [] e
    1
