/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module StdLibCli = StdLibCli.StdLib


let builtIns : RT.BuiltIns =
  let (fns, types, constants) =
    LibExecution.StdLib.combine
      [ StdLibExecution.StdLib.contents
        StdLibCli.StdLib.contents
        StdLib.contents
        StdLibCliHost.StdLib.contents
        StdLibCloudExecution.StdLib.contents
        TestUtils.LibTest.contents ]
      []
      []
  { types = types |> Map.fromListBy (fun typ -> typ.name)
    fns = fns |> Map.fromListBy (fun fn -> fn.name)
    constants = constants |> Map.fromListBy (fun c -> c.name) }


let defaultTLID = 7UL

let execute
  (mod' : LibParser.Canvas.PTCanvasModule)
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
        constants =
          mod'.constants
          |> List.map (fun c -> PT2RT.UserConstant.toRT c)
          |> Map.fromListBy (fun c -> c.name)
        dbs = Map.empty
        secrets = [] }

    let tracing = Exe.noTracing RT.Real

    let extraMetadata (state : RT.ExecutionState) : Metadata =
      [ "executing_fn_name", state.executingFnName; "callstack", state.callstack ]

    let notify (state : RT.ExecutionState) (msg : string) (metadata : Metadata) =
      let metadata = extraMetadata state @ metadata
      let metadata =
        metadata |> List.map (fun (k, v) -> $"  {k}: {v}") |> String.concat ", "
      print $"Notification: {msg}, {metadata}"

    let reportException
      (state : RT.ExecutionState)
      (metadata : Metadata)
      (exn : exn)
      =
      let metadata = extraMetadata state @ metadata @ Exception.toMetadata exn
      let metadata =
        metadata |> List.map (fun (k, v) -> $"  {k}: {v}") |> String.concat "\n"
      print
        $"Exception: {exn.Message}\nMetadata:\n{metadata}\nStacktrace:\n{exn.StackTrace}"

    let state =
      Exe.createState
        builtIns
        (LibCloud.PackageManager.packageManager (System.TimeSpan.FromMinutes 1.))
        tracing
        reportException
        notify
        defaultTLID
        program
        config

    return! Exe.executeExpr state symtable (PT2RT.Expr.toRT mod'.exprs[0])
  }

let sourceOf
  (filename : string)
  (tlid : tlid)
  (id : id)
  (modul : LibParser.Canvas.PTCanvasModule)
  : string =
  let data =
    if tlid = defaultTLID then
      Some(filename, modul.exprs[0])
    else
      modul.fns
      |> List.find (fun fn -> fn.tlid = tlid)
      |> Option.map (fun fn -> string fn.name, fn.body)
  let mutable result = "unknown caller", "unknown body", "unknown expr"
  data
  |> Option.tap (fun (fnName, e) ->
    LibExecution.ProgramTypesAst.preTraversal
      (fun expr ->
        if PT.Expr.toID expr = id then result <- fnName, string e, string expr
        expr)
      (fun pipeExpr ->
        if PT.PipeExpr.toID pipeExpr = id then
          result <- fnName, string e, string pipeExpr
        pipeExpr)
      identity
      identity
      identity
      identity
      identity
      identity
      e
    |> ignore<PT.Expr>)
  let (fnName, body, expr) = result
  $"fn {fnName}\nexpr:\n{expr}\n, body:\n{body}"




let initSerializers () =
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageFn.T>>
    "Parse packageFn list"
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageType.T>>
    "Parse packageType list"


module PackageBootstrapping =
  open Npgsql.FSharp
  open Npgsql
  open LibCloud.Db

  let isNormalFile (path : string) : bool =
    try
      let attrs = System.IO.File.GetAttributes(path)
      let isDir = attrs.HasFlag(System.IO.FileAttributes.Directory)
      let exists = System.IO.File.Exists(path) || System.IO.Directory.Exists(path)
      exists && not isDir
    with e ->
      false

  let rec listDirectoryRecursive (dir : string) : List<string> =
    let contents = System.IO.Directory.EnumerateFileSystemEntries dir |> Seq.toList
    let (files, dirs) = contents |> List.partition (fun x -> isNormalFile x)
    let nested = dirs |> List.map (fun d -> listDirectoryRecursive d) |> List.flatten
    dirs |> List.append files |> List.append nested

  let listPackageFilesOnDisk (dir : string) : List<string> =
    dir
    |> listDirectoryRecursive
    |> List.filter (fun x -> x |> String.endsWith ".dark")

  let clearLocalPackageDb : Ply<unit> =
    uply {
      debuG "clearing package stuff from DB" ()
      do! Sql.query "DELETE FROM package_types_v0" |> Sql.executeStatementAsync
      do! Sql.query "DELETE FROM package_functions_v0" |> Sql.executeStatementAsync
      do! Sql.query "DELETE FROM package_constants_v0" |> Sql.executeStatementAsync
    }

  let parseAndSave
    (nameResolver : LibParser.NameResolver.NameResolver)
    (path : string)
    (contents : string)
    : Ply<unit> =
    uply {
      let (fns, types, constants) =
        LibParser.Parser.parsePackage nameResolver path contents

      do! LibCloud.PackageManager.savePackageFunctions fns
      do! LibCloud.PackageManager.savePackageTypes types
      do! LibCloud.PackageManager.savePackageConstants constants

      return ()
    }

  let loadPackageFile
    (nameResolver : LibParser.NameResolver.NameResolver)
    (filename : string)
    : Ply<unit> =
    filename |> System.IO.File.ReadAllText |> parseAndSave nameResolver filename

  type PackageSourceFile = { name : string; sourceCode : string }

  let loadPackagesFromDb
    (nameResolver : LibParser.NameResolver.NameResolver)
    : Ply<int> =
    uply {
      do! clearLocalPackageDb

      // no packages are in the DB yet, so many names are expected to not resolve
      let nameResolver = { nameResolver with allowError = true }

      let files = listPackageFilesOnDisk "/home/dark/app/packages"

      // first, load all the packages, allowing unresolved names
      // (other package items won't be available yet)
      do!
        files
        |> Ply.List.iterSequentially (fun f ->
          print $"Loading {f}"
          loadPackageFile nameResolver f)

      let packageManager =
        LibCloud.PackageManager.packageManager (System.TimeSpan.FromMinutes 0.)
      let! nameResolver =
        { nameResolver with allowError = false }
        |> LibParser.NameResolver.withUpdatedPackages packageManager


      // then, load all the packages again, not allowing unresolved names
      do!
        files
        |> Ply.List.iterSequentially (fun f ->
          print $"Loading {f}"
          loadPackageFile nameResolver f)

      return 0
    }

[<EntryPoint>]
let main (args : string[]) : int =
  let name = "LocalExec"
  try
    initSerializers ()

    LibService.Init.init name
    LibService.Telemetry.Console.loadTelemetry
      name
      LibService.Telemetry.DontTraceDBQueries

    (LibCloud.Init.init LibCloud.Init.WaitForDB name).Result


    let nameResolver =
      // TODO: this may need more builtins, and packages
      LibParser.NameResolver.fromBuiltins (
        Map.values builtIns.fns |> Seq.toList,
        Map.values builtIns.types |> Seq.toList,
        Map.values builtIns.constants |> Seq.toList
      )

    match args with
    | [| "load-packages" |] ->
      System.Console.WriteLine "Loading packages to DB"
      let exitCode =
        PackageBootstrapping.loadPackagesFromDb nameResolver
        |> Ply.toTask
        |> Async.AwaitTask
        |> Async.RunSynchronously
      System.Console.WriteLine "Finished loading packages to DB"
      exitCode
    | _ ->
      let mainFile = "/home/dark/app/backend/src/LocalExec/local-exec.dark"
      let modul = LibParser.Canvas.parseFromFile nameResolver mainFile

      let args = args |> Array.toList |> List.map RT.DString |> RT.DList

      let result = execute modul (Map [ "args", args ])

      NonBlockingConsole.wait ()

      match result.Result with
      | RT.DError(RT.SourceID(tlid, id), msg) ->
        // TODO: execute the Package LocalExec.Errors.toSegments
        System.Console.WriteLine $"Error: {msg}"
        System.Console.WriteLine $"Failure at: {sourceOf mainFile tlid id modul}"
        // System.Console.WriteLine $"module is: {modul}"
        // System.Console.WriteLine $"(source {tlid}, {id})"
        1
      | RT.DError(RT.SourceNone, msg) ->
        System.Console.WriteLine $"Error: {msg}"
        System.Console.WriteLine $"(source unknown)"
        1
      | RT.DInt i -> (int i)
      | dval ->
        let output = LibExecution.DvalReprDeveloper.toRepr dval
        System.Console.WriteLine
          $"Error: main function must return an int, not {output}"
        1
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    // LibService.Init.shutdown name
    NonBlockingConsole.wait ()
    1
