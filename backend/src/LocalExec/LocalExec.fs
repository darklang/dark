/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module BuiltinCli = BuiltinCli.Builtin


let builtIns : RT.BuiltIns =
  let (fns, types, constants) =
    LibExecution.Builtin.combine
      [ BuiltinExecution.Builtin.contents
          BuiltinExecution.Libs.HttpClient.defaultConfig
        BuiltinCli.Builtin.contents
        Builtin.contents
        BuiltinCliHost.Builtin.contents
        BuiltinCloudExecution.Builtin.contents
        TestUtils.LibTest.contents ]
      []
  { types = types |> Map.fromListBy _.name
    fns = fns |> Map.fromListBy _.name
    constants = constants |> Map.fromListBy _.name }


let defaultTLID = 4989026UL

let state () =
  let program : RT.Program =
    { canvasID = System.Guid.NewGuid()
      internalFnsAllowed = false
      fns = Map.empty
      types = Map.empty
      constants = Map.empty
      dbs = Map.empty
      secrets = [] }

  let extraMetadata (_state : RT.ExecutionState) : Metadata = []

  let notify (state : RT.ExecutionState) (msg : string) (metadata : Metadata) =
    let metadata = extraMetadata state @ metadata
    let metadata =
      metadata |> List.map (fun (k, v) -> $"  {k}: {v}") |> String.concat ", "
    print $"Notification: {msg}, {metadata}"

  let reportException (state : RT.ExecutionState) (metadata : Metadata) (exn : exn) =
    let metadata = extraMetadata state @ metadata @ Exception.toMetadata exn
    let metadata =
      metadata |> List.map (fun (k, v) -> $"  {k}: {v}") |> String.concat "\n"
    print
      $"Exception: {exn.Message}\nMetadata:\n{metadata}\nStacktrace:\n{exn.StackTrace}"

  Exe.createState
    builtIns
    LibCloud.PackageManager.packageManager
    Exe.noTracing
    reportException
    notify
    program



let execute
  (mod' : LibParser.Canvas.PTCanvasModule)
  (symtable : Map<string, RT.Dval>)
  : Ply<RT.ExecutionResult> =
  uply {
    let program : RT.Program =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        fns = mod'.fns |> List.map PT2RT.UserFunction.toRT |> Map.fromListBy _.name
        types = mod'.types |> List.map PT2RT.UserType.toRT |> Map.fromListBy _.name
        constants =
          mod'.constants |> List.map PT2RT.UserConstant.toRT |> Map.fromListBy _.name
        dbs = Map.empty
        secrets = [] }

    let state = { state () with program = program }
    let expr = PT2RT.Expr.toRT mod'.exprs[0]
    return! Exe.executeExpr state 77777723478932UL symtable expr
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
  match data with
  | None -> ()
  | Some(fnName, e) ->
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
    |> ignore<PT.Expr>
  let (fnName, body, expr) = result
  $"fn {fnName}\nexpr:\n{expr}\n, body:\n{body}"



let initSerializers () =
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageFn.T>>
    "Parse packageFn list"
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageType.T>>
    "Parse packageType list"


module PackageBootstrapping =
  open Npgsql
  open Npgsql.FSharp
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

  let clearLocalPackageDb () : Ply<unit> =
    uply {
      do! Sql.query "DELETE FROM package_types_v0" |> Sql.executeStatementAsync
      do! Sql.query "DELETE FROM package_functions_v0" |> Sql.executeStatementAsync
      do! Sql.query "DELETE FROM package_constants_v0" |> Sql.executeStatementAsync
    }

  let savePackagesToLocalDb (packages : LibParser.Parser.Packages) : Ply<unit> =
    uply {
      let (fns, types, constants) = packages

      do! LibCloud.PackageManager.savePackageFunctions fns
      do! LibCloud.PackageManager.savePackageTypes types
      do! LibCloud.PackageManager.savePackageConstants constants
    }


  let flattenParsedPackages
    (packages : List<LibParser.Parser.Packages>)
    : LibParser.Parser.Packages =
    packages
    |> List.unzip3
    |> (fun (fns, types, constants) ->
      (List.concat fns, List.concat types, List.concat constants))

  type PackageSourceFile = { name : string; sourceCode : string }

  let loadPackagesFromDb () : Ply<int> =
    uply {
      // 1. clear the local DB of any packages
      do! clearLocalPackageDb ()

      let filesWithContents =
        listPackageFilesOnDisk "/home/dark/app/packages"
        |> List.map (fun fileName -> (fileName, System.IO.File.ReadAllText fileName))

      // 2. parse all the packages, allowing unresolved names, and save
      // (other package items won't be available yet)
      let nameResolver =
        LibParser.NameResolver.fromBuiltins (
          Map.values builtIns.fns |> Seq.toList,
          Map.values builtIns.types |> Seq.toList,
          Map.values builtIns.constants |> Seq.toList
        )
        |> fun nr -> { nr with allowError = true }

      let! (packagesParsedWithUnresolvedNamesAllowed : LibParser.Parser.Packages) =
        filesWithContents
        |> Ply.List.mapSequentially (fun (path, contents) ->
          print $"Parsing {path}, allowing unresolved names"
          LibParser.Parser.parsePackageFile nameResolver path contents)
        |> Ply.map flattenParsedPackages

      // 3. re-parse the packages, and save
      // this time, though, we don't allow unresolved names
      // (any package references that may have been unresolved a second ago should now be OK)
      let (fns, types, consts) = packagesParsedWithUnresolvedNamesAllowed

      let! (inMemPackageManager : RT.PackageManager) =
        uply {
          let types = types |> List.map PT2RT.PackageType.toRT
          let fns = fns |> List.map PT2RT.PackageFn.toRT
          let consts = consts |> List.map PT2RT.PackageConstant.toRT

          let pm : RT.PackageManager =
            { getType =
                fun name -> types |> List.find (fun t -> t.name = name) |> Ply
              getFn = fun name -> fns |> List.find (fun f -> f.name = name) |> Ply
              getFnByTLID =
                fun tlid -> fns |> List.find (fun f -> f.tlid = tlid) |> Ply
              getConstant =
                fun name -> consts |> List.find (fun c -> c.name = name) |> Ply

              init = uply { return () } }

          return pm
        }

      let nameResolver =
        { nameResolver with
            allowError = false
            packageManager = Some inMemPackageManager }

      let! (packagesParsedWithUnresolvedNamesNotAllowed : LibParser.Parser.Packages) =
        filesWithContents
        |> Ply.List.mapSequentially (fun (path, contents) ->
          print $"Parsing {path}, not allowing unresolved names"
          LibParser.Parser.parsePackageFile nameResolver path contents)
        |> Ply.map flattenParsedPackages

      do! clearLocalPackageDb ()
      do! savePackagesToLocalDb packagesParsedWithUnresolvedNamesNotAllowed

      return 0
    }

let runLocalExecScript (args : string[]) : Ply<int> =
  uply {
    let nameResolver =
      // TODO: this may need more builtins, and packages
      LibParser.NameResolver.fromBuiltins (
        Map.values builtIns.fns |> Seq.toList,
        Map.values builtIns.types |> Seq.toList,
        Map.values builtIns.constants |> Seq.toList
      )

    let mainFile = "/home/dark/app/backend/src/LocalExec/local-exec.dark"
    let nameResolver =
      { nameResolver with
          allowError = false
          packageManager = Some LibCloud.PackageManager.packageManager }
    let! modul = LibParser.Canvas.parseFromFile nameResolver mainFile

    let args = args |> Array.toList |> List.map RT.DString |> Dval.list RT.KTString

    let result = execute modul (Map [ "args", args ])

    NonBlockingConsole.wait ()

    match result.Result with
    | Error(source, rte) ->
      let state = state ()
      let source =
        match source with
        | Some(tlid, id) -> sourceOf mainFile tlid id modul
        | None -> "unknown"
      match! LibExecution.Execution.runtimeErrorToString state rte with
      | Ok(RT.DString s) ->
        System.Console.WriteLine $"Error: {s}"
        System.Console.WriteLine source
      | Ok unexpected ->
        System.Console.WriteLine $"Unexpected value while stringifying error\n"
        System.Console.WriteLine $"Original Error: {rte}"
        System.Console.WriteLine $"New Error is:\n{unexpected}"
      | Error(_, newErr) ->
        System.Console.WriteLine $"Error while stringifying error\n"
        System.Console.WriteLine $"Original Error: {rte}"
        System.Console.WriteLine $"New Error is:\n{newErr}"
      System.Console.WriteLine source
      // System.Console.WriteLine $"module is: {modul}"
      return 1
    | Ok(RT.DInt64 i) -> return (int i)
    | Ok dval ->
      let output = LibExecution.DvalReprDeveloper.toRepr dval
      System.Console.WriteLine
        $"Error: main function must return an int, not {output}"
      return 1
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

    (LibCloud.Init.waitForDB ()).Result

    match args with
    | [| "load-packages" |] ->
      print "Loading packages to DB"
      let exitCode = (PackageBootstrapping.loadPackagesFromDb ()).Result
      print "Finished loading packages to DB"
      NonBlockingConsole.wait ()
      exitCode
    | _ ->
      let result = (runLocalExecScript args).Result
      NonBlockingConsole.wait ()
      result
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    // LibService.Init.shutdown name
    NonBlockingConsole.wait ()
    1
