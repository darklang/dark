/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open Utils

module HandleCommand =
  let loadPackagesToInternalSqlTables () : Ply<Result<unit, string>> =
    uply {
      // first, load the packages from disk, ensuring all parse well
      let! packagesFromDisk = LoadPackagesFromDisk.load Builtins.all

      let typeLen = packagesFromDisk.types |> List.length
      let constantLen = packagesFromDisk.constants |> List.length
      let fnLen = packagesFromDisk.fns |> List.length

      print "Loaded packages from disk "
      print $"{typeLen} types, {constantLen} constants, and {fnLen} fns"

      print "Purging ..."
      do! LibCloud.PackageManager.purgeSqlite ()

      print "Filling ..."
      do! LibCloud.PackageManager.savePackageTypesSqlite packagesFromDisk.types
      do!
        LibCloud.PackageManager.savePackageConstantsSqlite packagesFromDisk.constants
      do! LibCloud.PackageManager.savePackageFunctionsSqlite packagesFromDisk.fns

      return Ok()
    }


  let reloadDarkPackagesCanvas () : Ply<Result<unit, string>> =
    uply {
      let! (canvasId, toplevels) =
        Canvas.loadFromDisk LibCloud.PackageManager.pt "dark-packages"

      print $"Loaded canvas {canvasId} with {List.length toplevels} toplevels"

      return Ok()
    }



let initSerializers () =
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageFn.PackageFn>>
    "Parse packageFn list"
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageType.PackageType>>
    "Parse packageType list"



[<EntryPoint>]
let main (args : string[]) : int =
  let name = "LocalExec"
  try
    initSerializers ()

    LibService.Init.init name
    LibService.Telemetry.Console.loadTelemetry
      name
      LibService.Telemetry.DontTraceDBQueries

    let _ = (LibCloud.Init.waitForDB LibCloud.Init.WaitForDB).Result

    let handleCommand
      (description : string)
      (command : Ply<Result<unit, string>>)
      : int =
      print $"Starting: {description}"
      match command.Result with
      | Ok() ->
        print $"Finished {description}"
        NonBlockingConsole.wait ()
        0
      | Error e ->
        print $"Error {description}:\n{e}"
        NonBlockingConsole.wait ()
        1

    match Array.toList args with
    | [ "load-packages-to-internal-sql-tables" ] ->
      handleCommand
        "reading, parsing packages from `packages` directory, and saving to internal SQL tables"
        (HandleCommand.loadPackagesToInternalSqlTables ())

    | [ "reload-dark-packages" ] ->
      handleCommand
        $"purging, re-creating, and seeding `dark-packages` canvas"
        (HandleCommand.reloadDarkPackagesCanvas ())

    | _ ->
      print "Invalid arguments"
      NonBlockingConsole.wait ()
      1
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    // LibService.Init.shutdown name
    NonBlockingConsole.wait ()
    1
