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
      do! LibCloud.PackageManager.purge ()

      print "Filling ..."
      do! LibCloud.PackageManager.savePackageTypes packagesFromDisk.types
      do! LibCloud.PackageManager.savePackageConstants packagesFromDisk.constants
      do! LibCloud.PackageManager.savePackageFunctions packagesFromDisk.fns

      return Ok()
    }


  let reloadDarkPackagesCanvas () : Ply<Result<unit, string>> =
    uply {
      // first, load the packages from disk, ensuring all parse well
      let! packagesFromDisk = LoadPackagesFromDisk.load Builtins.all
      let inMemPackageManager = inMemPackageManagerFromPackages packagesFromDisk

      // then, parse the canvas' `main.dark`, purge any previous data, and create the canvas
      let nameResolver =
        LibParser.NameResolver.fromBuiltins Builtins.accessibleByCanvas
        |> fun nr ->
          { nr with packageManager = Some inMemPackageManager; allowError = false }

      let! (canvasId, toplevels) = Canvas.loadFromDisk nameResolver "dark-packages"

      print $"Loaded canvas {canvasId} with {List.length toplevels} toplevels"


      let dbTlid =
        toplevels
        |> List.choose (fun tl ->
          //print $"Looking at toplevel {tl}"
          match tl with
          | PT.Toplevel.TLDB db -> Some db.tlid
          | _ -> None)
        |> List.tryHead
        |> Option.defaultWith (fun () ->
          Exception.raiseInternal
            "Surprisingly, no DB definiting db found in dark-packages/main.dark"
            [])

      let types = { RT.Types.empty with package = inMemPackageManager.getType }

      // finally, take of the packages we parsed from disk and load them into the canvas's DB
      do!
        DarkPackagesDataIngest.fillDarkPackagesCanvasWithData
          types
          canvasId
          dbTlid
          packagesFromDisk

      return Ok()
    }



let initSerializers () =
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageFn.T>>
    "Parse packageFn list"
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageType.T>>
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
