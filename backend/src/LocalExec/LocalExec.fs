/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

module PM = LibPackageManager.PackageManager

open Utils

module HandleCommand =
  let reloadDarkPackagesCanvas () : Ply<Result<unit, string>> =
    uply {
      let! (canvasId, toplevels) =
        Canvas.loadFromDisk LibPackageManager.PackageManager.pt "dark-packages"

      print $"Loaded canvas {canvasId} with {List.length toplevels} toplevels"

      return Ok()
    }

  let reloadPackages () : Ply<Result<unit, string>> =
    uply {
      // first, load the packages from disk, ensuring all parse well
      let! ops = LoadPackagesFromDisk.load Builtins.all

      let typeCount =
        ops
        |> List.filter (function
          | PT.PackageOp.AddType _ -> true
          | _ -> false)
        |> List.length
      let valueCount =
        ops
        |> List.filter (function
          | PT.PackageOp.AddValue _ -> true
          | _ -> false)
        |> List.length
      let fnCount =
        ops
        |> List.filter (function
          | PT.PackageOp.AddFn _ -> true
          | _ -> false)
        |> List.length

      print "Loaded packages from disk "
      print $"{typeCount} types, {valueCount} values, and {fnCount} fns"

      // TODO: Check for duplicates - need to extract locations from SetName ops
      // For now, skip duplicate checking

      print "Purging ..."
      do! LibPackageManager.Purge.purge ()

      print "Filling ..."
      let! _ = LibPackageManager.Inserts.insertOps None ops

      // print "Populating RT columns..."
      // do! PM.populateRTColumns ()

      //do! PM.flushCheckpoint ()

      // Reload dark-packages canvas after package reload
      print "Reloading dark-packages canvas..."
      let! _ = reloadDarkPackagesCanvas ()

      return Ok()
    }

  let reloadPackagesCanvas () : Ply<Result<unit, string>> =
    uply {
      print "Reloading dark-packages canvas..."
      let! _ = reloadDarkPackagesCanvas ()

      return Ok()
    }

  let runMigrations () : Ply<Result<unit, string>> =
    uply {
      try
        print "Running migrations"
        Migrations.run ()
        print "Migrations completed successfully."
        return Ok()
      with ex ->
        return Error $"Migration failed: {ex.Message}"
    }

  let listMigrations () : Ply<Result<unit, string>> =
    uply {
      try
        print "Migrations needed:\n"
        Migrations.migrationsToRun () |> List.iter (fun name -> print $" - {name}")
        return Ok()
      with ex ->
        return Error $"Failed to list migrations: {ex.Message}"
    }

let initSerializers () =
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageFn.PackageFn>>
    "Parse packageFn list"
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageType.PackageType>>
    "Parse packageType list"
  Json.Vanilla.allow<LibService.Rollbar.HoneycombJson>
    "Allow Rollbar HoneycombJson serialization"



[<EntryPoint>]
let main (args : string[]) : int =
  let name = "LocalExec"
  try
    initSerializers ()

    // Use minimal telemetry for CLI tools - enable telemetry but disable Rollbar
    LibService.Init.init name
    LibService.Telemetry.Console.loadTelemetry
      name
      LibService.Telemetry.DontTraceDBQueries

    //let _ = (LibCloud.Init.init name).Result

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
    | [ "reload-packages" ] ->
      handleCommand
        "reading, parsing packages from `packages` directory, and saving to internal SQL tables"
        (HandleCommand.reloadPackages ())

    | [ "reload-packages-canvas" ] ->
      handleCommand "TODO" (HandleCommand.reloadPackagesCanvas ())


    | [ "migrations"; "run" ] ->
      handleCommand
        "deleting database and running all migrations"
        (HandleCommand.runMigrations ())

    | [ "migrations"; "list" ] ->
      handleCommand "listing available migrations" (HandleCommand.listMigrations ())

    | [ "reload-dark-packages-canvas" ] ->
      handleCommand
        "loading dark-packages canvas from disk"
        (HandleCommand.reloadDarkPackagesCanvas ())

    | _ ->
      print "Invalid arguments"
      print "Available commands:"
      print "  reload-packages"
      print "  reload-dark-packages-canvas"
      print "  migrations run"
      print "  migrations list"
      NonBlockingConsole.wait ()
      1
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    LibService.Init.shutdown name
    NonBlockingConsole.wait ()
    1
