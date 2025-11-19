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

  let reloadCanvas (name : string) : Ply<Result<unit, string>> =
    uply {
      print $"Reloading {name} canvas..."

      let! (canvasId, toplevels) = Canvas.loadFromDisk PM.pt name

      print $"Loaded canvas {canvasId} with {List.length toplevels} toplevels"

      return Ok()
    }

  let reloadCanvases () : Ply<Result<unit, string>> =
    uply {
      // CLEANUP fetch the list of canvases by 'ls canvases' equiv.
      // CLEANUP stop tossing the result
      let! _ = reloadCanvas "dark-packages"
      return Ok()
    }

  let reloadPackages () : Ply<Result<unit, string>> =
    uply {
      // first, load the packages from disk, ensuring all parse well
      let! ops = LoadPackagesFromDisk.load Builtins.all

      // CLEANUP consider checking for duplicates (helps prevent a class of issues)

      print "Purging ..."
      do! LibPackageManager.Purge.purge ()

      print "Filling ..."
      do! PM.pt.applyOps (None, ops)

      // Get stats after ops are inserted/applied
      let! stats = LibPackageManager.PT.SQL.Stats.get ()
      print "Loaded packages from disk "
      print $"{stats.types} types, {stats.values} values, and {stats.fns} fns"

      // Reload dark-packages canvas after package reload
      print "Reloading dark-packages canvas..."
      let! _ = reloadCanvas "dark-packages"

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
        "Reload packages from `packages` directory"
        (HandleCommand.reloadPackages ())

    | [ "reload-canvases" ] ->
      handleCommand
        "Reload canvases from 'canvases' directory"
        (HandleCommand.reloadCanvases ())


    | [ "migrations"; "run" ] ->
      handleCommand
        "deleting database and running all migrations"
        (HandleCommand.runMigrations ())

    | [ "migrations"; "list" ] ->
      handleCommand "listing available migrations" (HandleCommand.listMigrations ())

    | _ ->
      print "Invalid arguments"
      print "Available commands:"
      print "  reload-packages"
      print "  reload-canvases"
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
