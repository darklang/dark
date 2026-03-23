/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Db

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution
module BS = LibSerialization.Binary.Serialization

module PM = LibPackageManager.PackageManager

open Utils


let evaluateAllValues = LibPackageManager.Seed.evaluateAllValues


module HandleCommand =

  let reloadCanvas (name : string) : Ply<Result<unit, string>> =
    uply {
      print $"Reloading {name} canvas..."

      let! (canvasId, toplevels) =
        Canvas.loadFromDisk LibPackageManager.PackageManager.pt name

      print $"Loaded canvas {canvasId} with {List.length toplevels} toplevels"

      return Ok()
    }

  let reloadCanvases () : Ply<Result<unit, string>> =
    uply {
      // CLEANUP fetch the list of canvases by 'ls canvases' equiv.
      // CLEANUP stop tossing the result
      let! _ = reloadCanvas "dark-packages"
      let! _ = reloadCanvas "dark-editor"
      return Ok()
    }

  let reloadPackages () : Ply<Result<unit, string>> =
    uply {
      // Load packages from disk, ensuring all parse well
      let! ops = LoadPackagesFromDisk.load (Builtins.all ())

      // CLEANUP consider checking for duplicates (helps prevent a class of issues)

      print "Purging ..."
      do! LibPackageManager.Purge.purge ()

      // Record CreateBranch op for main (the migration creates the row,
      // but we need the BranchOp so the DB can be rebuilt from ops alone)
      do!
        LibPackageManager.BranchOpPlayback.insertAndApply (
          PT.BranchOp.CreateBranch(PT.mainBranchId, "main", None, None)
        )

      print "Filling ..."
      // Create an "init" commit with all packages from disk
      // Note: values are stored with NULL rt_dval at this point
      // Darklang system account: 00000000-0000-0000-0000-000000000001
      let darklangAccountId =
        System.Guid.Parse "00000000-0000-0000-0000-000000000001"
      let! commitHash =
        LibPackageManager.Inserts.insertAndApplyOpsWithCommit
          darklangAccountId
          LibExecution.ProgramTypes.mainBranchId
          "Init: packages loaded from disk"
          ops

      // Generate hash file BEFORE evaluating values, so that PackageRefs
      // lookups resolve correctly during value evaluation.
      do! LibPackageManager.PackageRefsGenerator.generate ()
      LibExecution.PackageRefs.reloadHashes ()

      // Evaluate all values now that all definitions are in the DB
      let! evalResult = evaluateAllValues (Builtins.all ()) PM.rt
      match evalResult with
      | Error errors ->
        for e in errors do
          print $"  Value evaluation error: {e}"
        return Error "Some values failed to evaluate"
      | Ok() ->
        // Get stats after ops are inserted/applied
        let! stats = LibPackageManager.Stats.get ()
        print "Loaded packages from disk "
        print $"{stats.types} types, {stats.values} values, and {stats.fns} fns"
        let (Hash commitHashStr) = commitHash
        let shortHash = commitHashStr[..6]
        print $"Created init commit {shortHash}"

        // Reload dark-packages and dark-editor canvases after package reload
        print "Reloading dark-packages canvas..."
        let! _ = reloadCanvas "dark-packages"
        let! _ = reloadCanvas "dark-editor"

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

  let exportSeed (outputPath : string) : Ply<Result<unit, string>> =
    uply {
      try
        do! LibPackageManager.Seed.export outputPath
        let size = System.IO.FileInfo(outputPath).Length / 1024L / 1024L
        print $"Seed exported to {outputPath} ({size} MB)"
        return Ok()
      with ex ->
        return Error $"Export failed: {ex.Message}"
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



[<EntryPoint>]
let main (args : string[]) : int =
  let name = "LocalExec"
  try
    initSerializers ()

    LibService.Init.init name

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

    | [ "export-seed"; outputPath ] ->
      handleCommand
        $"Exporting seed to {outputPath}"
        (HandleCommand.exportSeed outputPath)

    | _ ->
      print "Invalid arguments"
      print "Available commands:"
      print "  reload-packages"
      print "  reload-canvases"
      print "  migrations run"
      print "  migrations list"
      print "  export-seed <output-path>"
      NonBlockingConsole.wait ()
      1
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    LibService.Init.shutdown name
    NonBlockingConsole.wait ()
    1
