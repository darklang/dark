/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution
module BS = LibSerialization.Binary.Serialization

module PM = LibDB.PackageManager

open Utils


let evaluateAllValues = LibDB.Seed.evaluateAllValues


module HandleCommand =

  let reloadPackages () : Ply<Result<unit, string>> =
    uply {
      // Load packages from disk, ensuring all parse well
      let! ops = LoadPackagesFromDisk.load (Builtins.all ())

      // CLEANUP consider checking for duplicates (helps prevent a class of issues)

      print "Purging ..."
      do! LibDB.Purge.purge ()

      // Record CreateBranch op for main (the migration creates the row,
      // but we need the BranchOp so the DB can be rebuilt from ops alone)
      do!
        LibDB.BranchOpPlayback.insertAndApply (
          PT.BranchOp.CreateBranch(PT.mainBranchId, "main", None, None)
        )

      print "Filling ..."
      // Create an "init" commit with all packages from disk
      // Note: values are stored with NULL rt_dval at this point
      let! commitHash =
        LibDB.Inserts.insertAndApplyOpsWithCommit
          (LibCloud.Account.resolve ())
          LibExecution.ProgramTypes.mainBranchId
          "Init: packages loaded from disk"
          ops

      // Generate hash file BEFORE evaluating values, so that PackageRefs
      // lookups resolve correctly during value evaluation.
      do! LibDB.PackageRefsGenerator.generate ()
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
        let! stats = LibDB.Stats.get ()
        print "Loaded packages from disk "
        print $"{stats.types} types, {stats.values} values, and {stats.fns} fns"
        let (Hash commitHashStr) = commitHash
        let shortHash = commitHashStr[..6]
        print $"Created init commit {shortHash}"

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
        do! LibDB.Seed.export outputPath
        let size = System.IO.FileInfo(outputPath).Length / 1024L / 1024L
        print $"Seed exported to {outputPath} ({size} MB)"
        return Ok()
      with ex ->
        return Error $"Export failed: {ex.Message}"
    }

  let listMigrations () : Ply<Result<unit, string>> =
    uply {
      try
        print
          "`migrations list` is gone — there's one schema.sql now and \
           it kill-and-fills on hash change. Run `migrations run` to \
           apply (or no-op if up-to-date)."
        return Ok()
      with ex ->
        return Error $"Failed to list migrations: {ex.Message}"
    }

  /// Scan `package_values.rt_dval` for referenced blob hashes and
  /// delete any `package_blobs` rows that aren't referenced.
  let sweepBlobs () : Ply<Result<unit, string>> =
    uply {
      try
        print "Sweeping orphan package_blobs..."
        let! deleted = LibDB.RuntimeTypes.Blob.sweepOrphans ()
        print $"Deleted {deleted} orphan blob row(s)"
        return Ok()
      with ex ->
        return Error $"Sweep failed: {ex.Message}"
    }

[<EntryPoint>]
let main (args : string[]) : int =
  try
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

    | [ "pm-sweep-blobs" ] ->
      handleCommand
        "sweeping orphan package_blobs rows"
        (HandleCommand.sweepBlobs ())

    | [ "bench" ] ->
      handleCommand
        "running allocation/timing benchmarks"
        (LocalExec.Benchmarks.runAll ())

    | [ "bench-render" ] ->
      handleCommand
        "rendering benchmarks/results.md from history.jsonl"
        (LocalExec.Benchmarks.render ())

    | _ ->
      print "Invalid arguments"
      print "Available commands:"
      print "  reload-packages"
      print "  migrations run"
      print "  migrations list"
      print "  export-seed <output-path>"
      print "  pm-sweep-blobs"
      print "  bench"
      print "  bench-render"
      NonBlockingConsole.wait ()
      1
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    NonBlockingConsole.wait ()
    1
