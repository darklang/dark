/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module RT = LibExecution.RuntimeTypes
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

      // Main has no CreateBranch op and no `branches` row, so a store with an empty `branches`
      // table is a store on main. Re-folding `package_ops` is the whole rebuild.

      print "Filling ..."
      // Load all packages from disk as live ops (commit-free authoring: no init commit).
      // Note: values are stored with NULL rt_dval at this point
      let! _ = LibDB.Inserts.insertAndApplyOpsAsWip ops

      // The .dark files are the shipped baseline, not your draft. Commit them, or
      // every `dark status` would open on the whole package tree as uncommitted
      // work.
      let! _ = LibDB.Inserts.commitAllAsBaseline "package reload (baseline)"

      // Generate hash file BEFORE evaluating values, so that PackageRefs
      // lookups resolve correctly during value evaluation.
      do! LibDB.PackageRefsGenerator.generate ()
      LibExecution.PackageRefs.reloadHashes ()

      // Evaluate all values now that all definitions are in the DB
      // The one trusted producer: these bodies come from the checked-in
      // `packages/` tree that was just parsed off disk, not from a guest.
      let! evalResult =
        evaluateAllValues LibDB.Seed.TrustedSeed (Builtins.all ()) PM.rt
      match evalResult with
      | Error errors ->
        for e in errors do
          print
            $"  Value evaluation error: {LibDB.Seed.ValueEvaluationError.toString e}"
        return Error "Some values failed to evaluate"
      | Ok() ->
        // Counted here rather than through the package layer, because this runs before there is one.
        // DISTINCT hash: total unique content, not a branch's view.
        let countDistinct table =
          Sql.query $"SELECT COUNT(DISTINCT hash) as count FROM {table}"
          |> Sql.executeRowAsync (fun read -> read.int64 "count")
        let! types = countDistinct "package_types"
        let! values = countDistinct "package_values"
        let! fns = countDistinct "package_functions"
        print "Loaded packages from disk "
        print $"{types} types, {values} values, and {fns} fns"

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
      // Not "deleting the database": an unchanged schema hash makes this a no-op, and even a
      // changed one only drops the regenerable projections. Nothing you authored is at risk, and
      // this runs on every build, so the wording has to stop being alarming.
      handleCommand "bringing the schema up to date" (HandleCommand.runMigrations ())

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

    | [ "platforms" ] ->
      print "Platforms in this build:"
      Platforms.Sets.describe (Platforms.Sets.everything ()) |> List.iter print
      print ""
      print
        $"fingerprint {(Platforms.Sets.everything ()).fingerprint}   (owner + identity + signature + effects)"
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "doors" ] ->
      // The tightening report asks what each platform reaches. This asks how many builtins reach
      // each effect, which is the number that has to come down before an effect can be granted per
      // door rather than per category.
      let set = Platforms.Sets.everything ()
      print "Doors to each effect, widest first:"
      print ""
      Platforms.Sets.effectDoors set |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "doors"; effectName ] ->
      // Every door to ONE effect, in full. The summary truncates, and the effect you want to split
      // is exactly the one whose list was too long to show.
      let set = Platforms.Sets.everything ()
      print $"Every builtin that reaches {effectName}:"
      print ""
      Platforms.Sets.doorsTo set effectName |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "tighten" ] ->
      // What is worth acting on: an effect with one contributor is one function away from being
      // gone from that platform.
      let set = Platforms.Sets.everything ()
      print "Effects reached, and which builtins are responsible:"
      print ""
      Platforms.Sets.tighteningReport set |> List.iter print
      print ""
      print
        "Impure builtins that declare no effects (deliberate for the sqlite ones):"
      Platforms.Sets.undeclaredImpure set |> List.iter (fun l -> print $"    {l}")
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "wrappers" ] ->
      print "Where each platform's builtins are wrapped on the Dark side:"
      print ""
      LocalExec.PlatformReport.report (Platforms.Sets.everything ())
      |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "cost" ] ->
      LocalExec.PlatformReport.costReport () |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "audit"; prefix ] ->
      (LocalExec.PlatformReport.Audit.report (Platforms.Sets.everything ()) prefix
       |> Ply.toTask)
        .Result
      |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "unreachable" ] ->
      (LocalExec.PlatformReport.Unreachable.report (Platforms.Sets.everything ())
       |> Ply.toTask)
        .Result
      |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "collapsible" ] ->
      LocalExec.PlatformReport.Collapsible.report (Platforms.Sets.everything ())
      |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "needed"; fnName ] ->
      LocalExec.PlatformReport.Needed.report (Platforms.Sets.everything ()) fnName
      |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "unused" ] ->
      (LocalExec.PlatformReport.unusedReport () |> Ply.toTask).Result
      |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platform-fingerprint" ] ->
      // Just the hash, on stdout, nothing else: the build reads this to decide whether the package
      // reload can be skipped, so anything decorative here becomes a parsing bug there. Built over
      // an EMPTY package manager so it needs no store and cannot be perturbed by one.
      let set =
        Platforms.Sets.everythingFor LibExecution.ProgramTypes.PackageManager.empty
      print set.fingerprint
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "cost"; "parts" ] ->
      LocalExec.PlatformReport.fnPartsReport () |> List.iter print
      NonBlockingConsole.wait ()
      0

    | [ "platforms"; "cost"; "core" ] ->
      LocalExec.PlatformReport.coreCostReport () |> List.iter print
      NonBlockingConsole.wait ()
      0

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
      print "  platforms            what this build ships, and its fingerprint"
      print "  platforms tighten    which builtins give each platform each effect"
      print "  platforms doors      how many builtins reach each effect, widest first"
      print
        "  platforms wrappers   which package areas wrap each platform's builtins"
      print
        "  platforms needed <Owner.Module.fn>   the minimum platform set that fn needs"
      print
        "  platforms collapsible   the numeric tower, and how much of it is replication"
      print
        "  platforms audit <Owner.Module>   functions reaching effects their module should not"
      print
        "  platforms unreachable   builtins no package function in this repo can reach"
      print "  bench"
      print "  bench-render"
      NonBlockingConsole.wait ()
      1
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    NonBlockingConsole.wait ()
    1
