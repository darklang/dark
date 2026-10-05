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

      // What the store holds BEFORE the purge, so the reload can say what it destroyed. A reload
      // replaces the log with what `packages/` produces, so anything else in it -- ops authored
      // here, ops pulled from a relay -- does not survive. That is the intended behaviour of a dev
      // reload and it used to happen in silence, which is how a morning's work disappears and gets
      // blamed on whatever was edited last.
      let countOps () =
        Sql.query "SELECT COUNT(*) as count FROM package_ops"
        |> Sql.executeRowAsync (fun read -> read.int64 "count")
      let! opsBefore = countOps ()


      // Main has no CreateBranch op and no `branches` row, so a store with an empty `branches`
      // table is a store on main. Re-folding `package_ops` is the whole rebuild.

      print "Filling ..."
      // Load all packages from disk as live ops (commit-free authoring: no init commit).
      // Note: values are stored with NULL rt_dval at this point.
      //
      // Filled twice: resolving what a trait call or operator runs needs the at-rest checker,
      // the hashes in `package-ref-hashes.txt` and the impl stamps, none of which exist until
      // the tree is in. Without the second pass nothing the CLI ships records its impl, since
      // resolution is reached from `addAuthored` and this path is not.
      let fill (commitBaseline : bool) (ops : List<PackageOp>) : Ply<unit> =
        uply {
          do! LibDB.Purge.purge ()
          let! _ = LibDB.Inserts.insertAndApplyOpsAsWip ops
          if commitBaseline then
            // The .dark files are the shipped baseline, not your draft. Commit them, or
            // every `dark status` would open on the whole package tree as uncommitted
            // work. Only the last fill needs it.
            let! _ = LibDB.Inserts.commitAllAsBaseline "package reload (baseline)"
            ()
          // Generate hash file BEFORE evaluating values, so that PackageRefs
          // lookups resolve correctly during value evaluation.
          do! LibDB.PackageRefsGenerator.generate ()
          LibExecution.PackageRefs.reloadHashes ()
          return ()
        }

      do! fill false ops

      // The checker is written in Dark, so resolving a call means executing Dark, which needs
      // a state. Safe to build here and nowhere earlier: this is after the first `fill`, which
      // is why the fill is done twice at all -- the checker's own package items, the ref hashes
      // and the impl stamps have to be in the store before anything can ask it a question.
      let exeState =
        let notify _ _ _ _ = uply { return () }
        let reportException _ _ _ _ = uply { return () }
        // `allowAll`, for the same reason `evaluateAllValues` runs as `TrustedSeed` a few lines
        // below: this is the one trusted producer. The code being checked is the checked-in
        // `packages/` tree that was just parsed off disk, not a guest's, and the checker has to
        // read the package store to answer which implementations exist. Without this the
        // reload's own policy refuses `package-read` and 795 items come back Incomplete, with
        // the pins silently absent rather than any error.
        //
        // The real package manager, not `Builtins.all`'s empty one: the checker loads from the
        // store whatever a body uses that the batch does not carry.
        { Execution.createState
            (Builtins.allWith PM.pt)
            PM.rt
            Execution.noTracing
            reportException
            notify
            { dbs = Map.empty } with
            access =
              LibExecution.Permissions.Access.start
                LibExecution.Permissions.Policy.allowAll }

      let checkTimer = System.Diagnostics.Stopwatch.StartNew()
      let! pinned =
        Builtins.Matter.Libs.PM.TraitCalls.resolveTraitCalls
          exeState
          BranchId.Main
          PM.pt
          true
          ops
      // The pins are written at the hashes in hand, then moved by the rehash, exactly as the
      // authoring path does it.
      let resolved = LibDB.HashStabilization.computeRealHashes pinned
      // Ops, not items: recording the implementation changes the item's content hash, and every
      // caller's hash moves with it, so the count is larger than the number of calls pinned.
      let changed (after : List<PackageOp>) =
        List.zip ops after |> List.filter (fun (a, b) -> a <> b) |> List.length
      let bodies =
        ops
        |> List.filter (fun op ->
          match op with
          | PackageOp.AddFn _
          | PackageOp.AddValue _ -> true
          | _ -> false)
        |> List.length
      print
        $"Resolved trait calls: {changed resolved} op(s) moved; {changed pinned} of {bodies} bodies pinned in {checkTimer.Elapsed.TotalSeconds:F1}s"
      if resolved <> ops then
        do! fill true resolved
      else
        // Nothing to pin, so the first fill is the final one; it still needs the baseline commit.
        let! _ = LibDB.Inserts.commitAllAsBaseline "package reload (baseline)"
        ()

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

        // Exact, not an estimate: the log now holds what `packages/` produces, so whatever the
        // count fell by is what a disk reload cannot reproduce.
        let! opsAfter = countOps ()
        if opsBefore > opsAfter then
          print
            $"WARNING: {opsBefore - opsAfter} op(s) did not survive this reload. A reload replaces \
              the log with what `packages/` produces; ops authored here or pulled from a relay are \
              not in it. `dark push` or `dark sync export <file>` before reloading keeps them."

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
          "`migrations list` is gone — the schema lives in migrations/schema/ now and \
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
