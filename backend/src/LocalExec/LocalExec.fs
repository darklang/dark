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

  let exportSeed
    (outputPath : string)
    (upToCommit : string option)
    : Ply<Result<unit, string>> =
    uply {
      try
        do! LibDB.Seed.exportAt outputPath upToCommit
        let size = System.IO.FileInfo(outputPath).Length / 1024L / 1024L
        print $"Seed exported to {outputPath} ({size} MB)"
        return Ok()
      with ex ->
        return Error $"Export failed: {ex.Message}"
    }

  /// Stand on the branch this rundir is on, the way the CLI does before it runs anything.
  ///
  /// Not global to LocalExec: the fill path deliberately refills MAIN from disk, and doing that
  /// while standing on a branch would be wrong. Only the commands that ask a question ABOUT the
  /// current branch select it.
  let selectStoredBranch () : Ply<unit> =
    uply {
      match! LibDB.BranchSelection.select None None with
      | Ok selection ->
        LibDB.PackageManager.selectBranch (
          selection.branchId
          |> Option.defaultValue LibExecution.ProgramTypes.BranchId.Main
        )
      | Error _ -> ()
    }

  /// Does this kernel agree with the package set in front of it?
  ///
  /// Direction one of the two-way interface: every name the kernel references has to resolve, in
  /// the store as seen from the current branch or in the pin. Direction two -- every builtin the
  /// package set calls existing in this kernel -- needs the store to record builtin edges, which
  /// it does not yet.
  ///
  /// Asked all at once, and at BUILD time, because the ref closures are lazy: an unresolvable ref
  /// is otherwise found whenever some code path happens to reach it, which can be a different day
  /// and an unrelated command. The case this exists for is checking out somebody's git branch
  /// without their package work: the F# in your tree names things your store has never heard of,
  /// and you should be told that then, in one list, rather than one at a time by whatever runs
  /// first.
  let checkRefs () : Ply<Result<unit, string>> =
    uply {
      do! selectStoredBranch ()

      let unresolved =
        LibExecution.PackageRefs.allRefs ()
        |> List.filter (fun (kind, modules, name) ->
          LibExecution.PackageRefs.tryResolve kind modules name |> Option.isNone)

      // Direction two: every builtin the package set calls has to exist in THIS kernel. Recorded
      // by the fold in `package_builtin_deps`, which is what makes a store able to say which
      // kernel it needs -- the check that used to answer this grepped `.dark` text off disk and
      // stops being possible the day packages come from a seed.
      let kernelBuiltins =
        let b = Builtins.all ()
        Set.union
          (b.fns.Values
           |> Seq.map (fun f -> (f.name.name, f.name.version))
           |> Set.ofSeq)
          (b.values.Values |> Seq.map (fun v -> (v.name.name, 0)) |> Set.ofSeq)

      let! calledBuiltins =
        Sql.query
          "SELECT DISTINCT builtin_name, builtin_version FROM package_builtin_deps"
        |> Sql.executeAsync (fun read ->
          (read.string "builtin_name", read.int "builtin_version"))

      let missingBuiltins =
        calledBuiltins |> List.filter (fun b -> not (Set.contains b kernelBuiltins))

      // An EMPTY table is not a pass. `package_builtin_deps` is a projection, so a store that got
      // the table from a release step without re-folding has no rows, and the builtin half of the
      // check would report success having asked nothing. Saying so is the difference between this
      // check and one that quietly stops covering what it was written for.
      if List.isEmpty calledBuiltins then
        return
          Error(
            "this store records no builtin calls at all, so the builtin half of this check asked "
            + "nothing. `package_builtin_deps` is a projection: re-fold the log to fill it "
            + "(`scripts/build/reload-packages`, or any migration that drops projections)."
          )
      elif List.isEmpty unresolved && List.isEmpty missingBuiltins then
        let n = List.length (LibExecution.PackageRefs.allRefs ())
        print (
          $"All {n} kernel refs resolve, and all {List.length calledBuiltins} builtins this "
          + "package set calls exist in this kernel."
        )
        return Ok()
      elif List.isEmpty unresolved then
        let lines =
          missingBuiltins
          |> List.sort
          |> List.map (fun (n, v) -> $"  Builtin.{n} (v{v})")
          |> String.concat "\n"

        return
          Error(
            $"this package set calls {List.length missingBuiltins} builtin(s) this kernel does "
            + $"not have:\n{lines}\n\nA builtin was removed or renamed out from under package "
            + "code that calls it. Land the package change that stops calling it, move the pin "
            + "forward, and only then remove the builtin."
          )
      else
        let lines =
          unresolved
          |> List.sortBy (fun (kind, m, n) -> (kind, m, n))
          |> List.map (fun (kind, modules, name) ->
            $"""  {kind} Darklang.{String.concat "." modules}.{name}""")
          |> String.concat
            "
"

        return
          Error(
            $"{List.length unresolved} kernel ref(s) do not resolve against this package set:
"
            + lines
            + "

This kernel and this package set do not agree. Usually that means the F# in "
            + "your tree names package code your store does not have: import the branch bundle "
            + "that goes with it, or move to the branch that has it."
          )
    }

  /// Write `package-ref-hashes.txt` from whatever store this rundir has.
  ///
  /// The kernel's entry points are pinned BY HASH, so a binary needs that file before it can resolve
  /// anything. The fill path writes it as a side effect of reloading `packages/`; this is the same
  /// step on its own, for a store that arrived as a SEED and has no `packages/` to reload. That is
  /// the only thing standing between a fetch-at-pin build and a working binary.
  let generateRefs () : Ply<Result<unit, string>> =
    uply {
      try
        do! selectStoredBranch ()
        do! LibDB.PackageRefsGenerator.generate ()
        LibExecution.PackageRefs.reloadHashes ()
        return Ok()
      with ex ->
        return Error $"Generating package refs failed: {ex.Message}"
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
        (HandleCommand.exportSeed outputPath None)

    // Cut at a commit, so what a pin fetches is fixed by the commit rather than by when it asked:
    // the same commit yields the same OPS however far the store has moved since, and ids are derived
    // from op content, so two stores built from it agree. Not byte-identical -- the stamp records
    // which build cut it and when -- and nothing needs it to be.
    | [ "export-seed"; outputPath; commit ] ->
      handleCommand
        $"Exporting seed at {commit} to {outputPath}"
        (HandleCommand.exportSeed outputPath (Some commit))

    | [ "refs"; "check" ] ->
      handleCommand
        "checking the kernel's refs against this package set"
        (HandleCommand.checkRefs ())

    | [ "refs"; "generate" ] ->
      handleCommand
        "writing package-ref-hashes.txt from this store"
        (HandleCommand.generateRefs ())

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
      print "  export-seed <output-path> [commit]"
      print "  refs generate"
      print "  refs check"
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
