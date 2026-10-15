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
      // Clear cached refs so an old file missing new refs won't block parsing.
      // Keep the disk file to detect changes to protected type hashes.
      LibExecution.PackageRefs.setHashes Map.empty
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
      // The first fill makes the checker and trait implementations available. The second
      // stores bodies with resolved trait calls. Initialize refs before BOTH fills: storing
      // a function also lowers it, which needs the identities in that fill's source ops.
      let fill (commitBaseline : bool) (ops : List<PackageOp>) : Ply<unit> =
        uply {
          LibDB.PackageRefsGenerator.initializeFromOps ops
          do! LibDB.Purge.purge ()
          let! _ = LibDB.Inserts.insertAndApplyOpsAsWip ops
          if commitBaseline then
            // The .dark files are the shipped baseline, not your draft. Commit them, or
            // every `dark status` would open on the whole package tree as uncommitted
            // work. Only the last fill needs it.
            let! _ = LibDB.Inserts.commitAllAsBaseline "package reload (baseline)"
            ()
          // Generate hashes BEFORE evaluating values, so that PackageRefs
          // lookups resolve correctly during value evaluation. The first fill's are
          // pre-resolution hashes, so only the last fill writes the shared file;
          // see `PackageRefsGenerator.generate`.
          do! LibDB.PackageRefsGenerator.generate commitBaseline
          return ()
        }

      do! fill false ops

      // The checker is written in Dark. Build its execution state after the first fill,
      // when its package items, refs and trait implementations are available.
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
            notify with
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
        // Nothing to pin, so the first fill is the final one; it still needs the baseline commit,
        // and the hash file it did not write.
        let! _ = LibDB.Inserts.commitAllAsBaseline "package reload (baseline)"
        do! LibDB.PackageRefsGenerator.generate true

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
        let! tests = countDistinct "package_tests"
        print "Loaded packages from disk "
        print $"{types} types, {values} values, {fns} fns, and {tests} tests"

        // Exact, not an estimate: the log now holds what `packages/` produces, so whatever the
        // count fell by is what a disk reload cannot reproduce.
        let! opsAfter = countOps ()
        if opsBefore > opsAfter then
          print
            $"WARNING: {opsBefore - opsAfter} op(s) did not survive this reload. A reload replaces \
              the log with what `packages/` produces; ops authored here or pulled from a relay are \
              not in it, and nothing in the base tree sends them anywhere else first."

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
  /// What is reachable, from the entry points a person actually drives.
  ///
  /// Two questions nothing else here answers. Which package items can no code path reach, and
  /// which builtins does no reachable item name. Both are asked of the STORE and of each item's
  /// own body, so a grep's two standing failures do not apply: a comment or a doc example cannot
  /// manufacture a reference, and a call spelled a different way in the source cannot hide one.
  ///
  /// The roots are the CLI's own entry points plus every ref the F# runtime resolves by name
  /// (`PackageRefs`), because those are calls the graph cannot see. Anything reached only through
  /// `eval` -- a demo tape, a testfile, a line a document tells you to type -- is invisible here
  /// by construction, so an unreachable LISTING is a place to look rather than a verdict. The
  /// stdlib is the obvious case: it exists to be called by code that is not in this tree, so most
  /// of it is unreachable and correctly so. The report separates it for that reason.
  let reachability () : Ply<Result<unit, string>> =
    uply {
      // The repo root: this runs from the source tree, which is the only place the test and doc
      // text exists. `LibConfig.Dirs` knows where the tree is.
      let root = LibConfig.Dirs.sourceRootDir

      let! rows =
        Sql.query
          "SELECT item_hash, owner, modules, name, item_type FROM locations
           WHERE unlisted_at IS NULL AND item_type <> 'module'"
        |> Sql.executeAsync (fun read ->
          (read.string "item_hash",
           read.string "owner",
           read.string "modules",
           read.string "name",
           read.string "item_type"))

      let label (o : string) (m : string) (n : string) =
        if m = "" then $"{o}.{n}" else $"{o}.{m}.{n}"

      let nameOf =
        rows |> List.map (fun (h, o, m, n, _) -> h, label o m n) |> Map.ofList

      let byName =
        rows |> List.map (fun (h, o, m, n, _) -> label o m n, h) |> Map.ofList

      // Every ref the runtime looks up by name is a root: F# calls it directly, so no package
      // item names it and the graph would call it dead.
      let refRoots =
        LibExecution.PackageRefs.allHashes ()
        |> Set.filter (fun h -> Map.containsKey h nameOf)

      let entryPoints =
        [ "Darklang.Cli.executeCliCommand"
          "Darklang.Cli.Registry.allCommands"
          "Darklang.Cli.initState"
          "Darklang.Cli.Workbench.execute" ]
        |> List.choose (fun n -> Map.tryFind n byName)
        |> Set.ofList

      // A third kind of root: a fully-qualified name written as a STRING in Dark source and
      // resolved at run time. The live-view host takes its callbacks that way
      // (`init = "Darklang.Cli.TraceWatch.init"`), so no body references those functions and the
      // walk calls the whole panel dead -- nine items of `ps --watch` and `traces --watch`, which
      // both demonstrably work. Any `"Darklang.X.y"` in a .dark file counts.
      let stringNamedRoots =
        let re =
          System.Text.RegularExpressions.Regex "\"(Darklang(?:\.[A-Za-z0-9_]+)+)\""

        let dark =
          if
            System.IO.Directory.Exists(System.IO.Path.Combine(root, "packages"))
          then
            System.IO.Directory.EnumerateFiles(
              System.IO.Path.Combine(root, "packages"),
              "*.dark",
              System.IO.SearchOption.AllDirectories
            )
            |> List.ofSeq
          else
            []

        dark
        |> List.collect (fun f ->
          try
            re.Matches(System.IO.File.ReadAllText f)
            |> Seq.map (fun m -> m.Groups[1].Value)
            |> List.ofSeq
          with _ ->
            [])
        |> List.distinct
        |> List.choose (fun n -> Map.tryFind n byName)
        |> Set.ofList

      print $"  {Set.count stringNamedRoots} root(s) named as strings in Dark source"
      let roots = Set.unionMany [ refRoots; entryPoints; stringNamedRoots ]
      print
        $"roots: {Set.count roots} ({Set.count entryPoints} CLI entry point(s), {Set.count refRoots} PackageRefs)"

      // A body's own references, by kind. `CallGraph.analyze` walks the PT expression, lambda
      // bodies and dictionary keys included, which is the same walk the permission analyzer
      // trusts.
      let builtinsOf = System.Collections.Generic.Dictionary<string, Set<string>>()
      let pkgRefsOf = System.Collections.Generic.Dictionary<string, Set<string>>()
      let mutable analyzed = 0
      let mutable missing = 0
      let mutable incomplete = 0

      // A body's VALUE references, which `Analysis.names` does not carry: it collects fn names
      // only. Without these, `DemoData.HttpServerTest.router` reaches its three handlers through a
      // `val` of them and the handlers read as unreachable.
      let valueRefsOf = System.Collections.Generic.Dictionary<string, Set<string>>()

      let record (h : string) (a : LibExecution.CallGraph.Analysis) =
        analyzed <- analyzed + 1
        if not a.complete then incomplete <- incomplete + 1

        builtinsOf[h] <-
          a.names
          |> List.choose (function
            | FQFnName.Builtin b -> Some $"{b.name}_v{b.version}"
            | _ -> None)
          |> Set.ofList

        pkgRefsOf[h] <-
          a.names
          |> List.choose (function
            | FQFnName.Package(Hash p) -> Some p
            | _ -> None)
          |> Set.ofList

      // Values as well as functions. A `val` body can name a builtin and nothing else will:
      // `Cli.Telemetry.logFilePath` is the only caller of `cliGetLogDir`, and a fn-only walk
      // reports that builtin as having no caller at all.
      for (h, _, _, _, t) in rows do
        match t with
        | "fn" ->
          match! PM.pt.getFn (Hash h) with
          | None -> missing <- missing + 1
          | Some fn ->
            record h (LibExecution.CallGraph.analyzeFn Set.empty fn)

            valueRefsOf[h] <-
              LibExecution.CallGraph.valueRefs fn.body |> Set.map (fun (Hash v) -> v)
        | "value" ->
          match! PM.pt.getValue (Hash h) with
          | None -> missing <- missing + 1
          | Some v ->
            record h (LibExecution.CallGraph.analyze Set.empty Set.empty v.body)

            valueRefsOf[h] <-
              LibExecution.CallGraph.valueRefs v.body |> Set.map (fun (Hash x) -> x)
        | _ -> ()

      print
        $"analyzed {analyzed} fn and value bodies ({missing} could not be loaded, {incomplete} statically incomplete)"
      if analyzed = 0 then
        return
          Error "analyzed no bodies at all, so this report would be about nothing"
      else

        // The type and value edges the fold already recorded. A type is reached through them, never
        // through a body's call list.
        let! depRows =
          Sql.query "SELECT item_hash, depends_on_hash FROM package_dependencies"
          |> Sql.executeAsync (fun read ->
            read.string "item_hash", read.string "depends_on_hash")

        let deps = System.Collections.Generic.Dictionary<string, HashSet<string>>()

        let addEdge (a : string) (b : string) =
          match deps.TryGetValue a with
          | true, s -> s.Add b |> ignore<bool>
          | _ ->
            let s = HashSet<string>()
            s.Add b |> ignore<bool>
            deps[a] <- s

        for (a, b) in depRows do
          addEdge a b

        for KeyValue(a, ps) in pkgRefsOf do
          for p in ps do
            addEdge a p

        for KeyValue(a, vs) in valueRefsOf do
          for v in vs do
            addEdge a v

        let seen = HashSet<string>()
        let stack = System.Collections.Generic.Stack<string>()
        for r in roots do
          stack.Push r

        while stack.Count > 0 do
          let h = stack.Pop()
          if seen.Add h then
            match deps.TryGetValue h with
            | true, s ->
              for n in s do
                stack.Push n
            | _ -> ()

        let reached =
          seen |> Seq.filter (fun h -> Map.containsKey h nameOf) |> Set.ofSeq
        print $"reachable package items: {Set.count reached} of {List.length rows}"

        let isLibrary (n : string) =
          n.StartsWith "Darklang.Stdlib."
          || n.StartsWith "Darklang.PrettyPrinter."
          || n.StartsWith "Darklang.LanguageTools."
          || n.StartsWith "Darklang.Internal."

        let unreached =
          rows
          |> List.map (fun (h, o, m, n, t) -> h, label o m n, t)
          |> List.filter (fun (h, _, _) -> not (Set.contains h reached))

        let app, lib =
          unreached |> List.partition (fun (_, n, _) -> not (isLibrary n))

        // A fn nothing in `packages/` calls can still have a driver: a testfile, an F# test holding
        // Dark source as a string, a demo tape, or a line a document tells you to type. All of those
        // go through `eval`, so no package body names them and this walk cannot see them. Classify
        // the unreachable list by whether its name appears in that text at all, because "only the
        // tests use it" and "nothing uses it" are different findings and only the second is dead.
        // Count per directory and print it. A total is not a floor: a directory that silently read
        // as empty would have gone unnoticed behind the other four, which is how the first run of
        // this reported eighteen testfile fixtures as named nowhere.
        let readDir (dir : string) : string list =
          let full = System.IO.Path.Combine(root, dir)
          if System.IO.Directory.Exists full then
            System.IO.Directory.EnumerateFiles(
              full,
              "*",
              System.IO.SearchOption.AllDirectories
            )
            |> List.ofSeq
          else
            []

        let driverParts =
          [ "backend/testfiles"; "backend/tests"; "docs"; "scripts" ]
          |> List.map (fun dir ->
            let files = readDir dir
            let text =
              files
              |> List.map (fun f ->
                try
                  System.IO.File.ReadAllText f
                with _ ->
                  "")
              |> String.concat "\n"
            dir, List.length files, text)

        // Package PROSE, which is a driver of its own: a doc comment that says "run
        // `dark eval 'Darklang.Cli.Ps.printAll ()'`" was the only caller that function had, and it was
        // a real one. Comment lines only, or every definition would name itself.
        let packageProse =
          readDir "packages"
          |> List.filter (fun f -> f.EndsWith ".dark")
          |> List.map (fun f ->
            try
              System.IO.File.ReadAllLines f
              |> Array.filter (fun l -> l.TrimStart().StartsWith "//")
              |> String.concat "\n"
            with _ ->
              "")
          |> String.concat "\n"

        for (dir, n, text) in driverParts do
          print $"  read {n} file(s), {text.Length} chars, from {dir}"
          if n = 0 then
            print
              $"  WARNING: {dir} read as empty, so the split below is not trustworthy"

        print $"  read {packageProse.Length} chars of comment text from packages/"

        let driverText =
          (driverParts |> List.map (fun (_, _, t) -> t) |> String.concat "\n")
          + "\n"
          + packageProse

        // Does any of that text name this item? The last segment as a WHOLE WORD, not `.leaf`:
        // `backend/testfiles` is parsed with owner `Tests`, so a testfile names a function BARE
        // (`stringFn`, not `Test.stringFn`), and requiring the dot put five such fixtures on the
        // list of things nothing anywhere references. A bare word over-reports instead -- `stamp`
        // and `confirm` turn up in prose that has nothing to do with the function -- and that is
        // the safe direction here: over-reporting keeps an item OFF the delete list, where
        // under-reporting puts a live one on it.
        let leafRe =
          System.Collections.Generic.Dictionary<string, System.Text.RegularExpressions.Regex>()

        let namedInDrivers (fullName : string) =
          let leaf = fullName.Split '.' |> Array.last

          if driverText.Contains fullName then
            true
          else
            let re =
              match leafRe.TryGetValue leaf with
              | true, r -> r
              | _ ->
                let r =
                  System.Text.RegularExpressions.Regex(
                    @"\b" + System.Text.RegularExpressions.Regex.Escape leaf + @"\b",
                    System.Text.RegularExpressions.RegexOptions.Compiled
                  )

                leafRe[leaf] <- r
                r

            re.IsMatch driverText

        let drivenByTests, reachedByNothing =
          app |> List.partition (fun (_, n, _) -> namedInDrivers n)

        print ""
        print $"UNREACHABLE application code: {List.length app} items"
        print
          $"  of those, named somewhere in tests/docs/demos: {List.length drivenByTests}"
        print
          $"  of those, named NOWHERE in the tree:           {List.length reachedByNothing}"
        print ""
        print "NAMED NOWHERE -- no package body, no test, no doc, no demo:"
        reachedByNothing
        |> List.sortBy (fun (_, n, _) -> n)
        |> List.iter (fun (_, n, t) -> print $"  {t, -6} {n}")
        print ""
        print $"UNREACHABLE, application code, by module ({List.length app}):"
        app
        |> List.countBy (fun (_, n, _) ->
          let parts = n.Split '.'
          String.concat "." (Array.truncate (max 1 (parts.Length - 1)) parts))
        |> List.sortByDescending snd
        |> List.iter (fun (m, c) -> print $"  {c, 5}  {m}")
        print ""
        print
          $"UNREACHABLE, library code ({List.length lib}): expected -- it exists for code outside this tree"
        lib
        |> List.countBy (fun (_, n, _) ->
          let parts = n.Split '.'
          String.concat
            "."
            (Array.truncate (min 3 (max 1 (parts.Length - 1))) parts))
        |> List.sortByDescending snd
        |> List.truncate 12
        |> List.iter (fun (m, c) -> print $"  {c, 5}  {m}")

        // Builtins: named by a reachable item, named only by an unreachable one, or named nowhere.
        let namedByReachable =
          builtinsOf
          |> Seq.filter (fun kv -> Set.contains kv.Key reached)
          |> Seq.collect (fun kv -> kv.Value)
          |> Set.ofSeq

        let namedAnywhere =
          builtinsOf |> Seq.collect (fun kv -> kv.Value) |> Set.ofSeq
        print ""
        print $"builtins named by a REACHABLE item: {Set.count namedByReachable}"
        print
          $"builtins named only by an UNREACHABLE item: {Set.count (Set.difference namedAnywhere namedByReachable)}"

        // The PRODUCT's builtins. `Builtins.all ()` also carries `TestUtils.LibTest`, which no
        // shipped code may name, so counting those would report seven permanent orphans.
        let all =
          LibExecution.Builtin.combine
            [ Builtins.Pure.Builtin.builtins ()
              Builtins.Http.Client.Builtin.builtins ()
              Builtins.Language.Builtin.builtins ()
              Builtins.Cli.Builtin.builtins ()
              Builtins.Time.Builtin.builtins ()
              Builtins.Random.Builtin.builtins ()
              Builtins.Matter.Builtin.builtins
                LibExecution.ProgramTypes.PackageManager.empty
              Builtins.CliHost.Builtin.builtins ()
              Builtins.Http.Server.Builtin.builtins () ]
            []
          |> fun b ->
            b.fns.Keys
            |> Seq.map (fun (k : RT.FQFnName.Builtin) -> $"{k.name}_v{k.version}")
            |> Set.ofSeq

        if Set.isEmpty all then
          print
            "(could not enumerate the registered builtins, so 'named nowhere' is not reported)"
        else
          // An operator does not reach its builtin by name: the interpreter answers two operands of
          // one numeric type directly (`FastOps.evalNumeric`), so `divide`, `bitwiseAnd`, `shiftLeft`
          // and their fifteen siblings are called by F# and named by no body. `tests/builtin` knows
          // the same class as `infixDispatched` and `languageIdioms`.
          let nowhere = Set.difference all namedAnywhere
          print
            $"product builtins: {Set.count all}; named by no package body at all: {Set.count nowhere}"
          print
            "  (an operator builtin belongs here: the interpreter calls it, not a body)"
          nowhere |> Set.toList |> List.sort |> List.iter (fun b -> print $"  {b}")

        return Ok()
    }

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

    | [ "reachability" ] ->
      handleCommand
        "reporting what is reachable from the CLI's entry points"
        (HandleCommand.reachability ())

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
      print "  reachability"
      print "  bench"
      print "  bench-render"
      NonBlockingConsole.wait ()
      1
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    NonBlockingConsole.wait ()
    1
