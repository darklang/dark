module Tests.All

// Main entry point for tests being run

open Expecto
open System.Threading.Tasks

open Prelude

/// `--shard INDEX/TOTAL` keeps the tests this node owns, and returns the remaining
/// arguments for Expecto, which knows nothing about the flag.
///
/// Ownership is by hash of the name rather than by group or by file, and that is the
/// whole point: the slow tests are concentrated in a few groups, so any split along
/// the tree's own lines puts them all on one node. Hashing spreads them by
/// construction, with no timing data to collect or keep current.
///
/// What this rules out: a test that exists to set process state for the tests after it.
/// Sharding partitions by test, so the setter and its dependants land on different nodes
/// and the dependants run against the default. A precondition has to be carried by the
/// tests that need it -- see `cliTestWithFreshTraces`, which learned this the hard way.
let private parseShard (args : string array) : (int * int) option * string array =
  match Array.tryFindIndex (fun a -> a = "--shard") args with
  | None -> None, args
  | Some i ->
    if i + 1 >= args.Length then
      Exception.raiseInternal "--shard needs an argument, like --shard 0/4" []
    let spec = args[i + 1]
    match spec.Split('/') with
    | [| idx; total |] ->
      let idx, total = int idx, int total
      if total < 1 || idx < 0 || idx >= total then
        Exception.raiseInternal $"--shard {spec}: INDEX must be in [0, TOTAL)" []
      Some(idx, total), Array.append args[0 .. i - 1] args[i + 2 ..]
    | _ ->
      Exception.raiseInternal $"--shard {spec}: expected INDEX/TOTAL, like 0/4" []

/// `--kind sequenced` or `--kind parallel` runs only that half of the suite, so
/// `run-fsharp-tests --split` can run the two halves as two processes at the same time.
/// Sequenced tests run one at a time because they share this process's state; they don't
/// need the parallel tests to finish first. Tests in a `testSequencedGroup` count as
/// parallel: they only wait for each other, and already run in the parallel phase.
let private parseKind (args : string array) : bool option * string array =
  match Array.tryFindIndex (fun a -> a = "--kind") args with
  | None -> None, args
  | Some i ->
    if i + 1 >= args.Length then
      Exception.raiseInternal "--kind needs an argument: sequenced or parallel" []
    let sequenced =
      match args[i + 1] with
      | "sequenced" -> true
      | "parallel" -> false
      | other ->
        Exception.raiseInternal $"--kind {other}: expected sequenced or parallel" []
    Some sequenced, Array.append args[0 .. i - 1] args[i + 2 ..]

/// `--only-groups A,B` keeps only the top-level groups named (`tests/A/...`), and `--skip-groups A,B`
/// drops them. `run-fsharp-tests --split` uses the pair to run the sequenced tests in two
/// processes, a group never divided between them: a sequenced group's tests can depend on what
/// the ones before them left behind.
let private parseGroups
  (flag : string)
  (args : string array)
  : Set<string> option * string array =
  match Array.tryFindIndex (fun a -> a = flag) args with
  | None -> None, args
  | Some i ->
    if i + 1 >= args.Length then
      Exception.raiseInternal $"{flag} needs a comma-separated list of groups" []
    let groups = args[i + 1].Split(',') |> Array.filter ((<>) "") |> Set.ofArray
    Some groups, Array.append args[0 .. i - 1] args[i + 2 ..]

/// Remove the tests not in `keep`, leaving the rest of the tree exactly as it was.
/// Don't swap in Expecto's `Test.filter` or `fromFlatTests`: they rebuild the tree, which
/// changes test names in the JUnit report, and a few full names contain characters XML
/// can't hold, so CircleCI can't read the report.
let rec private keepOnly
  (keep : Set<string list>)
  (path : string list)
  (test : Test)
  : Test option =
  match test with
  | TestCase _ -> if Set.contains path keep then Some test else None
  | TestLabel(label, inner, state) ->
    keepOnly keep (path @ [ label ]) inner
    |> Option.map (fun t -> TestLabel(label, t, state))
  | TestList(tests, state) ->
    match List.choose (keepOnly keep path) tests with
    | [] -> None
    | kept -> Some(TestList(kept, state))
  | Test.Sequenced(how, inner) ->
    keepOnly keep path inner |> Option.map (fun t -> Test.Sequenced(how, t))


[<EntryPoint>]
let main (args : string array) : int =
  try
    let shard, args = parseShard args
    let kind, args = parseKind args
    let onlyGroups, args = parseGroups "--only-groups" args
    let skipGroups, args = parseGroups "--skip-groups" args

    // The suite gets a policy of its own, so no test reads or writes the clone's
    // (`~/.darklang/policy`, the dev store's: a deny-all left there refuses the clone's own
    // gates and printing). Seeded with what a fresh install gets; a test that sets its own
    // still can, and this is what it goes back to. Beside the clone's, not in /tmp: the guards
    // protect a policy directory's ancestors, and /tmp among them would refuse the temp-file
    // tests.
    let darklangDir =
      System.IO.Path.Combine(
        System.Environment.GetFolderPath System.Environment.SpecialFolder.UserProfile,
        ".darklang"
      )
    // A killed run leaves its own behind; sweep the ones whose run is gone.
    if System.IO.Directory.Exists darklangDir then
      for stale in System.IO.Directory.GetDirectories(darklangDir, "test-policy-*") do
        let alive =
          match
            System.Int32.TryParse(System.IO.Path.GetFileName(stale).Substring 12)
          with
          | true, pid ->
            try
              not (System.Diagnostics.Process.GetProcessById pid).HasExited
            with _ ->
              false
          | _ -> false
        if not alive then System.IO.Directory.Delete(stale, true)
    let policyDir =
      System.IO.Path.Combine(
        darklangDir,
        $"test-policy-{System.Environment.ProcessId}"
      )
    if System.IO.Directory.Exists policyDir then
      System.IO.Directory.Delete(policyDir, true)
    LibExecution.HostSecurity.setPolicyDirectory policyDir
    LibDB.PolicyStore.seedInstanceIfMissing
      LibExecution.Permissions.Policy.defaultInstance
    System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
      try
        System.IO.Directory.Delete(policyDir, true)
      with _ ->
        ())
    // Tests has no CLI worker entry point. Use the CLI selected by run-fsharp-tests.
    match System.Environment.GetEnvironmentVariable "DARK_TEST_CLI" with
    | null
    | "" -> ()
    | path -> LibExecution.HostProcess.setProcessLauncher path

    // Most tests don't need trace data on disk; tests that DO check
    // trace contents (CliTraces) flip this to Detailed at their entry.
    LibDB.Tracing.TraceDetail.set LibDB.Tracing.TraceDetail.Off

    // Grow the DB from seed if needed. Builtins are deferred (constructed after
    // hashes are generated) because builtin construction triggers hash lookups.
    (LibDB.Seed.growIfNeeded
      (fun () -> TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
      TestUtils.TestUtils.pmRT
      (fun msg -> System.Console.Error.WriteLine msg))
      .Result
    |> ignore<bool>

    // Before any test touches the store: every CLI instance starts from this.
    Tests.CliInstance.snapshotStore ()

    let tests =
      [ // core
        Tests.Prelude.tests
        Tests.Normalization.tests
        Tests.ProgramTypesToRuntimeTypes.tests
        Tests.Interpreter.tests
        Tests.Scheduler.tests
        Tests.Spread.tests
        Tests.Lists.tests
        Tests.Strings.tests
        Tests.Scalars.tests
        Tests.AnalysisTypes.tests
        Tests.Builtin.tests
        Tests.HostBoundary.tests
        Tests.LibParserRoundTrip.tests
        Tests.LibParser.tests
        Tests.WrittenTypesLoweringParity.tests
        Tests.HttpClient.tests
        Tests.Terminal.tests
        Tests.PTConformance.tests

        // package manager
        Tests.Propagation.tests
        Tests.Draft.tests
        Tests.PackageTests.tests
        Tests.Purge.tests
        Tests.TestResultCache.tests
        Tests.Hashing.tests
        Tests.TraitCallCache.tests
        Tests.Config.tests

        // serialization
        Tests.BinarySerialization.tests
        Tests.DarkTypesSerialization.tests

        // http server
        Tests.HttpServer.tests
        // The in-process cases here are sequenced, because they capture `Console.Out` and
        // that is process-global. The rest run on CLI instances, beside everything else.
        //
        // Every destructive command here passes `--yes`. CI runs with a pty, so the
        // "is anyone watching?" check says yes and the confirmation prompt blocks on a
        // terminal nobody types into; a pipe skips the prompt, so the omission is
        // invisible locally. `packages/darklang/cli/tracing.dark` has the detail.
        Tests.CliTraces.tests
        // Executions: in-process CLI runs, so sequenced like CliTraces (the console capture
        // is process-global), and after it, since it flips trace detail on.
        testSequenced (testList "CliTraceVerbs" Tests.CliTraceVerbs.tests)
        // These suites mostly use private CLI instances. Any case that touches
        // the shared process or store marks itself sequenced.
        testList "CliAuthoring" Tests.CliAuthoring.tests
        testList "CliRename" Tests.CliRename.tests
        testList "CliUndo" Tests.CliUndo.tests
        testList "CliPartialCommit" Tests.CliPartialCommit.tests
        testList "CliWorkspace" Tests.CliWorkspace.tests
        testList "CliScmRegression" Tests.CliScmRegression.tests
        testList "CliPackages" Tests.CliPackages.tests
        Tests.CliScriptLowering.tests

        // cross-cutting
        Tests.Bytes.tests
        Tests.Lww.tests
        Tests.BranchOverlay.tests
        Tests.OpsProjections.tests
        Tests.ReferenceNames.tests
        Tests.Stream.tests
        Tests.Permissions.tests
        Tests.PackagePermissions.tests
        Tests.PermissionEscape.tests
        Tests.PolicyStore.tests
        Tests.Host.tests ]

    let cancelationTokenSource = new System.Threading.CancellationTokenSource()
    let httpClientTestsTask = Tests.HttpClient.init cancelationTokenSource.Token

    // this does async stuff within it, so do not run it from a task/async
    // context or it may hang
    //
    // JoinWith Slash because `--filter`'s own help says "a hierarchy that's slash (/)
    // separated" while Expecto's default separator is a dot. So the filter you write
    // after reading the help matches nothing, and Expecto reports that as
    // "0 tests run - Success!". Slashes also make the hierarchy machine-readable,
    // which dots don't: case names contain dots of their own
    // (`Map.mergeFavoringRight`), so nesting and naming were indistinguishable.
    // `--join-with .` gets the old behaviour back.
    let all = testList "tests" tests

    // A SEQUENCED group is indivisible: its tests run one at a time because they share
    // process state, and several of them depend on what the ones before them left in the
    // store. Split across nodes, each half runs against a store the other half was
    // maintaining. So a sequenced test is owned by the hash of its GROUP and travels with
    // its siblings; only parallel tests, which by definition share nothing, are owned
    // individually.
    let ownedBy (total : int) (t : FlatTest) : int =
      let key =
        match t.sequenced with
        | InParallel -> String.concat "/" t.name
        | _ -> t.name |> List.truncate 2 |> String.concat "/"
      int (TestUtils.TestUtils.stableHash key % uint32 total)

    let sharded =
      match shard with
      | None -> all
      | Some(idx, total) ->
        print $"Running shard {idx} of {total}."
        all
        |> Test.toTestCodeList
        |> List.filter (fun t -> ownedBy total t = idx)
        |> Test.fromFlatTests "/"

    let grouped =
      let group (t : FlatTest) = t.name |> List.tryItem 1 |> Option.defaultValue ""
      let wanted (t : FlatTest) =
        (onlyGroups |> Option.forall (fun gs -> Set.contains (group t) gs))
        && (skipGroups |> Option.forall (fun gs -> not (Set.contains (group t) gs)))
      match onlyGroups, skipGroups with
      | None, None -> sharded
      | _ ->
        let keep =
          sharded
          |> Test.toTestCodeList
          |> List.filter wanted
          |> List.map (fun t -> t.name)
          |> Set.ofList
        keepOnly keep [] sharded |> Option.defaultValue (TestList([], Normal))

    let toRun =
      match kind with
      | None -> grouped
      | Some sequenced ->
        print
          $"""Running the {if sequenced then "sequenced" else "parallel"} tests."""
        let keep =
          grouped
          |> Test.toTestCodeList
          |> List.filter (fun t ->
            match t.sequenced with
            | Synchronous -> sequenced
            | _ -> not sequenced)
          |> List.map (fun t -> t.name)
          |> Set.ofList
        keepOnly keep [] grouped |> Option.defaultValue (TestList([], Normal))

    let exitCode =
      runTestsWithCLIArgs [ Allow_Duplicate_Names; JoinWith "/" ] args toRun

    NonBlockingConsole.wait () // flush stdout
    cancelationTokenSource.Cancel()
    httpClientTestsTask.Wait()
    exitCode
  with e ->
    printException "Outer exception" [] e
    NonBlockingConsole.wait () // flush stdout
    1
