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


[<EntryPoint>]
let main (args : string array) : int =
  try
    let shard, args = parseShard args
    // Most tests don't need trace data on disk; tests that DO check
    // trace contents (CliTraces) flip this to Detailed at their entry.
    LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off

    // Grow the DB from seed if needed. Builtins are deferred (constructed after
    // hashes are generated) because builtin construction triggers hash lookups.
    (LibDB.Seed.growIfNeeded
      // The test store is built from this repo's own `packages/`, so it is the
      // trusted-seed case; PermissionEscape.Tests covers the guest one.
      LibDB.Seed.TrustedSeed
      (fun () -> TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
      TestUtils.TestUtils.pmRT
      (fun msg -> System.Console.Error.WriteLine msg))
      .Result
    |> ignore<bool>

    let tests =
      [ // core
        Tests.Prelude.tests
        Tests.ProgramTypesToRuntimeTypes.tests
        Tests.Interpreter.tests
        Tests.AnalysisTypes.tests
        Tests.Builtin.tests
        Tests.HostBoundary.tests
        Tests.DvalReprInternalQueryable.tests
        Tests.LibParserRoundTrip.tests
        Tests.LibParser.tests
        Tests.WrittenTypesLoweringParity.tests
        Tests.HttpClient.tests
        Tests.Terminal.tests
        Tests.AtRestTypeChecker.tests
        Tests.PTConformance.tests

        // package manager
        Tests.Propagation.tests
        Tests.Draft.tests
        Tests.Purge.tests
        Tests.Hashing.tests
        Tests.Config.tests

        // serialization
        Tests.BinarySerialization.tests
        Tests.DarkTypesSerialization.tests

        // http server
        Tests.HttpServer.tests
        // Sequenced, because these capture `Console.Out` and that is process-global.
        //
        // Every destructive command here passes `--yes`. CI runs with a pty, so the
        // "is anyone watching?" check says yes and the confirmation prompt blocks on a
        // terminal nobody types into; a pipe skips the prompt, so the omission is
        // invisible locally. `packages/darklang/cli/tracing.dark` has the detail.
        Tests.CliTraces.tests
        // Instance-backed: their commands run as children with a store each, so unlike
        // the rest of the CLI suites these need no sequencing.
        testList "CliJson" Tests.CliJson.tests
        testList "CliAuthoring" Tests.CliAuthoring.tests
        testList "CliSyncSurface" Tests.CliSyncSurface.tests
        testList "CliWorkspace" Tests.CliWorkspace.tests
        testList "CliScmRegression" Tests.CliScmRegression.tests
        testList "CliPackages" Tests.CliPackages.tests
        Tests.CliScriptLowering.tests
        Tests.Toplevels.tests

        // cross-cutting
        Tests.LibExecution.tests.Force()

        Tests.Blob.tests
        Tests.OpTransport.tests
        Tests.Lww.tests
        Tests.PropagationPolicy.tests
        Tests.BranchOverlay.tests
        Tests.OpsProjections.tests
        Tests.MultiInstance.tests
        Tests.MultiInstanceDark.tests
        Tests.Stream.tests
        Tests.Permissions.tests
        Tests.PackagePermissions.tests
        Tests.PermissionEscape.tests
        Tests.PolicyStore.tests
        Tests.Host.tests ]

    let cancelationTokenSource = new System.Threading.CancellationTokenSource()
    let httpClientTestsTask = Tests.HttpClient.init cancelationTokenSource.Token

    // Generate this so that we can see if the format has changed in a git diff
    BinarySerialization.generateTestFiles ()

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

    let toRun =
      match shard with
      | None -> all
      | Some(idx, total) ->
        print $"Running shard {idx} of {total}."
        all
        |> Test.toTestCodeList
        |> List.filter (fun t -> ownedBy total t = idx)
        |> Test.fromFlatTests "/"

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
