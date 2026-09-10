/// Shared harness for the CLI integration suites (CliSurface, CliScm, CliTraces):
/// builds an ExecutionState with the CLI's production builtin set, runs a command
/// in-process with `Console.Out` redirected, and hands back the captured stdout.
module Tests.CliTestHarness

open Expecto
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Dval = LibExecution.Dval

open TestUtils.TestUtils

/// Build an ExecutionState wired up with the same builtin set the CLI uses in
/// production. Re-built per test so trace-store side effects don't leak across tests.
/// The CLI's builtin table, built once for the whole file.
///
/// `buildState` runs per test and `builtinsToUse` combines and revalidates about a
/// thousand builtins every time it is called. Nothing here varies between tests: they all
/// drive the same store through `LibDB.PackageManager.pt`, which is itself a singleton.
let private cliBuiltins : Lazy<RT.Builtins> =
  lazy (Builtins.CliHost.Libs.Cli.builtinsToUse ())

let buildState () : Task<RT.ExecutionState> =
  task {
    let pmPTValue = pmPT
    let builtins = cliBuiltins.Force()
    // Read evaluated package values as the CLI does. The PT-to-RT value converter
    // handles literals only and turns computed values into Unit.
    let pmRT =
      { PT2RT.PackageManager.toRT builtins.values pmPTValue with
          getValue = LibDB.PackageManager.rt.getValue }
    let program : RT.Program = { dbs = Map.empty }

    let notify
      (_state : RT.ExecutionState)
      (_vm : RT.VMState)
      (_msg : string)
      (_metadata : Metadata)
      =
      uply { return () }

    let sendException
      (_ : RT.ExecutionState)
      (_ : RT.VMState)
      (_metadata : Metadata)
      (_exn : exn)
      =
      uply { return () }

    // The same host posture `Cli.fs` builds for CLI control code: `createState` now defaults
    // access to deny-all (an embedder that forgets a policy gets a confined run), so without
    // this every dispatched command dies on its first effect. Guest `run`/`eval` inside these
    // tests still narrow themselves, which is what the guest tests assert.
    // `dark run` / `dark eval` inside a dispatched command build their guest state from the
    // STORED instance policy, and a store with no policy file is deny-all -- so without this
    // every test that evals is denied `package-read` before it can resolve a name. The real
    // CLI seeds the same default on startup (`Cli.fs`), so seeding it here makes the harness
    // match an install rather than granting the tests anything an install does not have.
    //
    // Into `rundir`, never the real `~/.darklang/policy`: the suite must not write the
    // developer's own policy, and in the container that path is not writable anyway. The
    // override is process-wide and deliberately never disposed -- every CLI test wants this
    // same policy, and restoring it per test would race the sequenced dispatch.
    let policyDir = System.IO.Path.Combine(LibConfig.Config.runDir, "test-policy")
    System.IO.Directory.CreateDirectory policyDir |> ignore<System.IO.DirectoryInfo>
    LibExecution.HostSecurity.policyDirectoryForTesting policyDir
    |> ignore<System.IDisposable>

    // `defaultInstance` PLUS package-write, granted once here for every test.
    //
    // These tests drive authoring through `dark eval`, which is guest code, and a guest has no
    // package-write by default. Granting it inside a single test instead would leak into every
    // test after it in the same store, so a later test would pass or fail on runner order.
    //
    // What a GUEST may do without the grant is still tested, in `PermissionEscape.Tests`.
    let testInstancePolicy =
      LibExecution.Permissions.Policy.allowEffects (
        Set.add
          LibExecution.Effects.Effect.PackageWrite
          (LibExecution.Permissions.Policy.coverableEffects
            LibExecution.Permissions.Policy.defaultInstance
           |> Option.defaultValue Set.empty)
      )

    LibDB.PolicyStore.seedInstanceIfMissing testInstancePolicy

    let! bundled = LibDB.ProgramTypes.Fn.hashesOwnedBy "Darklang" |> Ply.toTask

    return
      { Exe.setInstancePolicy
          LibExecution.Permissions.Policy.allowAll
          (Exe.createState builtins pmRT Exe.noTracing sendException notify program) with
          canManagePolicies = true
          canUsePrivateNetworkHttp = true
          isBundledPackageFn = fun (RT.Hash h) -> bundled.Contains h }
  }

/// What a CLI test drives.
///
/// `InProcess` dispatches `executeCliCommand` here, against the one store this process
/// points at. That is the only way to reach F# state from a test -- the trace store, the
/// policy store, a SQL assertion -- and the price is that every such test has to run alone.
///
/// `Instance` starts the real binary against a store of its own. A command costs a process
/// start instead of a dispatch, and in exchange the test has nothing to share, so it runs
/// beside every other test. Use it unless the test reaches into F#.
type Target =
  | InProcess of RT.ExecutionState
  | Instance of Tests.CliInstance.T


/// The execution state behind an in-process target.
///
/// A test that needs this -- to call a package fn directly, or to hand a narrowed state to
/// the CLI -- has to be a `cliTest`. An instance is a separate process; there is no F#
/// state of its to reach.
let executionState (target : Target) : RT.ExecutionState =
  match target with
  | InProcess state -> state
  | Instance _ ->
    Expecto.Tests.failtestf
      "this test reaches into F# execution state, so it must be a cliTest, not an instanceTest"


/// How long one CLI command may take before the test says so.
///
/// A command reading stdin with nobody there waits rather than fails, and its output is being
/// captured, so the hang is silent and the log names no culprit. Generous on purpose: this turns
/// "forever" into a named failure, it does not police speed.
let private runCliTimeout = System.TimeSpan.FromMinutes 2.0

/// Invoke the CLI dispatch with the given args (e.g. `["traces"; "list"]`) and return the
/// trimmed captured stdout.
///
/// `NonBlockingConsole`'s capture, not `Console.SetOut`. Everything the CLI prints goes
/// through `Prelude.print`, so this catches the same text, and it catches only THIS flow's:
/// `Console.SetOut` is process-global, so it also swallowed whatever the rest of the suite
/// printed while it was open, which is why these tests had to be sequenced against every
/// other test rather than only against each other.
let rec runCli (target : Target) (args : string list) : Task<string> =
  match target with
  | Instance i -> Tests.CliInstance.run i args
  | InProcess state -> runCliInProcess state args

and private runCliInProcess
  (state : RT.ExecutionState)
  (args : string list)
  : Task<string> =
  task {
    let argsDval = args |> List.map RT.DString |> Dval.list RT.KTString
    let fnName =
      RT.FQFnName.fqPackage (LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())

    // Drain prior work queued in NonBlockingConsole, so it stays out of our capture.
    NonBlockingConsole.wait ()

    if not (NonBlockingConsole.startCapture ()) then
      return Tests.failtestf "runCli: a capture was already open (nested runCli?)"

    try
      let execution = Exe.executeFunction state fnName [] (NEList.singleton argsDval)

      // Bounds the WAIT, not the work: the call is not cancellable, so it finishes into a
      // buffer nobody reads while the test fails with the command's name.
      let! finished = Task.WhenAny(execution, Task.Delay runCliTimeout)

      if not (System.Object.ReferenceEquals(finished, execution :> Task)) then
        return
          Tests.failtestf
            "runCli timed out after %A: dark %s"
            runCliTimeout
            (String.concat " " args)

      let! result = execution
      // `Stdlib.printLine` queues to a background thread; drain before
      // reading the buffer or we capture nothing.
      NonBlockingConsole.wait ()
      match result with
      | Ok _ -> return (NonBlockingConsole.stopCapture ()).Trim()
      | Error(rte, _) -> return Tests.failtestf "runCli errored: %A" rte
    finally
      NonBlockingConsole.stopCapture () |> ignore<string>
  }

/// `runCli`, but a runtime error is a result rather than the end of the test.
///
/// For the sweeps: one command that throws must not stop the other sixty from being checked,
/// and WHICH command threw is the finding, so it has to come back as a value.
let runCliCatching
  (target : Target)
  (args : string list)
  : Task<Result<string, string>> =
  task {
    try
      let! output = runCli target args
      return Ok output
    with e ->
      return Error(e.Message.Split('\n')[0])
  }

/// Author a fn through the CLI (`fn <name> <decl>`), discarding the output.
/// For sites that assert on the authoring output itself, use `runCli` directly.
let author (target : Target) (name : string) (decl : string) : Task<unit> =
  task {
    let! _ = runCli target [ "fn"; name; decl ]
    return ()
  }

/// Teardown for tests that end off main: switch back, then archive each named
/// branch with `-y`. Asserts nothing -- a test that checks the archive output
/// keeps its own runCli + Expect.
let archiveBranches (target : Target) (names : List<string>) : Task<unit> =
  task {
    let! _ = runCli target [ "switch"; "main" ]
    for name in names do
      let! _ = runCli target [ "branch"; "archive"; name; "-y" ]
      ()
  }

/// The trace id in a `traces list 1 --json` output.
let parseTraceID (json : string) : string =
  let split = json.Split("\"traceId\":\"")
  if split.Length < 2 then
    Tests.failtestf "Couldn't parse trace id from %s" json
  else
    let parts = split[1].Split('"')
    if parts.Length = 0 then
      Tests.failtestf "Couldn't parse trace id from %s" json
    else
      parts[0]


// ─── Test builders ────────────────────────────────────────────────────────

/// Wrap a fresh ExecutionState in a task.
let withState (f : Target -> Task<unit>) : Task<unit> =
  task {
    let! state = buildState ()
    do! f (InProcess state)
  }

/// `cliTest "name" body` collapses the `testTask "..." { do! withState ... }`
/// boilerplate. Body receives the target and returns a Task<unit>.
///
/// In-process, so the whole list it lives in has to be sequenced. Prefer `instanceTest`
/// for anything that only reads what a command printed.
let cliTest (name : string) (body : Target -> Task<unit>) : Test =
  testTask name { do! withState body }

/// `cliTest`, but the commands run as a child against a store of this test's own.
///
/// Nothing to serialise against, so these need no `testSequenced` and run as wide as the
/// rest of the suite. The body cannot reach F# state; a test that needs to stays on
/// `cliTest`.
let instanceTest (name : string) (body : Target -> Task<unit>) : Test =
  testTask name {
    let i = Tests.CliInstance.create ()
    try
      do! body (Instance i)
    finally
      Tests.CliInstance.dispose i
  }

/// For a test that must start on main and might not end there. The precondition is ASSERTED, not
/// arranged: a test that silently switched itself back to main would hide whichever earlier test left
/// the store on a branch, and that one is the bug. The switch back runs whether the body passed or not,
/// so one polluter is named once rather than failing everything after it.
let cliTestOnMain (name : string) (body : Target -> Task<unit>) : Test =
  cliTest name (fun state ->
    task {
      let! where = runCli state [ "branch" ]
      Expect.stringContains
        where
        "on main"
        $"{name}: an earlier test left the store on a branch"
      // F#'s task `try/finally` takes only a synchronous finally, hence the captured exception.
      let! outcome =
        task {
          try
            do! body state
            return None
          with e ->
            return
              Some(System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture e)
        }
      let! _ = runCli state [ "switch"; "main" ]
      outcome |> Option.iter (fun e -> e.Throw())
    })

/// Adds a `traces delete --all --yes` step before the body, so tests that examine
/// the trace store start from a known-empty state.
/// Turns recording ON and empties the trace store, so a test that examines traces starts from
/// a known state, and turns it off again afterwards.
///
/// The recording level is carried HERE rather than by a test placed ahead of these in the
/// list. A test that exists to set process state cannot survive sharding: `--shard` partitions
/// by test, so the toggle and the tests it was meant to enable land on different nodes, and
/// what you get is every trace assertion reading "Recording is OFF".
///
/// Safe to flip per test only because everything built on `cliTest` is `testSequenced`.
let cliTestWithFreshTraces (name : string) (body : Target -> Task<unit>) : Test =
  cliTest name (fun state ->
    task {
      LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.On
      try
        // Recording is already on, so this clears the delete's own trace along with the rest.
        let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
        do! body state
      finally
        LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
    })


// ─── Base CLI command tests ───────────────────────────────────────────────

/// A variant of `runCli` that also reports what the process would EXIT with. `executeCliCommand`
/// returns it; plain `runCli` reads only the printed text.
let rec runCliWithExit
  (target : Target)
  (args : string list)
  : Task<string * int64> =
  match target with
  | Instance i -> Tests.CliInstance.runWithExit i args
  | InProcess state -> runCliWithExitInProcess state args

and private runCliWithExitInProcess
  (state : RT.ExecutionState)
  (args : string list)
  : Task<string * int64> =
  task {
    let argsDval = args |> List.map RT.DString |> Dval.list RT.KTString
    let fnName =
      RT.FQFnName.fqPackage (LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())
    NonBlockingConsole.wait ()

    if not (NonBlockingConsole.startCapture ()) then
      return Tests.failtestf "runCliWithExit: a capture was already open"

    try
      let! result = Exe.executeFunction state fnName [] (NEList.singleton argsDval)
      NonBlockingConsole.wait ()
      let printed = (NonBlockingConsole.stopCapture ()).Trim()
      match result with
      | Ok(RT.DInt64 code) -> return (printed, code)
      | Ok(RT.DInt code) -> return (printed, int64 (RT.DarkInt.toBigInt code))
      | Ok other ->
        return Tests.failtestf "executeCliCommand returned a non-int: %A" other
      | Error(rte, _) -> return Tests.failtestf "runCliWithExit errored: %A" rte
    finally
      NonBlockingConsole.stopCapture () |> ignore<string>
  }

/// swallowed. Not `Call-stack:`: `eval` prints that on a user error, which is a refusal, not a crash.
let looksLikeARuntimeFailure (output : string) : Option<string> =
  let patterns =
    [ "Encountered a Runtime Error"
      "No matching case found"
      "couldn't be found"
      "Internal error:"
      @"expects .* but got" ]
  output.Split('\n')
  |> Array.tryFind (fun line ->
    patterns
    |> List.exists (fun p -> System.Text.RegularExpressions.Regex.IsMatch(line, p)))
