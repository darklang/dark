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
let buildState () : Task<RT.ExecutionState> =
  task {
    let pmPTValue = pmPT
    let builtins = Builtins.CliHost.Libs.Cli.builtinsToUse ()
    let pmRT = PT2RT.PackageManager.toRT builtins.values pmPTValue
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

    return Exe.createState builtins pmRT Exe.noTracing sendException notify program
  }

/// Invoke the CLI dispatch with the given args (e.g. `["traces"; "list"]`) and return
/// the trimmed captured stdout, with `Console.Out` redirected to a `StringWriter` for
/// the duration. The surrounding `testSequenced` keeps the process-global
/// `Console.SetOut` from racing across tests.
let runCli (state : RT.ExecutionState) (args : string list) : Task<string> =
  task {
    let argsDval = args |> List.map RT.DString |> Dval.list RT.KTString
    let fnName =
      RT.FQFnName.fqPackage (LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())

    // Drain prior work queued in NonBlockingConsole, so it stays out of our capture.
    NonBlockingConsole.wait ()

    let captured = new System.IO.StringWriter()
    let originalOut = System.Console.Out
    try
      System.Console.SetOut(captured)
      let! result = Exe.executeFunction state fnName [] (NEList.singleton argsDval)
      // `Stdlib.printLine` queues to a background thread; drain before
      // reading the StringWriter or we capture nothing.
      NonBlockingConsole.wait ()
      match result with
      | Ok _ -> return captured.ToString().Trim()
      | Error(rte, _) ->
        System.Console.SetOut(originalOut)
        return Tests.failtestf "runCli errored: %A" rte
    finally
      System.Console.SetOut(originalOut)
  }

/// `runCli`, but a runtime error is a result rather than the end of the test.
///
/// For the sweeps: one command that throws must not stop the other sixty from being checked,
/// and WHICH command threw is the finding, so it has to come back as a value.
let runCliCatching
  (state : RT.ExecutionState)
  (args : string list)
  : Task<Result<string, string>> =
  task {
    try
      let! output = runCli state args
      return Ok output
    with e ->
      return Error(e.Message.Split('\n')[0])
  }

/// Author a fn through the CLI (`fn <name> <decl>`), discarding the output.
/// For sites that assert on the authoring output itself, use `runCli` directly.
let author (state : RT.ExecutionState) (name : string) (decl : string) : Task<unit> =
  task {
    let! _ = runCli state [ "fn"; name; decl ]
    return ()
  }

/// Teardown for tests that end off main: switch back, then archive each named
/// branch with `-y`. Asserts nothing -- a test that checks the archive output
/// keeps its own runCli + Expect.
let archiveBranches (state : RT.ExecutionState) (names : List<string>) : Task<unit> =
  task {
    let! _ = runCli state [ "switch"; "main" ]
    for name in names do
      let! _ = runCli state [ "branch"; "archive"; name; "-y" ]
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
let withState (f : RT.ExecutionState -> Task<unit>) : Task<unit> =
  task {
    let! state = buildState ()
    do! f state
  }

/// `cliTest "name" body` collapses the `testTask "..." { do! withState ... }`
/// boilerplate. Body receives the state and returns a Task<unit>.
let cliTest (name : string) (body : RT.ExecutionState -> Task<unit>) : Test =
  testTask name { do! withState body }

/// For a test that must start on main and might not end there. The precondition is ASSERTED, not
/// arranged: a test that silently switched itself back to main would hide whichever earlier test left
/// the store on a branch, and that one is the bug. The switch back runs whether the body passed or not,
/// so one polluter is named once rather than failing everything after it.
let cliTestOnMain (name : string) (body : RT.ExecutionState -> Task<unit>) : Test =
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
let cliTestWithFreshTraces
  (name : string)
  (body : RT.ExecutionState -> Task<unit>)
  : Test =
  cliTest name (fun state ->
    task {
      let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
      do! body state
    })


// ─── Base CLI command tests ───────────────────────────────────────────────

/// A variant of `runCli` that also reports what the process would EXIT with. `executeCliCommand`
/// returns it; plain `runCli` reads only the printed text.
let runCliWithExit
  (state : RT.ExecutionState)
  (args : string list)
  : Task<string * int64> =
  task {
    let argsDval = args |> List.map RT.DString |> Dval.list RT.KTString
    let fnName =
      RT.FQFnName.fqPackage (LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())
    NonBlockingConsole.wait ()
    let captured = new System.IO.StringWriter()
    let originalOut = System.Console.Out
    try
      System.Console.SetOut(captured)
      let! result = Exe.executeFunction state fnName [] (NEList.singleton argsDval)
      NonBlockingConsole.wait ()
      match result with
      | Ok(RT.DInt64 code) -> return (captured.ToString().Trim(), code)
      | Ok(RT.DInt code) ->
        return (captured.ToString().Trim(), int64 (RT.DarkInt.toBigInt code))
      | Ok other ->
        System.Console.SetOut(originalOut)
        return Tests.failtestf "executeCliCommand returned a non-int: %A" other
      | Error(rte, _) ->
        System.Console.SetOut(originalOut)
        return Tests.failtestf "runCliWithExit errored: %A" rte
    finally
      System.Console.SetOut(originalOut)
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
