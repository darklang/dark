/// Executions: a run kept beside its trace, resumed and forked by replaying the log of its
/// effectful calls (`docs/processes.md`, "Executions"). Driven through the CLI, since that is
/// where a run is made, suspended and taken up again.
///
/// The cases here need more than the CLI shows: the log's rows, the retention seams, a row the
/// CLI cannot make, or suspending the foreground run. What a person sees of resume, fork, rerun
/// and `traces show` is tested in Dark, in `packages/darklang/tests/cli/traceVerbs.dark`.
module Tests.CliTraceVerbs

open Expecto
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

open TestUtils.TestUtils
open Tests.CliTestHarness
open Tests.CliDsl

module RT = LibExecution.RuntimeTypes
module Traces = LibDB.Traces


let private latest () : Task<Traces.Trace> =
  task {
    let! rows = Traces.list 1
    match rows with
    | e :: _ -> return e
    | [] -> return failtest "no execution was recorded"
  }

/// A run's id, longer than `dark traces` shows it: the first twelve characters (eight is what
/// the CLI prints; twelve keeps the tests clear of a chance collision).
let private prefixOf (e : Traces.Trace) : string = (string e.id).Substring(0, 12)

let private latestPrefix () : Task<string> =
  task {
    let! e = latest ()
    return prefixOf e
  }

/// Two uuids, so a replay is told apart from a rerun by its output.
let private twoUuids =
  "let a = Stdlib.Uuid.generate()\nlet b = Stdlib.Uuid.generate()\nStdlib.String.join([ Stdlib.display(a), Stdlib.display(b) ], \" \")"

let private words (s : string) : string list =
  s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray


/// Save a trace with optional CLI input.
let private storeInvocation
  (input : Option<Traces.ScriptOrEvalInput>)
  : System.Guid =
  let id = System.Guid.NewGuid()
  Traces.create id "eval" "expression" (RT.DString "") input Traces.Done None
  id

let private setColumn (id : System.Guid) (column : string) value : unit =
  Sql.query
    $"UPDATE trace_cli_invocations SET {column} = @value WHERE trace_id = @id"
  |> Sql.parameters [ "value", value; "id", Sql.uuid id ]
  |> Sql.executeStatementSync

let private hasInvocation (id : System.Guid) : bool =
  Sql.query "SELECT 1 AS n FROM trace_cli_invocations WHERE trace_id = @id"
  |> Sql.parameters [ "id", Sql.uuid id ]
  |> Sql.executeExistsSync

let private scriptOrEvalInputPersistence =
  cliTestWithFreshTraces
    "CLI invocations round-trip through their table and incomplete ones are refused"
    (fun _state ->
      task {
        let args = [ "a b"; ""; "--help"; "é"; "a\"b\\c" ]
        let script =
          Traces.RunScript(
            "a path/脚本.dark",
            "args",
            args,
            { allowHarmful = true; sandbox = true }
          )
        for input in [ Traces.EvalExpression("1 + 2", true); script ] do
          let id = storeInvocation (Some input)
          let! read = Traces.getCliInput id
          Expect.equal read (Some input) "the invocation reads back whole"
          let! row = Traces.get id
          Expect.equal (row |> Option.bind _.cli) (Some input) "and through the row"

        let served = storeInvocation None
        Expect.isFalse
          (hasInvocation served)
          "a served request stores no invocation"
        let! read = Traces.getCliInput served
        Expect.isNone read "so it has none to read"

        // Reject incomplete or invalid invocation rows.
        let id = storeInvocation (Some script)
        for column, bad in
          [ "kind", Sql.string "nonsense"
            "kind", Sql.string "eval"
            "source", Sql.dbnull
            "filename", Sql.dbnull
            "args", Sql.dbnull
            "allow_harmful", Sql.dbnull
            "sandbox", Sql.dbnull
            "sandbox", Sql.int 2 ] do
          Expect.throws
            (fun () -> setColumn id column bad)
            $"the store refuses a missing or malformed {column}"
        let! read = Traces.getCliInput id
        Expect.equal read (Some script) "and the invocation is left whole"

        // Removing the retired warning mode preserves recorded invocations.
        Sql.query
          "ALTER TABLE trace_cli_invocations ADD COLUMN warn_permissions INTEGER NOT NULL DEFAULT 0"
        |> Sql.executeStatementSync
        LibDB.Releases.removeTraceWarningColumn ()
        LibDB.Releases.removeTraceWarningColumn ()
        let! afterCleanup = Traces.getCliInput id
        Expect.equal afterCleanup (Some script) "cleanup preserves the invocation"
        let obsoleteColumn =
          Sql.query
            "SELECT 1 FROM pragma_table_info('trace_cli_invocations') WHERE name = 'warn_permissions'"
          |> Sql.executeExistsSync
        Expect.isFalse obsoleteColumn "the retired column is absent"

        // The reader must also reject malformed argument bytes.
        for bad in
          [ [| 0x01uy |]
            // One argument, one byte long, and that byte is not UTF-8.
            [| 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0xFFuy |] ] do
          let id = storeInvocation (Some script)
          setColumn id "args" (Sql.bytes bad)
          let! read = Traces.getCliInput id
          Expect.isNone read "malformed arguments are refused"

        LibDB.Tracing.TraceRetention.deleteTraces [ string id ]
        Expect.isFalse (hasInvocation id) "deleting a trace deletes its invocation"
      })


/// Saved arguments must count towards the trace's storage limit.
let private invocationCountsTowardsTraceWeight =
  cliTestWithFreshTraces
    "a script's saved arguments count towards its trace's weight"
    (fun state ->
      task {
        let path =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            $"dark-trace-weight-{System.Guid.NewGuid()}.dark"
          )
        let argument = String.replicate 50_000 "x"
        try
          System.IO.File.WriteAllText(path, "0")
          let! _, exitCode = runCliWithExit state [ "run"; path; argument ]
          Expect.equal exitCode 0L "the script runs"
          let! (recorded : Traces.Trace) = latest ()
          let! bytes =
            Sql.query
              "SELECT t.bytes, LENGTH(i.args) AS args
               FROM traces t JOIN trace_cli_invocations i ON i.trace_id = t.id
               WHERE t.id = @id"
            |> Sql.parameters [ "id", Sql.uuid recorded.id ]
            |> Sql.executeRowAsync (fun read ->
              read.int64 "bytes", read.int64 "args")
          let weight, args = bytes
          Expect.isGreaterThan args 50_000L "the argument is saved"
          Expect.isGreaterThanOrEqual
            weight
            args
            "and counted in the trace's weight"
        finally
          if System.IO.File.Exists path then System.IO.File.Delete path
      })


/// Identify HTTP traces by input name, not display text.
let private resumeNamesServedRequests =
  cliTestWithFreshTraces
    "resume tells a served request from a CLI run by its row, not its entry"
    (fun state ->
      task {
        let served = System.Guid.NewGuid()
        Traces.create
          served
          "GET /orders"
          Traces.servedRequestInputName
          (RT.DString "")
          None
          Traces.Done
          None
        let! output, exitCode =
          runCliWithExit state [ "traces"; "resume"; string served ]
        Expect.equal exitCode 1L "a served request is not resumed"
        Expect.stringContains
          output
          "was a served request (GET /orders)"
          "resume names the request"
        Expect.stringContains output "traces show <fn>" "and says where to look"

        // A CLI row whose entry looks like a request path is still a CLI row.
        let cli = System.Guid.NewGuid()
        Traces.create cli "GET /orders" "code" (RT.DString "") None Traces.Done None
        let! output, exitCode =
          runCliWithExit state [ "traces"; "resume"; string cli ]
        Expect.equal exitCode 1L "with no invocation it is refused"
        Expect.isFalse
          (output.Contains "served request (")
          $"but not called a served request: {output}"
      })


let private legacyScriptDoesNotGuessItsInput =
  cliTestWithFreshTraces
    "CLI invocation refuses incomplete legacy script rows without guessing"
    (fun state ->
      task {
        let id = System.Guid.NewGuid()
        let source = "Stdlib.printLine(\"LEGACY_EFFECT\")\n0"
        Traces.create
          id
          "run old.dark"
          "code"
          (RT.DString source)
          None
          Traces.Suspended
          None
        for verb in [ "resume"; "rerun" ] do
          let! output, exitCode = runCliWithExit state [ "traces"; verb; string id ]
          Expect.equal exitCode 1L "missing invocation data is a refusal"
          Expect.isFalse
            ((plain output).Split('\n')
             |> Array.exists (fun line -> line = "LEGACY_EFFECT"))
            "legacy source is never guessed into a live run"
        let! (row : Option<Traces.Trace>) = Traces.get id
        Expect.equal
          (row |> Option.map (fun row -> row.status))
          (Some Traces.Suspended)
          "refusal preserves the recorded run"
      })


/// Corrupt display data or results must not prevent rerunning intact input.
let private rerunIsIndependentOfResult =
  cliTestWithFreshTraces
    "traces rerun survives a corrupt result and input value"
    (fun state ->
      task {
        let typedScript =
          Traces.RunScript(
            "deleted.dark",
            "if args == [\"saved input\", \"--trace\"] then 7 else 1",
            [ "saved input"; "--trace" ],
            { Traces.ScriptOptions.defaults with sandbox = true }
          )
        let cases =
          [ Traces.EvalExpression("42", false), 0L, "42"; typedScript, 7L, "" ]
        for input, expectedExit, expectedOutput in cases do
          let id = System.Guid.NewGuid()
          let entry, inputName, inputValue =
            Traces.ScriptOrEvalInput.toTraceParams input
          Traces.create
            id
            entry
            inputName
            inputValue
            (Some input)
            Traces.Suspended
            None
          let corruptBytes = [| 0x00uy; 0x01uy; 0x02uy |]
          let update column =
            let _ =
              Sql.executeTransactionSync
                [ $"UPDATE traces SET {column} = @bad WHERE id = @id",
                  [ [ "bad", Sql.bytes corruptBytes; "id", Sql.uuid id ] ] ]
            ()
          update "result_value"
          let! output, exitCode =
            runCliWithExit state [ "traces"; "rerun"; string id ]
          Expect.equal exitCode expectedExit "rerun uses the intact invocation"
          Expect.stringContains
            output
            expectedOutput
            "the saved source and arguments execute"
          update "input_value"
          let! _, exitCode = runCliWithExit state [ "traces"; "rerun"; string id ]
          Expect.equal exitCode expectedExit "rerun does not need the input value"
      })


let private suspendThenResume =
  cliTestWithFreshTraces
    "a run suspended midway resumes with its first effects answered from the log"
    (fun state ->
      task {
        // The pause between the two uuids waits for a file this test creates, and the run says
        // it has reached the pause by writing another. So the suspend lands after the first
        // uuid and before the second however slow the machine is; a fixed sleep raced it. The
        // wait polls with reads and sleeps, which a resume answers from the log; a shell would
        // be a process, which it cannot.
        let dir =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            $"dark-suspend-{System.Guid.NewGuid()}"
          )
        System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
        let started = System.IO.Path.Combine(dir, "started")
        let gate = System.IO.Path.Combine(dir, "gate")
        let release () = System.IO.File.WriteAllText(gate, "")
        let program =
          $"let a = Stdlib.Uuid.generate()\nlet _ = Stdlib.Cli.FileSystem.writeFile(\"{started}\", Stdlib.Utf8.encode(\"\"))\nlet _ = Stdlib.List.fold(Stdlib.List.range(1, 6000), false, fun(opened, _) -> if opened then true else if Stdlib.Result.isOk(Stdlib.Cli.FileSystem.readFile(\"{gate}\")) then true else (let _ = Stdlib.Cli.Posix.sleep(20.0) in false))\nlet b = Stdlib.Uuid.generate()\nStdlib.String.join([ Stdlib.display(a), Stdlib.display(b) ], \" \")"
        // The grant lands in the shared store, so it is taken back whatever happens.
        let grant = [ "file"; "read+write"; dir ]
        let! _ = runCli state ([ "permissions"; "allow" ] @ grant)
        try
          let running = runCli state [ "eval"; program ]
          let deadline = System.DateTime.UtcNow.AddMinutes 2.
          while not (System.IO.File.Exists started)
                && System.DateTime.UtcNow < deadline do
            do! Task.Delay 10
          Expect.isTrue (System.IO.File.Exists started) "the run reached the pause"
          let! suspended = Traces.Foreground.suspend ()
          Expect.isSome suspended "the foreground run was suspended"
          let! e = latest ()
          Expect.equal e.status Traces.Suspended "and marked so"
          let! log = Traces.log e.id
          Expect.equal
            (log
             |> List.filter (fun (_, _, _, call) -> call.builtin = "uuidGenerate")
             |> List.length)
            1
            "the log has the first uuid and nothing after the pause"
          // The interrupted run goes on (in the CLI it would have exited) and must not
          // overwrite what the suspend stored.
          release ()
          let! first = running
          let! e = latest ()
          Expect.equal
            e.status
            Traces.Suspended
            "the run's own ending left the suspend alone"
          // The log ends inside the pause, so the resume reads on live and finds the gate open.
          let! resumed = runCli state [ "exec"; "resume"; prefixOf e ]
          let last = resumed.Split('\n') |> Array.last
          match words first, words last with
          | [ a1; _ ], [ a2; b2 ] ->
            Expect.equal a2 a1 "the first uuid came from the log"
            Expect.isTrue (b2.Length > 30) "the second was made live"
          | _ -> failtest $"unexpected outputs: {first} / {last}"
        finally
          // Open even if a check failed, so the run still in flight can finish.
          release ()
          (runCli state ([ "permissions"; "remove" ] @ grant)).Wait()
          System.IO.Directory.Delete(dir, true)
      })


/// A spawned child runs on another core, so its first call can be logged before its parent's
/// spawn is. The log is in completion order, and matching the resumed processes by where they
/// first appear in it handed the parent the child's rows, and the resume refused at step 0.
/// A loaded machine did this now and then; the log here is in that order on purpose. Driven
/// through the tracer's hooks, since the CLI harness runs unscheduled and has no expression
/// process to mismatch.
let private aChildLoggedBeforeItsSpawnStillReplays =
  test "a resume replays a child that was logged before the spawn that started it" {
    let parent = System.Guid.NewGuid()
    let child = System.Guid.NewGuid()
    let childValue = RT.DString "the child's uuid"
    let call builtin spawned : LibDB.Tracing.RecordedCall =
      { builtin = builtin; args = None; spawned = spawned }
    let log =
      [ child, 0L, RT.Tracing.ReplayStep.Serve childValue, call "uuidGenerate" None
        parent, 0L, RT.Tracing.ReplayStep.PerformOnce, call "execSpawn" (Some child)
        parent,
        1L,
        RT.Tracing.ReplayStep.Serve(RT.DString "the parent's uuid"),
        call "uuidGenerate" None ]
    let tracer =
      LibDB.Tracing.createReplayTracer
        (LibExecution.AnalysisTypes.TraceID.create ())
        "eval"
        "expression"
        RT.DUnit
        log
    // The CLI starts the expression; the expression spawns.
    let expression = tracer.executionTracing.forProcess (System.Guid.NewGuid())
    let spawnOrd = expression.nextEffect ()
    match expression.replayEffect spawnOrd "execSpawn" [||] with
    | RT.Tracing.ReplayStep.PerformOnce -> ()
    | other -> failtest $"the expression was not handed its own spawn: {other}"
    let spawned = expression.forProcess (System.Guid.NewGuid())
    let childOrd = spawned.nextEffect ()
    match spawned.replayEffect childOrd "uuidGenerate" [||] with
    | RT.Tracing.ReplayStep.Serve v ->
      Expect.equal v childValue "the child replays the recorded child's uuid"
    | other -> failtest $"the child was not answered from its log: {other}"
  }


/// An edit that changes what the run DID: the first effect now gets different arguments. The
/// log holds what the old program did at that step, so serving it to the new call answers an
/// effect that never happened. The resume refuses there, says what differs, and leaves the run
/// as it was.
let private replayRefusesADivergedEffect =
  cliTestWithFreshTraces
    "a resume refuses when an edit changed what an effect was called with"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Exec.target" "() : String = \"divergeStaging\""
        do! commit state "target v1"
        let! _ =
          runCli state [ "eval"; "Stdlib.printLine(\"to \" + Tests.Exec.target())" ]
        let! recorded = latest ()
        let! logBefore = Traces.log recorded.id
        do! fn state "Tests.Exec.target" "() : String = \"divergeProd\""
        do! commit state "target v2"
        let! refused = runCli state [ "exec"; "resume"; prefixOf recorded ]
        Expect.stringContains refused "cannot resume past step" "the resume stops"
        Expect.stringContains
          refused
          "divergeStaging"
          "naming what the log recorded"
        Expect.stringContains
          refused
          "divergeProd"
          "and what this program asked for"
        Expect.stringContains refused "rerun" "and offering a rerun"
        Expect.isFalse
          (refused.Contains "[replayed] to divergeProd")
          "and never echoes output the old run did not produce"
        let! after = Traces.get recorded.id
        Expect.equal
          (after |> Option.map (fun e -> e.status))
          (Some recorded.status)
          "the run is left with the status it had"
        let! logAfter = Traces.log recorded.id
        Expect.equal
          (List.length logAfter)
          (List.length logBefore)
          "with its log as it was"
      })


let private retentionKeepsTheNewestAndTheSuspended =
  cliTestWithFreshTraces
    "retention drops the oldest runs past trace.keep but never a suspended one"
    (fun state ->
      task {
        let keep, bytes =
          LibDB.Tracing.TraceRetention.keep, LibDB.Tracing.TraceRetention.maxBytes
        LibDB.Tracing.TraceRetention.setForTesting 2L 0L
        try
          // Four runs; the first is suspended by hand so it must survive.
          let! _ = runCli state [ "eval"; "1" ]
          let! first = latest ()
          Traces.setStatus first.id Traces.Suspended
          let! _ = runCli state [ "eval"; "2" ]
          let! _ = runCli state [ "eval"; "3" ]
          // The pass runs at most every ten seconds; the seam reset its clock, and it ran on
          // the fourth store, which is the one past the cap.
          LibDB.Tracing.TraceRetention.setForTesting 2L 0L
          let! _ = runCli state [ "eval"; "4" ]
          let! traces =
            Sql.query "SELECT id FROM traces ORDER BY timestamp"
            |> Sql.executeAsync (fun read -> read.string "id")
          Expect.isTrue
            (List.contains (string first.id) traces)
            "the suspended run's trace is kept whatever its age"
          Expect.isLessThanOrEqual
            (List.length traces)
            3
            "at most the cap plus the exempt one"
          let! rows = Traces.list 10
          Expect.isTrue
            (rows |> List.exists (fun e -> e.id = first.id))
            "the suspended execution is still listed"
          Expect.isTrue
            (rows |> List.forall (fun e -> List.contains (string e.id) traces))
            "every listed execution still has its trace: the rows went together"
        finally
          LibDB.Tracing.TraceRetention.setForTesting keep bytes
      })


let private byteCapSparesTheRunThatTrippedIt =
  cliTestWithFreshTraces
    "the byte cap drops older logs, never the newest"
    (fun state ->
      task {
        // One byte: every stored log is over the cap on its own. The pass must keep the
        // newest trace, or a run's own log would go the moment it was written. Called
        // directly, so the ten-second spacing between passes does not decide the result.
        let! _ = runCli state [ "eval"; "Stdlib.printLine(\"one\")" ]
        let! _ = runCli state [ "eval"; "Stdlib.printLine(\"two\")" ]
        let! newest = latest ()
        let went = LibDB.Tracing.TraceRetention.prune None (Some 1L)
        let! traces =
          Sql.query "SELECT id FROM traces"
          |> Sql.executeAsync (fun read -> read.string "id")
        Expect.equal went 1 "the older one went"
        Expect.equal
          traces
          [ string newest.id ]
          "only the newest survives, and it does"
      })


/// The floor classic had as "the last 10 per route": a busy `serve` writes a trace per request,
/// and without a floor it evicts the `eval` you were working on. Driven through the table rather
/// than the CLI, because what is under test is which rows `prune` picks, and two entries with
/// several runs each is four lines of SQL and four CLI runs.
let private retentionKeepsTheNewestOfEachEntry =
  cliTestWithFreshTraces
    "retention keeps the newest run of each entry past the count cap"
    (fun state ->
      task {
        let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
        // id, entry, when. Newest first when sorted by timestamp descending.
        let rows =
          [ "t1", "eval", "2026-09-01T00:00:01Z"
            "t2", "eval", "2026-09-01T00:00:02Z"
            "t3", "GET /a?q=1", "2026-09-01T00:00:03Z"
            "t4", "GET /a?q=2", "2026-09-01T00:00:04Z"
            "t5", "GET /b", "2026-09-01T00:00:05Z" ]
        do!
          rows
          |> List.map (fun (id, desc, ts) ->
            Sql.query
              "INSERT INTO traces (id, root_tlid, handler_desc, timestamp, input_name, input_value)
               VALUES (@id, 0, @desc, @ts, 'expression', x'')"
            |> Sql.parameters
              [ "id", Sql.string id; "desc", Sql.string desc; "ts", Sql.string ts ]
            |> Sql.executeStatementAsync
            |> Task.map ignore<unit>)
          |> Task.WhenAll
          |> Task.map ignore<unit[]>

        // Keep one. Without the floor that is `t5` alone.
        let went = LibDB.Tracing.TraceRetention.prune (Some 1L) None
        let! left =
          Sql.query "SELECT id FROM traces ORDER BY id"
          |> Sql.executeAsync (fun read -> read.string "id")

        Expect.equal went 2 "the two older runs of an entry went"
        Expect.equal
          left
          [ "t2"; "t4"; "t5" ]
          "the newest eval, the newest GET /a whatever its query string, and GET /b"
      })


/// An older `traces delete --all` missed a table, so a store that ran it still holds that
/// table's rows under traces that are gone. Every form of `traces delete` now removes them,
/// and no live row. The tables come from the schema, so one added later without joining
/// `perTraceTables` fails here.
let private manualDeletesSweepOrphanedRows =
  cliTestWithFreshTraces
    "every form of traces delete removes rows whose trace is gone"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.Prev.looped"
            ("(n: Int64): Int64 =\n"
             + "  Stdlib.List.range(1, n)\n"
             + "  |> Stdlib.List.map(fun i -> i * 2)\n"
             + "  |> Stdlib.List.length")
        do! commit state "looped"
        let! _ =
          runCli state [ "permissions"; "approve"; "Tests.Prev.looped"; "--yes" ]
        // A loop, a logged call and a captured blob, so every table has live rows to copy.
        let! _ = runCli state [ "eval"; "Tests.Prev.looped(4)" ]
        let! _ = runCli state [ "eval"; twoUuids ]
        let! _ =
          runCli state [ "eval"; "Stdlib.Utf8.encode(\"held by the trace\")" ]

        let! tables =
          Sql.query
            "SELECT name FROM sqlite_master
             WHERE type = 'table' AND name LIKE 'trace%' AND name <> 'traces'"
          |> Sql.executeAsync (fun read -> read.string "name")
        Expect.equal
          (List.sort tables)
          (List.sort LibDB.Tracing.TraceRetention.perTraceTables)
          "every per-trace table in the schema is one the delete and the sweep know"

        let count (clause : string) =
          tables
          |> List.map (fun table ->
            Sql.query $"SELECT COUNT(*) AS n FROM {table} WHERE {clause}"
            |> Sql.executeRowAsync (fun read -> read.int64 "n"))
          |> Task.WhenAll
          |> Task.map List.ofArray
        let live = "trace_id IN (SELECT id FROM traces)"
        let orphaned = "trace_id NOT IN (SELECT id FROM traces)"

        // An orphan per table: a live trace's rows copied under an id with no `traces` row.
        let plant (suffix : string) =
          task {
            for table in tables do
              let! columns =
                Sql.query $"SELECT name FROM pragma_table_info('{table}')"
                |> Sql.executeAsync (fun read -> read.string "name")
              let selected =
                columns
                |> List.map (fun c -> if c = "trace_id" then "@orphan" else c)
                |> String.concat ", "
              let columnList = String.concat ", " columns
              do!
                Sql.query
                  $"INSERT INTO {table} ({columnList})
                    SELECT {selected} FROM {table}
                    WHERE trace_id = (SELECT trace_id FROM {table} WHERE {live} LIMIT 1)"
                |> Sql.parameters [ "orphan", Sql.string $"gone-{suffix}" ]
                |> Sql.executeStatementAsync
            let! planted = count orphaned
            Expect.all
              planted
              (fun n -> n > 0L)
              $"every table has an orphan ({suffix})"
          }

        let! oneTrace = latest ()
        let forms =
          [ [ "--keep"; "100" ]
            [ "--before"; "3650d" ]
            [ string oneTrace.id ]
            [ "--all" ] ]
        for args in forms do
          let name = String.concat " " args
          do! plant name
          let! liveBefore = count live
          let! _ = runCli state ([ "traces"; "delete" ] @ args @ [ "--yes" ])
          let! left = count orphaned
          Expect.all left (fun n -> n = 0L) $"traces delete {name} took the orphans"
          if name = "--keep 100" then
            let! liveAfter = count live
            Expect.equal liveAfter liveBefore "and left every live row"
      })


/// The pass after a store used to look at bytes only once there were fifty traces, so a
/// store holding a dozen enormous ones sat at 790 MB under a 256 MB cap and nothing ran.
let private byteCapActsBelowFiftyTraces =
  cliTestWithFreshTraces
    "the byte cap applies however few traces there are"
    (fun state ->
      task {
        let keep, bytes =
          LibDB.Tracing.TraceRetention.keep, LibDB.Tracing.TraceRetention.maxBytes
        try
          LibDB.Tracing.TraceRetention.setForTesting 200L 1L
          let! _ = runCli state [ "eval"; "Stdlib.printLine(\"one\")" ]
          LibDB.Tracing.TraceRetention.setForTesting 200L 1L
          let! _ = runCli state [ "eval"; "Stdlib.printLine(\"two\")" ]
          let! newest = latest ()
          let! traces =
            Sql.query "SELECT id FROM traces"
            |> Sql.executeAsync (fun read -> read.string "id")
          Expect.equal
            traces
            [ string newest.id ]
            "two traces over a one-byte cap: the pass after the second store took the first"
        finally
          LibDB.Tracing.TraceRetention.setForTesting keep bytes
      })


/// An entry is a concrete path, so a server answering many paths made the floor as large as
/// the number of paths: `keep = 20` kept 120 traces after 100 distinct ones.
let private theFloorIsBounded =
  cliTestWithFreshTraces
    "the newest-of-each-entry floor covers only the most recent entries"
    (fun state ->
      task {
        let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
        do!
          [ 1..30 ]
          |> List.map (fun i ->
            Sql.query
              "INSERT INTO traces (id, root_tlid, handler_desc, timestamp, input_name, input_value)
               VALUES (@id, 0, @desc, @ts, 'request', x'')"
            |> Sql.parameters
              [ "id", Sql.string $"p{i:D2}"
                "desc", Sql.string $"GET /hello/{i}"
                "ts", Sql.string $"2026-09-01T00:00:{i:D2}Z" ]
            |> Sql.executeStatementAsync
            |> Task.map ignore<unit>)
          |> Task.WhenAll
          |> Task.map ignore<unit[]>

        let went = LibDB.Tracing.TraceRetention.prune (Some 1L) None
        let! left =
          Sql.query "SELECT id FROM traces ORDER BY id"
          |> Sql.executeAsync (fun read -> read.string "id")

        Expect.equal
          (List.length left)
          LibDB.Tracing.TraceRetention.floorEntries
          "thirty distinct paths, a count cap of one: the floor keeps the newest twenty"
        Expect.equal
          went
          (30 - LibDB.Tracing.TraceRetention.floorEntries)
          "the rest went"
        Expect.contains left "p30" "the newest is among them"
        Expect.isFalse (List.contains "p01" left) "the oldest is not"
      })


/// A read of a package item by its hash is kept once per trace, however often the run read it,
/// and the rows refer to it. A replay then gets exactly what the run got even after the item
/// has left the store, which a dev reload does to every edited fn's old hash.
let private hashReadsAreKeptOnceAndOutliveTheStore =
  cliTestWithFreshTraces
    "a read of a package item by hash is kept once, and replays after the item is gone"
    (fun state ->
      task {
        let expr =
          "match Darklang.LanguageTools.PackageManager.Function.find(Darklang.SCM.Branch.mainBranchId, Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"Darklang\", modules = [ \"Stdlib\", \"Int128\" ], name = \"sqrt\" }) with\n| Some(h) -> [ Stdlib.Option.isSome(Darklang.LanguageTools.PackageManager.Function.get(h)), Stdlib.Option.isSome(Darklang.LanguageTools.PackageManager.Function.get(h)) ]\n| None -> []"
          + ""
          + ""
          + ""
          + ""
          + ""
        let! out = runCli state [ "eval"; expr ]
        Expect.stringContains out "true, true" "the eval read the fn twice"
        let! e = latest ()
        let! rows =
          Sql.query
            "SELECT result FROM trace_fn_calls WHERE trace_id = @t AND fn_hash = 'pmGet'"
          |> Sql.parameters [ "t", Sql.uuid e.id ]
          |> Sql.executeAsync (fun read -> read.string "result")
        let referred = List.distinct rows
        Expect.equal (List.length rows) 2 "both reads were logged"
        Expect.equal (List.length referred) 1 "and both refer to one kept answer"
        let! kept =
          Sql.query
            "SELECT COUNT(*) AS n FROM trace_blobs WHERE trace_id = @t AND hash = @h"
          |> Sql.parameters
            [ "t", Sql.string (string e.id)
              "h", Sql.string (referred |> List.head |> Option.defaultValue "") ]
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        Expect.equal kept 1L "kept once, with the trace"

        // The item leaves the store, as a dev reload does to an edited fn's old hash. `Int128.sqrt`
        // because nothing the CLI itself runs calls it, so only the replay can notice it is gone.
        let! fnHash =
          Sql.query
            "SELECT item_hash FROM locations WHERE owner = 'Darklang' AND modules = 'Stdlib.Int128'
             AND name = 'sqrt' AND item_type = 'fn' AND live = 1"
          |> Sql.executeRowAsync (fun read -> read.string "item_hash")
        let saved = "test_saved_package_function"
        do! Sql.query $"DROP TABLE IF EXISTS {saved}" |> Sql.executeStatementAsync
        do!
          Sql.query
            $"CREATE TABLE {saved} AS SELECT * FROM package_functions WHERE hash = @h"
          |> Sql.parameters [ "h", Sql.string fnHash ]
          |> Sql.executeStatementAsync
        do!
          Sql.query "DELETE FROM package_functions WHERE hash = @h"
          |> Sql.parameters [ "h", Sql.string fnHash ]
          |> Sql.executeStatementAsync
        LibDB.Caching.invalidateAll ()
        let! prefix = latestPrefix ()
        let! inspected = runCli state [ "traces"; "inspect"; prefix ]
        let! viewed =
          runCli
            state
            [ "traces"
              "show"
              "Darklang.LanguageTools.PackageManager.getItem"
              prefix ]
        // Put it back before asserting, so a failure here leaves the next test a whole store.
        do!
          Sql.query $"INSERT OR IGNORE INTO package_functions SELECT * FROM {saved}"
          |> Sql.executeStatementAsync
        do! Sql.query $"DROP TABLE {saved}" |> Sql.executeStatementAsync
        LibDB.Caching.invalidateAll ()

        Expect.stringContains
          inspected
          "pmGet(Darklang.LanguageTools.ProgramTypes.ItemKind.Fn, Hash("
          "the read is in the log, with what it was asked for"
        Expect.stringContains
          viewed
          "Builtin.pmGet(kind, hash) // = Some( Darklang.LanguageTools.ProgramTypes.PackageFn"
          "the replay got the fn the run got, with the store no longer holding it"
      })


/// A captured blob belongs to its trace: kept in `trace_blobs`, never the shared
/// `package_blobs`, readable by hash like any blob, and gone when the trace goes.
let private aTracesBytesGoWithIt =
  cliTestWithFreshTraces
    "a blob a trace captured is kept with the trace and deleted with it"
    (fun state ->
      task {
        let! sharedBefore =
          Sql.query "SELECT COUNT(*) AS n FROM package_blobs"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        let! _ =
          runCli state [ "eval"; "Stdlib.Utf8.encode(\"held by the trace\")" ]
        let! e = latest ()
        let! kept =
          Sql.query "SELECT hash, bytes FROM trace_blobs WHERE trace_id = @t"
          |> Sql.parameters [ "t", Sql.string (string e.id) ]
          |> Sql.executeAsync (fun read -> read.string "hash", read.bytes "bytes")
        let! sharedAfter =
          Sql.query "SELECT COUNT(*) AS n FROM package_blobs"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        match kept with
        | [ (hash, bytes) ] ->
          Expect.equal
            (System.Text.Encoding.UTF8.GetString bytes)
            "held by the trace"
            "the answer's bytes are kept with the trace"
          let! read = LibDB.RuntimeTypes.Bytes.get hash |> Ply.toTask
          Expect.equal read (Some bytes) "and a read by hash finds them there"
        | other ->
          failtest $"expected one blob kept with the trace, got {List.length other}"
        Expect.equal sharedAfter sharedBefore "nothing went into package_blobs"

        let! prefix = latestPrefix ()
        let! _ = runCli state [ "traces"; "delete"; prefix; "--yes" ]
        let! left =
          Sql.query "SELECT COUNT(*) AS n FROM trace_blobs WHERE trace_id = @t"
          |> Sql.parameters [ "t", Sql.string (string e.id) ]
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        Expect.equal left 0L "deleting the trace took its blob"
      })


/// `traces delete --all` empties every per-trace table, not a list of them written once. The
/// tables come from the schema, so a table added later is checked without touching this test.
let private deletingEveryTraceEmptiesEveryTraceTable =
  cliTestWithFreshTraces
    "deleting every trace leaves nothing in any trace table"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.Prev.looped"
            ("(n: Int64): Int64 =\n  Stdlib.List.range(1, n)\n  |> Stdlib.List.map(fun i -> i * 2)\n  |> Stdlib.List.length"
             + ""
             + ""
             + "")
        do! commit state "looped"
        let! _ =
          runCli state [ "permissions"; "approve"; "Tests.Prev.looped"; "--yes" ]
        // A loop, a logged call and a captured blob, so every table has rows to leave behind.
        let! _ = runCli state [ "eval"; "Tests.Prev.looped(4)" ]
        let! _ = runCli state [ "eval"; twoUuids ]
        let! _ =
          runCli state [ "eval"; "Stdlib.String.toBlob(\"held by the trace\")" ]

        let! tables =
          Sql.query
            "SELECT name FROM sqlite_master WHERE type = 'table' AND name LIKE 'trace%' ORDER BY name"
          |> Sql.executeAsync (fun read -> read.string "name")
        Expect.isGreaterThanOrEqual
          (List.length tables)
          5
          $"the schema's trace tables were found: {tables}"

        let countRows (table : string) =
          Sql.query $"SELECT COUNT(*) AS n FROM {table}"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")

        for table in tables do
          let! before = countRows table
          Expect.isGreaterThan before 0L $"{table} has rows before the delete"

        let! cleared = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
        Expect.stringContains cleared "cleared 3 traces" "the delete ran"

        for table in tables do
          let! after = countRows table
          Expect.equal after 0L $"{table} is empty after the delete"
      })


let private replayEchoesAndRefuses =
  cliTestWithFreshTraces
    "a resume echoes what the old run printed, and stops at a call it cannot reproduce"
    (fun state ->
      task {
        // Printed once, then a uuid: the echo must show the line again, the uuid must be the log's.
        let! first =
          runCli
            state
            [ "eval"
              "let _ = Stdlib.printLine(\"hello from the log\")\nStdlib.display(Stdlib.Uuid.generate())" ]
        let! prefix = latestPrefix ()
        let! resumed = runCli state [ "exec"; "resume"; prefix ]
        Expect.stringContains
          resumed
          "hello from the log"
          "the logged print is echoed"
        let uuid = first.Split('\n') |> Array.last
        Expect.stringContains resumed uuid "the uuid came from the log"
        // A spawned process is a live handle the log cannot hand back. The grant lands in the
        // shared store, so it is taken back whatever happens.
        let! _ = runCli state [ "permissions"; "allow"; "process"; "/bin/sh" ]
        try
          let! _ =
            runCli
              state
              [ "eval"
                "let h = Stdlib.Cli.Process.spawn(\"sleep 0\")\nStdlib.Cli.Process.terminate(h)" ]
          let! spawned = latest ()
          let! logBefore = Traces.log spawned.id
          let! refused = runCli state [ "exec"; "resume"; prefixOf spawned ]
          Expect.stringContains refused "cannot resume past step" "the resume stops"
          Expect.stringContains refused "runtimeChildStart" "naming the call"
          let! after = Traces.get spawned.id
          Expect.equal
            (after |> Option.map (fun e -> e.status))
            (Some spawned.status)
            "and the run is left with the status it had"
          let! logAfter = Traces.log spawned.id
          Expect.equal
            (List.length logAfter)
            (List.length logBefore)
            "with its log as it was"
        finally
          (runCli state [ "permissions"; "remove"; "process"; "/bin/sh" ]).Wait()
      })


/// A loop that made no impure call still knows how many times it went round.
///
/// The count comes from the RECORD, not from the replay, and the record used to keep only the
/// frames an effectful call sat under. So a pure loop stored nothing, its count came back zero,
/// and the view fell back to counting the passes IT reached. That is right until a replay
/// cannot reach them all, and then it reports "pass 3 of 3" about a loop that went round five
/// times, with nothing to say it is short.
///
/// Invisible for exactly as long as nobody wrote this.
let private pureLoopPassesAreCounted =
  cliTestWithFreshTraces
    "a loop that made no impure call still records how many passes it had"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.Prev.pure"
            ("(n: Int64): Int64 =\n  Stdlib.List.range(1, n)\n  |> Stdlib.List.map(fun i -> i * 2)\n  |> Stdlib.List.length"
             + ""
             + ""
             + "")
        do! commit state "pure"
        let! _ =
          runCli state [ "permissions"; "approve"; "Tests.Prev.pure"; "--yes" ]
        let! ran = runCli state [ "eval"; "Tests.Prev.pure(5)" ]
        Expect.stringContains ran "5" "the run went round five times"

        let! latest' = latest ()
        let! counted =
          Sql.query
            "SELECT passes FROM trace_loops WHERE trace_id = @t ORDER BY passes DESC LIMIT 1"
          |> Sql.parameters [ "t", Sql.string (string latest'.id) ]
          |> Sql.executeRowOptionAsync (fun read -> read.int64 "passes")

        match counted with
        | None ->
          failtest
            "a pure loop recorded no pass count, so a view that stops early cannot say how many passes there were"
        | Some n ->
          Expect.equal
            n
            5L
            "all five passes are counted, not just the ones with effects"
      })


/// A function that RAN and has since gone is not a typo, and must not be reported as one.
///
/// `scripts/dev/build` re-authors the store from the `.dark` files, so anything authored by hand
/// disappears while its runs stay. The index keeps the name, so the two cases can be told apart.
/// Before they were, `traces show` answered "no function named X on this branch" about something
/// the reader had watched run ten minutes earlier -- and `traces fns` will still offer it.
///
/// The second half covers what the index keeps when it has no name at all: the hash. Sixty-four
/// hex characters printed as if they were a name reads like corruption and blows the column out.
let private aFunctionThatRanAndWentIsNotATypo =
  cliTestWithFreshTraces
    "a function that ran and whose code has gone reads differently from a typo"
    (fun state ->
      task {
        do! start state
        let! _ = runCli state [ "eval"; "1" ]
        let! latest' = latest ()
        let traceId = string latest'.id

        // Two rows the store cannot resolve: one that kept its name, and one that kept only its
        // hash, which is what the index holds once the code is gone.
        let goneHash = String.replicate 64 "a"
        do!
          [ "Gone.Module.vanished", goneHash; goneHash, goneHash ]
          |> List.map (fun (name, hash) ->
            Sql.query
              "INSERT INTO trace_fns (trace_id, fn_name, fn_hash) VALUES (@t, @n, @h)"
            |> Sql.parameters
              [ "t", Sql.string traceId
                "n", Sql.string name
                "h", Sql.string hash ]
            |> Sql.executeStatementAsync
            |> Task.map ignore<unit>)
          |> Task.WhenAll
          |> Task.map ignore<unit[]>

        let! gone = runCli state [ "traces"; "show"; "Gone.Module.vanished" ]
        Expect.stringContains
          gone
          "no longer in this store"
          "it ran, so saying there is no such function sends the reader looking for a typo"

        let! typo = runCli state [ "traces"; "show"; "Stdlib.List.mpa" ]
        Expect.stringContains
          typo
          "no function named"
          "a real typo still gets the refusal it should"
        Expect.isFalse
          (typo.Contains "no longer in this store")
          "and is not dressed up as a function that went missing"

        // The hash-only row, rendered as what it is rather than as sixty-four characters of name.
        let! listed = runCli state [ "traces"; "fns"; "--all" ]
        Expect.stringContains
          listed
          "(gone) aaaaaaaa"
          "a row with no name says so, shortened"
        Expect.isFalse
          (listed.Contains goneHash)
          "and does not print the whole hash as though it were a name"
      })


/// Deleting a trace has to take its loop counts with it.
///
/// `trace_loops` arrived with this branch and every delete path was written before it existed,
/// so a deleted trace left its rows behind. Nothing can ever read them again: every query is
/// `WHERE trace_id = @t` against a trace that is gone. They are invisible, they only grow, and
/// retention runs on its own, so a long-lived store accumulates them for as long as it lives.
let private deletingATraceTakesItsLoopCounts =
  cliTestWithFreshTraces
    "deleting a trace drops its loop counts too, rather than orphaning them"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.Prev.looped"
            ("(n: Int64): Int64 =\n  Stdlib.List.range(1, n)\n  |> Stdlib.List.map(fun i -> i * 2)\n  |> Stdlib.List.length"
             + ""
             + ""
             + "")
        do! commit state "looped"
        let! _ =
          runCli state [ "permissions"; "approve"; "Tests.Prev.looped"; "--yes" ]
        let! _ = runCli state [ "eval"; "Tests.Prev.looped(4)" ]

        let! latest' = latest ()
        let traceId = string latest'.id

        let countRows () =
          Sql.query "SELECT COUNT(*) as c FROM trace_loops WHERE trace_id = @t"
          |> Sql.parameters [ "t", Sql.string traceId ]
          |> Sql.executeRowAsync (fun read -> read.int64 "c")

        let! before = countRows ()
        Expect.isGreaterThan
          before
          0L
          "the run recorded a loop, so there is something to orphan"

        // `--yes`: with no terminal to answer the prompt the verb cancels, and it used to
        // cancel and exit 0, which is how this test passed its first run against code that
        // deleted nothing.
        let! _ = runCli state [ "traces"; "delete"; traceId; "--yes" ]
        let! after = countRows ()
        Expect.equal after 0L "the loop rows went with the trace"

        let! stillThere =
          Sql.query "SELECT COUNT(*) as c FROM traces WHERE id = @t"
          |> Sql.parameters [ "t", Sql.string traceId ]
          |> Sql.executeRowAsync (fun read -> read.int64 "c")
        Expect.equal stillThere 0L "and so did the trace, so the delete really ran"
      })


/// The header half of the redaction, at the unit: the names in the table are blanked in the
/// arguments a row stores, whatever case they were written in, and nothing else is touched.
let private secretHeadersAreRedacted =
  testTask "a secret header's value is replaced in the stored arguments" {
    let headers =
      RT.DList(
        RT.ValueType.Unknown,
        RT.Items(
          [ RT.DTuple(RT.DString "Authorization", RT.DString "Bearer abc123", [])
            RT.DTuple(RT.DString "cookie", RT.DString "session=xyz", [])
            RT.DTuple(RT.DString "Accept", RT.DString "application/json", []) ]
        )
      )
    let stored =
      LibDB.Tracing.Redact.args
        "httpClientRequest"
        [ RT.DString "GET"; RT.DString "https://example.com/"; headers ]
    match stored with
    | [ _; _; RT.DList(_, RT.Items(items)) ] ->
      let value (name : string) =
        items
        |> List.tryPick (fun item ->
          match item with
          | RT.DTuple(RT.DString n, RT.DString v, []) when n = name -> Some v
          | _ -> None)
      Expect.equal
        (value "Authorization")
        (Some "[redacted]")
        "the bearer token is gone"
      Expect.equal (value "cookie") (Some "[redacted]") "the cookie is gone"
      Expect.equal
        (value "Accept")
        (Some "application/json")
        "an ordinary header is untouched"
    | other -> failtest $"expected three arguments with a header list, got {other}"
  }


let private headerList (pairs : List<string * string>) : RT.Dval =
  RT.DList(
    RT.ValueType.Unknown,
    RT.Items(
      pairs |> List.map (fun (k, v) -> RT.DTuple(RT.DString k, RT.DString v, []))
    )
  )

let private headerValue (name : string) (headers : RT.Dval) : Option<string> =
  match headers with
  | RT.DList(_, RT.Items(items)) ->
    items
    |> List.tryPick (fun item ->
      match item with
      | RT.DTuple(RT.DString n, RT.DString v, []) when n = name -> Some v
      | _ -> None)
  | _ -> None

/// The rule is the header's name, not a list of names: anything that says it carries a
/// credential is blanked, in a request's headers and URL and in a response's headers.
let private credentialsAreRedactedByName =
  testTask
    "credentials are redacted from HTTP arguments and responses by their names" {
    let stored =
      LibDB.Tracing.Redact.args
        "httpClientRequest"
        [ RT.DString "POST"
          RT.DString
            "https://me:hunter2@example.com/a?api_key=K1&q=dogs&access_token=T1#top"
          headerList
            [ "X-Goog-Api-Key", "k"
              "X-Auth-Token", "t"
              "Proxy-Authorization", "Basic p"
              "Accept", "text/plain" ]
          RT.DBytes(RT.Persistent("", 0L)) ]
    match stored with
    | [ _; RT.DString url; headers; _ ] ->
      Expect.equal
        url
        "https://[redacted]@example.com/a?api_key=[redacted]&q=dogs&access_token=[redacted]#top"
        "the URL keeps its shape and loses its credentials"
      for name in [ "X-Goog-Api-Key"; "X-Auth-Token"; "Proxy-Authorization" ] do
        Expect.equal (headerValue name headers) (Some "[redacted]") $"{name} is gone"
      Expect.equal
        (headerValue "Accept" headers)
        (Some "text/plain")
        "an ordinary header is untouched"
    | other -> failtest $"expected four arguments, got {other}"

    let responseType =
      RT.FQTypeName.fqPackage (
        LibExecution.PackageRefs.Type.Stdlib.HttpClient.response ()
      )
    let response =
      RT.DRecord(
        responseType,
        responseType,
        [],
        RT.RecordFields.ofList
          [ "statusCode", RT.DInt64 200L
            "headers",
            headerList [ "Set-Cookie", "sid=s"; "Content-Type", "text/plain" ]
            "body", RT.DBytes(RT.Persistent("", 0L)) ]
      )
    let ok = LibExecution.Dval.resultOk RT.KTUnit RT.KTUnit response
    match LibDB.Tracing.Redact.result "httpClientRequest" ok with
    | RT.DEnum(_, _, _, "Ok", [ RT.DRecord(_, _, _, fields) ]) ->
      let headers = RT.RecordFields.tryFind "headers" fields |> Option.get
      Expect.equal
        (headerValue "Set-Cookie" headers)
        (Some "[redacted]")
        "the response's cookie is gone"
      Expect.equal
        (headerValue "Content-Type" headers)
        (Some "text/plain")
        "and its other headers are kept"
    | other -> failtest $"expected Ok(Response), got {other}"
  }

/// A served request's trace: its input (the request), its result (the handler's response)
/// and its description keep no credential the client sent or the handler set.
let private servedRequestsKeepNoCredentials =
  cliTestWithFreshTraces
    "a served request's trace keeps no credential from either side"
    (fun _ ->
      task {
        let marker = string (System.Guid.NewGuid())
        let secrets =
          [ "bearer"; "cookie"; "apikey"; "query"; "setcookie" ]
          |> List.map (fun s -> $"{s}-{marker}")
        let secret (s : string) = $"{s}-{marker}"
        let setCookie = secret "setcookie"
        let query = secret "query"
        let bearer = secret "bearer"
        let cookie = secret "cookie"
        let! exeState = executionStateFor pmPT true
        let source =
          $"fun request -> {{ Darklang.Stdlib.Http.responseWithText(200, \"ok\") with headers = [(\"Set-Cookie\", \"sid={setCookie}\")] }}"
        let! ptExpr = parsePTExpr source
        let! router =
          LibExecution.Execution.executeExpr
            exeState
            (LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT
              Map.empty
              0
              None
              ptExpr)
        let handler =
          match router with
          | Ok(RT.DApplicable a) -> a
          | other -> failtest $"the router did not evaluate: {other}"
        let cts = new System.Threading.CancellationTokenSource()
        let! port, listener = Tests.HttpServer.bindFreshListener ()
        let serving =
          Builtins.Http.Server.Libs.HttpServer.runListener
            exeState
            listener
            (int64 port)
            handler
            Builtins.Http.Server.Libs.HttpServer.defaultMaxBodyBytes
            Builtins.Http.Server.Libs.HttpServer.defaultRequestTimeoutMs
            false
            false
            false
            cts.Token
        try
          use client = new System.Net.Http.HttpClient()
          use request =
            new System.Net.Http.HttpRequestMessage(
              System.Net.Http.HttpMethod.Get,
              $"http://127.0.0.1:{port}/probe-{marker}?token={query}&page=2"
            )
          request.Headers.TryAddWithoutValidation(
            "Authorization",
            $"Bearer {bearer}"
          )
          |> ignore<bool>
          request.Headers.TryAddWithoutValidation("Cookie", $"sid={cookie}")
          |> ignore<bool>
          request.Headers.TryAddWithoutValidation("X-Api-Key", secret "apikey")
          |> ignore<bool>
          let! response = client.SendAsync request
          Expect.equal (int response.StatusCode) 200 "the handler answered"
        finally
          cts.Cancel()
          try
            serving.Wait 2000 |> ignore<bool>
          with _ ->
            ()
        let! rows =
          Sql.query
            "SELECT handler_desc, input_value, COALESCE(result_value, X'') AS result_value FROM traces
             WHERE handler_desc LIKE @probe"
          |> Sql.parameters [ "probe", Sql.string $"%%probe-{marker}%%" ]
          |> Sql.executeAsync (fun read ->
            read.string "handler_desc",
            read.bytes "input_value",
            read.bytes "result_value")
        Expect.equal (List.length rows) 1 "the request was traced"
        for (desc, input, result) in rows do
          let text =
            desc
            + UTF8.ofBytesWithReplacement input
            + UTF8.ofBytesWithReplacement result
          for s in secrets do
            Expect.isFalse (text.Contains s) $"{s} is not in the trace"
          Expect.stringContains
            desc
            "page=2"
            "the description keeps the rest of the query"
          Expect.stringContains
            (UTF8.ofBytesWithReplacement input)
            "[redacted]"
            "the input says what it left out"
      })


/// `dark ps` as a command: the captioned table, and a refusal that says what to do.
/// Secrets: a request header the log must not keep, and an env read whose value it must not
/// keep. The header is redacted in the stored arguments; the env read is not stored at all and
/// is performed again on a resume, so the run still replays.
let private secretsAreNotInTheLog =
  cliTestWithFreshTraces
    "an authorization header is redacted in the log, and an env read is run again on resume"
    (fun state ->
      task {
        let! _ =
          runCli
            state
            [ "permissions"; "allow"; "env"; "read"; "'DARK_TEST_SECRET'" ]
        try
          // The program never prints the value (stdout IS logged, by design); it prints its
          // length, so the test can tell the two runs apart without putting a secret in the log.
          System.Environment.SetEnvironmentVariable(
            "DARK_TEST_SECRET",
            "twelve-chars"
          )
          let! _ =
            runCli
              state
              [ "eval"
                "match Stdlib.Env.get(\"DARK_TEST_SECRET\") with | Some(v) -> Stdlib.printLine(Stdlib.display(Stdlib.String.length(v))) | None -> Stdlib.printLine(\"unset\")" ]
          let! e = latest ()
          let! rows =
            Sql.query
              "SELECT c.fn_hash, c.args, r.bytes AS result FROM trace_fn_calls c
               JOIN trace_blobs r ON r.trace_id = c.trace_id AND r.hash = c.result
               WHERE c.trace_id = @t"
            |> Sql.parameters [ "t", Sql.string (string e.id) ]
            |> Sql.executeAsync (fun read ->
              (read.stringOrNone "fn_hash" |> Option.defaultValue ""),
              read.bytes "args",
              read.bytes "result")
          let envRows =
            rows |> List.filter (fun (fn, _, _) -> fn = "runtimeEnvironmentGet")
          Expect.isNonEmpty envRows "the env read is in the log"
          for (_, _, result) in envRows do
            let dv =
              LibSerialization.Binary.Serialization.RT.Dval.deserialize "t" result
            Expect.equal dv RT.DUnit "with no value in it"
          let bytes = rows |> List.collect (fun (_, a, r) -> [ a; r ])
          for b in bytes do
            Expect.isFalse
              ((UTF8.ofBytesWithReplacement b).Contains "twelve-chars")
              "the value is nowhere in any row"
          // The resume runs the env read again, so it still answers, and the run replays.
          System.Environment.SetEnvironmentVariable(
            "DARK_TEST_SECRET",
            "nineteen-chars-long"
          )
          let! resumed = runCli state [ "exec"; "resume"; prefixOf e ]
          Expect.stringContains resumed "19" "the resume read the environment again"
        finally
          System.Environment.SetEnvironmentVariable("DARK_TEST_SECRET", null)
          (runCli
            state
            [ "permissions"; "remove"; "env"; "read"; "'DARK_TEST_SECRET'" ])
            .Wait()
      })


let tests =
  [ scriptOrEvalInputPersistence
    legacyScriptDoesNotGuessItsInput
    resumeNamesServedRequests
    invocationCountsTowardsTraceWeight
    rerunIsIndependentOfResult
    suspendThenResume
    aChildLoggedBeforeItsSpawnStillReplays
    replayRefusesADivergedEffect
    pureLoopPassesAreCounted
    aFunctionThatRanAndWentIsNotATypo
    deletingATraceTakesItsLoopCounts
    retentionKeepsTheNewestAndTheSuspended
    retentionKeepsTheNewestOfEachEntry
    manualDeletesSweepOrphanedRows
    byteCapSparesTheRunThatTrippedIt
    byteCapActsBelowFiftyTraces
    theFloorIsBounded
    hashReadsAreKeptOnceAndOutliveTheStore
    aTracesBytesGoWithIt
    deletingEveryTraceEmptiesEveryTraceTable
    replayEchoesAndRefuses
    secretsAreNotInTheLog
    secretHeadersAreRedacted
    credentialsAreRedactedByName
    servedRequestsKeepNoCredentials ]
