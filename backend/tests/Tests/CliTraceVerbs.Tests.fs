/// Executions: a run kept beside its trace, resumed and forked by replaying the log of its
/// effectful calls (`docs/processes.md`, "Executions"). Driven through the CLI, since that is
/// where a run is made, suspended and taken up again.
module Tests.CliRuns

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

/// A run's id as `dark traces` shows it: the first twelve characters (a trace id carries an
/// inverted timestamp in front, so eight is not unique within a second).
let private prefixOf (e : Traces.Trace) : string = (string e.id).Substring(0, 12)

let private latestPrefix () : Task<string> =
  task {
    let! e = latest ()
    return prefixOf e
  }

/// Two uuids, so a replay is told apart from a rerun by its output.
let private twoUuids =
  "let a = Stdlib.Uuid.generate ()\nlet b = Stdlib.Uuid.generate ()\nStdlib.String.join [ Stdlib.Uuid.toString a, Stdlib.Uuid.toString b ] \" \""

let private words (s : string) : string list =
  s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray


let private recordThenResume =
  cliTestWithFreshTraces
    "a run is kept, and resume replays its effects rather than redoing them"
    (fun state ->
      task {
        let! first = runCli state [ "eval"; twoUuids ]
        let! listed = runCli state [ "exec" ]
        Expect.stringContains listed "done" $"the run is listed, done ({first})"
        Expect.stringContains listed "eval" "with its entry"
        let! prefix = latestPrefix ()
        let! resumed = runCli state [ "exec"; "resume"; prefix ]
        // The last line is the value; the first says what is being resumed.
        let last = resumed.Split('\n') |> Array.last
        Expect.equal
          (words last)
          (words first)
          "the same two uuids: both calls answered from the log"
        let! e = latest ()
        Expect.equal e.status Traces.Done "and the run is done again"
      })


let private forkDivergesAfterThePosition =
  cliTestWithFreshTraces
    "a fork keeps the log up to a position and goes live after it"
    (fun state ->
      task {
        let! first = runCli state [ "eval"; twoUuids ]
        let! parent = latest ()
        let prefix = prefixOf parent
        // Position 1: the first uuid's row (seq 0) is kept, the second (seq 1) is not.
        let! forked = runCli state [ "exec"; "fork"; prefix; "--at"; "1" ]
        Expect.stringContains forked "forked" "the fork is announced"
        let! child = latest ()
        Expect.equal child.status Traces.Suspended "a fork waits to be resumed"
        Expect.equal
          child.parent
          (Some(parent.id, 1L))
          "and knows where it came from"
        let! shown = runCli state [ "exec"; "details"; prefixOf child ]
        Expect.stringContains shown "forked" "details says so"
        Expect.stringContains
          shown
          ((string parent.id).Substring(0, 8))
          "and names the run it came from"
        let! resumed = runCli state [ "exec"; "resume"; prefixOf child ]
        let last = resumed.Split('\n') |> Array.last
        match words first, words last with
        | [ a1; b1 ], [ a2; b2 ] ->
          Expect.equal a2 a1 "the first uuid came from the log"
          Expect.notEqual b2 b1 "the second was generated afresh"
        | _ -> failtest $"unexpected outputs: {first} / {last}"
      })


let private suspendThenResume =
  cliTestWithFreshTraces
    "a run suspended midway resumes with its first effects answered from the log"
    (fun state ->
      task {
        // The second uuid comes after a pause long enough to suspend during.
        let program =
          "let a = Stdlib.Uuid.generate ()\nlet _ = Stdlib.Cli.Posix.sleep 800.0\nlet b = Stdlib.Uuid.generate ()\nStdlib.String.join [ Stdlib.Uuid.toString a, Stdlib.Uuid.toString b ] \" \""
        let running = runCli state [ "eval"; program ]
        // Until the run is in the foreground, then a moment more for the first uuid (made in
        // the first few milliseconds; the tracer holds it until a flush, so there is nothing
        // to poll for).
        let deadline = System.DateTime.UtcNow.AddSeconds 5.
        while (Traces.Foreground.currentId ()).IsNone
              && System.DateTime.UtcNow < deadline do
          do! Task.Delay 10
        do! Task.Delay 300
        let! suspended = Traces.Foreground.suspend ()
        Expect.isSome suspended "the foreground run was suspended"
        let! e = latest ()
        Expect.equal e.status Traces.Suspended "and marked so"
        let! log = Traces.log e.id
        Expect.equal
          (List.length log)
          1
          "the log has the first uuid and nothing after the pause"
        // The interrupted run goes on (in the CLI it would have exited) and must not overwrite
        // what the suspend stored.
        let! first = running
        let! e = latest ()
        Expect.equal
          e.status
          Traces.Suspended
          "the run's own ending left the suspend alone"
        let! resumed = runCli state [ "exec"; "resume"; prefixOf e ]
        let last = resumed.Split('\n') |> Array.last
        match words first, words last with
        | [ a1; _ ], [ a2; b2 ] ->
          Expect.equal a2 a1 "the first uuid came from the log"
          Expect.isTrue (b2.Length > 30) "the second was made live"
        | _ -> failtest $"unexpected outputs: {first} / {last}"
      })


let private replayAfterAnEdit =
  cliTestWithFreshTraces
    "replay after a package edit runs the new pure code against the old effects"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Exec.shape" "(s: String) : String = \"v1:\" ++ s"
        do! commit state "shape v1"
        let! first =
          runCli
            state
            [ "eval"
              "Tests.Exec.shape (Stdlib.Uuid.toString (Stdlib.Uuid.generate ()))" ]
        Expect.stringStarts first "v1:" "the first run went through v1"
        do! fn state "Tests.Exec.shape" "(s: String) : String = \"v2:\" ++ s"
        do! commit state "shape v2"
        let! prefix = latestPrefix ()
        let! resumed = runCli state [ "exec"; "resume"; prefix ]
        let last = resumed.Split('\n') |> Array.last
        Expect.stringStarts last "v2:" "the resume ran the new code"
        let uuidOf (s : string) = s.Substring(s.IndexOf(':') + 1).TrimEnd('"')
        Expect.equal
          (uuidOf last)
          (uuidOf first)
          "against the uuid the old run made"
        // The one thing about a resume the run itself cannot notice, so the resume says it.
        Expect.stringContains
          resumed
          "edited since this ran"
          "the resume says what moved"
        Expect.stringContains
          resumed
          "Tests.Exec.shape"
          "and names the function that moved"
      })


/// The other half of `replayAfterAnEdit`: a resume against code nobody touched must not
/// warn, or the warning means nothing on the run that has it.
let private replayWithoutAnEdit =
  cliTestWithFreshTraces
    "a resume against unchanged code says nothing about edits"
    (fun state ->
      task {
        do! start state
        // A body nothing else in these tests has: a package hash is structural, so two functions
        // with identical bodies share one, and `trace_fns` would name whichever of them the
        // store resolves that hash to.
        do! fn state "Tests.Exec.steady" "(s: String) : String = \"steady:\" ++ s"
        do! commit state "steady"
        let! _ = runCli state [ "eval"; "Tests.Exec.steady \"x\"" ]
        let! prefix = latestPrefix ()
        let! resumed = runCli state [ "exec"; "resume"; prefix ]
        Expect.stringContains resumed "steady:x" "the resume ran"
        Expect.isFalse
          (resumed.Contains "edited since this ran")
          "nothing moved, so nothing is said"
      })


/// The call inbox and the preview, which are classic's trace dots and live values with our
/// effect log underneath.
///
/// Both at the SHIPPED recording level, on purpose: the whole point is that a run recorded the
/// cheap way is enough to look at. The values come from replaying the run with its effects
/// answered from the log, so the print in the middle of the function must not print again.
let private previewShowsValuesAndPerformsNothing =
  cliTestWithFreshTraces
    "traces calls finds the runs, and traces show replays one without performing its effects"
    (fun state ->
      task {
        // No level pinning: recording is on or off, and the harness turns it on.
        do! start state
        do!
          fn
            state
            "Tests.Prev.greet"
            "(name: String) : String =\n  let upper = Stdlib.String.toUppercase name\n  let shouted = Stdlib.String.append upper \"!\"\n  let _ = Stdlib.printLine shouted\n  shouted"
        do! commit state "greet"
        let! _ = runCli state [ "permissions"; "approve"; "Tests.Prev.greet" ]
        let! ran = runCli state [ "eval"; "Tests.Prev.greet \"bob\"" ]
        Expect.stringContains ran "BOB!" "the run printed for real"

        // The index knows which runs went through the function, at this level, where the
        // call itself is not recorded at all.
        let! listed = runCli state [ "traces"; "calls"; "Tests.Prev.greet" ]
        Expect.stringContains listed "eval" "the run that went through it"

        let! viewed = runCli state [ "traces"; "show"; "Tests.Prev.greet" ]
        Expect.stringContains
          viewed
          "toUppercase name // = \"BOB\""
          "the recorded input flowed through the first call"
        Expect.stringContains
          viewed
          "append upper \"!\" // = \"BOB!\""
          "and through the second"
        // The print is an effect: answered from the log, not performed, and not echoed
        // either -- looking at code is silent.
        let printed =
          viewed.Split('\n')
          |> Array.filter (fun l -> l.Trim() = "BOB!")
          |> Array.length
        Expect.equal printed 0 "the preview did not print"
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
          let! _ = runCli state [ "eval"; "1L" ]
          let! first = latest ()
          Traces.setStatus first.id Traces.Suspended
          let! _ = runCli state [ "eval"; "2L" ]
          let! _ = runCli state [ "eval"; "3L" ]
          // The pass runs at most every ten seconds; the seam reset its clock, and it ran on
          // the fourth store, which is the one past the cap.
          LibDB.Tracing.TraceRetention.setForTesting 2L 0L
          let! _ = runCli state [ "eval"; "4L" ]
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
        // directly: the pass after a store only scans bytes once there are fifty traces.
        let! _ = runCli state [ "eval"; "Stdlib.printLine \"one\"" ]
        let! _ = runCli state [ "eval"; "Stdlib.printLine \"two\"" ]
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
              "let _ = Stdlib.printLine \"hello from the log\"\nStdlib.Uuid.toString (Stdlib.Uuid.generate ())" ]
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
        let! _ = runCli state [ "permissions"; "allow"; "process"; "/bin/bash" ]
        try
          let! _ =
            runCli
              state
              [ "eval"
                "let h = Stdlib.Cli.Process.spawn \"sleep 0\"\nStdlib.Cli.Process.terminate h" ]
          let! spawned = latest ()
          let! logBefore = Traces.log spawned.id
          let! refused = runCli state [ "exec"; "resume"; prefixOf spawned ]
          Expect.stringContains refused "cannot resume past step" "the resume stops"
          Expect.stringContains refused "cliSpawnProcess" "naming the call"
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
          (runCli state [ "permissions"; "remove"; "process"; "/bin/bash" ]).Wait()
      })


/// A run that spawned a Dark process resumes like any other: the spawn is performed again, and
/// the new child replays the recorded child's own log, so the value it made comes back the same.
///
/// The parent's own effect AFTER the spawn is in here on purpose. Serving the spawn from the
/// log instead of performing it (which is what `Redact.performAgain` prevents) leaves the
/// handle naming a process that no longer exists; in a fresh OS process the next `await`
/// raises "no process has this handle". This test runs in-process, where the recorded child
/// may still be findable, so the handle alone would not catch it -- the second uuid is what
/// pins that the parent went on replaying past the spawn.
let private spawnedChildReplays =
  cliTestWithFreshTraces
    "a resumed run spawns again, and the child replays its own effects"
    (fun state ->
      task {
        let program =
          "let h = Stdlib.Exec.spawn (fun () -> Stdlib.Uuid.toString (Stdlib.Uuid.generate ()))\n"
          + "let child = Stdlib.Exec.await h\n"
          + "let mine = Stdlib.Uuid.toString (Stdlib.Uuid.generate ())\n"
          + "Stdlib.printLine $\"{child} {mine}\""
        // The resume echoes a logged print with a `[replayed]` prefix, and prints its own
        // progress above it; the two uuids are the last non-empty line either way.
        let uuidLine (out : string) : string =
          out.Split('\n')
          |> Array.map (fun l -> l.Replace("[replayed]", "").Trim())
          |> Array.filter (fun l -> l.Contains "-" && l.Length > 60)
          |> Array.last
        let! first = runCli state [ "eval"; program ]
        let line = uuidLine first
        let! e = latest ()
        let! resumed = runCli state [ "exec"; "resume"; prefixOf e ]
        Expect.equal
          (uuidLine resumed)
          line
          "the child's uuid and the parent's own both came from the log, not from a fresh roll"
      })


/// The header half of the redaction, at the unit: the names in the table are blanked in the
/// arguments a row stores, whatever case they were written in, and nothing else is touched.
let private secretHeadersAreRedacted =
  testTask "a secret header's value is replaced in the stored arguments" {
    let headers =
      RT.DList(
        RT.ValueType.Unknown,
        [ RT.DTuple(RT.DString "Authorization", RT.DString "Bearer abc123", [])
          RT.DTuple(RT.DString "cookie", RT.DString "session=xyz", [])
          RT.DTuple(RT.DString "Accept", RT.DString "application/json", []) ]
      )
    let stored =
      LibDB.Tracing.Redact.args
        "httpClientRead"
        [ RT.DString "GET"; RT.DString "https://example.com/"; headers ]
    match stored with
    | [ _; _; RT.DList(_, items) ] ->
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
                "match Stdlib.Env.get \"DARK_TEST_SECRET\" with | Some v -> Stdlib.printLine (Stdlib.Int.toString (Stdlib.String.length v)) | None -> Stdlib.printLine \"unset\"" ]
          let! e = latest ()
          let! rows =
            Sql.query
              "SELECT fn_hash, args, result FROM trace_fn_calls WHERE trace_id = @t"
            |> Sql.parameters [ "t", Sql.string (string e.id) ]
            |> Sql.executeAsync (fun read ->
              (read.stringOrNone "fn_hash" |> Option.defaultValue ""),
              read.bytes "args",
              read.bytes "result")
          let envRows =
            rows |> List.filter (fun (fn, _, _) -> fn = "environmentGet")
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


let private psListsTheTree =
  cliTestWithFreshTraces
    "ps prints this dark's table and refuses an unknown id by name"
    (fun state ->
      task {
        // The harness runs commands unscheduled, so the table is empty here; the shape is
        // what this pins. The tree itself is `Scheduler.Tests`' and the demo's.
        let! out = runCli state [ "ps" ]
        Expect.stringContains
          out
          "this instance (pid"
          "the local table is captioned"
        // The columns are as wide as their widest cell (and the header is bold), so it is
        // matched word by word.
        let plain =
          System.Text.RegularExpressions.Regex.Replace(out, "\u001b\\[[0-9;]*m", "")
        let header = plain.Split('\n') |> Array.find (fun l -> l.StartsWith "id ")
        for column in [ "what it runs"; "status"; "steps" ] do
          Expect.stringContains header column "with its columns"
        // This instance first: it is the one the reader is standing in. Both sections always
        // print, `(none)` and all, so an empty one reads as an answer.
        let elsewhere = plain.IndexOf "other Dark processes on this machine"
        Expect.isGreaterThan elsewhere 0 "the machine's table is always there"
        Expect.isLessThan
          (plain.IndexOf "this instance (pid")
          elsewhere
          "this instance's table comes before the machine's"
        let! missing = runCli state [ "ps"; "show"; "nope" ]
        Expect.stringContains
          missing
          "no process whose id starts with nope"
          "an unknown id is refused, not answered plausibly"
      })


let tests =
  [ recordThenResume
    forkDivergesAfterThePosition
    suspendThenResume
    replayAfterAnEdit
    replayWithoutAnEdit
    previewShowsValuesAndPerformsNothing
    retentionKeepsTheNewestAndTheSuspended
    retentionKeepsTheNewestOfEachEntry
    byteCapSparesTheRunThatTrippedIt
    replayEchoesAndRefuses
    secretsAreNotInTheLog
    spawnedChildReplays
    secretHeadersAreRedacted
    psListsTheTree ]
