/// Executions: a run kept beside its trace, resumed and forked by replaying the log of its
/// effectful calls (`docs/processes.md`, "Executions"). Driven through the CLI, since that is
/// where a run is made, suspended and taken up again.
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
        let! shown = runCli state [ "exec"; "inspect"; prefixOf child ]
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
        do! fn state "Tests.Exec.shape" "(s: String) : String = \"v1:\" + s"
        do! commit state "shape v1"
        let! first =
          runCli
            state
            [ "eval"
              "Tests.Exec.shape (Stdlib.Uuid.toString (Stdlib.Uuid.generate ()))" ]
        Expect.stringStarts first "v1:" "the first run went through v1"
        do! fn state "Tests.Exec.shape" "(s: String) : String = \"v2:\" + s"
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
        // Indented, because the resume header echoes the input and the input CALLS this
        // function, so an unindented substring check passed on the header whether or not
        // the moved list named anything. The moved names are printed four spaces in.
        Expect.stringContains
          resumed
          "    Tests.Exec.shape"
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
        do! fn state "Tests.Exec.steady" "(s: String) : String = \"steady:\" + s"
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
    "traces calls finds the traces, and traces show replays one without performing its effects"
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
          let candidates =
            out.Split('\n')
            |> Array.map (fun l -> l.Replace("[replayed]", "").Trim())
            |> Array.filter (fun l -> l.Contains "-" && l.Length > 60)

          if Array.isEmpty candidates then
            // `Array.last` on empty threw away the output and reported "the input array was
            // empty", which says nothing about what the command actually printed.
            failtest $"no line held two uuids. The command printed:\n{out}"
          else
            Array.last candidates
        let! first = runCli state [ "eval"; program ]
        let line = uuidLine first
        let! e = latest ()
        let! resumed = runCli state [ "exec"; "resume"; prefixOf e ]
        Expect.equal
          (uuidLine resumed)
          line
          "the child's uuid and the parent's own both came from the log, not from a fresh roll"
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
            ("(n: Int) : Int =\n"
             + "  Stdlib.List.range 1 n\n"
             + "  |> Stdlib.List.map (fun i -> i * 2)\n"
             + "  |> Stdlib.List.length")
        do! commit state "pure"
        let! _ = runCli state [ "permissions"; "approve"; "Tests.Prev.pure" ]
        let! ran = runCli state [ "eval"; "Tests.Prev.pure 5" ]
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


/// Deleting a trace has to take its loop counts with it.
///
/// `trace_loops` arrived with this branch and every delete path was written before it existed,
/// so a deleted trace left its rows behind. Nothing can ever read them again: every query is
/// `WHERE trace_id = @t` against a trace that is gone. They are invisible, they only grow, and
/// retention runs on its own, so a long-lived store accumulates them for as long as it lives.
/// Recursion is a loop, and an iteration that never reached a line must not borrow another's.
///
/// Both of these are the same failure in different clothes, and both shipped wrong until they
/// were walked by hand. `fact 5` recursed five times and the page showed the last call's numbers
/// with no footer and nothing saying there had been others, because only LAMBDA frames counted as
/// a loop. And asking for the base case, which returns before the lines below it run, printed the
/// first call's values under a heading naming the fifth: the lines fell through to the flat
/// last-value-wins figure.
let private recursionIsALoopAndEmptyIterationsStaySilent =
  cliTestWithFreshTraces
    "recursion has iterations, and one that returned early shows no values rather than another's"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.Prev.fact"
            ("(n: Int) : Int =\n"
             + "  if n <= 1 then\n"
             + "    1\n"
             + "  else\n"
             + "    let sub = Tests.Prev.fact (n - 1)\n"
             + "    n * sub")
        do! commit state "fact"
        let! _ = runCli state [ "permissions"; "approve"; "Tests.Prev.fact" ]
        let! ran = runCli state [ "eval"; "Tests.Prev.fact 5" ]
        Expect.stringContains ran "120" "the run went five deep"

        // Five entries into the same function, so five iterations, named after it.
        let! viewed = runCli state [ "traces"; "show"; "Tests.Prev.fact" ]
        Expect.stringContains
          viewed
          "fact: showing iteration 5 of 5"
          "recursion is counted as a loop, not shown as a single call"

        // The second call is `fact 4`: its sub is 6 and its product is 24. These are the values
        // that were unreachable before, because only the last call's survived.
        let! second =
          runCli
            state
            [ "traces"; "show"; "Tests.Prev.fact"; "--iteration"; "fact:2" ]
        Expect.stringContains
          second
          "// = 24"
          "the second iteration has its own product"
        Expect.stringContains second "// = 6" "and its own sub"
        // And what it was GIVEN, which is the other half of reading an iteration: the values of
        // a call mean nothing without the inputs that produced them. `fact 5` is what the RUN
        // was given; `n = 4` is what this time round was.
        Expect.stringContains
          second
          "n = 4"
          "and says what this iteration was given"
        Expect.stringContains
          second
          "given"
          "while still saying what the whole run was given"

        // The fifth is the base case. It returns before `sub` and the product ever run, so those
        // lines have no value in it -- and must not show the ones that another iteration left.
        let! baseCase =
          runCli
            state
            [ "traces"; "show"; "Tests.Prev.fact"; "--iteration"; "fact:5" ]
        Expect.stringContains
          baseCase
          "fact: showing iteration 5 of 5"
          "it is the iteration that was asked for"
        Expect.isFalse
          (baseCase.Contains "// = 24")
          "the base case did not run the product, so it must not carry iteration 2's value"
        Expect.isFalse (baseCase.Contains "// = 120") "nor the first iteration's"
      })


/// You could always ask a function for its runs. This is the other direction.
///
/// `traces stats` groups by entry, so sixty-six runs collapse into one row saying `eval`, and
/// `hotspots` reads the effect log, which holds only impure calls, so every name it can print is
/// a builtin. Neither answers "what have recent runs been about". `trace_fns` always held the
/// answer and nothing read it.
/// The first few iterations and the last few are held; any other is fetched when asked for.
///
/// Holding every iteration of every loop means the values scale with the RUN rather than with
/// what can be shown, and `fib 20` pushes twenty-two thousand frames. Holding only the first few
/// means the END of a long loop -- usually the half worth seeing -- is unreachable. So: a window
/// at each end, and anything in between costs one more replay, which is what opening the trace
/// costs anyway. The point is that nothing is unreachable however long the loop is.
let private everyIterationOfALoopIsReachable =
  cliTestWithFreshTraces
    "the view holds the first and last iterations, and fetches any other when asked"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.Prev.scaled"
            ("(n: Int) : Int =\n"
             + "  Stdlib.List.range 1 n\n"
             + "  |> Stdlib.List.map (fun i ->\n"
             + "    let big = i * 100\n"
             + "    big)\n"
             + "  |> Stdlib.List.length")
        do! commit state "scaled"
        let! _ = runCli state [ "permissions"; "approve"; "Tests.Prev.scaled" ]
        let! ran = runCli state [ "eval"; "Tests.Prev.scaled 30" ]
        Expect.stringContains ran "30" "the loop went round thirty times"

        // In the head, in the tail, and the ones in between that are the whole point: 7 and 15
        // are outside the window at both ends and have to be gone and got.
        for (which, expected) in [ 1, "100"; 7, "700"; 15, "1500"; 30, "3000" ] do
          let! shown =
            runCli
              state
              [ "traces"
                "show"
                "Tests.Prev.scaled"
                "--iteration"
                $"map:{which}" ]
          Expect.stringContains
            shown
            $"// = {expected}"
            $"iteration {which} shows its own value, not another iteration's"
          Expect.stringContains
            shown
            $"showing iteration {which} of 30"
            $"and the page says which one it is"

        // The same reach for an agent. `--json` used to ignore `--iteration` entirely, so an
        // agent saw the first few and the last few of a long loop and had no way to ask for the
        // rest, while a person could. The two are one question asked by two readers.
        let! asJson =
          runCli
            state
            [ "traces"
              "show"
              "Tests.Prev.scaled"
              "--json"
              "--iteration"
              "map:15" ]
        Expect.stringContains
          asJson
          "\"at\":15"
          "the agent gets the iteration it asked for"
        Expect.stringContains asJson "1500" "with its own value"
      })


let private tracesFnsNamesWhatRunsWentThrough =
  cliTestWithFreshTraces
    "traces fns names the functions recent runs went through, bundled library hidden"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.Prev.counted"
            "(xs: List<Int>) : Int =\n  Stdlib.List.length xs"
        do! commit state "counted"
        let! _ = runCli state [ "permissions"; "approve"; "Tests.Prev.counted" ]
        let! ran = runCli state [ "eval"; "Tests.Prev.counted [1, 2, 3]" ]
        Expect.stringContains ran "3" "the run happened"

        let! listed = runCli state [ "traces"; "fns" ]
        Expect.stringContains
          listed
          "Tests.Prev.counted"
          "the function the run went through, which is the whole point of the verb"
        // The run also went through `Darklang.Stdlib.List.length`, and so does almost every run.
        // Listing it buries the handful of names that are actually yours.
        Expect.isFalse
          (listed.Contains "Darklang.Stdlib")
          "the bundled library is hidden unless asked for"

        let! all = runCli state [ "traces"; "fns"; "--all" ]
        Expect.stringContains all "Tests.Prev.counted" "yours are still there"
        Expect.stringContains
          all
          "Darklang.Stdlib"
          "--all puts the bundled library back, or the flag does nothing"
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
/// Two names for one function find the same runs, because they ARE one function.
///
/// Dark is content-addressed: byte-identical bodies have one hash, and the name is a label on
/// it. The index records whichever name resolved when the run happened, so asking by name found
/// nothing under the other one and said "no recorded trace went through it" about code that had
/// just run. The lookup asks by hash now, and the name is only used when there is no hash --
/// which is the case where the store no longer has the function at all.
let private twoNamesForOneFunctionFindTheSameRuns =
  cliTestWithFreshTraces
    "an alias of a function finds its runs, because content addressing makes them one function"
    (fun state ->
      task {
        do! start state
        let body = "(n: Int) : Int =\n  n * 3"
        do! fn state "Tests.Prev.tripled" body
        do! fn state "Tests.Other.tripled" body
        do! commit state "tripled twice"
        let! _ = runCli state [ "permissions"; "approve"; "Tests.Prev.tripled" ]
        let! ran = runCli state [ "eval"; "Tests.Prev.tripled 4" ]
        Expect.stringContains ran "12" "one of them ran"

        let! other = runCli state [ "traces"; "calls"; "Tests.Other.tripled" ]
        Expect.stringContains
          other
          "eval"
          "the run is found under the name that did NOT run, because it is the same code"
        Expect.isFalse
          (other.Contains "no recorded trace")
          "asking by name alone used to answer this with nothing"
      })


let private aFunctionThatRanAndWentIsNotATypo =
  cliTestWithFreshTraces
    "a function that ran and whose code has gone reads differently from a typo"
    (fun state ->
      task {
        do! start state
        let! _ = runCli state [ "eval"; "1L" ]
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
            ("(n: Int) : Int =\n"
             + "  Stdlib.List.range 1 n\n"
             + "  |> Stdlib.List.map (fun i -> i * 2)\n"
             + "  |> Stdlib.List.length")
        do! commit state "looped"
        let! _ = runCli state [ "permissions"; "approve"; "Tests.Prev.looped" ]
        let! _ = runCli state [ "eval"; "Tests.Prev.looped 4" ]

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


/// Two identical calls in one run have two answers, and the view has to show both.
///
/// The preview keys recorded results on (name, arguments), which is what lets a view survive the
/// code moving. On its own it cannot tell two identical calls apart: `Uuid.generate ()` twice
/// has one key and two results, and last-write-wins served the second to both callers. The run
/// made two different uuids and the view showed one of them, twice, with no error and no marker.
/// That is the worst shape a bug can take in a debugging tool.
let private identicalCallsKeepTheirOwnValues =
  cliTestWithFreshTraces
    "two identical calls in one run show their own recorded values, not one twice"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.Prev.two"
            ("() : String =\n"
             + "  let a = Stdlib.Uuid.toString (Stdlib.Uuid.generate ())\n"
             + "  let b = Stdlib.Uuid.toString (Stdlib.Uuid.generate ())\n"
             + "  $\"{a} {b}\"")
        do! commit state "two"
        let! _ = runCli state [ "permissions"; "approve"; "Tests.Prev.two" ]
        let! ran = runCli state [ "eval"; "Tests.Prev.two ()" ]

        // The run really did make two different uuids. If it did not, the rest proves nothing.
        let ranPair =
          ran.Split('\n')
          |> Array.map (fun l -> l.Trim())
          |> Array.filter (fun l -> l.Contains " " && l.Length > 60)
          |> Array.tryLast
        match ranPair with
        | None -> failtest "the run did not print two uuids"
        | Some pair ->
          let parts = pair.Split(' ')
          Expect.notEqual
            parts[0]
            parts[1]
            "the run itself made two different uuids"

          let! viewed = runCli state [ "traces"; "show"; "Tests.Prev.two" ]
          // Each `let` line carries its own recorded value. Collapsed, both lines showed the
          // same one.
          let shown =
            viewed.Split('\n')
            |> Array.filter (fun l ->
              l.Contains "Uuid.generate" && l.Contains "// =")
            |> Array.map (fun l -> l.Substring(l.IndexOf "// =").Trim())
          Expect.equal shown.Length 2 "both calls carry a value"
          Expect.notEqual
            shown[0]
            shown[1]
            "and they are the two the run made, not the second one twice"
      })


/// Looking at code must never touch the world, and a SPAWNED process is the case where that
/// was easiest to get wrong.
///
/// The scheduler hands each process its own tracer, and it decided whether to bother by asking
/// whether anything was being recorded. A preview records nothing, so the answer was no, and a
/// spawned child inherited the DEFAULT tracer: the one that performs effects for real. Viewing
/// a recorded run that used `parallelMap`, or any spawn, would have run its writes again.
///
/// A uuid rather than a print, because the child's stdout does not come back through the
/// harness and its absence would prove nothing. A uuid is evidence either way: served from the
/// log it matches the recording, rolled for real it cannot. The spawn ITSELF is performed again
/// on purpose (`Redact.performAgain`), so a real child process is created here and the question
/// is only which tracer it gets.
let private previewOfASpawnServesTheChildFromTheLog =
  cliTestWithFreshTraces
    "previewing a run that spawned a process serves the child from the log, not the world"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.Prev.conc"
            ("() : String =\n"
             + "  let h = Stdlib.Exec.spawn (fun () -> Stdlib.Uuid.toString (Stdlib.Uuid.generate ()))\n"
             + "  Stdlib.Exec.await h")
        do! commit state "conc"
        let! _ = runCli state [ "permissions"; "approve"; "Tests.Prev.conc" ]

        let! ran = runCli state [ "eval"; "Tests.Prev.conc ()" ]
        let uuidIn (out : string) : string option =
          out.Split('\n')
          |> Array.map (fun l -> l.Trim().Trim('"'))
          |> Array.filter (fun l -> l.Length = 36 && l.Split('-').Length = 5)
          |> Array.tryLast
        let recorded =
          match uuidIn ran with
          | Some u -> u
          | None -> failtest $"the run did not produce a uuid: {ran}"

        let! viewed = runCli state [ "traces"; "show"; "Tests.Prev.conc" ]
        // The value beside `await h` is what the child made. If the child ran for real during
        // the preview it is a fresh uuid, and the view is showing something that never happened.
        Expect.stringContains
          viewed
          recorded
          "the child's value came from the log, so the spawned process previewed too"
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
    previewOfASpawnServesTheChildFromTheLog
    identicalCallsKeepTheirOwnValues
    pureLoopPassesAreCounted
    recursionIsALoopAndEmptyIterationsStaySilent
    everyIterationOfALoopIsReachable
    tracesFnsNamesWhatRunsWentThrough
    twoNamesForOneFunctionFindTheSameRuns
    aFunctionThatRanAndWentIsNotATypo
    deletingATraceTakesItsLoopCounts
    retentionKeepsTheNewestAndTheSuspended
    retentionKeepsTheNewestOfEachEntry
    byteCapSparesTheRunThatTrippedIt
    replayEchoesAndRefuses
    secretsAreNotInTheLog
    spawnedChildReplays
    secretHeadersAreRedacted
    psListsTheTree ]
