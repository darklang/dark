/// Executions: a run kept beside its trace, resumed and forked by replaying the log of its
/// effectful calls (`docs/processes.md`, "Executions"). Driven through the CLI, since that is
/// where a run is made, suspended and taken up again.
module Tests.CliExec

open Expecto
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

open TestUtils.TestUtils
open Tests.CliTestHarness
open Tests.CliDsl

module Executions = LibDB.Executions


/// The most recent execution's id, as `exec` shows it (the first eight characters).
let private latestPrefix () : Task<string> =
  task {
    let! rows = Executions.list 1
    match rows with
    | e :: _ -> return (string e.id).Substring(0, 8)
    | [] -> return failtest "no execution was recorded"
  }

let private latest () : Task<Executions.Execution> =
  task {
    let! rows = Executions.list 1
    match rows with
    | e :: _ -> return e
    | [] -> return failtest "no execution was recorded"
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
        Expect.equal e.status Executions.Done "and the run is done again"
      })


let private forkDivergesAfterThePosition =
  cliTestWithFreshTraces
    "a fork keeps the log up to a position and goes live after it"
    (fun state ->
      task {
        let! first = runCli state [ "eval"; twoUuids ]
        let! parent = latest ()
        let prefix = (string parent.id).Substring(0, 8)
        // Position 1: the first uuid's row (seq 0) is kept, the second (seq 1) is not.
        let! forked = runCli state [ "exec"; "fork"; prefix; "--at"; "1" ]
        Expect.stringContains forked "forked" "the fork is announced"
        let! child = latest ()
        Expect.equal child.status Executions.Suspended "a fork waits to be resumed"
        Expect.equal
          child.parent
          (Some(parent.id, 1L))
          "and knows where it came from"
        let! shown =
          runCli state [ "exec"; "show"; (string child.id).Substring(0, 8) ]
        Expect.stringContains shown "forked from" "show says so"
        let! resumed =
          runCli state [ "exec"; "resume"; (string child.id).Substring(0, 8) ]
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
          "let a = Stdlib.Uuid.generate ()\nlet _ = Stdlib.Cli.Posix.sleep 2000.0\nlet b = Stdlib.Uuid.generate ()\nStdlib.String.join [ Stdlib.Uuid.toString a, Stdlib.Uuid.toString b ] \" \""
        let running = runCli state [ "eval"; program ]
        // Until the run is in the foreground, then a moment more for the first uuid.
        let deadline = System.DateTime.UtcNow.AddSeconds 5.
        while (Executions.Foreground.currentId ()).IsNone
              && System.DateTime.UtcNow < deadline do
          do! Task.Delay 10
        do! Task.Delay 300
        let! suspended = Executions.Foreground.suspend ()
        Expect.isSome suspended "the foreground run was suspended"
        let! e = latest ()
        Expect.equal e.status Executions.Suspended "and marked so"
        let! log = Executions.log e.traceId
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
          Executions.Suspended
          "the run's own ending left the suspend alone"
        let! resumed =
          runCli state [ "exec"; "resume"; (string e.id).Substring(0, 8) ]
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
      })


let tests =
  [ recordThenResume
    forkDivergesAfterThePosition
    suspendThenResume
    replayAfterAnEdit ]
