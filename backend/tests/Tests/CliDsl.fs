/// A small vocabulary for CLI tests, so a test reads like the session it describes.
///
/// The tests underneath this are end-to-end: they drive `dark` the way a person does, and what they
/// assert on is what a person sees. That makes them valuable and it made them verbose, because every
/// step was `let! _ = runCli state [ "commit"; msg; "-y" ]` and every assertion was three lines of
/// `Expect.stringContains` with a hand-written message. The noise was most of the file, and the cost
/// showed up as tests nobody wrote.
///
/// So: verbs for the things a test DOES (`fn`, `commit`, `switch`), and assertions for the things a
/// test CLAIMS (`shows`, `lacks`, `evals`, `refuses`). Each assertion puts the command's real output
/// in its failure message without the caller composing one, which is the other thing that was being
/// skipped.
///
/// Anything a verb here does not cover is still `runCli`, which these are built from. Reach for it
/// rather than bending a verb into a shape it does not have.
module Tests.CliDsl

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness


// ─── doing ────────────────────────────────────────────────────────────────

/// Run a command for its EFFECT, discarding what it printed. The workhorse the rest are built on.
let run (state : RT.ExecutionState) (args : List<string>) : Task<unit> =
  task {
    let! _ = runCli state args
    return ()
  }

/// Author a function. `body` is the source after the name, as you would type it.
let fn (state : RT.ExecutionState) (name : string) (body : string) : Task<unit> =
  run state [ "fn"; name; body ]

/// Author a value.
let value (state : RT.ExecutionState) (name : string) (body : string) : Task<unit> =
  run state [ "val"; name; body ]

/// Commit the whole draft. `-y` always: a test that waits on a prompt hangs the suite, and CI gives
/// the run a pty, so "nobody is there" is not a refusal it can rely on.
let commit (state : RT.ExecutionState) (message : string) : Task<unit> =
  run state [ "commit"; message; "-y" ]

/// Commit part of the draft, by name.
let commitOnly
  (state : RT.ExecutionState)
  (message : string)
  (names : string)
  : Task<unit> =
  run state [ "commit"; message; $"--include={names}"; "-y" ]

/// Move onto a branch, starting it if it is new.
let switch (state : RT.ExecutionState) (branch : string) : Task<unit> =
  run state [ "switch"; branch ]

let onMain (state : RT.ExecutionState) : Task<unit> = switch state "main"

/// Drop the whole draft.
let discardAll (state : RT.ExecutionState) : Task<unit> =
  run state [ "discard"; "-y" ]

/// Drop one name from the draft.
let discardName (state : RT.ExecutionState) (name : string) : Task<unit> =
  run state [ "discard"; name; "-y" ]

let merge (state : RT.ExecutionState) (branch : string) : Task<unit> =
  run state [ "merge"; branch ]

let rebase (state : RT.ExecutionState) (branch : string) : Task<unit> =
  run state [ "rebase"; branch ]

let deprecate (state : RT.ExecutionState) (name : string) : Task<unit> =
  run state [ "deprecate"; "fn"; name; "--kind"; "obsolete"; "-y" ]

let pin (state : RT.ExecutionState) (name : string) : Task<unit> =
  run state [ "propagate"; "pin"; name ]

/// A clean slate on main: no draft, standing where every test expects to start.
let start (state : RT.ExecutionState) : Task<unit> =
  task {
    do! onMain state
    do! discardAll state
  }


// ─── claiming ─────────────────────────────────────────────────────────────

/// This command's output contains `expected`. The failure message carries what it actually printed,
/// always: an assertion that says only "expected X" makes you re-run the thing by hand to learn
/// anything, and these commands are not cheap to re-run.
let shows
  (state : RT.ExecutionState)
  (args : List<string>)
  (expected : string)
  (why : string)
  : Task<unit> =
  task {
    let! out = runCli state args
    Expect.stringContains out expected $"{why}, got: {out}"
  }

/// This command's output does NOT contain `unexpected`.
let lacks
  (state : RT.ExecutionState)
  (args : List<string>)
  (unexpected : string)
  (why : string)
  : Task<unit> =
  task {
    let! out = runCli state args
    Expect.isFalse (out.Contains unexpected) $"{why}, got: {out}"
  }

/// Case-insensitive `shows`, for output whose casing is a display decision rather than a fact
/// (a "DEPRECATED" banner, say).
let showsAnyCase
  (state : RT.ExecutionState)
  (args : List<string>)
  (expected : string)
  (why : string)
  : Task<unit> =
  task {
    let! out = runCli state args
    Expect.stringContains (out.ToLower()) (expected.ToLower()) $"{why}, got: {out}"
  }

let lacksAnyCase
  (state : RT.ExecutionState)
  (args : List<string>)
  (unexpected : string)
  (why : string)
  : Task<unit> =
  task {
    let! out = runCli state args
    Expect.isFalse
      (out.ToLower().Contains(unexpected.ToLower()))
      $"{why}, got: {out}"
  }

/// Evaluating `expr` prints `expected`. The commonest assertion in the file: it is how a test says
/// "and the code actually does this now", which is the only claim that cannot be faked by a message.
let evals
  (state : RT.ExecutionState)
  (expr : string)
  (expected : string)
  (why : string)
  : Task<unit> =
  shows state [ "eval"; expr ] expected why

/// `expr` does not resolve. Spelled out rather than `evals ... "not found"` so the intent survives a
/// change to the wording.
let notFound
  (state : RT.ExecutionState)
  (expr : string)
  (why : string)
  : Task<unit> =
  shows state [ "eval"; expr ] "not found" why

/// This command REFUSES, saying `reason`, and does not also claim to have done the thing. The second
/// half matters: several bugs here printed a refusal and a success in the same breath.
let refuses
  (state : RT.ExecutionState)
  (args : List<string>)
  (reason : string)
  (claimOfSuccess : string)
  (why : string)
  : Task<unit> =
  task {
    let! out = runCli state args
    Expect.stringContains out reason $"{why}, got: {out}"
    Expect.isFalse
      (out.Contains claimOfSuccess)
      $"{why}: it refused AND claimed success, got: {out}"
  }

/// The draft holds nothing.
let clean (state : RT.ExecutionState) (why : string) : Task<unit> =
  shows state [ "status" ] "clean" why

/// The draft holds something.
let dirty (state : RT.ExecutionState) (why : string) : Task<unit> =
  shows state [ "status" ] "changed" why
