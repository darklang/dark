/// The CLI's command surface as a whole: the registry-driven sweeps (every command
/// bare, with --help, with a bogus argument, standing on a branch), the workbench
/// views on their frame contract, and the docs' claims checked against the registry.
/// Run order and sequencing live in CliTraces.Tests.fs, which composes this list.
module Tests.CliSurface

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

open Tests.CliTestHarness

module CliDsl = Tests.CliDsl


let private reusesCompiledFunctions =
  cliTest "the CLI harness reuses compiled package functions" (fun target ->
    task {
      let state = executionState target
      let hash = RT.Hash(LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())
      let! first = state.fns.package hash |> Ply.toTask
      let! second = state.fns.package hash |> Ply.toTask
      match first, second with
      | Some first, Some second ->
        Expect.isTrue
          (System.Object.ReferenceEquals(first, second))
          "fetching a function again must reuse its compiled instructions"
      | _ -> Tests.failtest "the CLI entry point must be available"
    })

let private timeoutBoundsSynchronousWork =
  testTask "the CLI timeout bounds work before its first await" {
    let release =
      TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
    let stopped =
      TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
    try
      let! result =
        runWithTimeout (System.TimeSpan.FromMilliseconds 100.0) (fun () ->
          try
            // A bounded wait keeps a broken timeout from hanging this regression.
            release.Task.Wait(System.TimeSpan.FromSeconds 5.0) |> ignore<bool>
            Task.FromResult 42
          finally
            stopped.SetResult())
      Expect.isNone result "the deadline wins while the synchronous work is blocked"
    finally
      release.SetResult()
    do! stopped.Task.WaitAsync(System.TimeSpan.FromSeconds 5.0)
  }

let private timeoutPreservesCapture =
  testTask "the CLI timeout preserves results and captured output" {
    Expect.isTrue (NonBlockingConsole.startCapture ()) "capture starts"
    try
      let! result =
        runWithTimeout (System.TimeSpan.FromSeconds 5.0) (fun () ->
          print "captured by the caller"
          Task.FromResult 42)
      Expect.equal result (Some 42) "the result crosses the worker boundary"
      Expect.equal
        ((NonBlockingConsole.stopCapture ()).Trim())
        "captured by the caller"
        "the worker inherits the caller's output capture"
    finally
      NonBlockingConsole.stopCapture () |> ignore<string>
  }

/// stderr shares stdout's queue and capture window. Written straight to the console, an error could
/// print above the output that led to it and escaped the workbench's in-frame capture.
let private stderrIsCapturedInOrder =
  cliTest "stderr is captured apart from stdout, and in order with it" (fun target ->
    task {
      let program =
        "let _ = Stdlib.printLine \"first\" in "
        + "let _ = Stdlib.printErrorLine \"second\" in "
        + "let _ = Stdlib.printLine \"third\" in 4L"

      let! (out, err, status) = runCliStreams target [ "eval"; program ]
      Expect.equal status 0 "the eval succeeded"
      Expect.equal out "first\nthird\n4" "stdout holds the answer and nothing else"
      Expect.equal err "second" "stderr holds the one line written to it"

      let! both = runCli target [ "eval"; program ]
      Expect.equal
        both
        "first\nsecond\nthird\n4"
        "runCli keeps both streams, in order"
    })

let private testHelpCommand =
  cliTest "help command" (fun state ->
    task {
      let! output = runCli state [ "help" ]
      Expect.stringContains output "Packages:" "category header"
      Expect.stringContains output "Changes:" "changes header"
      Expect.stringContains output "Branches:" "branches header"
      Expect.stringContains output "Sync:" "sync header"
      Expect.stringContains output "help" "help command"
      Expect.stringContains output "version" "version command"
      Expect.stringContains output "status" "status command"
    })

/// Every registered command name, read out of `dark help`.
///
/// `help` prints each as `  name (alias, ...) - description`, so a name is the first token of any
/// indented line containing " - ". Parsing the human output is deliberate: a command that stops
/// appearing on the surface a person sees has stopped existing.
let private registeredCommands (state : Target) : Task<List<string>> =
  task {
    let! output = runCli state [ "help" ]

    return
      output.Split('\n')
      |> Array.toList
      |> List.choose (fun line ->
        if line.StartsWith "  " && line.Contains " - " then
          let name = line.Trim().Split(' ')[0]
          if name = "" || name.StartsWith "-" then None else Some name
        else
          None)
      |> List.distinct
  }

/// Phrases that mean "you used this wrong". A request for HELP must never be answered with one.
///
/// Deliberately NOT "usage:" or "required": both appear in good help text, and a check that flags
/// them flags forty commands and gets switched off. `--help` is handled centrally; bare `help` is
/// each command's own job.
let private soundsLikeMisuse (output : string) : bool =
  let o = output.ToLower()
  [ "error:"
    "internal error"
    "is not a valid"
    "unknown topic"
    "unknown command" ]
  |> List.exists (fun phrase -> o.Contains phrase)

/// A runtime error a command caught and PRINTED, so the run itself looked fine. Returns the offending
/// line. `runCliCatching` already reports an error that escaped; this is for the one that did not: the
/// Dark-side path where a command prints `ExecutionError.toString` and carries on. The four phrases are
/// the ones AGENTS.md says to grep a sweep's output for; `Internal error:` is a host exception a command
/// What is wrong with a swept command's answer, or `None` if nothing is.
///
/// The sweeps differ in what they hand a command, never in how they judge what comes back: it must
/// not throw, it must say something, and it must not print a runtime error it caught. The `help`
/// sweep is the exception -- it asks a further question of the text -- so it judges its own.
let private sweepFailure (outcome : Result<string, string>) : Option<string> =
  match outcome with
  | Error e -> Some $"crashed: {e}"
  | Ok output ->
    if output.Trim() = "" then
      Some "said nothing"
    else
      looksLikeARuntimeFailure output
      |> Option.map (fun line -> $"printed a runtime error: {line}")

// The workbench renders. `initialState` builds a state without seizing the terminal, and this goes
// through `dark eval` rather than a `.dark` testfile because building one reads the package tree,
// and the execution testfiles are for pure functions (see `scm/propagation-policy.dark`).

/// One line, because `eval` takes the expression as a single argument.
let private renderExpr (body : string) : string =
  "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Tester\" \"test-instance\" [] false in "
  + "let s = Darklang.Cli.Workbench.refreshScmStatus st st in "
  + "let frame = fun v w h -> (Darklang.Cli.Workbench.viewAtSize { s with activeView = v } (Darklang.Stdlib.Cli.Tui.Size { width = w; height = h })).rows in "
  + body

let private workbenchViewsRender =
  cliTest "every workbench view renders a full frame" (fun state ->
    task {
      // A view that throws produces no frame at all, which is what these row counts
      // are really checking.
      let! output =
        runCli
          state
          [ "eval"
            renderExpr
              "let renderable = Darklang.Cli.Workbench.viewSpecs |> Stdlib.List.filter (fun (v, _name, _gate) -> v != Darklang.Cli.Workbench.vDevices) in Stdlib.List.all renderable (fun (v, _name, _gate) -> Stdlib.List.length (frame v 120 40) == 40)" ]

      Expect.stringContains
        output
        "true"
        "every registered view renders a full 40-row frame"
    })

/// The key-hint row must not drop the way out.
///
/// Clipping one long string from the right loses the hints at the END -- `?` (the keymap) and
/// `esc/q` (quit), the two a person needs most when lost. The row drops whole hints cheapest-first
/// instead: secondary globals, then the view's own actions, and `?`/`esc/q` never.
let private hintRowKeepsTheWayOut =
  cliTest
    "the hint row drops secondary keys before it drops help and quit"
    (fun state ->
      task {
        let lastRow (w : int) : string =
          "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Stachu\" \"i\" [] false in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
          + "let s1 = { s0 with activeView = 4 } in "
          + "let s = { s1 with items = Darklang.Cli.Workbench.reloadItems s1 } in "
          // Tall enough that the hint row is always in frame: a clean tree renders a
          // roomier panel than a draft, so a short frame loses the hints in one store.
          + $"Stdlib.String.join (Darklang.Cli.Workbench.viewAtSize s (Darklang.Stdlib.Cli.Tui.Size {{ width = {w}; height = 26 }})).rows \"\\n\""

        // 168, not 160: the SCM row gained `u/d push/pull` when sync became an action here rather than a
        // readout, and one more action is one less secondary global that fits. The rule under test is the
        // ORDER things are dropped in, not the width at which dropping starts.
        let! wide = runCli state [ "eval"; lastRow 168 ]
        Expect.stringContains
          wide
          "views"
          "the full row advertises the secondary keys too"

        // Narrow enough that something has to go. Single words, not "? help": the key
        // and its label are coloured separately, so an escape sequence sits between
        // them and a two-word substring never matches.
        //
        // The pane hints are the NAVBAR's here, since that is what holds the keyboard when the workbench
        // opens; they are still "the view's own actions" as far as the drop order is concerned.
        let! narrow = runCli state [ "eval"; lastRow 72 ]
        Expect.stringContains narrow "help" "the keymap key survives"
        Expect.stringContains narrow "quit" "and so does the way out"
        Expect.stringContains
          narrow
          "move"
          "the pane's own actions outlive the secondary globals"
        Expect.isFalse
          (narrow.Contains "prompt")
          "and a secondary global is what gave way to fit them"

        // Narrower still: actions start giving way, but never the escape hatches.
        let! tiny = runCli state [ "eval"; lastRow 56 ]
        Expect.stringContains tiny "help" "the keymap key still survives"
        Expect.stringContains tiny "quit" "and the way out is still there"
      })

/// The header must not overwrite its own tail on a narrower terminal.
///
/// It writes the left-hand text at column 0 and the sync glance right-aligned over the same row, so
/// anything the left side spills past the glance is lost. It drops whole segments in priority order
/// instead (the instance first, then the account), so the branch survives far narrower.
let private headerKeepsTheBranchWhenNarrow =
  cliTest
    "the header drops the instance before the account, and never the branch"
    (fun state ->
      task {
        // A deliberately long instance name, so the row is over-full whatever the shared
        // store holds: the test creates the condition rather than hoping.
        let row (w : int) : string =
          "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Stachu\" \"inst-with-a-deliberately-long-name-for-this-test\" [] false in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
          + "let s1 = { s0 with activeView = 4 } in "
          + "let s = { s1 with items = Darklang.Cli.Workbench.reloadItems s1 } in "
          + $"Stdlib.String.join (Stdlib.List.take (Darklang.Cli.Workbench.viewAtSize s (Darklang.Stdlib.Cli.Tui.Size {{ width = {w}; height = 4 }})).rows 1) \"\""

        let! wide = runCli state [ "eval"; row 150 ]
        Expect.stringContains
          wide
          "Stachu @ inst-with-a-deliberately-long-name-for-this-test"
          "the full row shows who and where"
        Expect.stringContains wide "main" "and the branch"

        let! narrow = runCli state [ "eval"; row 70 ]
        Expect.isFalse
          (narrow.Contains "inst-with-a-deliberately")
          "the instance is dropped to make room, rather than the row colliding"
        Expect.stringContains narrow "Stachu" "the account survives, being shorter"
        Expect.stringContains
          narrow
          "main"
          "and so does the branch, being worth more than either"
      })

let private workbenchBranchActionsWork =
  cliTestOnMain
    "the workbench can start, switch and merge a branch without throwing"
    (fun state ->
      task {
        let act (action : string) (text : string) : string =
          "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"T\" \"t\" [] false in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
          + "let s = { s0 with activeView = 4 } in "
          + $"match Darklang.Cli.Workbench.performInputAction s (Darklang.Cli.Workbench.InputState {{ prompt = \"p\"; field = Stdlib.Cli.UI.TextField.fromText \"{text}\"; action = \"{action}\" }}) with "
          + "| Continue ns -> ns.message | _ -> \"(exit)\""

        let! created = runCli state [ "eval"; act "branch-create" "wbTestBranch" ]
        Expect.stringContains
          created
          "on branch wbTestBranch"
          "starting a branch lands you on it, rather than throwing on an Option"

        let! switched = runCli state [ "eval"; act "branch-switch" "wbTestBranch" ]
        Expect.stringContains switched "switched to" "and it can be switched to"

        // These act on `state.branchId`, which is main here, so merge refuses because
        // you are not on a branch at all. That is the gate, not a crash.
        let! merged = runCli state [ "eval"; act "merge" "y" ]
        Expect.stringContains
          merged
          "main is the trunk"
          "and merge reports the gate rather than throwing"
      })

/// Neither merge nor rebase means anything on main, and both must say so.
///
/// The message names MAIN rather than where you are standing: the same gate answers `dark rebase main`
/// typed from a branch, and "you're on main" was a plain lie there.
///
/// A rebase on main rewrites nothing -- it loops over a branch's `branch_name_bases` rows and main
/// has none -- so a success message would be a no-op you believe, and "the branch has no changes"
/// counts the changes of a branch you are not on.
let private mergeAndRebaseRefuseOnMain =
  cliTest
    "merge and rebase refuse on main, rather than claiming to have run"
    (fun state ->
      task {
        let act (action : string) : string =
          "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"T\" \"t\" [] false in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
          + "let s = { s0 with activeView = 4 } in "
          + $"match Darklang.Cli.Workbench.performInputAction s (Darklang.Cli.Workbench.InputState {{ prompt = \"p\"; field = Stdlib.Cli.UI.TextField.fromText \"y\"; action = \"{action}\" }}) with "
          + "| Continue ns -> ns.message | _ -> \"(exit)\""

        let! rebased = runCli state [ "eval"; act "rebase" ]
        Expect.stringContains
          rebased
          "main is the trunk"
          "rebase names the reason rather than reporting a rebase that did not happen"
        Expect.isFalse
          (rebased.Contains "rebased onto parent")
          "and does not claim success"

        let! merged = runCli state [ "eval"; act "merge" ]
        Expect.stringContains
          merged
          "main is the trunk"
          "merge names the same reason"
      })

/// Displaying a commit's ops must not fetch all of them.
///
/// The seed commit holds about twelve thousand. Taking the whole list and keeping the first handful
/// deserializes every blob to discard almost all of them, which in the workbench is a freeze on a
/// keypress. Asserted on the OUTPUT, not a stopwatch: if the cap goes, so does "showing the first 50".
let private showingACommitDoesNotFetchEveryOp =
  cliTest "showing a big commit's ops is capped, not fetched whole" (fun state ->
    task {
      let! commits = runCli state [ "commits"; "--json" ]
      // The baseline commit is the one with thousands of ops: the last row, oldest first.
      let hashes =
        commits.Split("\"hash\":\"")
        |> Array.skip 1
        |> Array.map (fun (s : string) -> s.Split('"')[0])
      let hash = hashes[hashes.Length - 1]

      let! shown = runCli state [ "show"; hash ]
      Expect.stringContains
        shown
        "showing the first 50 of"
        "the op list is capped and says so, rather than printing thousands"
    })

/// Drive the Dark key handlers; the registry includes document-only views too.
let private workbenchNavigationRegressions =
  [ "testWorkbenchHistoryRoundTrip"
    "testWorkbenchDocumentScroll"
    "testWorkbenchSyncStanding"
    "testWorkbenchBranchPicker"
    "testWorkbenchPageBuiltOncePerSelection" ]
  |> List.map (fun name ->
    cliTest $"workbench regression: {name}" (fun state ->
      task {
        let! output = runCli state [ "eval"; $"Darklang.Cli.Tests.{name} ()" ]
        Expect.stringContains output "TestResult.Pass" name
      }))

let private workbenchHandlesTerminalSizes =
  cliTest "the workbench frames a tiny terminal instead of breaking" (fun state ->
    task {
      // 56x12 is the documented floor: at it you get a real frame, below it a resize
      // message rather than a mangled layout. Both fill the space they were given.
      let! atFloor =
        runCli state [ "eval"; renderExpr "Stdlib.List.length (frame 0 56 12)" ]
      Expect.stringContains atFloor "12" "a frame at the documented minimum size"

      let! tooSmall =
        runCli
          state
          [ "eval"
            renderExpr
              "Stdlib.String.join (frame 0 40 10) \"|\" |> Stdlib.String.contains \"too small\"" ]
      Expect.stringContains
        tooSmall
        "true"
        "below the minimum, the frame says to resize"
    })

/// No workbench row is WIDER than the frame it was given, wide glyphs included.
///
/// Short rows are fine and deliberate: `Frame` erases each line before writing it, so a row need not pad
/// to the edge. Overflow is the harmful direction -- a row past the last column wraps, every row below it
/// shifts, and the frame diffing then repaints against a layout the terminal no longer has.
///
/// Wide glyphs are the way this breaks. Widths here are terminal COLUMNS, and `padEnd`/`String.length`
/// count characters, so the first CJK character or emoji in a name silently makes a row wider than its
/// measurement said. The fitters were moved to F# for speed, which is exactly the kind of rewrite that
/// loses a cell-vs-character distinction, and nothing looks until someone's name is not ASCII.
let private noWorkbenchRowOverflowsItsFrame =
  cliTest
    "no workbench row is wider than its frame, wide glyphs included"
    (fun state ->
      task {
        // A CJK name and an emoji commit message, so the frame is measuring real wide glyphs.
        let! _ = runCli state [ "fn"; "Tests.Wide.世界"; "() : Int64 = 1L" ]
        let! _ = runCli state [ "commit"; "コミット 🎉 wide"; "-y" ]

        let over (width : int) : string =
          renderExpr (
            $"let over = fun v -> frame v {width} 24 "
            + "|> Stdlib.List.map (fun r -> Darklang.Stdlib.Cli.Tui.Text.styledWidth r) "
            + $"|> Stdlib.List.filter (fun n -> n > {width}) "
            + "|> Stdlib.List.length in "
            + "[ over 0, over 1, over 3, over 4, over 6, over 7, over 8, over 9 ]"
          )

        for width in [ 56; 80; 120 ] do
          let! result = runCli state [ "eval"; over width ]
          Expect.stringContains
            result
            "[0, 0, 0, 0, 0, 0, 0, 0]"
            $"no row overflows a {width}-column frame in any view"

        return ()
      })

let private workbenchContextRowSaysWhereYouAre =
  cliTest "the workbench header names the branch, in every view" (fun state ->
    task {
      // The header is the first row: the branch, then `who @ where`. The branch and the account are
      // styled apart, so they are never adjacent in the string; each is matched on its own.
      let! output =
        runCli
          state
          [ "eval"
            renderExpr
              "let top = fun v -> Stdlib.String.join (Stdlib.List.take (frame v 120 40) 1) \"|\" in let has = fun v -> Stdlib.String.contains (top v) \"main\" in [ has 0, has 1, has 4 ]" ]

      Expect.stringContains
        output
        "[true, true, true]"
        "Home, Matter and SCM all carry the header, branch named"

      let! named =
        runCli
          state
          [ "eval"
            renderExpr
              "Stdlib.String.contains (Stdlib.String.join (Stdlib.List.take (frame 0 120 40) 1) \"|\") \"Tester @ test-instance\"" ]

      Expect.stringContains
        named
        "true"
        "and it names who you are and the instance you're on"
    })

let private everyCommandAnswersHelp =
  cliTest "every registered command answers `help` with help" (fun state ->
    task {
      let! commands = registeredCommands state
      Expect.isGreaterThan (List.length commands) 20 "the registry was read"

      let mutable failures : List<string * string> = []

      for cmd in commands do
        // `quit` ends the session rather than printing, and is the one command whose
        // help can't be asked for this way. Everything else must answer.
        if cmd <> "quit" then
          match! runCliCatching state [ cmd; "help" ] with
          | Error e -> failures <- (cmd, $"crashed: {e}") :: failures
          | Ok output ->
            if output = "" then
              failures <- (cmd, "printed nothing") :: failures
            else
              match looksLikeARuntimeFailure output with
              | Some line ->
                failures <- (cmd, $"printed a runtime error: {line}") :: failures
              | None ->
                if soundsLikeMisuse output then
                  let first = output.Split('\n')[0]
                  failures <- (cmd, first) :: failures

      // Reported together: when this breaks it usually breaks for a whole group of
      // commands at once, and finding that out one re-run at a time is the slow way.
      if not (List.isEmpty failures) then
        let detail =
          failures
          |> List.rev
          |> List.map (fun (c, why) -> $"  dark {c} help -> {why}")
          |> String.concat "\n"

        Tests.failtestf "commands that don't answer `help`:\n%s" detail
    })

/// Commands the sweeps must not RUN, because running them is the problem, with the reason beside
/// each. A bare entry is an exclusion nobody has justified, which is how a sweep stops covering
/// what it was written for.
///
/// Also asserted to be REGISTERED, below: an exclusion for a command that no longer exists is the
/// same failure by a different route. `agent` is excluded from the bogus-argument sweep only -- the
/// bare sweep is the one that catches what `agent` does with no subcommand.
let private notSweepable =
  Set.ofList
    [ "quit" // ends the session
      "install" // rewrites the install
      "uninstall" // ditto, destructively
      "update" // ditto, and fetches a build
      "install-status" // reports on the machine's install, which the harness is not
      "serve" // binds a port and blocks
      "outliner" // takes over the screen
      "views" // ditto
      "text-editor" // ditto
      "apps" // starts and stops daemons
      "login" // network, and writes credentials
      "logout" // ditto
      "typecheck" // audits every declaration; swept with a module in CliJson.Tests.fs
      "export-seed" // takes its argument as a path and writes a multi-MB database there
      "devices" // shells out to `tailscale`
      "clear" ] // clears the screen, taking the sweep's own output with it

/// Every `--help` opens with a sentence saying what the command IS.
///
/// Not `Usage:`, which is syntax before purpose, and not `dark <name> - ...`, which repeats the name
/// you just typed. The renderer falls back to the registry description when a help body has no
/// summary of its own, so a new command satisfies this without doing anything.
let private everyHelpLeadsWithASummary =
  cliTest "every command's help starts by saying what it is" (fun state ->
    task {
      let! commands = registeredCommands state
      Expect.isGreaterThan (List.length commands) 20 "the registry was read"

      let mutable bad : List<string> = []

      for cmd in commands do
        if not (Set.contains cmd (Set.add "agent" notSweepable)) then
          let! out = runCliCatching state [ cmd; "--help" ]

          match out with
          | Error _ -> ()
          | Ok text ->
            let first = (text.Split('\n')[0]).Trim()

            if
              first = "" || first.StartsWith "Usage:" || first.StartsWith "dark "
            then
              bad <- $"  {cmd} -> {first}" :: bad

      if not (List.isEmpty bad) then
        Tests.failtestf
          "help that doesn't start with a summary:\n%s"
          (bad |> List.rev |> String.concat "\n")
    })

let private everyExclusionIsReal =
  cliTest "every command excluded from the sweeps still exists" (fun state ->
    task {
      let! commands = registeredCommands state
      Expect.isGreaterThan (List.length commands) 20 "the registry was read"

      let stale =
        Set.difference (Set.add "agent" notSweepable) (Set.ofList commands)

      if not (Set.isEmpty stale) then
        Tests.failtestf
          "excluded from the sweeps but not registered: %s"
          (stale |> Set.toList |> String.concat ", ")
    })

/// Shape 1 of the four-shape sweep in `AGENTS.md`: every command, BARE.
///
/// The shape that catches a command going interactive with nothing to read: with no subcommand it
/// drops into a read loop and blocks on a stdin that never arrives. Every command must instead print
/// help, print a result, or refuse.
///
/// Safe to run here BECAUSE the harness is not a terminal: anything that would go interactive asks
/// `TerminalSupport.current ()` first and gets `Unavailable`. The harness stops the suite
/// with the command's name if a command exceeds its deadline.
let everyCommandAnswersWhenBare =
  cliTest
    "no registered command goes silent or hangs when run with no arguments"
    (fun state ->
      task {
        let! commands = registeredCommands state

        let mutable failures : List<string * string> = []

        for cmd in commands do
          if not (Set.contains cmd notSweepable) then
            let! outcome = runCliCatching state [ cmd ]
            match sweepFailure outcome with
            | Some why -> failures <- (cmd, why) :: failures
            | None -> ()

        if not (List.isEmpty failures) then
          let detail =
            failures
            |> List.rev
            |> List.map (fun (c, why) -> $"  dark {c} -> {why}")
            |> String.concat "\n"

          Tests.failtestf "commands that answer nothing when run bare:\n%s" detail
      })

let everyCommandSurvivesABogusArgument =
  cliTestOnMain
    "no registered command crashes on an argument that means nothing"
    (fun state ->
      task {
        let! commands = registeredCommands state

        let skip = Set.add "agent" notSweepable

        let mutable failures : List<string * string> = []

        for cmd in commands do
          if not (Set.contains cmd skip) then
            // A command that silently ignores an argument it didn't understand looks
            // exactly like one that did what you asked.
            let! outcome = runCliCatching state [ cmd; "zzz-no-such-thing-zzz" ]
            match sweepFailure outcome with
            | Some why -> failures <- (cmd, why) :: failures
            | None -> ()

            // `branch <junk>` and `switch <junk>` START that branch and move the store onto it, so every
            // command after them in this loop would be swept on a junk branch, as the store was found to be
            // after the last run. Back to main, and say so if it is not.
            if cmd = "branch" || cmd = "switch" then
              let! _ = runCli state [ "switch"; "main" ]
              ()

        if not (List.isEmpty failures) then
          let detail =
            failures
            |> List.rev
            |> List.map (fun (c, why) ->
              $"  dark {c} zzz-no-such-thing-zzz -> {why}")
            |> String.concat "\n"

          Tests.failtestf
            "commands that ignore an argument they don't understand:\n%s"
            detail
      })

/// The same two sweeps again, standing on a BRANCH.
///
/// Worth its own run because of the trap `AGENTS.md` names: `locations` has no `branch_id`, so a read
/// that goes straight to it answers about MAIN while you are on a branch -- and it answers plausibly,
/// which is how call sites drift that way unnoticed. A command that
/// only misbehaves on a branch is invisible to the sweeps above, all of which run on main.
///
/// Both ends of the setup are asserted, not assumed. A sweep that silently failed to switch would pass
/// forever while testing main twice, and one that failed to switch BACK would leave every later test in
/// this file quietly asserting about a branch. Nothing here throws before the switch back: the sweep
/// collects failures rather than raising, and the report comes after.
let everyCommandSurvivesABranch =
  cliTestOnMain
    "no registered command crashes or goes silent while on a branch"
    (fun state ->
      task {
        let! commands = registeredCommands state
        Expect.isGreaterThan (List.length commands) 20 "the registry was read"

        let! switched = runCli state [ "switch"; "cli-sweep-branch" ]
        Expect.stringContains
          switched
          "cli-sweep-branch"
          "the sweep is on the branch"

        let mutable failures : List<string * string> = []

        let sweep (label : string) (extra : List<string>) =
          task {
            for cmd in commands do
              if not (Set.contains cmd (Set.add "agent" notSweepable)) then
                let! outcome = runCliCatching state (cmd :: extra)
                match sweepFailure outcome with
                | Some why -> failures <- ($"{cmd} {label}", why) :: failures
                | None -> ()
          }

        do! sweep "" []
        do! sweep "zzz-no-such-thing-zzz" [ "zzz-no-such-thing-zzz" ]

        let! back = runCli state [ "switch"; "main" ]
        Expect.stringContains back "main" "and it put the run back on main"

        if not (List.isEmpty failures) then
          let detail =
            failures
            |> List.rev
            |> List.map (fun (c, why) -> $"  dark {c} (on a branch) -> {why}")
            |> String.concat "\n"

          Tests.failtestf "commands that misbehave while on a branch:\n%s" detail
      })

/// ─── Shape 3: every command with VALID arguments ───────────────────────────────
///
/// The three sweeps above run each command bare, with `--help`, and with a word that means
/// nothing. None of them ever gives a command something to DO, and three of this branch's bugs
/// lived in exactly that gap: a Dark call site is not type-checked until it executes, so
/// `traces pin <id>` was dead on arrival with a green build and a green suite behind it.
///
/// This is the fourth shape (`AGENTS.md`, "Sweeping the CLI after a change"): one known-good
/// invocation per command, against a store seeded with something to name. It asserts what the
/// other sweeps assert -- nothing printed that looks like a runtime failure, and something
/// printed at all -- plus the exit code, which for a command that was asked a fair question and
/// answered it is 0.
///
/// The exit code is asserted in one direction only. A refusal exits non-zero, but some refusals
/// still return the state unchanged and so exit 0, so `= 0` is a safe thing to require of a
/// valid invocation and a useless thing to invert.
/// What the seed leaves in the store for the sweep to name.
type private Seeded =
  {
    /// A committed package fn, with a caller, so `deps`, `view`, `hash` and `undo` have a target.
    fn : string
    /// A branch that exists and differs from main.
    branch : string
    /// A commit hash `show` can open.
    commit : string
    /// A recorded run `traces inspect` can open.
    run : string
  }

/// The invocation a person would actually type, per command.
///
/// `[]` means "bare IS the valid invocation" -- `status`, `whoami`, `branches` answer a question
/// that needs no argument, and running them again here costs one dispatch and keeps the table
/// complete rather than clever. Anything cheap and read-only is preferred: the sweep runs sixty
/// commands, and one that takes ten seconds makes the whole thing something people skip.
let private knownGood (seed : Seeded) : Map<string, List<string>> =
  Map.ofList
    [ "help", [ "status" ]
      // `config list` reads; `config set` writes keys the F# boot reads at startup.
      "config", [ "list" ]
      // Bare asks GitHub for the latest release, which is a network call on a timeout.
      "version", [ "--local" ]
      "nav", [ "Darklang.Stdlib.List" ]
      "ls", [ "Darklang.Stdlib.List" ]
      "back", []
      "eval", [ "1L + 1L" ]
      "scripts", [ "list" ]
      "view", [ seed.fn ]
      "tree", [ "Darklang.Stdlib"; "--depth=1" ]
      "search", [ "mergeFavoring" ]
      "deps", [ seed.fn ]
      "hash", [ seed.fn ]
      "status", [ "--json" ]
      "commits", [ "3"; "--json" ]
      // Bare, because on the seed there is nothing of this instance's own that is unpushed, and
      // "nothing to squash" is an ANSWER rather than a refusal. Giving it a message would be a
      // fair question too; it would just do the same thing.
      "squash", []
      "show", [ seed.commit ]
      "branch", [ "list" ]
      "branches", [ "--json" ]
      "switch", [ "main" ]
      "diff", [ seed.branch; "--json" ]
      "log", [ "--json" ]
      "rebase", [ seed.branch; "--dry-run" ]
      "merge", [ seed.branch; "--dry-run" ]
      "propagate", [ "show"; seed.fn ]
      "constraints", [ "--json" ]
      "builtins", [ "listMap" ]
      "find-values", [ "Darklang.Stdlib.Option.Option" ]
      "docs", [ "scm" ]
      "db", [ "list" ]
      "ops", [ "3" ]
      "ps", [ "--json" ]
      "traces", [ "inspect"; seed.run ]
      "conflicts", [ "list" ]
      "backups", [ "list" ]
      "whoami", []
      "permissions", [ "show"; seed.fn ]
      // Scoped to a module that is clean. The shared store holds failing fixtures other tests
      // leave on purpose, so an audit of all of it rightly exits 1.
      "typecheck", [ "Darklang.Stdlib.List" ]
      "workbench", []
      "commit", [ "--json" ]
      // Reads the implementations of a trait on the branch. Fully qualified, like `nav` and
      // `find-values` above, rather than relying on the bare-name fallback to the stdlib.
      "impls", [ "Darklang.Stdlib.Add" ] ]

/// Commands that are safe to run BARE and must not be given real arguments, with the reason.
///
/// Same rule as `notSweepable`: a bare entry is an exclusion nobody has justified. The reasons
/// fall into four groups -- it reaches the network (and the relay url is a STORED default, so no
/// argument is needed to reach production), it takes over the screen, it never returns, or it
/// changes state the rest of the suite is standing on.
let private unsafeWithArguments : Map<string, string> =
  Map.ofList
    [ "agent",
      "asks a model: network, money, and `agent code` writes the answer into the store"
      "push", "the relay url is a stored default, so this reaches the real one"
      "pull", "ditto"
      "sync", "ditto, and `sync setup` is a question flow that reads stdin"
      "connect",
      "rewrites the store's relay, so every later sync-shaped command follows it"
      "review", "`review pull` with no url reaches the stored relay, quietly"
      "identity", "renames the instance, and the name goes out on the next push"
      "run-script", "takes a path and executes whatever is at it"
      "fn",
      "authors into the shared store; the seed above is the only fixture that should"
      "val", "ditto"
      "type", "ditto"
      "module", "ditto"
      "trait", "ditto"
      "impl", "ditto"
      "rename", "moves an item other tests may be naming"
      "edit", "without a file argument it spawns $EDITOR as an interactive child"
      "discard",
      "drops the WHOLE draft, which takes other tests' uncommitted work with it"
      "delete", "takes an item off the shelf"
      "deprecate", "ditto, by the other door"
      "undeprecate", "puts one back"
      "undo", "steps a real item back a version"
      "resolve", "writes a Resolve op, and an op syncs"
      "ack", "closes a finding other tests assert on" ]

/// Seed the store, run every command with a known-good invocation, and check the drift both ways.
let everyCommandWorksWithValidArguments =
  cliTestOnMain
    "every registered command answers a fair question with exit 0"
    (fun state ->
      task {
        let branch = "cli-sweep-args"
        let fnName = "Tests.Sweep.f"

        // Authored and committed, so `show`, `deps` and `propagate` have something real, and
        // committed by NAME: `commit` with no `--include` would take whatever other tests left
        // in the draft.
        do! CliDsl.onMain state
        do! CliDsl.fn state "Tests.Sweep.dep" "() : Int64 = 1L"
        do! CliDsl.fn state fnName "() : Int64 = Tests.Sweep.dep ()"
        do!
          CliDsl.commitOnly
            state
            "cli-sweep fixture"
            "Tests.Sweep.dep,Tests.Sweep.f"

        // A branch that differs from main, for `diff` / `merge --dry-run` / `rebase --dry-run`.
        do! CliDsl.switch state branch
        do! CliDsl.fn state "Tests.Sweep.onBranch" "() : Int64 = 2L"
        do! CliDsl.onMain state

        let! commitsJson = runCli state [ "commits"; "1"; "--json" ]
        let commit =
          let split = commitsJson.Split("\"hash\":\"")
          if split.Length < 2 then "" else split[1].Split('"')[0]

        // A recorded run. Recording is off by default now, so the sweep turns it on for its own
        // seed and puts it back, the way `cliTestWithFreshTraces` does.
        let recordingBefore = LibDB.Tracing.TraceDetail.current
        LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.On
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! runsJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let run =
          let split = runsJson.Split("\"id\":\"")
          if split.Length < 2 then "" else split[1].Split('"')[0]

        try
          Expect.isFalse (commit = "") "the seed made a commit to name"
          Expect.isFalse (run = "") "the seed made a run to name"

          let table =
            knownGood { fn = fnName; branch = branch; commit = commit; run = run }

          let! commands = registeredCommands state
          Expect.isGreaterThan (List.length commands) 20 "the registry was read"

          // Drift, both ways, so the table cannot quietly stop covering the registry.
          let accounted =
            Set.unionMany
              [ table |> Map.keys |> Set.ofSeq
                unsafeWithArguments |> Map.keys |> Set.ofSeq
                Set.add "agent" notSweepable ]

          let unaccounted = Set.difference (Set.ofList commands) accounted
          if not (Set.isEmpty unaccounted) then
            Tests.failtestf
              "registered but no known-good invocation and no stated reason not to sweep: %s"
              (unaccounted |> Set.toList |> String.concat ", ")

          let stale =
            Set.difference
              (Set.union
                (table |> Map.keys |> Set.ofSeq)
                (unsafeWithArguments |> Map.keys |> Set.ofSeq))
              (Set.ofList commands)
          if not (Set.isEmpty stale) then
            Tests.failtestf
              "named in the valid-argument table but not registered: %s"
              (stale |> Set.toList |> String.concat ", ")

          let mutable failures : List<string * string> = []

          for KeyValue(cmd, args) in table do
            let! outcome = runCliCatchingWithStatus state (cmd :: args)
            let printed = $"""dark {cmd} {String.concat " " args}"""

            match outcome with
            | Error e -> failures <- (printed, $"crashed: {e}") :: failures
            | Ok(output, status) ->
              match sweepFailure (Ok output) with
              | Some why -> failures <- (printed, why) :: failures
              | None ->
                if status <> 0 then
                  failures <- (printed, $"exited {status}") :: failures

          if not (List.isEmpty failures) then
            let detail =
              failures
              |> List.rev
              |> List.map (fun (c, why) -> $"  {c} -> {why}")
              |> String.concat "\n"

            Tests.failtestf
              "commands that did not answer a fair question:\n%s"
              detail
        finally
          LibDB.Tracing.TraceDetail.setForTesting recordingBefore

        do! archiveBranches state [ branch ]
      })


/// An empty grant is not a grant, and must not report that it is.
let private permissionsRefusesAnEmptyRule =
  cliTest
    "`permissions allow` with no rule is refused rather than reported as allowed"
    (fun state ->
      task {
        let! refused = runCli state [ "permissions"; "allow"; "" ]

        Expect.stringContains
          refused
          "invalid permission rule"
          "it should say what was wrong with it"

        Expect.isFalse
          (refused.Contains "allowed ")
          "and must not report a rule it did not add"
      })

let private nonexistentTargets : List<string * List<string>> =
  [ "view", [ "view"; "Zzz.Nope.nope" ]
    "deps", [ "deps"; "Zzz.Nope.nope" ]
    "undo", [ "undo"; "Zzz.Nope.nope" ]
    "merge", [ "merge"; "zzznope" ]
    "rebase", [ "rebase"; "zzznope" ]
    "diff", [ "diff"; "zzznope" ]
    "log", [ "log"; "zzznope" ]
    "show", [ "show"; "zzznope" ]
    "branch archive", [ "branch"; "archive"; "zzznope" ]
    "review approve", [ "review"; "approve"; "zzznope" ]
    "review reject", [ "review"; "reject"; "zzznope" ]
    "conflicts show", [ "conflicts"; "show"; "zzznope" ]
    "conflicts branch", [ "conflicts"; "branch"; "zzznope" ]
    "propagate show", [ "propagate"; "show"; "Zzz.Nope.nope" ]
    "propagate pin", [ "propagate"; "pin"; "Zzz.Nope.nope" ]
    "propagate follow", [ "propagate"; "follow"; "Zzz.Nope.nope" ]
    "constraints resolve", [ "constraints"; "resolve"; "zzznope" ]
    "ack", [ "ack"; "zzznope" ]
    "permissions requirements", [ "permissions"; "requirements"; "Zzz.Nope.nope" ]
    "permissions show", [ "permissions"; "show"; "Zzz.Nope.nope" ]
    "permissions approve", [ "permissions"; "approve"; "--yes"; "Zzz.Nope.nope" ] ]

let private missingTargetsAreNamed =
  cliTestOnMain
    "a command that can't find its target says which target"
    (fun state ->
      task {
        // On main, deliberately: `ack` refuses on a branch (an ack is a statement about
        // the store), and the process is a shared global another test may have moved.
        let! _ = runCli state [ "switch"; "main" ]

        let mutable failures : List<string * string> = []

        for (label, args) in nonexistentTargets do
          let! output = runCli state args
          let target = args |> List.last |> Option.defaultValue ""

          // The first segment, not the whole target: a traversal stops at the first
          // segment it cannot find, so `Zzz.Nope.nope` is answered with `Zzz`.
          let firstSegment = target.Split('.')[0]

          if output.Trim() = "" then
            failures <- (label, "said nothing") :: failures
          elif not (output.Contains firstSegment) then
            failures <- (label, output.Split('\n')[0]) :: failures

        if not (List.isEmpty failures) then
          let detail =
            failures
            |> List.rev
            |> List.map (fun (c, why) -> $"  dark {c} <missing> -> {why}")
            |> String.concat "\n"

          Tests.failtestf
            "commands that don't name the target they couldn't find:\n%s"
            detail
      })

/// Refusals that printed their reason and exited 0. A refusal is a failed command: that is the one
/// part of it a shell `&&`, a CI step or an agent's authoring loop can read, and the rest of the
/// CLI already exits 1 for a parse error or a name collision.
///
/// Each entry is a refusal a person meets on the traits side in their first hour. Read-only, so
/// it runs against the shared store; on main, because `ack` refuses on a branch for a different
/// reason and that is not what this is checking.
let private traitRefusals : List<string * List<string>> =
  [ "trait, no definition", [ "trait"; "Tests.ExitT.T" ]
    "impl, no definition", [ "impl"; "Tests.ExitT" ]
    "impls, no such trait", [ "impls"; "Zzz.Nope.Trait" ]
    "impls, not a trait", [ "impls"; "Darklang.Stdlib.Option.Option" ]
    "impls, an extra argument", [ "impls"; "Darklang.Stdlib.Add"; "extra" ]
    "impls --json, no such trait", [ "impls"; "Zzz.Nope.Trait"; "--json" ]
    "constraints, an argument it does not take", [ "constraints"; "zzznope" ]
    "constraints resolve, no such id", [ "constraints"; "resolve"; "#deadbeef" ]
    "ack, no such id", [ "ack"; "#deadbeef" ]
    "ack, no id", [ "ack" ] ]

let private traitRefusalsExitNonZero =
  cliTestOnMain "a refusal on the traits side exits non-zero" (fun state ->
    task {
      let! _ = runCli state [ "switch"; "main" ]

      let mutable failures : List<string * string> = []
      let mutable examined = 0

      for (label, args) in traitRefusals do
        let! (output, code) = runCliWithExit state args
        examined <- examined + 1
        if code = 0L then
          let firstLine = (CliDsl.plain output).Trim().Split('\n')[0]
          failures <- (label, firstLine) :: failures

      // Every entry ran, so a pass is a statement about ten refusals rather than about none.
      Expect.equal examined (List.length traitRefusals) "every refusal was run"

      if not (List.isEmpty failures) then
        let detail =
          failures
          |> List.rev
          |> List.map (fun (c, firstLine) -> $"  {c} -> exit 0: {firstLine}")
          |> String.concat "\n"

        Tests.failtestf "refusals that exit 0:\n%s" detail
    })

/// Refusals outside the traits side, same rule: a refusal reports failure or no script can tell.
///
/// `ps show ""` is here because an empty prefix matches every id, so it used to take whichever
/// process came first and exit 0; a shell variable that came back empty is how a person meets it.
let private otherRefusals : List<string * List<string>> =
  [ "ps show, an empty id", [ "ps"; "show"; "" ]
    "ps cancel, an empty id", [ "ps"; "cancel"; "" ]
    "ps show, no such id", [ "ps"; "show"; "zzznope" ]
    "commit, a flag it does not take", [ "commit"; "--zzznope" ]
    // An argument the command understood well enough to refuse. These printed a reason and then
    // returned the state unchanged, so they reported success: the two shapes are a refusal that
    // returns a bare `state`, and a caller that gets None from a helper which already said why.
    "commits, a branch name where a count goes", [ "commits"; "zzznope" ]
    "backups restore, no name", [ "backups"; "restore" ]
    "propagate pin, no names", [ "propagate"; "pin" ]
    "diff, a branch that does not exist", [ "diff"; "zzznope" ]
    "log, a branch that does not exist", [ "log"; "zzznope" ]
    "rebase, a branch that does not exist", [ "rebase"; "zzznope" ] ]

let private otherRefusalsExitNonZero =
  cliTestOnMain "a refusal outside the traits side exits non-zero" (fun state ->
    task {
      let mutable failures : List<string * string> = []
      let mutable examined = 0

      for (label, args) in otherRefusals do
        let! (output, code) = runCliWithExit state args
        examined <- examined + 1
        if code = 0L then
          let firstLine = (CliDsl.plain output).Trim().Split('\n')[0]
          failures <- (label, firstLine) :: failures

      Expect.equal examined (List.length otherRefusals) "every refusal was run"

      if not (List.isEmpty failures) then
        let detail =
          failures
          |> List.rev
          |> List.map (fun (c, firstLine) -> $"  {c} -> exit 0: {firstLine}")
          |> String.concat "\n"

        Tests.failtestf "refusals that exit 0:\n%s" detail
    })

/// Every `dark <word>` the in-CLI docs mention has to be a real command.
///
/// A doc that confidently describes a command that is not there is worse than no doc, and nothing
/// else checks. Narrow by design: only the WORD after `dark`, which is what is verifiable
/// mechanically. It cannot tell you the prose is wrong, only that the commands are real.
let private docTopicsToCheck =
  [ "scm"; "for-ai"; "cli"; "processes"; "live"; "http-server" ]

let private documentedCommandsAreReal =
  cliTest "every command the docs mention exists" (fun state ->
    task {
      let! registered = registeredCommands state
      // Aliases don't appear in the group listing's first token, so read them out of the parenthesised
      // part of each line: `  status (wip, changes) - ...`.
      let! help = runCli state [ "help" ]

      let aliases =
        help.Split('\n')
        |> Array.toList
        |> List.collect (fun line ->
          if line.Contains "(" && line.Contains ")" && line.Contains " - " then
            let inner = line.Substring(line.IndexOf "(" + 1)
            let inner = inner.Substring(0, inner.IndexOf ")")
            inner.Split(',') |> Array.toList |> List.map (fun s -> s.Trim())
          else
            [])

      let known = Set.ofList (registered @ aliases)
      let mutable failures : List<string * string> = []

      for topic in docTopicsToCheck do
        let! doc = runCli state [ "docs"; topic ]

        let mentioned =
          doc.Split([| ' '; '\n'; '\t' |])
          |> Array.toList
          |> List.pairwise
          |> List.choose (fun (a, b) ->
            if a = "dark" then
              let cmd = b.Trim([| '`'; ','; '.'; ':'; ')'; '"' |])
              // `dark --branch <id> <cmd>` and `dark <path>` placeholders aren't commands.
              if cmd = "" || cmd.StartsWith "-" || cmd.StartsWith "<" then
                None
              else
                Some cmd
            else
              None)
          |> List.distinct

        for cmd in mentioned do
          if not (Set.contains cmd known) then failures <- (topic, cmd) :: failures

      if not (List.isEmpty failures) then
        let detail =
          failures
          |> List.rev
          |> List.map (fun (topic, cmd) ->
            $"  docs {topic} says `dark {cmd}`, which isn't a command")
          |> String.concat "\n"

        Tests.failtestf "the docs describe commands that don't exist:\n%s" detail
    })

/// A dash-led argument is a flag someone mistyped, or a sweep passed in. It is never a name.
///
/// `everyCommandSurvivesABogusArgument` above cannot catch this, because these commands ANSWER, at
/// length and cheerfully, while doing something nobody asked for. Unguarded, `dark switch --help`
/// starts a branch called "--help" and moves the store onto it, so the next `dark fn` authors on a
/// branch that exists by accident; `dark identity --help` renames the instance to "--help", and the
/// name goes out on the next push, so everyone else sees it before you do.
let private aDashLedArgumentIsNeverAName =
  cliTestOnMain
    "a dash-led argument is refused as a name rather than taken as one"
    (fun state ->
      task {
        let! before = runCli state [ "whoami" ]

        for cmd in [ "switch"; "branch" ] do
          let! output = runCli state [ cmd; "--zzz-not-a-branch" ]

          Expect.stringContains
            output
            "starts with a dash"
            $"dark {cmd} --zzz-not-a-branch should refuse the name"

          let! branches = runCli state [ "branches" ]

          Expect.isFalse
            (branches.Contains "--zzz-not-a-branch")
            $"dark {cmd} --zzz-not-a-branch must not START a branch"

        let! where = runCli state [ "branch" ]
        Expect.stringContains where "main" "and it must not move you off main"

        let! identityOut = runCli state [ "identity"; "--zzz-not-a-name" ]

        Expect.stringContains
          identityOut
          "starts with a dash"
          "dark identity --zzz-not-a-name should refuse the name"

        let! after = runCli state [ "whoami" ]
        Expect.equal after before "and the instance identity is unchanged"
      })

/// `serve` and `apps` are not swept, because their success binds a port or takes the screen, so
/// their refusals are checked here. A refusal that exits 0 is one a script cannot see, and these
/// all did; `apps view <slug> --dev` also opened the view, taking the flag as its name.
let private unsweptCommandsRefuseWithExit1 =
  cliTestOnMain "serve and apps view refusals exit 1" (fun state ->
    task {
      let cases =
        [ [ "serve" ], "Usage: serve"
          [ "serve"; "--live" ], "Missing router path"
          [ "serve"; "Tests.NoSuch.router"; "--live" ], "is it defined"
          [ "apps"; "view"; "Tests.NoSuch"; "--dev" ], "--dev is --live"
          [ "apps"; "view"; "Tests.NoSuch"; "--zzz" ], "apps view has no --zzz" ]

      for args, expected in cases do
        let printed = String.concat " " args
        let! output, exitCode = runCliWithExit state args
        Expect.stringContains output expected $"dark {printed} names what is wrong"
        Expect.equal exitCode 1L $"dark {printed} exits 1"
    })

/// Two names with byte-identical bodies are ONE item, and a `PackageFn` carries no name, so showing one
/// means resolving its hash back to a name. With several to choose from, the scoring picked whichever it
/// liked and `dark view Tests.SharedBody.alpha` printed a definition headed `beta`.
///
/// Scoring is right for a name appearing inside someone else's code and wrong for the thing you just
/// asked to see, and nothing in the scoring can know which name was typed, so the asked-for location is
/// carried into the printer and wins outright when it is one of the candidates.
let private viewHeadsWithTheNameYouAskedFor =
  cliTestOnMain
    "view heads a shared body with the name that was asked for"
    (fun state ->
      task {
        let! _ = runCli state [ "switch"; "main" ]
        let! _ =
          runCli state [ "fn"; "Tests.SharedBody.alpha"; "() : Int64 = 4242L" ]
        let! _ =
          runCli state [ "fn"; "Tests.SharedBody.beta"; "() : Int64 = 4242L" ]

        // Colour codes sit BETWEEN `let` and the name, so the two-word header never matches raw output.
        // Stripping is better than asserting on the bare name here, which appears in the location line too
        // and so would pass whichever name the header carried.
        let plain (text : string) =
          System.Text.RegularExpressions.Regex.Replace(text, @"\x1b\[[0-9;]*m", "")

        let! alpha = runCli state [ "view"; "Tests.SharedBody.alpha" ]
        let! beta = runCli state [ "view"; "Tests.SharedBody.beta" ]

        Expect.stringContains (plain alpha) "let alpha" "view alpha is headed alpha"
        Expect.stringContains (plain beta) "let beta" "view beta is headed beta"

        // --raw is the half `dark edit` and `dark module` consume, so a wrong header there does not just
        // mislead, it round-trips the definition onto the wrong name.
        let! rawAlpha = runCli state [ "view"; "Tests.SharedBody.alpha"; "--raw" ]
        Expect.stringContains
          (plain rawAlpha)
          "let alpha"
          "view --raw alpha is headed alpha"
      })


let private unwrapErrorsAreReadable =
  cliTest "eval checks unwrap operands and runs valid unwraps" (fun state ->
    task {
      let! rejected, exitCode =
        runCliWithExit state [ "eval"; "(fun value -> Some (value? + 1)) 4" ]
      Expect.equal exitCode 1L "invalid unwrap fails eval"
      Expect.stringContains
        rejected
        "`?` needs an Option or Result"
        "names the ? and its required operand"
      Expect.isFalse
        (rejected.Contains "Encountered a Runtime Error")
        "the error renderer itself succeeds"
      let! accepted =
        runCli state [ "eval"; "(fun value -> Some (value? + 1)) (Some 4)" ]
      Expect.stringContains accepted "Some(5)" "valid unwrap still executes"
    })


// Editing from the workbench, the way a person does it: `e` on an item, change some text, `^s`.
// Driven through `openEditExisting` and `saveEditing` rather than keystrokes, so what is under test
// is the save and not the renderer. The editor is prefilled by the pretty-printer, which is where
// the impl's qualified names come from.

/// A Dark string literal holding <param s>.
let private darkString (s : string) : string =
  "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n") + "\""

/// Open <param item> in <param modules> in the editor, replace <param find> with <param replace> in
/// what it prefilled, save, and answer the footer (or what went wrong instead).
let private workbenchEdit
  (modules : List<string>)
  (item : string)
  (find : string)
  (replace : string)
  : string =
  let path = modules |> List.map darkString |> String.concat ", "
  "let st0 = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Tester\" \"i\" [] false in\n"
  + $"let st = {{ st0 with activeView = Darklang.Cli.Workbench.vMatter; location = Darklang.Cli.Packages.PackageLocation.Module [ {path} ] }} in\n"
  + "let items = Darklang.Cli.Workbench.reloadItems st in\n"
  + $"match Stdlib.List.findFirst items (fun i -> i.name == {darkString item}) with\n"
  + "| None -> \"no item to edit\"\n"
  + "| Some item ->\n"
  + "  match Darklang.Cli.Workbench.openEditExisting st item with\n"
  + "  | Continue opened ->\n"
  + "    match opened.editing with\n"
  + "    | None -> \"did not open: \" + opened.message\n"
  + "    | Some es ->\n"
  + $"      let edited = Stdlib.String.replaceAll (Stdlib.Cli.UI.Editor.toText es.buf) {darkString find} {darkString replace} in\n"
  + "      match Darklang.Cli.Workbench.saveEditing opened { es with buf = Stdlib.Cli.UI.Editor.fromText edited } with\n"
  + "      | Continue saved ->\n"
  + "        match saved.editing with\n"
  + "        | Some still -> \"save refused: \" + still.err\n"
  + "        | None -> \"footer: \" + saved.message\n"
  + "      | _ -> \"save did not continue\"\n"
  + "  | _ -> \"open did not continue\""

/// <fn workbenchEdit>, run with the CLI's own authority rather than as a guest `eval`: the
/// workbench is a CLI command, so its save may read the CLI's config, which the instance policy
/// rightly refuses to code typed at `dark eval`.
let private editInWorkbench
  (target : Target)
  (modules : List<string>)
  (item : string)
  (find : string)
  (replace : string)
  : Task<string> =
  task {
    match!
      evalUnder (executionState target) (workbenchEdit modules item find replace)
    with
    | RT.DString footer -> return footer
    | other -> return Tests.failtestf "the workbench edit answered %A" other
  }

/// Write a NEW item in the workbench, as `n`/`t`/`v`/`T`/`I` does: open the editor on <param kind>
/// at <param location> saving into <param target>, put <param source> in it, save. Answers the
/// footer, where the view ended up and the selected row, or why the save was refused.
let private workbenchNew
  (location : List<string>)
  (kind : string)
  (target : List<string>)
  (source : string)
  : string =
  let path xs = xs |> List.map darkString |> String.concat ", "
  "let st0 = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Tester\" \"i\" [] false in\n"
  + $"let st = {{ st0 with activeView = Darklang.Cli.Workbench.vMatter; location = Darklang.Cli.Packages.PackageLocation.Module [ {path location} ] }} in\n"
  + $"match Darklang.Cli.Workbench.openNewEditor st {darkString kind} [ {path target} ] with\n"
  + "| Continue opened ->\n"
  + "  match opened.editing with\n"
  + "  | None -> \"did not open: \" + opened.message\n"
  + "  | Some es ->\n"
  + $"    match Darklang.Cli.Workbench.saveEditing opened {{ es with buf = Stdlib.Cli.UI.Editor.fromText {darkString source} }} with\n"
  + "    | Continue saved ->\n"
  + "      match saved.editing with\n"
  + "      | Some still -> \"save refused: \" + still.err\n"
  + "      | None ->\n"
  + "        let on = (Stdlib.List.getAt saved.items saved.selected) |> Stdlib.Option.map (fun i -> i.name) |> Stdlib.Option.withDefault \"\" in\n"
  + "        \"footer: \" + saved.message + \" | at: \" + (Stdlib.String.join (Darklang.Cli.Packages.modulePathOf saved.location) \".\") + \" | on: \" + on\n"
  + "    | _ -> \"save did not continue\"\n"
  + "| _ -> \"open did not continue\""

let private newInWorkbench
  (target : Target)
  (location : List<string>)
  (kind : string)
  (into : List<string>)
  (source : string)
  : Task<string> =
  task {
    match!
      evalUnder (executionState target) (workbenchNew location kind into source)
    with
    | RT.DString answer -> return answer
    | other -> return Tests.failtestf "the workbench save answered %A" other
  }

/// The item page is cached on the workbench's state, so a save that leaves the item list as it was
/// must still rebuild it. Under the CLI's authority for the same reason as `editInWorkbench`.
let private workbenchPageFollowsYourSave =
  cliTest "workbench: the item page shows a save made in its editor" (fun state ->
    task {
      match!
        evalUnder
          (executionState state)
          "Darklang.Cli.Tests.testWorkbenchPageFollowsYourSave ()"
      with
      | RT.DEnum(_, _, _, "Pass", []) -> return ()
      | other -> return Tests.failtestf "the page test answered %A" other
    })

/// An implementation that would take a trait's name is refused, as `dark impl` refuses it.
///
/// Written in a module named for its type, an implementation lands on `<module>.<Trait>`, which is
/// where a trait declared in that module already is. One name holds one item, so binding the
/// implementation there unlisted the trait and nothing could reach it by name.
let private workbenchRefusesImplOverTrait =
  cliTest
    "the workbench refuses an implementation that would take a trait's name"
    (fun state ->
      task {
        let! _ = runCli state [ "type"; "/Tests.WbPt.WbPt"; "{ wbPtMark: Int64 }" ]
        let! _ =
          runCli
            state
            [ "trait"; "/Tests.WbPt.WbSh"; "<'a> = let sh (v: 'a) : String" ]

        let! answer =
          newInWorkbench
            state
            [ "Tests"; "WbPt" ]
            "impl"
            [ "Tests"; "WbPt" ]
            "impl WbSh for WbPt =\n  let sh (p: WbPt) : String = \"p\""
        Expect.stringContains answer "save refused" "the save is refused"
        Expect.stringContains
          answer
          "already called Tests.WbPt.WbSh"
          "and says which name is taken"

        let! view = runCli state [ "view"; "Tests.WbPt.WbSh" ]
        Expect.stringContains view "trait" "the trait is still there under its name"
      })

/// The workbench's refusals say what is wrong, as the CLI's do.
let private workbenchRefusalsNameTheProblem =
  cliTest "the workbench names an unresolved name and a missing trait" (fun state ->
    task {
      let! unresolved =
        newInWorkbench
          state
          [ "Tests"; "WbRefuse" ]
          "fn"
          [ "Tests"; "WbRefuse" ]
          "let wbQ (x: Int64) : Int64 = Nope.wbZzz x"
      Expect.stringContains
        unresolved
        "save refused"
        "an unresolved name refuses the save"
      Expect.stringContains unresolved "Nope.wbZzz" "and names the name"

      let! noTrait =
        newInWorkbench
          state
          [ "Tests"; "WbRefuse" ]
          "impl"
          [ "Tests"; "WbRefuse" ]
          "impl WbNoSuchTrait for Int64 =\n  let m (x: Int64) : String = \"\""
      Expect.stringContains
        noTrait
        "save refused"
        "an impl of a missing trait is refused"
      Expect.stringContains
        noTrait
        "no trait called WbNoSuchTrait"
        "and says it is the trait that is missing"
    })

/// A save that changes nothing says so, and a new item is where the view lands.
let private workbenchSaysUnchangedAndLands =
  cliTest
    "the workbench says when a save changed nothing, and lands on what it saved"
    (fun state ->
      task {
        // A body no other test writes: content-addressing makes `x + 1L` in two modules one item.
        let! _ =
          runCli
            state
            [ "fn"; "/Tests.WbSame.same"; "(x: Int64) : Int64 = x + 7311L" ]

        let! same =
          editInWorkbench state [ "Tests"; "WbSame" ] "same" "x + 7311L" "x + 7311L"
        Expect.stringContains
          same
          "unchanged"
          "an edit that changes nothing says so"

        let! landed =
          newInWorkbench
            state
            [ "Tests" ]
            "fn"
            [ "Tests"; "WbLand" ]
            "let landedHere (x: Int64) : Int64 = x"
        Expect.stringContains
          landed
          "at: Tests.WbLand"
          "the view moves to the module saved into"
        Expect.stringContains landed "on: landedHere" "with the new item selected"
      })

/// A doc comment written above a declaration documents it, rather than naming a new item.
///
/// The save took the name from the editor's FIRST line, so `/// Adds a number` above `let docme`
/// saved a new function called `Adds` with docme's body, and docme kept no doc.
let private workbenchDocCommentDocuments =
  cliTest
    "a doc comment added in the workbench documents the item it sits on"
    (fun state ->
      task {
        let! _ =
          runCli
            state
            [ "fn"; "/Tests.WbDoc.docme"; "(x: Int64) : Int64 = x + 9137L" ]

        let! saved =
          editInWorkbench
            state
            [ "Tests"; "WbDoc" ]
            "docme"
            "let docme"
            "/// Adds a number nobody else uses.\nlet docme"
        Expect.stringContains saved "saved docme" "the save is of docme"

        let! view = runCli state [ "view"; "Tests.WbDoc.docme" ]
        Expect.stringContains view "nobody else uses" "and docme carries the doc"

        let! stray = runCli state [ "view"; "Tests.WbDoc.Adds" ]
        Expect.isFalse
          (stray.Contains "9137")
          $"and no item was made from the doc's first word:\n{stray}"
      })

/// A refusal is marked as one in the footer, not with a success tick.
///
/// The mark used to be chosen by substring ("fail", "conflict"), so each of these four went out
/// behind a green tick. Driven through the functions that produce them, each asserted on the text
/// AND the kind: the substring test also looked like it worked.
let private workbenchRefusalsAreMarkedAsRefusals =
  cliTest "the workbench marks a refusal as a refusal, not a success" (fun state ->
    task {
      let code =
        "let st0 = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Tester\" \"i\" [] false in\n"
        + "let st = { st0 with activeView = Darklang.Cli.Workbench.vMatter } in\n"
        + "let kindOf = fun step -> match step with | Continue s -> s.message + \" => \" + (match s.messageKind with | Succeeded -> \"Succeeded\" | Refused -> \"Refused\" | Failed -> \"Failed\") | _ -> \"(no state)\" in\n"
        + "let input = fun action text -> Darklang.Cli.Workbench.InputState { prompt = \"\"; field = Stdlib.Cli.UI.TextField.fromText text; action = action } in\n"
        + "Stdlib.String.join [ kindOf (Darklang.Cli.Workbench.performInputAction st (input \"author-fn\" \"Tests.\")), kindOf (Darklang.Cli.Workbench.performInputAction st (input \"search\" \"zzzWbNothingIsCalledThis\")), kindOf (Darklang.Cli.Workbench.resolveConflict st \"ok\"), kindOf (Darklang.Cli.Workbench.syncInFrame st \"push\") ] \"\\n\""

      let! answer =
        task {
          match! evalUnder (executionState state) code with
          | RT.DString lines -> return lines
          | other -> return Tests.failtestf "the workbench answered %A" other
        }

      let lines = answer.Split('\n')
      Expect.equal lines.Length 4 $"four answers:\n{answer}"

      for (line, text) in
        Array.zip
          lines
          [| "need at least owner.Module"
             "no matches for"
             "no conflict selected"
             "no relay yet" |] do
        Expect.stringContains line text $"the message is the one meant:\n{answer}"
        Expect.stringContains
          line
          "=> Refused"
          $"and it is marked as a refusal:\n{answer}"
    })

/// Editing an implementation updates it in place.
///
/// The save used to take the impl's location from the first name on its `impl` line, which is the
/// TRAIT, printed qualified, and resolve it under the module being edited in. So the edit landed as
/// a second implementation at `Box.WbEdit.Box.WbShout`, newer than the original, and the original
/// went "not used". The call still answered with the new text, which is why it looked fine.
let private workbenchImplEditUpdatesInPlace =
  cliTest
    "editing an implementation in the workbench leaves exactly one"
    (fun state ->
      task {
        let! _ =
          runCli
            state
            [ "type"; "/Tests.WbImplEdit.Box"; "{ wbImplEditMark: Int64 }" ]
        let! _ =
          runCli
            state
            [ "trait"
              "/Tests.WbImplEdit.WbShout"
              "<'a> = let shout (v: 'a) : String" ]
        let! _ =
          runCli
            state
            [ "impl"
              "/Tests.WbImplEdit"
              "WbShout for Box = let shout (b: Box) : String = \"box\"" ]

        let! saved =
          editInWorkbench
            state
            [ "Tests"; "WbImplEdit"; "Box" ]
            "WbShout"
            "\"box\""
            "\"box2\""
        Expect.stringContains saved "footer: saved" "the edit saved"

        let! impls = runCli state [ "impls"; "Tests.WbImplEdit.WbShout" ]
        let rows =
          impls.Split('\n')
          |> Array.filter (fun l -> l.Contains "Tests.WbImplEdit.Box")
        Expect.equal
          rows.Length
          1
          $"one implementation after the edit, not a rival beside it:\n{impls}"
        Expect.isFalse
          (impls.Contains "not used")
          $"and nothing it left behind is marked unused:\n{impls}"

        let! answer =
          runCli
            state
            [ "eval"
              "Tests.WbImplEdit.WbShout.shout (Tests.WbImplEdit.Box { wbImplEditMark = 1L })" ]
        Expect.stringContains answer "box2" "the call answers with the edited body"
      })

/// Editing a trait carries its implementations along, as `dark trait` does.
///
/// The workbench saved straight through `addAuthored` and never propagated, so the trait's hash
/// moved and every implementation stayed on the old one: none of them counted any more, and even
/// the ORIGINAL method stopped answering.
let private workbenchTraitEditKeepsImplementations =
  cliTest
    "editing a trait in the workbench keeps its implementations working"
    (fun state ->
      task {
        let! _ =
          runCli
            state
            [ "type"; "/Tests.WbTraitEdit.Box"; "{ wbTraitEditMark: Int64 }" ]
        let! _ =
          runCli
            state
            [ "trait"
              "/Tests.WbTraitEdit.WbWave"
              "<'a> = let shout (v: 'a) : String" ]
        let! _ =
          runCli
            state
            [ "impl"
              "/Tests.WbTraitEdit"
              "WbWave for Box = let shout (b: Box) : String = \"box\"" ]

        let! saved =
          editInWorkbench
            state
            [ "Tests"; "WbTraitEdit" ]
            "WbWave"
            "let shout (v: 'a) : String"
            "let shout (v: 'a) : String\n  let wave (v: 'a) : String"
        Expect.stringContains saved "footer: saved" "the edit saved"

        let! answer =
          runCli
            state
            [ "eval"
              "Tests.WbTraitEdit.WbWave.shout (Tests.WbTraitEdit.Box { wbTraitEditMark = 1L })" ]
        Expect.stringContains
          answer
          "box"
          "the existing implementation still answers the method it has"

        let! impls = runCli state [ "impls"; "Tests.WbTraitEdit.WbWave" ]
        Expect.stringContains
          impls
          "Tests.WbTraitEdit.Box"
          "and the implementation is still listed against the trait"

        // It follows the trait without the new method, and the save says so rather than "saved" alone.
        Expect.stringContains
          saved
          "no longer type-checks"
          "the footer names the implementation the new method left incomplete"

        let! missing =
          runCli
            state
            [ "eval"
              "Tests.WbTraitEdit.WbWave.wave (Tests.WbTraitEdit.Box { wbTraitEditMark = 1L })" ]
        Expect.stringContains
          missing
          "chosen here has no `wave`"
          "calling the new method blames the implementation, not the trait that declares it"
      })

/// The same omission, for the plainest case: a caller of an edited fn moves to the new version.
let private workbenchFnEditCarriesCallers =
  cliTest
    "editing a function in the workbench carries its callers along"
    (fun state ->
      task {
        let! _ =
          runCli state [ "fn"; "/Tests.WbFnEdit.g"; "(x: Int64) : Int64 = x + 1L" ]
        let! _ =
          runCli
            state
            [ "fn"; "/Tests.WbFnEdit.f"; "(x: Int64) : Int64 = Tests.WbFnEdit.g x" ]

        let! saved =
          editInWorkbench state [ "Tests"; "WbFnEdit" ] "g" "x + 1L" "x + 5L"
        Expect.stringContains saved "footer: saved" "the edit saved"

        let! answer = runCli state [ "eval"; "Tests.WbFnEdit.f 1L" ]
        Expect.stringContains answer "6" "the caller runs the edited body"
      })


/// In the run order CliTraces.Tests.fs composes; sequencing lives there too.
let tests : List<Test> =
  [ unwrapErrorsAreReadable
    workbenchImplEditUpdatesInPlace
    workbenchTraitEditKeepsImplementations
    workbenchFnEditCarriesCallers
    workbenchRefusesImplOverTrait
    workbenchRefusalsNameTheProblem
    workbenchSaysUnchangedAndLands
    workbenchDocCommentDocuments
    workbenchRefusalsAreMarkedAsRefusals
    reusesCompiledFunctions
    timeoutBoundsSynchronousWork
    timeoutPreservesCapture
    stderrIsCapturedInOrder
    testHelpCommand
    everyCommandAnswersHelp
    workbenchViewsRender
    showingACommitDoesNotFetchEveryOp
    workbenchBranchActionsWork
    mergeAndRebaseRefuseOnMain
    everyExclusionIsReal
    everyHelpLeadsWithASummary
    permissionsRefusesAnEmptyRule
    aDashLedArgumentIsNeverAName
    unsweptCommandsRefuseWithExit1
    viewHeadsWithTheNameYouAskedFor
    headerKeepsTheBranchWhenNarrow
    hintRowKeepsTheWayOut
    workbenchHandlesTerminalSizes
    noWorkbenchRowOverflowsItsFrame
    workbenchContextRowSaysWhereYouAre
    missingTargetsAreNamed
    traitRefusalsExitNonZero
    otherRefusalsExitNonZero
    documentedCommandsAreReal
    workbenchPageFollowsYourSave ]
  @ workbenchNavigationRegressions
