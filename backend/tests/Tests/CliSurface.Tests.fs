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
let private registeredCommands (state : RT.ExecutionState) : Task<List<string>> =
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
  "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Tester\" \"test-instance\" [] in "
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
              "let n = fun v -> Stdlib.List.length (frame v 120 40) in [ n 0, n 1, n 3, n 4, n 6, n 7, n 8, n 9 ]" ]

      Expect.stringContains
        output
        "[40, 40, 40, 40, 40, 40, 40, 40]"
        "all eight views render a full 40-row frame"
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
          "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Stachu\" \"i\" [] in "
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
        // The pane hints are the SIDEBAR's here, since that is what holds the keyboard when the workbench
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

/// The context row must not overwrite its own tail on a narrower terminal.
///
/// It writes the left-hand text at column 0 and the sync glance right-aligned over the same row, so
/// anything the left side spills past the glance is lost, starting with the END of the draft
/// summary. It drops whole segments in priority order instead (`instance:` first, then the account
/// name), so the draft split survives far narrower.
let private contextRowKeepsTheDraftWhenNarrow =
  cliTest
    "the context row drops labels before it drops the draft summary"
    (fun state ->
      task {
        // A deliberately long instance name, so the row is over-full whatever the shared
        // store holds in its draft: the test creates the condition rather than hoping.
        let row (w : int) : string =
          "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Stachu\" \"inst-with-a-deliberately-long-name-for-this-test\" [] in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
          + "let s1 = { s0 with activeView = 4 } in "
          + "let s = { s1 with items = Darklang.Cli.Workbench.reloadItems s1 } in "
          + $"Stdlib.String.join (Stdlib.List.take (Darklang.Cli.Workbench.viewAtSize s (Darklang.Stdlib.Cli.Tui.Size {{ width = {w}; height = 4 }})).rows 1) \"\""

        let! wide = runCli state [ "eval"; row 150 ]
        Expect.stringContains
          wide
          "instance:"
          "the full row shows the instance label"
        Expect.stringContains wide "branch:" "and the branch"

        let! narrow = runCli state [ "eval"; row 90 ]
        Expect.isFalse
          (narrow.Contains "instance:")
          "the instance label is dropped to make room, rather than the row colliding"
        Expect.stringContains
          narrow
          "branch:"
          "the branch survives, being worth more than the label"
        // Asserted WHOLE, not by name: which glance wins is a priority decision, and the
        // shared store decides which exist, so naming one makes this hostage to other tests.
        let glanceIsWhole =
          [ "waiting"; "need you"; "in sync" ]
          |> List.exists (fun g -> narrow.Contains g)

        Expect.isTrue
          glanceIsWhole
          $"the right-aligned glance is whole, not overwritten (row: {narrow})"
      })

/// The workbench's own mutating actions, driven the way a keypress drives them.
///
/// `b` in the SCM view prompts for a name and then runs `branch-create`. `Branch.create` returns an
/// `Option<Branch>`, so reading `.id` off it throws. The render tests cannot reach any of this: it
/// is behind a prompt, not a render.
let private workbenchBranchActionsWork =
  cliTestOnMain
    "the workbench can start, switch and merge a branch without throwing"
    (fun state ->
      task {
        let act (action : string) (text : string) : string =
          "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"T\" \"t\" [] in "
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
          "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"T\" \"t\" [] in "
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

/// The SCM view has four sections behind `tab`, and the view-level render test above never switches
/// section, so it only exercises Changes. Dark catches a wrong type at runtime, so a bad section
/// throws on the keypress that switches to it rather than at build time.
let private everyScmSectionRenders =
  cliTest "every SCM section renders, not just the one it opens on" (fun state ->
    task {
      let! output =
        runCli
          state
          [ "eval"
            "let st = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId (Stdlib.Option.Option.None) \"Tester\" \"test-instance\" [] in "
            + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
            // The section is a DU now, not an int, and it lives in `scm: ScmState` rather than a flat
            // `scmSection` field -- same for the AI section and the Matter lens.
            + "let sect = fun n -> "
            + "  let s1 = { s0 with activeView = 4; scm = Darklang.Cli.Workbench.ScmState { section = n } } in "
            + "  let s = { s1 with items = Darklang.Cli.Workbench.itemsForView 4 s1.branchId s1.location n s1.ai.section s1.matter.lens } in "
            + "  Stdlib.List.length (Darklang.Cli.Workbench.viewAtSize s (Darklang.Stdlib.Cli.Tui.Size { width = 120; height = 40 })).rows in "
            + "[ sect Darklang.Cli.Workbench.ScmSection.Changes"
            + ", sect Darklang.Cli.Workbench.ScmSection.History"
            + ", sect Darklang.Cli.Workbench.ScmSection.Conflicts"
            + ", sect Darklang.Cli.Workbench.ScmSection.Branches ]" ]

      Expect.stringContains
        output
        "[40, 40, 40, 40]"
        "Changes, History, Conflicts and Branches each render a full frame"
    })

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
  cliTest "the workbench context row names the branch, in every view" (fun state ->
    task {
      // Matches the LABEL, not "branch: main": the context row styles its label and value separately,
      // so colour codes sit between them and they are never adjacent in the string.
      let! output =
        runCli
          state
          [ "eval"
            renderExpr
              "let has = fun v -> Stdlib.String.contains (Stdlib.String.join (frame v 120 40) \"|\") \"branch:\" in [ has 0, has 1, has 4 ]" ]

      Expect.stringContains
        output
        "[true, true, true]"
        "Home, Matter and SCM all carry the context row"

      let! named =
        runCli
          state
          [ "eval"
            renderExpr
              "Stdlib.String.contains (Stdlib.String.join (frame 0 120 40) \"|\") \"test-instance\"" ]

      Expect.stringContains named "true" "and it names the instance you're on"
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
      "export-seed" // takes its argument as a path and writes a multi-MB database there
      "devices" // shells out to `tailscale`
      "clear" ] // clears the screen, taking the sweep's own output with it

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
/// `TerminalSupport.current ()` first and gets `Unavailable`. Note the limit, though: `runCli` has no
/// timeout, so a command that truly blocks forever hangs the RUN rather than failing it. If a second
/// interactive command turns up, this wants a bound rather than a comment.
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
    "ack", [ "ack"; "zzznope" ] ]

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

/// Every `dark <word>` the in-CLI docs mention has to be a real command.
///
/// A doc that confidently describes a command that is not there is worse than no doc, and nothing
/// else checks. Narrow by design: only the WORD after `dark`, which is what is verifiable
/// mechanically. It cannot tell you the prose is wrong, only that the commands are real.
let private docTopicsToCheck = [ "scm"; "for-ai"; "cli" ]

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


/// In the run order CliTraces.Tests.fs composes; sequencing lives there too.
let tests : List<Test> =
  [ testHelpCommand
    everyCommandAnswersHelp
    workbenchViewsRender
    everyScmSectionRenders
    showingACommitDoesNotFetchEveryOp
    workbenchBranchActionsWork
    mergeAndRebaseRefuseOnMain
    everyExclusionIsReal
    permissionsRefusesAnEmptyRule
    aDashLedArgumentIsNeverAName
    viewHeadsWithTheNameYouAskedFor
    contextRowKeepsTheDraftWhenNarrow
    hintRowKeepsTheWayOut
    workbenchHandlesTerminalSizes
    noWorkbenchRowOverflowsItsFrame
    workbenchContextRowSaysWhereYouAre
    missingTargetsAreNamed
    documentedCommandsAreReal ]
