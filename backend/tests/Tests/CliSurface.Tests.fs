/// The CLI's command surface as a whole: the registry-driven sweeps (every command
/// bare, with --help, with a bogus argument, standing on a branch), the workbench
/// views on their frame contract, and the docs' claims checked against the registry.
/// Run order and sequencing live in CliTraces.Tests.fs, which composes this list.
module Tests.CliSurface

open Expecto
open System.Threading.Tasks

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


let private storedPermissionsAreIsolated =
  testSequenced
  <| testTask "CLI tests isolate stored permissions even after a failure" {
    let originalDirectory = LibExecution.HostSecurity.policyDirectory ()
    let mutable initial = LibDB.PolicyStore.empty
    try
      do!
        withState (fun _ ->
          task {
            initial <- LibDB.PolicyStore.get ()
            LibDB.PolicyStore.setInstancePolicy
              LibExecution.Permissions.Policy.denyAll
            let approved =
              LibDB.PolicyStore.recordApproval
                None
                "Tests.PolicyIsolation.root"
                (LibDB.PolicyStore.reviewToken (LibDB.PolicyStore.get ()))
                { hash = "test-policy-root"
                  policy = LibExecution.Permissions.Policy.denyAll
                  fingerprint = "test-fingerprint"
                  explicitRules = false }
            Expect.equal approved (Ok()) "the first test stored an approval"
            Exception.raiseInternal "simulated test failure" []
          })
      failtest "the first test should have raised"
    with e when e.Message = "simulated test failure" ->
      ()
    Expect.equal
      (LibExecution.HostSecurity.policyDirectory ())
      originalDirectory
      "the failed test restored the policy directory"
    do!
      withState (fun _ ->
        task {
          Expect.equal
            (LibDB.PolicyStore.get ())
            initial
            "the next test gets fresh instance permissions and no old approvals"
        })
  }


let private reusesCompiledFunctions =
  cliTest "the CLI harness reuses compiled package functions" (fun target ->
    task {
      let state = executionState target
      let hash = RT.Hash(LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())
      let first = state.fns.package hash
      let second = state.fns.package hash
      match first, second with
      | Some first, Some second ->
        Expect.isTrue
          (System.Object.ReferenceEquals(first, second))
          "fetching a function again must reuse its compiled instructions"
      | _ -> Tests.failtest "the CLI entry point must be available"
    })

let private timeoutBoundsSynchronousWork =
  testSequenced
  <| testTask "the CLI timeout bounds work before its first await" {
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
  testSequenced
  <| testTask "the CLI timeout preserves results and captured output" {
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
        "let _ = Stdlib.printLine(\"first\") in let _ = Stdlib.printErrorLine(\"second\") in let _ = Stdlib.printLine(\"third\") in 4"
        + ""
        + ""

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

let private stdinThatIsNotUtf8IsRefused =
  Tests.CliInstance.instanceTest
    "stdin that is not UTF-8 is refused, not replaced"
    (fun i ->
      task {
        let notUtf8 = Some [| 0xFFuy; 0x00uy; 0xFEuy |]
        let! (code, out, _) =
          Tests.CliInstance.runRawWithBytes
            i
            [ "eval"; "Stdlib.Cli.Stdin.readAll()" ]
            notUtf8
        Expect.equal code 0 "readAll answers"
        Expect.equal
          ((CliDsl.plain out).Trim())
          "Error(\"stdin is not valid UTF-8: FF\")"
          "an Error naming the first bad byte"

        let! (code, out, err) =
          Tests.CliInstance.runRawWithBytes i [ "eval"; "-" ] notUtf8
        Expect.equal code 1 "eval - refuses"
        Expect.equal out "" "no answer"
        Expect.stringContains err "stdin is not valid UTF-8: FF" "says why"
      })

/// `ps` and `top` read a process's name and command line from /proc; the process reads its own.
let private processTitleSaysWhatRuns =
  Tests.CliInstance.instanceTest "a process's title says what it runs" (fun i ->
    task {
      if System.OperatingSystem.IsLinux() then
        let! (code, _, err) =
          Tests.CliInstance.runRaw
            i
            [ "permissions"; "allow"; "file"; "read"; "/proc" ]
        Expect.equal code 0 $"allowing /proc: {err}"
        let read (file : string) =
          task {
            let! (_, out, err) =
              Tests.CliInstance.runRaw
                i
                [ "eval"; $"Stdlib.Cli.File.readText(\"/proc/self/{file}\")" ]
            return out + err
          }
        let! comm = read "comm"
        Expect.stringContains comm "dark eval" "the name"
        let! cmdline = read "cmdline"
        Expect.stringContains
          cmdline
          "dark eval Stdlib.Cli.File.readText"
          "the command line names `dark`, not the binary's path"
    })

/// `dark run ... | head` ends when `head` does: once nobody reads stdout, the run stops, quietly.
let private runStopsWhenStdoutCloses =
  Tests.CliInstance.instanceTest "a run stops when its stdout closes" (fun i ->
    task {
      if not (System.OperatingSystem.IsWindows()) then
        // A line every 20 ms, for a minute.
        let script = System.IO.Path.Combine(i.dir, "ticks.dark")
        System.IO.File.WriteAllText(
          script,
          "let tick(n: Int64): Int64 =\n"
          + "  Stdlib.printLine(Stdlib.display(n))\n"
          + "  Stdlib.Cli.Posix.sleep(20.0)\n"
          + "  if n >= 3000 then 0 else tick(n + 1)\n"
          + "tick(1)\n"
        )
        let p = Tests.CliInstance.startUndrained i [ "run"; script ]
        try
          let! first = p.StandardOutput.ReadLineAsync()
          Expect.equal first "1" "the run prints"
          p.StandardOutput.Close()
          Expect.isTrue
            (p.WaitForExit 30000)
            "it stops soon after its stdout closes"
          Expect.equal p.ExitCode 141 "and exits as a process SIGPIPE ended"
          Expect.equal (p.StandardError.ReadToEnd()) "" "saying nothing"
        finally
          Tests.CliInstance.stop p
    })

let private testHelpCommand =
  instanceTest "help command" (fun state ->
    task {
      let! output = runCli state [ "help" ]
      Expect.stringContains output "Packages:" "category header"
      Expect.stringContains output "Changes:" "changes header"
      Expect.stringContains output "Branches:" "branches header"
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

// The workbench renders. `initialState` builds a state without seizing the terminal, and this goes
// through `dark eval` rather than a `.dark` testfile because building one reads the package tree,
// and the execution testfiles are for pure functions.

/// One line, because `eval` takes the expression as a single argument.
let private renderExpr (body : string) : string =
  "let st = Darklang.Cli.Workbench.initialState(Darklang.SCM.Branch.mainBranchId, Stdlib.Option.Option.None, \"test-instance\", Darklang.Cli.Workbench.Updates.Manual) in let s = Darklang.Cli.Workbench.refreshScmStatus(st, st) in let frame = fun(v, w, h) -> (Darklang.Cli.Workbench.viewAtSize({ s with activeView = v }, Darklang.Stdlib.Cli.Tui.Size { width = w, height = h })).rows in "
  + ""
  + ""
  + body

let private workbenchViewsRender =
  instanceTest "every workbench view renders a full frame" (fun state ->
    task {
      // A view that throws produces no frame at all, which is what these row counts
      // are really checking.
      let! output =
        runCli
          state
          [ "eval"
            renderExpr
              "Stdlib.List.all(Darklang.Cli.Workbench.viewSpecs, fun((v, _name)) -> Stdlib.List.length(frame(v, 120, 40)) == 40)" ]

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
  instanceTest
    "the hint row drops secondary keys before it drops help and quit"
    (fun state ->
      task {
        let lastRow (w : int) : string =
          "let st = Darklang.Cli.Workbench.initialState(Darklang.SCM.Branch.mainBranchId, Stdlib.Option.Option.None, \"i\", Darklang.Cli.Workbench.Updates.Manual) in let s0 = Darklang.Cli.Workbench.refreshScmStatus(st, st) in let s1 = { s0 with activeView = Darklang.Cli.Workbench.vSCM } in let s = { s1 with items = Darklang.Cli.Workbench.reloadItems(s1) } in "
          + ""
          + ""
          + ""
          // Tall enough that the hint row is always in frame: a clean tree renders a
          // roomier panel than a draft, so a short frame loses the hints in one store.
          + $"Stdlib.String.join((Darklang.Cli.Workbench.viewAtSize(s, Darklang.Stdlib.Cli.Tui.Size {{ width = {w}, height = 26 }})).rows, \"\\n\")"

        // The rule under test is the ORDER things are dropped in, not the width at which
        // dropping starts.
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
          (narrow.Contains "views")
          "and a secondary global is what gave way to fit them"

        // Narrower still: actions start giving way, but never the escape hatches.
        let! tiny = runCli state [ "eval"; lastRow 56 ]
        Expect.stringContains tiny "help" "the keymap key still survives"
        Expect.stringContains tiny "quit" "and the way out is still there"
      })

/// The header must not overwrite its own tail on a narrower terminal.
///
/// It writes the left-hand text at column 0 and the glance right-aligned over the same row, so
/// anything the left side spills past the glance is lost. It drops whole segments in priority order
/// instead, the instance first, so the branch survives far narrower.
let private headerKeepsTheBranchWhenNarrow =
  instanceTest
    "the header drops the instance before the branch, and never the branch"
    (fun state ->
      task {
        // A deliberately long instance name, so the row is over-full whatever the shared
        // store holds: the test creates the condition rather than hoping.
        let row (w : int) : string =
          "let st = Darklang.Cli.Workbench.initialState(Darklang.SCM.Branch.mainBranchId, Stdlib.Option.Option.None, \"inst-with-a-deliberately-long-name-for-this-test\", Darklang.Cli.Workbench.Updates.Manual) in let s0 = Darklang.Cli.Workbench.refreshScmStatus(st, st) in let s1 = { s0 with activeView = Darklang.Cli.Workbench.vSCM } in let s = { s1 with items = Darklang.Cli.Workbench.reloadItems(s1) } in "
          + ""
          + ""
          + ""
          + $"Stdlib.String.join(Stdlib.List.take((Darklang.Cli.Workbench.viewAtSize(s, Darklang.Stdlib.Cli.Tui.Size {{ width = {w}, height = 4 }})).rows, 1), \"\")"

        let! wide = runCli state [ "eval"; row 150 ]
        Expect.stringContains
          wide
          "inst-with-a-deliberately-long-name-for-this-test"
          "the full row shows where you are"
        Expect.stringContains wide "main" "and the branch"

        // 56 columns: two under what the row needs for a 48-character instance name beside a
        // four-character branch. Narrower than about 50 and the workbench's too-small guard draws
        // nothing at all, which would pass this assertion for the wrong reason.
        let! narrow = runCli state [ "eval"; row 56 ]
        Expect.isFalse
          (narrow.Contains "inst-with-a-deliberately")
          "the instance is dropped to make room, rather than the row colliding"
        Expect.stringContains
          narrow
          "main"
          "and the branch survives, being worth more than the instance"
      })

let private workbenchBranchActionsWork =
  instanceTest
    "the workbench can start, switch and merge a branch without throwing"
    (fun state ->
      task {
        let act (action : string) (text : string) : string =
          "let st = Darklang.Cli.Workbench.initialState(Darklang.SCM.Branch.mainBranchId, Stdlib.Option.Option.None, \"t\", Darklang.Cli.Workbench.Updates.Manual) in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus(st, st) in "
          + "let s = { s0 with activeView = Darklang.Cli.Workbench.vSCM } in "
          + $"match Darklang.Cli.Workbench.performInputAction(s, Darklang.Cli.Workbench.InputState {{ prompt = \"p\", field = Stdlib.Cli.UI.TextField.fromText(\"{text}\"), action = \"{action}\" }}) with "
          + "| Continue(ns) -> ns.message | _ -> \"(exit)\""

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
  instanceTest
    "merge and rebase refuse on main, rather than claiming to have run"
    (fun state ->
      task {
        let act (action : string) : string =
          "let st = Darklang.Cli.Workbench.initialState(Darklang.SCM.Branch.mainBranchId, Stdlib.Option.Option.None, \"t\", Darklang.Cli.Workbench.Updates.Manual) in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus(st, st) in "
          + "let s = { s0 with activeView = Darklang.Cli.Workbench.vSCM } in "
          + $"match Darklang.Cli.Workbench.performInputAction(s, Darklang.Cli.Workbench.InputState {{ prompt = \"p\", field = Stdlib.Cli.UI.TextField.fromText(\"y\"), action = \"{action}\" }}) with "
          + "| Continue(ns) -> ns.message | _ -> \"(exit)\""

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
  instanceTest
    "showing a big commit's ops is capped, not fetched whole"
    (fun state ->
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

let private workbenchHandlesTerminalSizes =
  instanceTest
    "the workbench frames a tiny terminal instead of breaking"
    (fun state ->
      task {
        // 56x12 is the documented floor: at it you get a real frame, below it a resize
        // message rather than a mangled layout. Both fill the space they were given.
        let! atFloor =
          runCli state [ "eval"; renderExpr "Stdlib.List.length(frame(0, 56, 12))" ]
        Expect.stringContains atFloor "12" "a frame at the documented minimum size"

        let! tooSmall =
          runCli
            state
            [ "eval"
              renderExpr
                "Stdlib.String.join(frame(0, 40, 10), \"|\") |> Stdlib.String.contains(\"too small\")" ]
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
  instanceTest
    "no workbench row is wider than its frame, wide glyphs included"
    (fun state ->
      task {
        // A CJK name and an emoji commit message, so the frame is measuring real wide glyphs.
        let! _ = runCli state [ "fn"; "Tests.Wide.世界"; "() : Int64 = 1" ]
        let! _ = runCli state [ "commit"; "コミット 🎉 wide"; "-y" ]

        let over (width : int) : string =
          renderExpr (
            $"let over = fun v -> frame(v, {width}, 24) "
            + "|> Stdlib.List.map(fun r -> Darklang.Stdlib.Cli.Tui.Text.styledWidth(r)) "
            + $"|> Stdlib.List.filter(fun n -> n > {width}) "
            + "|> Stdlib.List.length in "
            + "[ over(0), over(1), over(3), over(4), over(6), over(7), over(8), over(9) ]"
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
  instanceTest "the workbench header names the branch, in every view" (fun state ->
    task {
      // The header is the first row: the branch, then the instance. The two are styled apart, so
      // they are never adjacent in the string; each is matched on its own.
      let! output =
        runCli
          state
          [ "eval"
            renderExpr
              "let top = fun v -> Stdlib.String.join(Stdlib.List.take(frame(v, 120, 40), 1), \"|\") in let has = fun v -> Stdlib.String.contains(top(v), \"main\") in [ has(0), has(1), has(4) ]" ]

      Expect.stringContains
        output
        "[true, true, true]"
        "Home, Matter and SCM all carry the header, branch named"

      let! named =
        runCli
          state
          [ "eval"
            renderExpr
              "Stdlib.String.contains(Stdlib.String.join(Stdlib.List.take(frame(0, 120, 40), 1), \"|\"), \"test-instance\")" ]

      Expect.stringContains named "true" "and it names the instance you are on"
    })

// ─── Refusals ──────────────────────────────────────────────────────────────────
//
// A command REFUSES what it was not built to take, and refusing means exiting non-zero: a refusal
// that exits 0 is success to a shell, a CI step or an agent, and `dark discard --only-this -y`
// discarded the whole draft because the junk was ignored. Each test below states one part of
// that, by name. Where a command departs from it, the departure is written down beside the test
// with what the command does instead, and that is asserted too, or said plainly not to run.

/// A Dark string literal holding <param s>.
let private darkString (s : string) : string =
  "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n") + "\""

/// An empty grant is not a grant, and must not report that it is.
let private permissionsRefusesAnEmptyRule =
  instanceTest
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

/// Several rules in one call, `;`-separated. One that is not a rule saves none of them.
let private permissionsAllowTakesSeveralRules =
  instanceTest
    "`permissions allow` takes several rules, and saves none when one is not a rule"
    (fun state ->
      task {
        let! refused, code =
          runCliWithExit
            state
            [ "permissions"; "allow"; "http-server 127.0.0.1 8123; zzznope" ]
        Expect.equal code 1L $"a rule that is not one fails the call:\n{refused}"
        Expect.stringContains
          refused
          "invalid permission rule: zzznope"
          "it names the one that is not a rule"
        let! listed = runCli state [ "permissions"; "list" ]
        Expect.isFalse
          (listed.Contains "http-server")
          $"and the good rule beside it was not saved:\n{listed}"

        let! _ =
          runCli
            state
            [ "permissions"; "allow"; "http-server 127.0.0.1 8123; file read /tmp" ]
        let! listed = runCli state [ "permissions"; "list" ]
        Expect.stringContains
          listed
          "http-server"
          $"the first rule is saved:\n{listed}"
        Expect.stringContains listed "file read '/tmp'" $"and the second:\n{listed}"
      })

/// Every `dark <word>` the in-CLI docs mention has to be a real command.
///
/// A doc that confidently describes a command that is not there is worse than no doc, and nothing
/// else checks. Narrow by design: only the WORD after `dark`, which is what is verifiable
/// mechanically. It cannot tell you the prose is wrong, only that the commands are real.
let private docTopicsToCheck =
  [ "scm"
    "for-ai"
    "for-ai packages"
    "for-ai tests"
    "for-ai permissions"
    "for-ai traces"
    "for-ai scm"
    "for-ai syntax"
    "cli"
    "processes"
    "live"
    "http-server" ]

let private documentedCommandsAreReal =
  instanceTest "every command the docs mention exists" (fun state ->
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
        let! doc = runCli state ("docs" :: List.ofArray (topic.Split(' ')))

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

/// Two names with byte-identical bodies are ONE item, and a `PackageFn` carries no name, so showing one
/// means resolving its hash back to a name. With several to choose from, the scoring picked whichever it
/// liked and `dark view Tests.SharedBody.alpha` printed a definition headed `beta`.
///
/// Scoring is right for a name appearing inside someone else's code and wrong for the thing you just
/// asked to see, and nothing in the scoring can know which name was typed, so the asked-for location is
/// carried into the printer and wins outright when it is one of the candidates.
let private viewHeadsWithTheNameYouAskedFor =
  instanceTest
    "view heads a shared body with the name that was asked for"
    (fun state ->
      task {
        let! _ = runCli state [ "switch"; "main" ]
        let! _ =
          runCli state [ "fn"; "Tests.SharedBody.alpha"; "() : Int64 = 4242" ]
        let! _ = runCli state [ "fn"; "Tests.SharedBody.beta"; "() : Int64 = 4242" ]

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
  instanceTest "eval checks unwrap operands and runs valid unwraps" (fun state ->
    task {
      let! rejected, exitCode =
        runCliWithExit state [ "eval"; "(fun value -> Some(value? + 1))(4)" ]
      Expect.equal exitCode 1L "invalid unwrap fails eval"
      Expect.stringContains
        rejected
        "`?` needs an Option or Result"
        "names the ? and its required operand"
      Expect.isFalse
        (rejected.Contains "Encountered a Runtime Error")
        "the error renderer itself succeeds"
      let! accepted =
        runCli state [ "eval"; "(fun value -> Some(value? + 1))(Some(4))" ]
      Expect.stringContains accepted "Some(5)" "valid unwrap still executes"
    })


// Editing from the workbench, the way a person does it: `e` on an item, change some text, `^s`.
// Driven through `openEditExisting` and `saveEditing` rather than keystrokes, so what is under test
// is the save and not the renderer. The editor is prefilled by the pretty-printer, which is where
// the impl's qualified names come from.

/// Open <param item> in <param modules> in the editor, replace <param find> with <param replace> in
/// what it prefilled, save, and answer the footer (or what went wrong instead).
let private workbenchEdit
  (modules : List<string>)
  (item : string)
  (find : string)
  (replace : string)
  : string =
  let path = modules |> List.map darkString |> String.concat ", "
  "let st0 = Darklang.Cli.Workbench.initialState(Darklang.SCM.Branch.mainBranchId, Stdlib.Option.Option.None, \"i\", Darklang.Cli.Workbench.Updates.Manual) in\n"
  + $"let st = {{ st0 with activeView = Darklang.Cli.Workbench.vMatter, location = Darklang.Cli.Packages.PackageLocation.Module([ {path} ]) }} in\n"
  + "let items = Darklang.Cli.Workbench.reloadItems(st) in\n"
  + $"match Stdlib.List.findFirst(items, fun i -> i.name == {darkString item}) with\n"
  + "| None -> \"no item to edit\"\n"
  + "| Some(item) ->\n"
  + "  match Darklang.Cli.Workbench.openEditExisting(st, item) with\n"
  + "  | Continue(opened) ->\n"
  + "    match opened.editing with\n"
  + "    | None -> \"did not open: \" + opened.message\n"
  + "    | Some(es) ->\n"
  + $"      let edited = Stdlib.String.replaceAll(Stdlib.Cli.UI.Editor.toText(es.buf), {darkString find}, {darkString replace}) in\n"
  + "      match Darklang.Cli.Workbench.saveEditing(opened, { es with buf = Stdlib.Cli.UI.Editor.fromText(edited) }) with\n"
  + "      | Continue(saved) ->\n"
  + "        match saved.editing with\n"
  + "        | Some(still) -> \"save refused: \" + still.err\n"
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
  "let st0 = Darklang.Cli.Workbench.initialState(Darklang.SCM.Branch.mainBranchId, Stdlib.Option.Option.None, \"i\", Darklang.Cli.Workbench.Updates.Manual) in\n"
  + $"let st = {{ st0 with activeView = Darklang.Cli.Workbench.vMatter, location = Darklang.Cli.Packages.PackageLocation.Module([ {path location} ]) }} in\n"
  + $"match Darklang.Cli.Workbench.openNewEditor(st, {darkString kind}, [ {path target} ]) with\n"
  + "| Continue(opened) ->\n"
  + "  match opened.editing with\n"
  + "  | None -> \"did not open: \" + opened.message\n"
  + "  | Some(es) ->\n"
  + $"    match Darklang.Cli.Workbench.saveEditing(opened, {{ es with buf = Stdlib.Cli.UI.Editor.fromText({darkString source}) }}) with\n"
  + "    | Continue(saved) ->\n"
  + "      match saved.editing with\n"
  + "      | Some(still) -> \"save refused: \" + still.err\n"
  + "      | None ->\n"
  + "        let on = (Stdlib.List.getAt(saved.items, saved.selected)) |> Stdlib.Option.map(fun i -> i.name) |> Stdlib.Option.withDefault(\"\") in\n"
  + "        \"footer: \" + saved.message + \" | at: \" + (Stdlib.String.join(Darklang.Cli.Packages.modulePathOf(saved.location), \".\")) + \" | on: \" + on\n"
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
            [ "trait"; "/Tests.WbPt.WbSh"; "<'a> = let sh(v: 'a): String" ]

        let! answer =
          newInWorkbench
            state
            [ "Tests"; "WbPt" ]
            "impl"
            [ "Tests"; "WbPt" ]
            "impl WbSh for WbPt =\n  let sh(p: WbPt): String = \"p\""
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
          "let wbQ(x: Int64): Int64 = Nope.wbZzz(x)"
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
          "impl WbNoSuchTrait for Int64 =\n  let m(x: Int64): String = \"\""
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
            [ "fn"; "/Tests.WbSame.same"; "(x: Int64) : Int64 = x + 7311" ]

        let! same =
          editInWorkbench state [ "Tests"; "WbSame" ] "same" "x + 7311" "x + 7311"
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
            "let landedHere(x: Int64): Int64 = x"
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
            [ "fn"; "/Tests.WbDoc.docme"; "(x: Int64) : Int64 = x + 9137" ]

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
        "let st0 = Darklang.Cli.Workbench.initialState(Darklang.SCM.Branch.mainBranchId, Stdlib.Option.Option.None, \"i\", Darklang.Cli.Workbench.Updates.Manual) in\nlet st = { st0 with activeView = Darklang.Cli.Workbench.vMatter } in\nlet kindOf = fun step -> match step with | Continue(s) -> s.message + \" => \" + (match s.messageKind with | Succeeded -> \"Succeeded\" | Refused -> \"Refused\" | Failed -> \"Failed\") | _ -> \"(no state)\" in\nlet input = fun(action, text) -> Darklang.Cli.Workbench.InputState { prompt = \"\", field = Stdlib.Cli.UI.TextField.fromText(text), action = action } in\nStdlib.String.join([ kindOf(Darklang.Cli.Workbench.performInputAction(st, input(\"author-fn\", \" \"))), kindOf(Darklang.Cli.Workbench.performInputAction(st, input(\"search\", \"zzzWbNothingIsCalledThis\"))), kindOf(Darklang.Cli.Workbench.resolveConflict(st, \"ok\")) ], \"\\n\")"
        + ""
        + ""
        + ""
        + ""

      let! answer =
        task {
          match! evalUnder (executionState state) code with
          | RT.DString lines -> return lines
          | other -> return Tests.failtestf "the workbench answered %A" other
        }

      let lines = answer.Split('\n')
      Expect.equal lines.Length 3 $"three answers:\n{answer}"

      for (line, text) in
        Array.zip
          lines
          [| "need an owner"; "no matches for"; "no conflict selected" |] do
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
              "<'a> = let shout(v: 'a): String" ]
        let! _ =
          runCli
            state
            [ "impl"
              "/Tests.WbImplEdit"
              "WbShout for Box = let shout(b: Box): String = \"box\"" ]

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
              "Tests.WbImplEdit.WbShout.shout(Tests.WbImplEdit.Box { wbImplEditMark = 1 })" ]
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
              "<'a> = let shout(v: 'a): String" ]
        let! _ =
          runCli
            state
            [ "impl"
              "/Tests.WbTraitEdit"
              "WbWave for Box = let shout(b: Box): String = \"box\"" ]

        let! saved =
          editInWorkbench
            state
            [ "Tests"; "WbTraitEdit" ]
            "WbWave"
            "let shout(v: 'a): String"
            "let shout(v: 'a): String\n  let wave(v: 'a): String"
        Expect.stringContains saved "footer: saved" "the edit saved"

        let! answer =
          runCli
            state
            [ "eval"
              "Tests.WbTraitEdit.WbWave.shout(Tests.WbTraitEdit.Box { wbTraitEditMark = 1 })" ]
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
              "Tests.WbTraitEdit.WbWave.wave(Tests.WbTraitEdit.Box { wbTraitEditMark = 1 })" ]
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
          runCli state [ "fn"; "/Tests.WbFnEdit.g"; "(x: Int64) : Int64 = x + 1" ]
        let! _ =
          runCli
            state
            [ "fn"; "/Tests.WbFnEdit.f"; "(x: Int64): Int64 = Tests.WbFnEdit.g(x)" ]

        let! saved =
          editInWorkbench state [ "Tests"; "WbFnEdit" ] "g" "x + 1" "x + 5"
        Expect.stringContains saved "footer: saved" "the edit saved"

        let! answer = runCli state [ "eval"; "Tests.WbFnEdit.f(1)" ]
        Expect.stringContains answer "6" "the caller runs the edited body"
      })


/// Missing old approval code must not interfere with the current function.
let private missingApprovalVersionDoesNotBlockCurrentCode =
  cliTest
    "an approval of a missing version does not block current code"
    (fun state ->
      task {
        let name = "Tests.HonestApproval.stale"
        let! _ = runCli state [ "fn"; "/" + name; "(x: Int64) : Int64 = x + 6143" ]
        let! approved, approvalStatus =
          runCliWithStatus
            state
            [ "permissions"; "approve"; name; "deny all"; "--yes" ]
        Expect.equal approvalStatus 0 approved

        // Move the approval to a version this store never held: what a store reset leaves behind.
        // CLI commands restore the logged-in account; the host state may be anonymous.
        let! viewer =
          evalUnder (executionState state) "(Darklang.Cli.initState()).accountID"
        let account =
          match viewer with
          | RT.DEnum(_, _, _, "Some", [ RT.DUuid id ]) -> Some id
          | RT.DEnum(_, _, _, "None", []) -> None
          | other -> failtestf "unexpected CLI account: %A" other
        let current = LibDB.PolicyStore.approvedVersion account name
        Expect.isSome current "the approval landed"
        let gone = String.replicate 64 "e"
        let moved =
          LibDB.PolicyStore.recordApproval
            account
            name
            (LibDB.PolicyStore.reviewToken (LibDB.PolicyStore.get ()))
            { hash = gone
              policy = LibExecution.Permissions.Policy.denyAll
              fingerprint = "missing-version"
              explicitRules = false }
        Expect.isOk moved "the approval now names a version the store does not have"

        let! ran, status = runCliWithStatus state [ "eval"; name + "(1)" ]
        let! _ = runCli state [ "permissions"; "unapprove"; name ]
        Expect.equal status 0 ran
        Expect.equal (ran.Trim()) "6144" "the current body runs"
      })


/// A policy file that cannot be trusted is a refusal naming the file, not an internal error.
///
/// It printed the sentence twice, once as `Internal error` with an F# frame and once as an
/// uncaught exception with a Dark call stack, and it named `~/.darklang/policy`, which is not
/// where the file is when the store lives somewhere else.
let private damagedPolicyIsARefusal =
  instanceTest
    "a damaged policy file refuses plainly, naming where it is"
    (fun target ->
      task {
        let dir =
          match target with
          | Instance i -> i.dir
          | InProcess _ -> Tests.failtestf "needs its own instance"
        let file = System.IO.Path.Combine(dir, "policy", "policies.bin")
        System.IO.File.WriteAllText(file, "not a policy file at all")

        let! said, exitCode =
          runCliWithExit
            target
            [ "permissions"; "allow"; "http"; "GET"; "https://example.com:443/" ]
        Expect.equal exitCode 1L $"it refuses:\n{said}"
        Expect.stringContains
          said
          "The policy file is damaged"
          $"says what is wrong:\n{said}"
        Expect.stringContains
          said
          file
          $"and names the file where it actually is:\n{said}"
        Expect.isFalse
          (said.Contains "Internal error")
          $"not as an internal error:\n{said}"
        Expect.isFalse
          (said.Contains "Call stack")
          $"and with no call stack:\n{said}"
      })


/// A rule ending in `/*` covers the paths under it, end to end: what `permissions allow` saves is
/// what a request is checked against.
///
/// `https://api.github.com:443/*` printed "policy updated" and was saved with the literal path
/// `/*`, which no request has, so the next GET to `/repos/darklang/dark` was refused anyway.
let private httpRuleWithTrailingStarMatches =
  instanceTest "an http rule ending in /* allows the paths under it" (fun target ->
    task {
      let dir =
        match target with
        | Instance i -> i.dir
        | InProcess _ -> Tests.failtestf "needs its own instance"

      let! said, exitCode =
        runCliWithExit
          target
          [ "permissions"; "allow"; "http"; "GET"; "https://api.github.com:443/*" ]
      Expect.equal exitCode 0L $"the rule is saved:\n{said}"

      let file = System.IO.Path.Combine(dir, "policy", "policies.bin")
      let policy =
        (LibDB.PolicyStore.fromBytes (System.IO.File.ReadAllBytes file)).instance
      let allows (url : string) =
        match LibExecution.Permissions.Request.http "GET" url with
        | Ok request -> LibExecution.Permissions.Policy.allows request policy
        | Error error -> Tests.failtestf "%s" error

      Expect.isTrue
        (allows "https://api.github.com:443/repos/darklang/dark")
        "the URL the agent fetched is allowed"
      Expect.isFalse
        (allows "https://example.com/repos/darklang/dark")
        "and another host is not"
    })


/// A doc-only edit through `dark module` lands, as it does through `dark fn`.
///
/// Docs are not part of the content hash, so an edit that changes only a `///` line leaves every
/// item on the hash it had, and the module path, which never wrote the `UpdateDoc` the single-item
/// commands do, saved nothing while printing the same "Defined" line as a real save. Two items, one
/// doc changed, because a module saves a batch and one item's words must not land on the other.
let private moduleDocOnlyEditLands =
  instanceTest
    "a doc-only edit through dark module lands on the item it was made to"
    (fun state ->
      task {
        let write (text : string) =
          let file = System.IO.Path.GetTempFileName()
          System.IO.File.WriteAllText(file, text)
          file
        let source (docA : string) =
          $"/// {docA}\nlet docA(x: Int64): Int64 = x + 7741\n\n/// The second item's words, untouched.\nlet docB(x: Int64): Int64 = x + 7742\n"

        let! _ =
          runCli
            state
            [ "module"; "/Tests.HonestDocs"; write (source "First wording.") ]
        let! saved =
          runCli
            state
            [ "module"; "/Tests.HonestDocs"; write (source "Second wording.") ]

        let! viewA = runCli state [ "view"; "Tests.HonestDocs.docA" ]
        Expect.stringContains
          viewA
          "Second wording."
          $"the edited doc landed:\n{viewA}\n{saved}"

        let! viewB = runCli state [ "view"; "Tests.HonestDocs.docB" ]
        Expect.stringContains
          viewB
          "The second item's words, untouched."
          $"and the other item kept its own:\n{viewB}"
        Expect.isFalse
          (viewB.Contains "Second wording.")
          $"rather than taking the first's:\n{viewB}"
      })


/// In the run order CliTraces.Tests.fs composes; sequencing lives there too.
/// An answer that is not UTF-8 is a no, not a crash. Only a terminal is asked, so `script` gives
/// `discard` one, with CI's "nobody is here" switches off.
let nonUtf8AnswerIsANo =
  CliInstance.instanceTest "an answer that is not UTF-8 is a no" (fun i ->
    task {
      let! (code, _, err) = CliInstance.runRaw i [ "val"; "Tests.Utf8Probe.x"; "1" ]
      Expect.equal code 0 $"the draft has something to drop: {err}"
      let! (code, out, err) =
        CliInstance.runRawWithBytes
          { i with cli = "/usr/bin/env" }
          [ "-u"
            "CI"
            "-u"
            "DARK_NO_INPUT"
            "script"
            "-qec"
            $"{i.cli} discard"
            "/dev/null" ]
          (Some [| 0xFFuy; byte '\n' |])
      Expect.equal code 1 $"a no exits 1: {out}{err}"
      Expect.stringContains out "Drop these? [y/N]" "it was asked"
      Expect.stringContains out "kept." "and kept the draft"
      let! status = CliInstance.run i [ "status" ]
      Expect.stringContains
        status
        "draft: 1 item changed"
        "the draft is still there"
    })


let tests : List<Test> =
  [ unwrapErrorsAreReadable
    nonUtf8AnswerIsANo
    missingApprovalVersionDoesNotBlockCurrentCode
    damagedPolicyIsARefusal
    httpRuleWithTrailingStarMatches
    moduleDocOnlyEditLands
    workbenchImplEditUpdatesInPlace
    workbenchTraitEditKeepsImplementations
    workbenchFnEditCarriesCallers
    workbenchRefusesImplOverTrait
    workbenchRefusalsNameTheProblem
    workbenchSaysUnchangedAndLands
    workbenchDocCommentDocuments
    workbenchRefusalsAreMarkedAsRefusals
    storedPermissionsAreIsolated
    reusesCompiledFunctions
    timeoutBoundsSynchronousWork
    timeoutPreservesCapture
    stderrIsCapturedInOrder
    stdinThatIsNotUtf8IsRefused
    processTitleSaysWhatRuns
    runStopsWhenStdoutCloses
    testHelpCommand
    workbenchViewsRender
    showingACommitDoesNotFetchEveryOp
    workbenchBranchActionsWork
    mergeAndRebaseRefuseOnMain
    permissionsRefusesAnEmptyRule
    permissionsAllowTakesSeveralRules
    viewHeadsWithTheNameYouAskedFor
    headerKeepsTheBranchWhenNarrow
    hintRowKeepsTheWayOut
    workbenchHandlesTerminalSizes
    noWorkbenchRowOverflowsItsFrame
    workbenchContextRowSaysWhereYouAre
    documentedCommandsAreReal ]
