/// The instance-local half of the CLI: the store's own settings, backups, scripts,
/// apps, permissions, and the commands that report on the op log.
///
/// These are the commands that talk about THIS install rather than about packages or
/// branches. Most of them had no test at all, and one of them was broken outright:
/// `dark backups now` died on an uncaught permission denial, because taking a copy
/// of the store went through the file builtins and the store path is guarded against
/// exactly that.
module Tests.CliWorkspace

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


// ─── this instance ────────────────────────────────────────────────────────

let versionAndStatusAnswer =
  instanceTest "version and install-status describe this install" (fun state ->
    task {
      do! sane state [ "version" ] "version says something about itself"
      do!
        shows
          state
          [ "install-status" ]
          "Version:"
          "install-status names the version it is"
    })

let configRoundTrips =
  instanceTest "config set is what config get reads back" (fun state ->
    task {
      do!
        shows
          state
          [ "config"; "set"; "tests.workspace"; "hello" ]
          "tests.workspace"
          "set says what it set"
      do!
        shows
          state
          [ "config"; "get"; "tests.workspace" ]
          "hello"
          "and get reads it back"
      do!
        shows
          state
          [ "config"; "get"; "tests.neverset" ]
          "not found"
          "an unset key is not an empty value"
    })

let scriptsRoundTrip =
  instanceTest "a script can be added, listed and read back" (fun state ->
    task {
      do! run state [ "scripts"; "add"; "tests-probe"; "1L + 2L" ]
      do! shows state [ "scripts"; "list" ] "tests-probe" "the listing names it"
      do!
        shows
          state
          [ "scripts"; "view"; "tests-probe" ]
          "1L + 2L"
          "and its content comes back unchanged"
      do!
        shows
          state
          [ "scripts"; "view"; "tests-nosuch" ]
          "tests-nosuch"
          "a script that isn't there names itself back"
    })

/// Regression: this crashed. `backups now` asked whether the store existed, that
/// question went through the guarded file API, and the denial is an uncaught runtime
/// error rather than a `false`. It goes through SQLite's online backup now, which is
/// also the only way to copy an open store.
let backupsCanBeTaken =
  instanceTest
    "backups now copies the store instead of dying on the guard"
    (fun state ->
      task {
        do! shows state [ "backups"; "now" ] "copied the store" "the copy is taken"
        do! shows state [ "backups"; "list" ] "manual-" "and the listing names it"
        // Clean up: the store is not small, and every run of this would leave another
        // copy.
        do! run state [ "backups"; "prune"; "0"; "-y" ]
        do!
          shows
            state
            [ "backups"; "list" ]
            "no backups"
            "and pruning to zero leaves none"
      })

let backupsRefusesToRestoreWhatIsNotThere =
  instanceTest "backups restore refuses a name it doesn't have" (fun state ->
    task {
      do!
        refuses
          state
          [ "backups"; "restore"; "nosuchbackup"; "-y" ]
          "no backup named"
          "restored"
          "restore names what it looked for"
    })

let appsCatalogLists =
  instanceTest "apps list-available names the catalog" (fun state ->
    task {
      do!
        shows
          state
          [ "apps"; "list-available" ]
          "Available apps:"
          "the catalog lists something"
    })

let appsInstalledLists =
  instanceTest "apps list answers on an instance with none" (fun state ->
    task { do! sane state [ "apps"; "list" ] "the installed list answers" })

let permissionsLists =
  instanceTest "permissions list names the effects the policy covers" (fun state ->
    task {
      do!
        shows
          state
          [ "permissions"; "list" ]
          "stdout"
          "the policy lists what it covers"
    })

/// An approved version is the point of the whole permissions surface: it says "when I call this
/// name, I mean the body I reviewed", and it has to hold when code actually RUNS.
///
/// This is what was missing until now. The approval was stored, listed and shown, and nothing
/// narrowed name resolution by it, so every run took the latest version regardless.
let anApprovedVersionIsWhatRuns =
  instanceTest
    "an approved version is what a run resolves, until it is withdrawn"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Appr.rate" "() : Int64 = 6101L"
        do! commit state "rate v1"
        do! run state [ "permissions"; "approve"; "Tests.Appr.rate"; "--yes" ]
        do!
          shows
            state
            [ "permissions"; "approved" ]
            "Tests.Appr.rate"
            "the approval is recorded against the name"

        // A newer version, published and live: the NAME now points at it for everything that has
        // not approved a version.
        do! fn state "Tests.Appr.rate" "() : Int64 = 6102L"
        do! commit state "rate v2"

        do!
          evals
            state
            "Tests.Appr.rate ()"
            "6101"
            "a run resolves the version this account approved, not the latest"

        do! run state [ "permissions"; "unapprove"; "Tests.Appr.rate" ]
        do!
          evals
            state
            "Tests.Appr.rate ()"
            "6102"
            "and follows the latest again once the approval is withdrawn"
        do! discardAll state
      })

/// Withdrawing what was never approved says so, rather than reporting a release that did not
/// happen.
let unapprovingAnUnapprovedNameSaysSo =
  instanceTest "withdrawing an approval nobody made says so" (fun state ->
    task {
      do!
        shows
          state
          [ "permissions"; "unapprove"; "Tests.Appr.neverApproved" ]
          "has no approved version"
          "an unmatched name withdraws nothing and says so"
    })


let dbAndTracesAnswer =
  instanceTest "db and traces answer without a canvas or a recording" (fun state ->
    task {
      do! sane state [ "db"; "list" ] "db list answers on an empty canvas"
      do! sane state [ "traces"; "stats" ] "traces stats answers"
    })


// ─── the log ──────────────────────────────────────────────────────────────

let opsAndCommitsDescribeTheLog =
  instanceTest "ops, commits and log describe the same log" (fun state ->
    task {
      do! start state
      do!
        showsAll
          state
          [ "ops" ]
          [ "ops on main"; "op" ]
          "ops lists the most recent ops"
      do! shows state [ "commits" ] "commit" "commits lists commits"
      do! shows state [ "log" ] "commit" "log is the same listing"
      do! shows state [ "history" ] "commit" "and so is history"
    })

let showTellsYouWhatACommitHolds =
  instanceTest
    "show explains a commit, and says so when there's nothing to show"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Show.f" "() : Int64 = 1L"
        do! commit state "a commit to show"
        do! shows state [ "log" ] "a commit to show" "the commit is in the log"
        do!
          shows
            state
            [ "show"; "deadbeef" ]
            "nothing matching"
            "and an id that matches nothing says so"
        do! discardAll state
      })

let constraintsAndConflictsReportQuiet =
  instanceTest "constraints and conflicts say nothing is pending" (fun state ->
    task {
      do! start state
      // Not "no constraints", and not "no conflicts": both are standing properties of the STORE,
      // and every CLI test shares one store, so whether any stand here depends on what ran before
      // -- the doc tests put a divergence in deliberately. What is worth pinning is that each
      // command answers, and that an id nobody has says so.
      do! sane state [ "constraints" ] "constraints answers"
      do! sane state [ "conflicts" ] "conflicts answers"
      do!
        shows
          state
          [ "ack"; "no-such-finding" ]
          "no constraint matching"
          "acking a finding that isn't there says so"
      do!
        shows
          state
          [ "resolve"; "bogus" ]
          "usage"
          "and resolve needs its three arguments"
    })

// ─── live: running things follow your edits ──────────────────────────────

// In this file rather than `HttpServer.Tests.fs` only because the CLI harness these need
// compiles after it; the listener helpers are borrowed from there.

module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution
open TestUtils.TestUtils
open System.Threading
open Prelude
open Fumble
open LibDB.Sqlite

let private getText (port : int) : Task<int * string> =
  task {
    use client = new System.Net.Http.HttpClient()
    let! response = client.GetAsync($"http://localhost:{port}/")
    let! body = response.Content.ReadAsStringAsync()
    return (int response.StatusCode, body)
  }

let private isNone (dv : RT.Dval) : bool =
  match dv with
  | RT.DEnum(_, _, _, "None", []) -> true
  | _ -> false

/// The Dark source for the test router's location.
let private routerLocation =
  "Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"Tests\"; modules = [\"LiveHttp\"]; name = \"router\" }"

/// The claim `dark serve` now makes: a saved edit is on the next request, a broken save is not.
///
/// In-process on purpose (`cliTest`): the listener and the author have to share one store, and the
/// diagnostic the routing step prints has to be capturable. Authoring goes through the real `fn`
/// command so propagation runs, which is what repoints the router at the edited callee.
let private serveFollowsEdits =
  cliTest "serve follows edits and keeps the last good version" (fun target ->
    task {
      let state = executionState target
      let author = author target

      do! author "Tests.LiveHttp.page" "(): String = \"one\""
      do!
        author
          "Tests.LiveHttp.router"
          "(req: Stdlib.Http.Request): Stdlib.Http.Response = Stdlib.Http.responseWithText (Tests.LiveHttp.page ()) 200"

      let! init =
        evalUnder
          state
          $"Darklang.Stdlib.Live.Router.start Darklang.SCM.Branch.mainBranchId ({routerLocation})"
      let! step = evalUnder state "Darklang.Stdlib.Live.Router.step"
      let step =
        match step with
        | RT.DApplicable a -> a
        | other -> failtest $"expected the step to be a fn, got {other}"

      let port = Tests.HttpServer.allocateFreePort ()
      let cts = new CancellationTokenSource()
      let! listener = Tests.HttpServer.bindListener port

      let listenerTask =
        Builtins.Http.Server.Libs.HttpServer.runListenerLive
          state
          listener
          (int64 port)
          init
          step
          false
          Builtins.Http.Server.Libs.HttpServer.defaultMaxBodyBytes
          false
          false
          false
          cts.Token

      try
        let! (status, body) = getText port
        Expect.equal (status, body) (200, "one") "the version at start"

        do! author "Tests.LiveHttp.page" "(): String = \"two\""
        let! (_, body) = getText port
        Expect.equal body "two" "an edit to a callee is on the next request"

        // A body that does not match the declared return type: the save lands (WIP is yours to
        // break), the router is repointed at it, and the check on what landed refuses it.
        let! watch =
          evalUnder
            state
            "Darklang.Stdlib.Live.watch Darklang.SCM.Branch.mainBranchId"
        do! author "Tests.LiveHttp.page" "(): String = 3"
        let! (status, body) = getText port
        Expect.equal
          (status, body)
          (200, "two")
          "a broken save keeps the last good version"

        // The diagnostic the server printed went to its own thread's stdout, out of this flow's
        // capture; ask the same question the routing step asked and check the words.
        let! polled = callByName state "Darklang.Stdlib.Live.poll" [ watch ]
        let change =
          match polled with
          | RT.DTuple(_, RT.DEnum(_, _, _, "Some", [ change ]), []) -> change
          | other ->
            failtest $"expected the broken save to be reported, got {other}"
        let! routerLoc = evalUnder state routerLocation
        let! why =
          callByName
            state
            "Darklang.Stdlib.Live.diagnose"
            [ RT.DUuid PT.BranchId.Main.Guid; change; routerLoc ]
        let why =
          match why with
          | RT.DEnum(_, _, _, "Some", [ RT.DString s ]) -> s
          | other -> failtest $"expected a diagnostic, got {other}"
        Expect.stringContains
          why
          "newest version not applied"
          "the diagnostic names what it kept"
        Expect.stringContains why "expected String, got Int" "and says why"

        do! author "Tests.LiveHttp.page" "(): String = \"three\""
        let! (_, body) = getText port
        Expect.equal body "three" "the fix is on the next request"

        do!
          author
            "Tests.LiveHttp.router"
            "(req: Stdlib.Http.Request): Stdlib.Http.Response = Stdlib.Http.responseWithText (Tests.LiveHttp.page ()) 201"
        let! (status, _) = getText port
        Expect.equal
          status
          201
          "an edit to the router itself is on the next request"
      finally
        cts.Cancel()
        try
          listenerTask.Wait 2000 |> ignore<bool>
        with _ ->
          ()
    })

/// `poll` names what landed and `affects` walks to what depends on it, on one store.
let private pollAndAffects =
  cliTest "poll reports an edit and affects reaches its dependents" (fun target ->
    task {
      let state = executionState target
      let author = author target

      do! author "Tests.LivePoll.leaf" "(): Int = 1"
      do! author "Tests.LivePoll.branch" "(): Int = (Tests.LivePoll.leaf ()) + 1"
      do! author "Tests.LivePoll.bystander" "(): Int = 7"

      let loc (name : string) : Task<RT.Dval> =
        evalUnder
          state
          $"Darklang.LanguageTools.ProgramTypes.PackageLocation {{ owner = \"Tests\"; modules = [\"LivePoll\"]; name = \"{name}\" }}"

      let! watch =
        evalUnder
          state
          "Darklang.Stdlib.Live.watch Darklang.SCM.Branch.mainBranchId"

      let! quiet = callByName state "Darklang.Stdlib.Live.poll" [ watch ]
      let watch, change =
        match quiet with
        | RT.DTuple(w, c, []) -> w, c
        | other -> failtest $"poll returned {other}"
      Expect.isTrue (isNone change) "a fresh watch has nothing to report"

      do! author "Tests.LivePoll.leaf" "(): Int = 2"

      let! polled = callByName state "Darklang.Stdlib.Live.poll" [ watch ]
      let change =
        match polled with
        | RT.DTuple(_, RT.DEnum(_, _, _, "Some", [ change ]), []) -> change
        | other -> failtest $"expected the edit to be reported, got {other}"

      let! touched = callByName state "Darklang.Stdlib.Live.touchedNames" [ change ]
      let names =
        match touched with
        | RT.DList(_, items) ->
          items
          |> List.map (fun d ->
            match d with
            | RT.DString s -> s
            | other -> string other)
        | other -> failtest $"touchedNames returned {other}"
      Expect.contains names "Tests.LivePoll.leaf" "the edited name is reported"
      Expect.isFalse
        (List.contains "Tests.LivePoll.bystander" names)
        "a name nothing touched is not"

      let! branch = loc "branch"
      let! bystander = loc "bystander"
      let! affectsBranch =
        callByName state "Darklang.Stdlib.Live.affects" [ change; branch ]
      let! affectsBystander =
        callByName state "Darklang.Stdlib.Live.affects" [ change; bystander ]
      Expect.equal affectsBranch (RT.DBool true) "the dependent is affected"
      Expect.equal affectsBystander (RT.DBool false) "the bystander is not"

      // And the walk itself, from a change that names only the leaf: propagation had already
      // repointed `branch`, so the poll above reports both; this is the transitive half on its own.
      let! leaf = loc "leaf"
      let! synthetic = callByName state "Darklang.Stdlib.Live.touchingOnly" [ leaf ]
      let! reached =
        callByName state "Darklang.Stdlib.Live.affects" [ synthetic; branch ]
      Expect.equal
        reached
        (RT.DBool true)
        "a dependent is reached through the edges"
    })


// ─── live: the tree and the host loop ────────────────────────────────────

let private plainRows (dv : RT.Dval) : List<string> =
  match dv with
  | RT.DList(_, rows) ->
    rows
    |> List.map (fun r ->
      match r with
      | RT.DString s -> s
      | other -> string other)
  | other -> failtest $"expected rows, got {other}"

/// The terminal and page renderers over one fixture tree, and a table at 0, 1 and many rows.
let private treeRendersTheSameEverywhere =
  cliTest "a Node tree paints to rows and writes as markup" (fun target ->
    task {
      let state = executionState target
      let tree =
        "Darklang.Stdlib.Cli.UI.Node.column [ Darklang.Stdlib.Cli.UI.Node.bold \"Stats\", Darklang.Stdlib.Cli.UI.Node.table [ \"module\", \"fns\" ] [ [ \"Stdlib\", \"247\" ], [ \"Cli\", \"89\" ] ], Darklang.Stdlib.Cli.UI.Node.band Darklang.Stdlib.Cli.UI.Node.Severity.Error \"boom\", Darklang.Stdlib.Cli.UI.Node.row [ Darklang.Stdlib.Cli.UI.Node.text \"a\", Darklang.Stdlib.Cli.UI.Node.Node.Button (\"go\", 1L) ] ]"
      let region =
        "Darklang.Stdlib.Cli.UI.Layout.Region { top = 1; left = 1; rows = 10; cols = 40 }"

      let! rows =
        evalUnder
          state
          $"Darklang.Stdlib.Cli.UI.Canvas.compose 40 8 (Darklang.Stdlib.Cli.UI.Node.toSpans ({tree}) ({region}) \"go\") |> Darklang.Stdlib.List.map (fun r -> Darklang.Stdlib.String.trimEnd (Darklang.Stdlib.Cli.Tui.Text.stripSgr r))"
      Expect.equal
        (plainRows rows)
        [ "Stats"
          "module  fns"
          "------  ---"
          "Stdlib  247"
          "Cli      89"
          " boom"
          "a [ go ]"
          "" ]
        "the terminal frame"

      let! html =
        evalUnder state $"Darklang.Stdlib.Cli.UI.Html.renderStatic ({tree})"
      let html =
        match html with
        | RT.DString s -> s
        | other -> failtest $"expected markup, got {other}"
      Expect.stringContains
        html
        "<th>module</th><th class=\"dark-num\">fns</th>"
        "the table's header, numeric column marked"
      Expect.stringContains
        html
        "<td>Stdlib</td><td class=\"dark-num\">247</td>"
        "a table row"
      Expect.stringContains html "dark-band-error\">boom</div>" "the band"
      Expect.stringContains html "<button type=\"submit\">go</button>" "the button"

      let tableRows (rowsSource : string) =
        evalUnder
          state
          $"Darklang.Stdlib.Cli.UI.Canvas.compose 20 5 (Darklang.Stdlib.Cli.UI.Node.toSpans (Darklang.Stdlib.Cli.UI.Node.table [ \"k\", \"v\" ] {rowsSource}) ({region}) \"\") |> Darklang.Stdlib.List.map (fun r -> Darklang.Stdlib.String.trimEnd (Darklang.Stdlib.Cli.Tui.Text.stripSgr r))"
      let! none = tableRows "[]"
      Expect.equal
        (List.take 3 (plainRows none))
        [ "k  v"; "-  -"; "" ]
        "a table with no rows is a header and a rule"
      let! one = tableRows "[ [ \"a\", \"1\" ] ]"
      Expect.equal
        (List.take 3 (plainRows one))
        [ "k  v"; "-  -"; "a  1" ]
        "one row"
      let! many = tableRows "[ [ \"a\", \"1\" ], [ \"bb\", \"22\" ] ]"
      Expect.equal
        (List.take 4 (plainRows many))
        [ "k    v"; "--  --"; "a    1"; "bb  22" ]
        "widths follow the widest cell; numbers right-align"
    })

/// Demo 1, driven a turn at a time: a key reaches the view; an edit from elsewhere is on the next
/// frame; a broken save keeps the frame and shows the diagnostic; the fix clears it. The model
/// (what was typed) survives every swap.
let private viewFollowsEdits =
  cliTest
    "a live view repaints on an edit and keeps the last good frame across a broken one"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        let run (name : string) (args : List<RT.Dval>) = callByName state name args

        do! author "Tests.LiveView.init" "(): Int = 0"
        do!
          author
            "Tests.LiveView.update"
            "(m: Int) (e: Darklang.Cli.Apps.Host.Event<Int>): Int = match e with | Key _ -> m + 1 | Msg n -> m + n"
        do!
          author
            "Tests.LiveView.render"
            "(m: Int): Stdlib.Cli.UI.Node.Node<Int> = Stdlib.Cli.UI.Node.column [ Stdlib.Cli.UI.Node.text \"version one\", Stdlib.Cli.UI.Node.text (\"keys: \" ++ Stdlib.Int.toString m), Stdlib.Cli.UI.Node.Node.Button (\"ten\", 10) ]"

        let view =
          "Darklang.Cli.Apps.Model.View { name = \"live\"; title = \"Live\"; init = \"Tests.LiveView.init\"; update = \"Tests.LiveView.update\"; render = \"Tests.LiveView.render\" }"
        let! prepared =
          evalUnder
            state
            $"Darklang.Cli.Apps.Host.prepare Darklang.SCM.Branch.mainBranchId ({view})"
        let session =
          match prepared with
          | RT.DEnum(_, _, _, "Ok", [ s ]) -> s
          | other -> failtest $"the view did not start: {other}"

        let size = "Darklang.Stdlib.Cli.Tui.Size { width = 40; height = 8 }"
        let! sizeDv = evalUnder state size
        let rowsOf (s : RT.Dval) =
          task {
            let! rows = run "Darklang.Cli.Apps.Host.plainRows" [ s; sizeDv ]
            return plainRows rows |> List.filter (fun r -> r <> "")
          }
        // The loop runs as a process; keys and store changes reach it through the scheduler's
        // queue, as they do in the CLI.
        let driver = loopDriver state
        let pushKey = pushKey driver
        let pushTick () = pushTick driver
        let step (s : RT.Dval) = stepOn driver "Darklang.Cli.Apps.Host.step" [ s ]

        let! first = rowsOf session
        Expect.contains first "version one" "the first frame is the view's init"
        Expect.contains first "keys: 0" "with the model at init"

        // A key goes to the view's update.
        do! pushKey "A" "a"
        let! session = step session
        let! afterKey = rowsOf session
        Expect.contains afterKey "keys: 1" "a key reached update"

        // Tab focuses the button, Enter presses it: the message reaches update.
        do! pushKey "Tab" ""
        let! session = step session
        do! pushKey "Enter" ""
        let! session = step session
        let! afterPress = rowsOf session
        Expect.contains afterPress "keys: 11" "the button's message reached update"

        // An edit from elsewhere: the next turn with nothing typed sees it.
        do!
          author
            "Tests.LiveView.render"
            "(m: Int): Stdlib.Cli.UI.Node.Node<Int> = Stdlib.Cli.UI.Node.column [ Stdlib.Cli.UI.Node.text \"version two\", Stdlib.Cli.UI.Node.text (\"keys: \" ++ Stdlib.Int.toString m) ]"
        pushTick ()
        let! session = step session
        let! afterEdit = rowsOf session
        Expect.contains afterEdit "version two" "the edit is on the next frame"
        Expect.contains afterEdit "keys: 11" "and the model survived the swap"
        Expect.contains
          afterEdit
          "updated: Tests.LiveView.render"
          "the toast names what moved"

        // A broken save: the frame stays, the diagnostic is under it.
        do!
          author
            "Tests.LiveView.render"
            "(m: Int): Stdlib.Cli.UI.Node.Node<Int> = Stdlib.Cli.UI.Node.column [ Stdlib.Cli.UI.Node.text 3 ]"
        pushTick ()
        let! session = step session
        let! afterBreak = rowsOf session
        Expect.contains afterBreak "version two" "the last good frame is still up"
        // The band wraps at the width and the rows are padded, so look at the words together.
        let words (rows : List<string>) =
          rows
          |> String.concat " "
          |> String.split " "
          |> List.filter ((<>) "")
          |> String.concat " "
        Expect.stringContains
          (words afterBreak)
          "newest version not applied"
          "with the diagnostic in a band"

        // The fix clears the band.
        do!
          author
            "Tests.LiveView.render"
            "(m: Int): Stdlib.Cli.UI.Node.Node<Int> = Stdlib.Cli.UI.Node.column [ Stdlib.Cli.UI.Node.text \"version three\", Stdlib.Cli.UI.Node.text (\"keys: \" ++ Stdlib.Int.toString m) ]"
        pushTick ()
        let! session = step session
        let! afterFix = rowsOf session
        Expect.contains afterFix "version three" "the fix is on the next frame"
        Expect.isFalse
          ((words afterFix).Contains "not applied")
          "and the band is gone"

        // Escape leaves.
        do! pushKey "Escape" ""
        let! session = step session
        match session with
        | RT.DRecord(_, _, _, fields) ->
          Expect.equal
            (Map.find "exiting" fields)
            (Some(RT.DBool true))
            "Escape ends the loop"
        | other -> failtest $"expected a session, got {other}"
      })


/// H5's second half: a model saved as a `val` comes back through `--resume`, and keeps the model
/// the view had rather than its init.
let private modelSavesAndResumes =
  cliTest "a view's model saves as a val and resumes from it" (fun target ->
    task {
      let state = executionState target
      let author = author target

      do! author "Tests.LiveSave.init" "(): Int = 0"
      do!
        author
          "Tests.LiveSave.update"
          "(m: Int) (e: Darklang.Cli.Apps.Host.Event<Int>): Int = match e with | Key _ -> m + 1 | Msg n -> m + n"
      do!
        author
          "Tests.LiveSave.render"
          "(m: Int): Stdlib.Cli.UI.Node.Node<Int> = Stdlib.Cli.UI.Node.text (\"keys: \" ++ Stdlib.Int.toString m)"

      let view =
        "Darklang.Cli.Apps.Model.View { name = \"s\"; title = \"S\"; init = \"Tests.LiveSave.init\"; update = \"Tests.LiveSave.update\"; render = \"Tests.LiveSave.render\" }"

      let! saved =
        evalUnder
          state
          $"Darklang.Cli.Apps.Host.snapshot Darklang.SCM.Branch.mainBranchId ({view}) 5"
      let name =
        match saved with
        | RT.DEnum(_, _, _, "Ok", [ RT.DString name ]) -> name
        | other -> failtest $"the snapshot did not save: {other}"
      Expect.stringStarts
        name
        "Tests.LiveSave.Sessions.s"
        "it lands under the view's Sessions module"

      let! resumed =
        evalUnder
          state
          $"Darklang.Cli.Apps.Host.prepareWith Darklang.SCM.Branch.mainBranchId ({view}) (Darklang.Stdlib.Option.Option.Some \"{name}\")"
      let session =
        match resumed with
        | RT.DEnum(_, _, _, "Ok", [ s ]) -> s
        | other -> failtest $"the view did not resume: {other}"
      let! sizeDv =
        evalUnder state "Darklang.Stdlib.Cli.Tui.Size { width = 40; height = 6 }"
      let! rows =
        callByName state "Darklang.Cli.Apps.Host.plainRows" [ session; sizeDv ]
      Expect.contains
        (plainRows rows)
        "keys: 5"
        "the resumed session shows the saved model, not init"

      let! missing =
        evalUnder
          state
          $"Darklang.Cli.Apps.Host.prepareWith Darklang.SCM.Branch.mainBranchId ({view}) (Darklang.Stdlib.Option.Option.Some \"Tests.LiveSave.Sessions.nope\")"
      match missing with
      | RT.DEnum(_, _, _, "Error", [ RT.DString why ]) ->
        Expect.stringContains
          why
          "no value named"
          "a missing snapshot is named, not a crash"
      | other -> failtest $"expected an error for a missing snapshot, got {other}"
    })


/// The window a live host must never observe: an op is in the log but not yet folded into
/// `locations`. Authoring inserts, folds, then marks applied in three steps; a poll that lands
/// between the first and the last used to take the op, resolve the name to the previous hash,
/// and never look again. Now the op is reported only once it is applied.
let private pollIgnoresAnOpUntilItIsApplied =
  cliTest
    "a poll between an op's insert and its fold reports nothing; the poll after reports it"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        do! author "Tests.LiveFold.leaf" "(): Int = 1"

        let! watch =
          evalUnder
            state
            "Darklang.Stdlib.Live.watch Darklang.SCM.Branch.mainBranchId"
        let! quiet = callByName state "Darklang.Stdlib.Live.poll" [ watch ]
        let watch =
          match quiet with
          | RT.DTuple(w, RT.DEnum(_, _, _, "None", []), []) -> w
          | other -> failtest $"a fresh watch reported something: {other}"

        // Phase 1 of a save, by hand: the op rows land, unapplied. This is what a poll mid-fold sees.
        // A fresh body per run: the log is content-addressed and the store outlives the run.
        let body = System.Random.Shared.Next(1_000, 1_000_000_000)
        let! ops =
          parsePackageOps $"module Tests.LiveFold\n\nlet leaf () : Int = {body}"
        let statements =
          ops
          |> List.map (fun op ->
            let opId = LibDB.Inserts.computeOpHash op
            let blob =
              LibSerialization.Binary.Serialization.PT.PackageOp.serialize opId op
            ("INSERT INTO package_ops (id, op_blob, applied, origin_ts) VALUES (@id, @op_blob, 0, @ts)",
             [ [ "id", Sql.uuid opId
                 "op_blob", Sql.bytes blob
                 "ts", Sql.string (LibDB.Inserts.nextOriginTs ()) ] ]))
        statements |> Sql.executeTransactionSync |> ignore<List<int>>

        let! midFold = callByName state "Darklang.Stdlib.Live.poll" [ watch ]
        let watch =
          match midFold with
          | RT.DTuple(w, RT.DEnum(_, _, _, "None", []), []) -> w
          | other -> failtest $"an op that is not folded yet was reported: {other}"

        // The fold, then the applied mark, as `insertAndApplyOps` does them.
        do! LibDB.PackageOpPlayback.applyOpsFrom "op" ops
        ops
        |> List.map (fun op ->
          ("UPDATE package_ops SET applied = 1 WHERE id = @id",
           [ [ "id", Sql.uuid (LibDB.Inserts.computeOpHash op) ] ]))
        |> Sql.executeTransactionSync
        |> ignore<List<int>>

        let! afterFold = callByName state "Darklang.Stdlib.Live.poll" [ watch ]
        let change =
          match afterFold with
          | RT.DTuple(_, RT.DEnum(_, _, _, "Some", [ change ]), []) -> change
          | other -> failtest $"the folded op was not reported: {other}"
        let! names = callByName state "Darklang.Stdlib.Live.touchedNames" [ change ]
        match names with
        | RT.DList(_, items) ->
          Expect.contains
            (items |> List.map string)
            (string (RT.DString "Tests.LiveFold.leaf"))
            "the op is reported once it is folded, and the name resolves to it"
        | other -> failtest $"touchedNames returned {other}"
      })


/// The callee-fix race: a callee is broken (its dependent has been repointed at it), then fixed,
/// and the poll lands after the fix but before propagation repoints the dependent again. The
/// dependent's newest version is still built against the broken callee; its own check passes;
/// it must not be adopted.
let private aFixedCalleeIsNotAdoptedThroughItsBrokenDependent =
  cliTest
    "a dependent still built against a broken callee is not adopted when the callee is fixed"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        let m = "LiveCallee"

        do! author $"Tests.{m}.page" "(): String = \"one\""
        do!
          author
            $"Tests.{m}.router"
            $"(req: Stdlib.Http.Request): Stdlib.Http.Response = Stdlib.Http.responseWithText (Tests.{m}.page ()) 200"

        let routerLoc =
          $"(Darklang.LanguageTools.ProgramTypes.PackageLocation {{ owner = \"Tests\"; modules = [\"{m}\"]; name = \"router\" }})"
        let! lg =
          evalUnder
            state
            $"Darklang.Stdlib.Live.refresh Darklang.SCM.Branch.mainBranchId [] (Darklang.Stdlib.Live.start {routerLoc})"
        let hashOf (lg : RT.Dval) =
          match lg with
          | RT.DRecord(_, _, _, fields) ->
            Map.tryFind "hash" fields
            |> Option.defaultWith (fun () -> failtest "a LastGood has a hash")
          | other -> failtest $"expected a LastGood, got {other}"
        let good = hashOf lg

        // The break, through the CLI, so propagation repoints the router at the broken page.
        let! watch =
          evalUnder
            state
            "Darklang.Stdlib.Live.watch Darklang.SCM.Branch.mainBranchId"
        do! author $"Tests.{m}.page" "(): String = 3"
        let! polled = callByName state "Darklang.Stdlib.Live.poll" [ watch ]
        let watch, change =
          match polled with
          | RT.DTuple(w, RT.DEnum(_, _, _, "Some", [ c ]), []) -> w, c
          | other -> failtest $"the break was not reported: {other}"
        let opsOf (change : RT.Dval) =
          match change with
          | RT.DRecord(_, _, _, fields) ->
            Map.tryFind "ops" fields
            |> Option.defaultWith (fun () -> failtest "a Change has ops")
          | other -> failtest $"expected a Change, got {other}"
        let! lg =
          callByName
            state
            "Darklang.Stdlib.Live.refresh"
            [ RT.DUuid PT.BranchId.Main.Guid; opsOf change; lg ]
        Expect.equal
          (hashOf lg)
          good
          "the break keeps the router on its last good version"

        // The fix, WITHOUT propagation: the router's newest version still calls the broken page.
        let body = System.Random.Shared.Next(1_000, 1_000_000_000)
        let! _ =
          authorIntoMain
            $"module Tests.{m}\n\nlet page () : String = \"fixed {body}\""
        let! polled = callByName state "Darklang.Stdlib.Live.poll" [ watch ]
        let change =
          match polled with
          | RT.DTuple(_, RT.DEnum(_, _, _, "Some", [ c ]), []) -> c
          | other -> failtest $"the fix was not reported: {other}"
        let! lg =
          callByName
            state
            "Darklang.Stdlib.Live.refresh"
            [ RT.DUuid PT.BranchId.Main.Guid; opsOf change; lg ]
        Expect.equal
          (hashOf lg)
          good
          "the router's version built against the broken page is not adopted; the last good one stays"
        let! why = callByName state "Darklang.Stdlib.Live.diagnostic" [ lg ]
        match why with
        | RT.DEnum(_, _, _, "Some", [ RT.DString s ]) ->
          Expect.stringContains
            s
            "expected String, got Int"
            "and the reason names the broken callee's error"
        | other -> failtest $"expected a diagnostic, got {other}"
      })

/// Under `--dev`, a handler that fails at run time answers a page that carries the reload
/// listener, so the tab recovers when the edit that fixes it lands.
let private devErrorPageCarriesTheListener =
  cliTest
    "a serve --dev error page still carries the /__live listener"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        do!
          author
            "Tests.LiveDev.router"
            "(req: Stdlib.Http.Request): Stdlib.Http.Response = Stdlib.Http.responseWithText (Stdlib.Int.toString (Stdlib.Int.divide 1 0)) 200"
        let routerLoc =
          "Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"Tests\"; modules = [\"LiveDev\"]; name = \"router\" }"
        let! init =
          evalUnder
            state
            $"Darklang.Stdlib.Live.Router.start Darklang.SCM.Branch.mainBranchId ({routerLoc})"
        let! step = evalUnder state "Darklang.Stdlib.Live.Router.step"
        let step =
          match step with
          | RT.DApplicable a -> a
          | other -> failtest $"expected the step to be a fn, got {other}"
        let port = Tests.HttpServer.allocateFreePort ()
        let cts = new CancellationTokenSource()
        let! listener = Tests.HttpServer.bindListener port
        let listenerTask =
          Builtins.Http.Server.Libs.HttpServer.runListenerLive
            state
            listener
            (int64 port)
            init
            step
            true
            Builtins.Http.Server.Libs.HttpServer.defaultMaxBodyBytes
            false
            false
            false
            cts.Token
        try
          use client = new System.Net.Http.HttpClient()
          let! response = client.GetAsync($"http://localhost:{port}/")
          let! body = response.Content.ReadAsStringAsync()
          Expect.equal (int response.StatusCode) 500 "the handler failed"
          Expect.stringContains
            (string response.Content.Headers.ContentType)
            "text/html"
            "the failure is a page"
          Expect.stringContains body "/__live" "and the page carries the listener"
          Expect.stringContains body "error" "with the error on it"
        finally
          cts.Cancel()
          try
            listenerTask.Wait 2000 |> ignore<bool>
          with _ ->
            ()
      })


let tests : List<Test> =
  [ versionAndStatusAnswer
    configRoundTrips
    scriptsRoundTrip
    backupsCanBeTaken
    backupsRefusesToRestoreWhatIsNotThere
    appsCatalogLists
    appsInstalledLists
    permissionsLists
    anApprovedVersionIsWhatRuns
    unapprovingAnUnapprovedNameSaysSo
    dbAndTracesAnswer
    opsAndCommitsDescribeTheLog
    showTellsYouWhatACommitHolds
    constraintsAndConflictsReportQuiet
    testSequenced (
      testList
        "live"
        [ serveFollowsEdits
          pollAndAffects
          treeRendersTheSameEverywhere
          viewFollowsEdits
          modelSavesAndResumes
          pollIgnoresAnOpUntilItIsApplied
          aFixedCalleeIsNotAdoptedThroughItsBrokenDependent
          devErrorPageCarriesTheListener ]
    ) ]
