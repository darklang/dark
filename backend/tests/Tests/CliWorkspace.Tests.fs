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

/// The sqlite builtins declared nothing, so an approval of a function using them installed an
/// empty policy, and the body's own check then asked that policy for package-read: the function
/// could not run, and re-approving installed the same empty policy again.
let approvedSqliteOnTheStoreRuns =
  instanceTest
    "an approved function querying the store with sqlite runs"
    (fun state ->
      task {
        do!
          shows
            state
            [ "permissions"; "requirements"; "Darklang.Stdlib.Sqlite.query" ]
            "native"
            "arbitrary SQL can reach any file, so the worst case is declared"
        do!
          fn
            state
            "Tests.SqliteAppr.probe"
            "() : Bool = Stdlib.Result.isOk (Stdlib.Sqlite.query (Stdlib.LocalStore.path ()) \"select 1 as x\")"
        do!
          run state [ "permissions"; "approve"; "Tests.SqliteAppr.probe"; "--yes" ]
        do!
          evals
            state
            "Tests.SqliteAppr.probe ()"
            "true"
            "the approval covers what the body asks for"
        do! run state [ "permissions"; "unapprove"; "Tests.SqliteAppr.probe" ]
        do! discardAll state
      })
/// A dependency approved as part of a root's closure has no approved name, and it used to be
/// listed as a bare hash. That listing is where the stale-approvals warning sends people, so a
/// row nobody can identify was most of the problem.
let approvalsNameTheirDependencies =
  instanceTest "permissions approvals names the dependencies it lists" (fun state ->
    task {
      do!
        fn
          state
          "Tests.DepName.measure"
          "() : Int64 = Stdlib.String.length \"probe\""
      do! run state [ "permissions"; "approve"; "Tests.DepName.measure"; "--yes" ]
      do!
        shows
          state
          [ "permissions"; "approvals" ]
          "Darklang.Stdlib.String.length (dependency)"
          "a closure member is shown by its store name"
      do! run state [ "permissions"; "unapprove"; "Tests.DepName.measure" ]
      do! discardAll state
    })

/// Three store reads used to declare no effect, so `requirements` answered "effect-free" for
/// every Dark function that reached the store through them. Each wrapper is the one Dark caller
/// of its builtin, so asking about the wrapper is asking about the declaration.
let storeReadsRequirePackageRead =
  instanceTest
    "permissions requirements names package-read for the store reads"
    (fun state ->
      task {
        for name in
          [ "Darklang.LanguageTools.PackageManager.ownerHasItems"
            "Darklang.LanguageTools.PackageManager.resolveTraitCalls"
            "Darklang.SCM.PackageOps.getCommitNamedOps" ] do
          do!
            shows
              state
              [ "permissions"; "requirements"; name ]
              "package-read"
              $"{name} reads the package store"
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
          "needs 3 arguments"
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

/// The Dark source for a package location under `Tests`.
let private locSource (modules : List<string>) (name : string) : string =
  let mods = modules |> List.map (fun m -> $"\"{m}\"") |> String.concat "; "
  $"Darklang.LanguageTools.ProgramTypes.PackageLocation {{ owner = \"Tests\"; modules = [{mods}]; name = \"{name}\" }}"

/// A live server (`serve` without the command) for the router at <param routerLoc>, resolved
/// on <param branchSource> (Dark source for the branch id), with the browser half (what
/// `--live` turns on at the command line) when <param dev>:
/// the port is handed to <param body>, and the listener is stopped after it.
let private withLiveServer
  (state : RT.ExecutionState)
  (branchSource : string)
  (routerLoc : string)
  (dev : bool)
  (expectedOutput : List<string>)
  (body : int -> Task<unit>)
  : Task<unit> =
  task {
    let! init =
      evalUnder
        state
        $"Darklang.Stdlib.Live.Router.start {branchSource} ({routerLoc})"
    let! step = evalUnder state "Darklang.Stdlib.Live.Router.step"
    let step =
      match step with
      | RT.DApplicable a -> a
      | other -> failtest $"expected the step to be a fn, got {other}"
    let cts = new CancellationTokenSource()
    let! port, listener = Tests.HttpServer.bindFreshListener ()
    let mutable bodyPassed = false
    let listenerTask =
      task {
        // The server owns this capture. The test driver still opens independent
        // command captures when it authors edits through runCli.
        use output = new OutputCapture()
        do!
          Builtins.Http.Server.Libs.HttpServer.runListenerLive
            state
            listener
            (int64 port)
            init
            step
            dev
            Builtins.Http.Server.Libs.HttpServer.defaultMaxBodyBytes
            false
            false
            false
            cts.Token
        if bodyPassed then
          output.Check(fun stdout stderr ->
            let expected = expectedOutput |> List.map (fun s -> s + "\n")
            Expect.equal stdout (String.concat "" expected) "server diagnostics"
            Expect.equal stderr "" "no other server diagnostics")
      }
    try
      do! body port
      bodyPassed <- true
    finally
      cts.Cancel()
      Expect.isTrue (listenerTask.Wait 2000) "the live listener stopped"
  }

/// The Dark source for the test router's location.
let private routerLocation = locSource [ "LiveHttp" ] "router"

/// The claim `dark serve` makes: a saved edit is on the next request, a broken save is not.
///
/// In-process on purpose (`cliTest`): the listener and the author have to share one store, and the
/// diagnostic the routing step prints has to be capturable. Authoring goes through the real `fn`
/// command so propagation runs, which is what repoints the router at the edited callee.
/// The case this was built for: a request comes in, and afterwards you can stare at the handler
/// and see what that request did to every line of it.
///
/// A served request's input is a record, not source, so there is nothing to re-run the way an
/// `eval` is re-run. The row carries the handler that served it, and the preview applies that
/// to the recorded request with the same substitution.
let private previewOfAServedRequest =
  cliTest
    "a served request can be viewed against the handler that served it"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        // `cliTest` leaves the suite default, which is `off`; `cliTestWithFreshTraces` is the
        // harness that sets a rung. This test is about what a recorded trace can show, so it
        // needs one.
        LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.On
        try
          do! author "Tests.PrevHttp.page" "(): String = \"hello\""
          do!
            author
              "Tests.PrevHttp.router"
              "(req: Stdlib.Http.Request): Stdlib.Http.Response =\n  let body = Tests.PrevHttp.page ()\n  Stdlib.Http.responseWithText body 200"

          do!
            withLiveServer
              state
              "Darklang.SCM.Branch.mainBranchId"
              (locSource [ "PrevHttp" ] "router")
              false
              []
              (fun port ->
                task {
                  let! (status, body) = getText port
                  Expect.equal
                    (status, body)
                    (200, "hello")
                    "the request was served"

                  // The request is a run, and it knows what served it.
                  let! listed =
                    runCli target [ "traces"; "calls"; "Tests.PrevHttp.router" ]
                  Expect.stringContains
                    listed
                    "GET /"
                    "the request is listed under the handler"

                  let! viewed =
                    runCli target [ "traces"; "show"; "Tests.PrevHttp.router" ]
                  Expect.stringContains
                    viewed
                    "page () // = \"hello\""
                    "the handler's own call, with the value that request produced"
                })
        finally
          LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
      })


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

      do!
        withLiveServer
          state
          "Darklang.SCM.Branch.mainBranchId"
          routerLocation
          false
          [ "[live] now on the new version of Tests.LiveHttp.router"
            "[live] Tests.LiveHttp.router: still on the last good version; "
            + "the newest has a type error: expected String, got Int (return value)"
            "[live] now on the new version of Tests.LiveHttp.router"
            "[live] now on the new version of Tests.LiveHttp.router" ]
          (fun port ->
            task {
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

              // The fixture checks the actual server diagnostic after shutdown.
              // The direct diagnosis API should explain the same rejected save.
              let! _ =
                pollChange state watch "expected the broken save to be reported"
              let! routerLoc = evalUnder state routerLocation
              let! why =
                callByName
                  state
                  "Darklang.Stdlib.Live.diagnose"
                  [ RT.DUuid PT.BranchId.Main.Guid; routerLoc ]
              let why =
                match why with
                | RT.DEnum(_, _, _, "Some", [ RT.DString s ]) -> s
                | other -> failtest $"expected a diagnostic, got {other}"
              Expect.stringContains
                why
                "still on the last good version"
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
            })
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
        evalUnder state (locSource [ "LivePoll" ] name)

      let! watch =
        evalUnder
          state
          "Darklang.Stdlib.Live.watch Darklang.SCM.Branch.mainBranchId"

      // A fresh watch has nothing to report.
      let! watch = pollQuiet state watch

      do! author "Tests.LivePoll.leaf" "(): Int = 2"

      let! (_, change) = pollChange state watch "expected the edit to be reported"

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
            "(m: Int): Stdlib.Cli.UI.Node.Node<Int> = Stdlib.Cli.UI.Node.column [ Stdlib.Cli.UI.Node.text \"version one\", Stdlib.Cli.UI.Node.text (\"keys: \" + Stdlib.toString m), Stdlib.Cli.UI.Node.Node.Button (\"ten\", 10) ]"

        let view =
          "Darklang.Cli.Apps.Model.View { name = \"live\"; title = \"Live\"; init = \"Tests.LiveView.init\"; update = \"Tests.LiveView.update\"; render = \"Tests.LiveView.render\"; every = Stdlib.Option.Option.None; keys = Stdlib.Option.Option.None }"
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
            "(m: Int): Stdlib.Cli.UI.Node.Node<Int> = Stdlib.Cli.UI.Node.column [ Stdlib.Cli.UI.Node.text \"version two\", Stdlib.Cli.UI.Node.text (\"keys: \" + Stdlib.toString m) ]"
        pushTick ()
        let! session = step session
        let! afterEdit = rowsOf session
        Expect.contains afterEdit "version two" "the edit is on the next frame"
        Expect.contains afterEdit "keys: 11" "and the model survived the swap"
        Expect.contains
          afterEdit
          "changed: Tests.LiveView.render"
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
          "still on the last good version"
          "with the diagnostic in a band"

        // The fix clears the band.
        do!
          author
            "Tests.LiveView.render"
            "(m: Int): Stdlib.Cli.UI.Node.Node<Int> = Stdlib.Cli.UI.Node.column [ Stdlib.Cli.UI.Node.text \"version three\", Stdlib.Cli.UI.Node.text (\"keys: \" + Stdlib.toString m) ]"
        pushTick ()
        let! session = step session
        let! afterFix = rowsOf session
        Expect.contains afterFix "version three" "the fix is on the next frame"
        Expect.isFalse
          ((words afterFix).Contains "last good version")
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


/// `serve --branch`: the same as above, with the router and its callee authored on a branch.
/// The branch's ops are inert (never `applied`), so a watch on the branch must see them.
let private serveFollowsEditsOnABranch =
  cliTest "serve --branch follows edits made on the branch" (fun target ->
    task {
      let state = executionState target
      let author = author target
      let! _ = runCli target [ "branch"; "create"; "live-serve" ]
      try
        do! author "Tests.LiveBranchHttp.page" "(): String = \"one\""
        do!
          author
            "Tests.LiveBranchHttp.router"
            "(req: Stdlib.Http.Request): Stdlib.Http.Response = Stdlib.Http.responseWithText (Tests.LiveBranchHttp.page ()) 200"
        do!
          withLiveServer
            state
            "(Darklang.SCM.PackageOps.currentBranch ())"
            (locSource [ "LiveBranchHttp" ] "router")
            false
            [ "[live] now on the new version of Tests.LiveBranchHttp.router" ]
            (fun port ->
              task {
                let! (status, body) = getText port
                Expect.equal (status, body) (200, "one") "the version at start"
                do! author "Tests.LiveBranchHttp.page" "(): String = \"two\""
                let! (_, body) = getText port
                Expect.equal
                  body
                  "two"
                  "an edit on the branch is on the next request"
              })
      finally
        (archiveBranches target [ "live-serve" ]).Wait()
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
          "(m: Int): Stdlib.Cli.UI.Node.Node<Int> = Stdlib.Cli.UI.Node.text (\"keys: \" + Stdlib.toString m)"

      let view =
        "Darklang.Cli.Apps.Model.View { name = \"s\"; title = \"S\"; init = \"Tests.LiveSave.init\"; update = \"Tests.LiveSave.update\"; render = \"Tests.LiveSave.render\"; every = Stdlib.Option.Option.None; keys = Stdlib.Option.Option.None }"

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
          $"Darklang.Cli.Apps.Host.prepareWith Darklang.SCM.Branch.mainBranchId ({view}) (Darklang.Stdlib.Option.Option.Some \"{name}\") false"
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
          $"Darklang.Cli.Apps.Host.prepareWith Darklang.SCM.Branch.mainBranchId ({view}) (Darklang.Stdlib.Option.Option.Some \"Tests.LiveSave.Sessions.nope\") false"
      match missing with
      | RT.DEnum(_, _, _, "Error", [ RT.DString why ]) ->
        Expect.stringContains
          why
          "no value named"
          "a missing snapshot is named, not a crash"
      | other -> failtest $"expected an error for a missing snapshot, got {other}"
    })


/// The window a live host must never observe: an op is in the log but not yet folded into
/// `locations`. An op is reported only once it is applied. Authoring inserts, folds, then marks
/// applied in three steps; a poll that took the op between the insert and the fold would resolve
/// the name to the previous hash and never look again.
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
        let! watch = pollQuiet state watch

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

        let! watch = pollQuiet state watch

        // The fold, then the applied mark, as `insertAndApplyOps` does them.
        do! LibDB.PackageOpPlayback.applyOpsFrom "op" ops
        ops
        |> List.map (fun op ->
          ("UPDATE package_ops SET applied = 1 WHERE id = @id",
           [ [ "id", Sql.uuid (LibDB.Inserts.computeOpHash op) ] ]))
        |> Sql.executeTransactionSync
        |> ignore<List<int>>

        let! (_, change) = pollChange state watch "the folded op was not reported"
        let! names = callByName state "Darklang.Stdlib.Live.touchedNames" [ change ]
        match names with
        | RT.DList(_, items) ->
          Expect.contains
            (items |> List.map string)
            (string (RT.DString "Tests.LiveFold.leaf"))
            "the op is reported once it is folded, and the name resolves to it"
        | other -> failtest $"touchedNames returned {other}"
      })


/// A branch's own ops are never `applied`: they are stored inert and tagged in one transaction,
/// so the applied-only rule for main (above) must not hide them from a watch on the branch.
let private pollOnABranchSeesTheBranchsOwnSaves =
  cliTest "a poll on a branch reports the branch's own saves" (fun target ->
    task {
      let state = executionState target
      let author = author target
      let! _ = runCli target [ "branch"; "create"; "live-poll" ]
      try
        let! watch =
          evalUnder
            state
            "Darklang.Stdlib.Live.watch (Darklang.SCM.PackageOps.currentBranch ())"
        do! author "Tests.LiveBranchPoll.leaf" "(): Int = 1"
        let! (_, change) = pollChange state watch "the branch save was not reported"
        let! names = callByName state "Darklang.Stdlib.Live.touchedNames" [ change ]
        match names with
        | RT.DList(_, items) ->
          Expect.contains
            (items |> List.map string)
            (string (RT.DString "Tests.LiveBranchPoll.leaf"))
            "the branch's save is reported by name"
        | other -> failtest $"touchedNames returned {other}"
      finally
        (archiveBranches target [ "live-poll" ]).Wait()
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

        let routerLoc = "(" + locSource [ m ] "router" + ")"
        let! lg =
          evalUnder
            state
            $"Darklang.Stdlib.Live.refresh Darklang.SCM.Branch.mainBranchId (Darklang.Stdlib.Live.start {routerLoc})"
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
        let! (watch, _) = pollChange state watch "the break was not reported"
        let! lg =
          callByName
            state
            "Darklang.Stdlib.Live.refresh"
            [ RT.DUuid PT.BranchId.Main.Guid; lg ]
        Expect.equal
          (hashOf lg)
          good
          "the break keeps the router on its last good version"

        // The fix, WITHOUT propagation: the router's newest version still calls the broken page.
        let body = System.Random.Shared.Next(1_000, 1_000_000_000)
        let! _ =
          authorIntoMain
            $"module Tests.{m}\n\nlet page () : String = \"fixed {body}\""
        let! _ = pollChange state watch "the fix was not reported"
        let! lg =
          callByName
            state
            "Darklang.Stdlib.Live.refresh"
            [ RT.DUuid PT.BranchId.Main.Guid; lg ]
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

/// The `--live` stream: a page's listener says which version served it, and an edit that lands
/// after the page was served (even before the stream opened) is reported to it.
let private devStreamReportsAnEditAfterTheServe =
  cliTest
    "a serve --live page is told to reload for an edit made after it was served"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        do! author "Tests.LiveStream.page" "(): String = \"one\""
        do!
          author
            "Tests.LiveStream.router"
            "(req: Stdlib.Http.Request): Stdlib.Http.Response = Stdlib.Http.responseWithHtml (Tests.LiveStream.page ()) 200"
        do!
          withLiveServer
            state
            "Darklang.SCM.Branch.mainBranchId"
            (locSource [ "LiveStream" ] "router")
            true
            [ "[live] now on the new version of Tests.LiveStream.router"
              "[live] page told to reload" ]
            (fun port ->
              task {
                use client = new System.Net.Http.HttpClient()
                let! page = client.GetStringAsync($"http://localhost:{port}/")
                // The page carries its own version, so the edit below is reported even though it
                // lands before the stream opens.
                Expect.stringContains
                  page
                  "/__live?from="
                  "the listener says which version"
                let from =
                  let marker = "/__live?from="
                  let start = page.IndexOf marker + marker.Length
                  page.Substring(start, page.IndexOf("'", start) - start)
                do! author "Tests.LiveStream.page" "(): String = \"two\""
                use cts = new CancellationTokenSource(20_000)
                let! stream =
                  client.GetStreamAsync(
                    $"http://localhost:{port}/__live?from={from}",
                    cts.Token
                  )
                use reader = new System.IO.StreamReader(stream)
                let mutable said = ""
                while said = "" && not cts.IsCancellationRequested do
                  let! line = reader.ReadLineAsync()
                  if line <> null && line.StartsWith "data:" then said <- line
                Expect.equal
                  said
                  "data: reload"
                  "the stream told the page to reload"
                // EOF means the server finished the event, including its diagnostic.
                let! _ = reader.ReadToEndAsync(cts.Token)
                return ()
              })
      })


/// Under `--live`, a handler that fails at run time answers a page that carries the reload
/// listener, so the tab recovers when the edit that fixes it lands.
let private devErrorPageCarriesTheListener =
  cliTest
    "a serve --live error page still carries the /__live listener"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        do!
          author
            "Tests.LiveDev.router"
            "(req: Stdlib.Http.Request): Stdlib.Http.Response = Stdlib.Http.responseWithText (Stdlib.toString (1 / 0)) 200"
        do!
          withLiveServer
            state
            "Darklang.SCM.Branch.mainBranchId"
            (locSource [ "LiveDev" ] "router")
            true
            [ "[HttpServer] the handler failed: Cannot divide by 0" ]
            (fun port ->
              task {
                use client = new System.Net.Http.HttpClient()
                let! response = client.GetAsync($"http://localhost:{port}/")
                let! body = response.Content.ReadAsStringAsync()
                Expect.equal (int response.StatusCode) 500 "the handler failed"
                Expect.stringContains
                  (string response.Content.Headers.ContentType)
                  "text/html"
                  "the failure is a page"
                Expect.stringContains
                  body
                  "/__live"
                  "and the page carries the listener"
                Expect.stringContains
                  body
                  "The handler failed"
                  "with the error on it"
              })
      })

/// The Dark source for an annotated print of a function: its live values, one per call at a
/// line position, as `// = value` after the code.
let private annotatedPrint (owner : string) (modul : string) (name : string) =
  $"""let loc = Darklang.LanguageTools.ProgramTypes.PackageLocation {{ owner = "{owner}"; modules = ["{modul}"]; name = "{name}" }}
let bid = Darklang.SCM.Branch.mainBranchId
let values =
  match Darklang.Stdlib.Live.Values.replay (Darklang.Cli.initState ()).accountID bid loc with
  | Some v -> v.byExpr |> Darklang.Stdlib.Dict.map (fun _ d -> Darklang.PrettyPrinter.RuntimeTypes.dval bid d)
  | None -> Darklang.Stdlib.Dict.empty
let base = Darklang.PrettyPrinter.ProgramTypes.Context.forModule bid ["{owner}", "{modul}"]
let ctx = {{ base with liveValues = values }}
match Darklang.LanguageTools.PackageManager.Function.find bid loc with
| Some hash ->
  match Darklang.LanguageTools.PackageManager.Function.get hash with
  | Some fn -> Darklang.PrettyPrinter.ProgramTypes.packageFn ctx fn
  | None -> "no fn"
| None -> "no hash"
"""

/// The Dark source for two prints of one function under the EDITOR's context: the plain one and
/// the annotated one, joined by a marker. The LSP finds its hints by zipping these line for line,
/// so they must have the same number of lines whatever the code is.
let private plainAndAnnotated (owner : string) (modul : string) (name : string) =
  $"""let loc = Darklang.LanguageTools.ProgramTypes.PackageLocation {{ owner = "{owner}"; modules = ["{modul}"]; name = "{name}" }}
let bid = Darklang.SCM.Branch.mainBranchId
let values =
  match Darklang.Stdlib.Live.Values.replay (Darklang.Cli.initState ()).accountID bid loc with
  | Some v -> v.byExpr |> Darklang.Stdlib.Dict.map (fun _ d -> Darklang.PrettyPrinter.RuntimeTypes.dval bid d)
  | None -> Darklang.Stdlib.Dict.empty
let ctx = Darklang.PrettyPrinter.ProgramTypes.Context.forBranch bid
match Darklang.LanguageTools.PackageManager.Function.find bid loc with
| Some hash ->
  match Darklang.LanguageTools.PackageManager.Function.get hash with
  | Some fn ->
    (Darklang.PrettyPrinter.ProgramTypes.packageFn ctx fn)
    + "@@@"
    + (Darklang.PrettyPrinter.ProgramTypes.packageFn {{ ctx with liveValues = values }} fn)
  | None -> "no fn"
| None -> "no hash"
"""


/// A stage's `// = value` is a comment, so nothing the enclosing expression prints after it on the
/// same line may follow it: a `)` or `,` there would be commented out, and the source shown could
/// not be pasted back.
let private pipeStageValuesStayOutsideTheirDelimiters =
  cliTestWithFreshTraces
    "a pipe's stage values sit after the delimiter that closes it, not around it"
    (fun target ->
      task {
        let author = author target
        do!
          author
            "Tests.PipeVals.asArg"
            "(xs: List<Int>): String =\n  Stdlib.toString (xs |> Stdlib.List.map (fun x -> x + 1) |> Stdlib.List.length)"
        do!
          author
            "Tests.PipeVals.inTuple"
            "(xs: List<Int>): (Int * Int) =\n  (xs |> Stdlib.List.map (fun x -> x + 1) |> Stdlib.List.length, 7)"
        do!
          author
            "Tests.PipeVals.inList"
            "(xs: List<Int>): List<Int> =\n  [ xs |> Stdlib.List.map (fun x -> x + 1) |> Stdlib.List.length ]"

        for fn in [ "asArg"; "inTuple"; "inList" ] do
          let! _ = runCli target [ "eval"; $"Tests.PipeVals.{fn} [1, 2]" ]
          ()

        let! asArg = runCli target [ "traces"; "show"; "Tests.PipeVals.asArg" ]
        Expect.stringContains
          asArg
          "|> Stdlib.List.length) // = 2"
          "the paren closes before the value"
        Expect.isFalse (asArg.Contains "// = 2)") "and is not inside the comment"

        let! inTuple = runCli target [ "traces"; "show"; "Tests.PipeVals.inTuple" ]
        Expect.stringContains
          inTuple
          "|> Stdlib.List.length, // = 2"
          "the comma comes before the value"
        Expect.isFalse (inTuple.Contains "// = 2,") "and is not inside the comment"

        // Already right before the change, since the bracket goes on a line of its own: kept so
        // it stays right.
        let! inList = runCli target [ "traces"; "show"; "Tests.PipeVals.inList" ]
        Expect.stringContains
          inList
          "|> Stdlib.List.length // = 2"
          "the last stage keeps its value"
      })


/// Live values: a function's last recorded call, run again through the code as it is NOW, with
/// the value of every call inside it put beside the code. The trace names the call by the
/// function's dotted name; the current version's hash is what runs. So an edit to a callee shows
/// up on the next replay without a new call being recorded.
/// Pointing the preview at a run other than the newest: classic's trace dots, where clicking a
/// different dot shows you that request's values.
let private previewPicksWhichRunToShow =
  cliTestWithFreshTraces
    "traces calls lists the traces, and traces show renders the one you pick"
    (fun target ->
      task {
        let author = author target
        do!
          author
            "Tests.Inbox.describe"
            "(name: String): String =\n  let upper = Stdlib.String.toUppercase name\n  Stdlib.String.append upper \"!\""

        let! _ = runCli target [ "eval"; "Tests.Inbox.describe \"bob\"" ]
        let! _ = runCli target [ "eval"; "Tests.Inbox.describe \"alice\"" ]

        let! listed = runCli target [ "traces"; "calls"; "Tests.Inbox.describe" ]
        let rows =
          listed.Split('\n')
          |> Array.filter (fun l -> l.Contains "eval")
          |> Array.toList
        Expect.equal (List.length rows) 2 "one row per run that went through it"

        // The newest run by default.
        let! newest = runCli target [ "traces"; "show"; "Tests.Inbox.describe" ]
        Expect.stringContains newest "\"ALICE\"" "the newest run's value"

        // ... and an older one by its id, which is the whole point of the list.
        let older = (List.item 1 rows).Trim().Split(' ') |> Array.head
        let! chosen =
          runCli target [ "traces"; "show"; "Tests.Inbox.describe"; older ]
        Expect.stringContains chosen "\"BOB\"" "the run that was asked for"
        Expect.isFalse (chosen.Contains "\"ALICE\"") "and not the newest one"
      })


/// A pipe's stages each carry a value, and the value of step two is the thing you wanted. Showing
/// them means one stage per line, because a comment cannot sit in the middle of one -- so the
/// layout depends on the values, which is exactly what the editor's hints cannot survive. The two
/// halves of that are what this pins.
let private pipeStagesCarryTheirValues =
  cliTestWithFreshTraces
    "a pipe shows a value per stage, and the editor's print keeps its lines"
    (fun target ->
      task {
        let author = author target
        do!
          author
            "Tests.Pipes.sizes"
            "(n: Int64): Int =\n  [1L, 2L, n] |> Stdlib.List.map (fun x -> x * 2L) |> Stdlib.List.length"

        let! ran = runCli target [ "eval"; "Tests.Pipes.sizes 5L" ]
        Expect.stringContains ran "3" "the call ran"

        let! shown = runCli target [ "traces"; "show"; "Tests.Pipes.sizes" ]
        Expect.stringContains
          shown
          "|> Stdlib.List.map (fun x -> x * 2L) // = [2, 4, 10]"
          "what came out of the middle stage, on its own line"
        Expect.stringContains
          shown
          "|> Stdlib.List.length // = 3"
          "and out of the last one"

        // The editor's contract: the annotated print gains characters, never lines. A `// = value`
        // is zero columns wide for layout, and the pipe is left on one line here, so the two
        // prints zip.
        let state = executionState target
        let! both = evalUnder state (plainAndAnnotated "Tests" "Pipes" "sizes")
        match both with
        | RT.DString printed ->
          match printed.Split "@@@" with
          | [| plain; annotated |] ->
            Expect.equal
              (annotated.Split('\n').Length)
              (plain.Split('\n').Length)
              "same number of lines with the values as without"
            Expect.isFalse
              (annotated.Contains "// =")
              "and no annotation at all on a pipe the editor has to keep on one line"
          | other -> failtest $"expected two prints, got {other.Length}"
        | other -> failtest $"expected the prints, got {other}"
      })


/// A hint goes on the line its value came from, even when two lines are identical.
///
/// The hints are built by printing the function twice, once plain and once with values, and
/// diffing. Finding where each changed line LIVES used to be a search of the document for a line
/// with the same text, which cannot tell two identical lines apart: a function with two of them
/// put both hints on the first and none on the second, and pushed the line after the pair off
/// the end of the document, losing its hint entirely. Counting from the function's header in
/// both the printed form and the document is exact.
let private hintsLandOnTheRightIdenticalLine =
  cliTestWithFreshTraces
    "a hint lands on its own line when another line is identical to it"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        do!
          author
            "Tests.Dup.twice"
            ("(n: Int64): Int64 =\n"
             + "  let a =\n"
             + "    let v = (n * 2L)\n"
             + "    v\n"
             + "  let b =\n"
             + "    let v = (n * 2L)\n"
             + "    v\n"
             + "  (a + b)")
        let! ran = runCli target [ "eval"; "Tests.Dup.twice 5L" ]
        Expect.stringContains ran "20" "the run happened"

        let! hints =
          evalUnder
            state
            """let bid = Darklang.SCM.Branch.mainBranchId
let ctx = Darklang.PrettyPrinter.ProgramTypes.Context.forBranch bid
let q = Darklang.LanguageTools.ProgramTypes.Search.SearchQuery { currentModule = ["Tests", "Dup"]; text = ""; searchDepth = Darklang.LanguageTools.ProgramTypes.Search.SearchDepth.AllDescendants; entityTypes = []; exactMatch = false }
let r = Darklang.LanguageTools.PackageManager.Search.search bid q
let defs = Darklang.LanguageTools.ProgramTypes.Definitions { types = []; fns = r.fns |> Darklang.Stdlib.List.map (fun f -> f.entity); values = []; traits = []; impls = []; exprs = [] }
let docLines = (Darklang.PrettyPrinter.definitions ctx defs) |> Darklang.Stdlib.String.split "\n"
r.fns
|> Darklang.Stdlib.List.map (fun item -> Darklang.LanguageTools.LspServer.InlayHints.hintsFor (Darklang.Cli.initState ()).accountID bid docLines item)
|> Darklang.Stdlib.List.flatten
|> Darklang.Stdlib.List.map (fun h -> Darklang.Stdlib.toString h.position.line)"""
        let lines =
          match hints with
          | RT.DList(_, items) ->
            items
            |> List.map (fun i ->
              match i with
              | RT.DString l -> l
              | other -> string other)
            |> List.sort
          | other -> failtest $"expected the hints, got {other}"

        // One line each, so no line carries two and none is missing. Matching by text gave
        // four hints across three lines, with one line holding two of them.
        Expect.equal
          (List.length lines)
          (List.length (List.distinct lines))
          $"every hint is on a line of its own, got {lines}"
      })


let private liveValuesReplayTheLastCall =
  cliTestWithFreshTraces
    "live values replay the last recorded call through the current code"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        do! author "Tests.LiveVals.double" "(n: Int64): Int64 = (n * 2L)"
        do!
          author
            "Tests.LiveVals.greet"
            // `n + 1L` is here for the hints: an INFIX expression had no value in any trace Dark
            // had ever taken until this branch, because the compiler marks the expression a
            // replay should collect and the infix cases built their call by hand and never
            // emitted the marker. The CLI shows them; this is the editor's half, which until now
            // was only confirmed by the two paths sharing a replay rather than by an assertion.
            ("(name: String): String =\n"
             + "  let up = Stdlib.String.toUppercase name\n"
             + "  let n = Tests.LiveVals.double 21L\n"
             + "  let m = n + 1L\n"
             + "  $\"hi {up} {Stdlib.toString m}\"")

        // Nothing recorded yet: no values, and no error.
        let! before = evalUnder state (annotatedPrint "Tests" "LiveVals" "greet")
        match before with
        | RT.DString printed ->
          Expect.isFalse
            (printed.Contains "// =")
            "no call recorded, so nothing beside the code"
          Expect.stringContains printed "let greet" "the code itself still prints"
        | other -> failtest $"expected the print, got {other}"

        let! out = runCli target [ "eval"; "Tests.LiveVals.greet \"bob\"" ]
        Expect.stringContains out "hi BOB 43" "the call ran"

        let! after = evalUnder state (annotatedPrint "Tests" "LiveVals" "greet")
        match after with
        | RT.DString printed ->
          Expect.stringContains
            printed
            "Stdlib.String.toUppercase name // = \"BOB\""
            "the recorded input flowed through the first call"
          Expect.stringContains
            printed
            "double 21L // = 42"
            "and the callee's result is beside its call"
          Expect.isFalse
            (printed.Contains "toString m // =")
            "a call inside an interpolated string is left bare: a comment there would break the string"
        | other -> failtest $"expected the print, got {other}"

        // The LSP's hints: the same values, placed on the document's lines. The document is
        // the module as the editor reads it (`fileSystem/read`), where the fns sit two columns
        // in; `double` has a recorded call of its own, from the eval above.
        let! hints =
          evalUnder
            state
            """let bid = Darklang.SCM.Branch.mainBranchId
let ctx = Darklang.PrettyPrinter.ProgramTypes.Context.forBranch bid
let q = Darklang.LanguageTools.ProgramTypes.Search.SearchQuery { currentModule = ["Tests", "LiveVals"]; text = ""; searchDepth = Darklang.LanguageTools.ProgramTypes.Search.SearchDepth.AllDescendants; entityTypes = []; exactMatch = false }
let r = Darklang.LanguageTools.PackageManager.Search.search bid q
let defs = Darklang.LanguageTools.ProgramTypes.Definitions { types = []; fns = r.fns |> Darklang.Stdlib.List.map (fun f -> f.entity); values = []; traits = []; impls = []; exprs = [] }
let docLines = (Darklang.PrettyPrinter.definitions ctx defs) |> Darklang.Stdlib.String.split "\n"
r.fns
|> Darklang.Stdlib.List.map (fun item -> Darklang.LanguageTools.LspServer.InlayHints.hintsFor (Darklang.Cli.initState ()).accountID bid docLines item)
|> Darklang.Stdlib.List.flatten
|> Darklang.Stdlib.List.map (fun h -> (Darklang.Stdlib.toString h.position.line) + ":" + (Darklang.Stdlib.toString h.position.character) + " " + h.label)"""
        let hints =
          match hints with
          | RT.DList(_, items) ->
            items
            |> List.map (fun i ->
              match i with
              | RT.DString s -> s
              | other -> string other)
            |> List.sort
          | other -> failtest $"expected the hints, got {other}"
        // The last one is the point of the extra line: `7:18 = 43` is `n + 1L`, an INFIX
        // expression. Every other hint here is a CALL's value, and calls had hints before this
        // branch. Without this row the editor's half of the headline feature is confirmed only
        // by sharing a replay with the CLI rather than by anything asserting it.
        Expect.equal
          hints
          [ "2:10 = 42"; "5:43 = \"BOB\""; "6:31 = 42"; "7:18 = 43" ]
          "one hint per value at a line position, at the end of the document's line"

        // The callee changes; the replay runs the current code on the same recorded input.
        do! author "Tests.LiveVals.double" "(n: Int64): Int64 = (n * 3L)"
        let! edited = evalUnder state (annotatedPrint "Tests" "LiveVals" "greet")
        match edited with
        | RT.DString printed ->
          Expect.stringContains
            printed
            "double 21L // = 63"
            "the edit is in the values, with no new call"
        | other -> failtest $"expected the print, got {other}"

        // A version that fails at run time reports the failure and keeps what ran before it.
        do! author "Tests.LiveVals.double" "(n: Int64): Int64 = (n / 0L)"
        let! failed =
          evalUnder
            state
            """match Darklang.Stdlib.Live.Values.replay (Darklang.Cli.initState ()).accountID Darklang.SCM.Branch.mainBranchId (Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = "Tests"; modules = ["LiveVals"]; name = "greet" }) with
| Some v -> (false, Darklang.Stdlib.Dict.size v.byExpr, v.problem)
| None -> (false, 0, Darklang.Stdlib.Option.Option.Some "no trace")"""
        match failed with
        | RT.DTuple(RT.DBool _placeholder,
                    RT.DInt count,
                    [ RT.DEnum(_, _, _, "Some", [ RT.DString problem ]) ]) ->
          // The first slot is a literal `false` in both Dark arms, so asserting it was false
          // could not fail and was testing nothing. `Values` no longer carries the run's own
          // answer: the preview's business is the values inside the code, and `traces inspect`
          // is where a trace's answer lives. The two assertions below are the real ones.
          Expect.isGreaterThan
            (RT.DarkInt.toBigInt count)
            0I
            "the values up to the failure are kept"
          Expect.stringContains
            problem
            "divide by 0"
            "and the problem is the runtime error"
        | other -> failtest $"expected (false, n, Some problem), got {other}"
      })


/// A replay re-runs the whole recorded run, so a run past the limits is refused rather than
/// replayed on a keystroke, and the workbench replays once per fn rather than once per key.
let private liveValuesStayBounded =
  cliTestWithFreshTraces
    "live values refuse a run past the limits, and replay once per selected fn"
    (fun target ->
      task {
        let state = executionState target
        do! author target "Tests.LiveBound.inc" "(n: Int64): Int64 = (n + 1L)"
        let! out = runCli target [ "eval"; "Tests.LiveBound.inc 41L" ]
        Expect.stringContains out "42" "the call ran"

        let within (ms : string) (fns : string) =
          $"""match Darklang.Stdlib.Live.Values.replayWithin {ms} {fns} (Darklang.Cli.initState ()).accountID Darklang.SCM.Branch.mainBranchId (Darklang.LanguageTools.ProgramTypes.PackageLocation {{ owner = "Tests"; modules = ["LiveBound"]; name = "inc" }}) with
| Some v -> (Darklang.Stdlib.Dict.size v.byExpr, Darklang.Stdlib.Option.withDefault v.skipped "")
| None -> (-1, "no trace")"""
        let expectSkipped (label : string) (ms : string) (fns : string) =
          task {
            match! evalUnder state (within ms fns) with
            | RT.DTuple(RT.DInt n, RT.DString why, []) ->
              Expect.equal (RT.DarkInt.toBigInt n) 0I $"{label}: nothing replayed"
              Expect.stringContains
                why
                "too large to show live values"
                $"{label}: and says why"
            | other -> failtest $"{label}: expected (0, why), got {other}"
          }
        match! evalUnder state (within "1000L" "500") with
        | RT.DTuple(RT.DInt n, RT.DString "", []) ->
          Expect.isGreaterThan (RT.DarkInt.toBigInt n) 0I "a small run is replayed"
        | other -> failtest $"expected (n, \"\"), got {other}"
        do! expectSkipped "over the time limit" "-1L" "500"
        do! expectSkipped "over the fn limit" "1000L" "0"

        // A sentinel in the values survives a refresh that moved nothing, so that refresh did not
        // replay; the reload a store change forces does, and puts the real values back.
        let! wb =
          evalUnder
            state
            """let base = Darklang.Cli.Workbench.initialState Darklang.SCM.Branch.mainBranchId Darklang.Stdlib.Option.Option.None "t" "t" [] false
let s0 = { base with activeView = Darklang.Cli.Workbench.vMatter; location = Darklang.Cli.Packages.PackageLocation.Module ["Tests", "LiveBound"] }
let items = Darklang.Cli.Workbench.reloadItems s0
let idx = (Darklang.Stdlib.List.findFirstIndex items (fun i -> i.name == "inc")) |> Darklang.Stdlib.Option.withDefault 0
let s = Darklang.Cli.Workbench.refresh base { s0 with items = items; selected = idx }
let marked = { s with liveValues = Darklang.Stdlib.Dict.singleton "sentinel" "x" }
let kept = Darklang.Cli.Workbench.refresh marked { marked with detailScroll = 1 }
let reloaded = Darklang.Cli.Workbench.refresh kept (Darklang.Cli.Workbench.forceScmRefresh kept)
(Darklang.Stdlib.Dict.size s.liveValues, Darklang.Stdlib.Dict.keys kept.liveValues, Darklang.Stdlib.Dict.keys reloaded.liveValues)"""
        match wb with
        | RT.DTuple(RT.DInt first,
                    RT.DList(_, [ RT.DString "sentinel" ]),
                    [ RT.DList(_, again) ]) ->
          Expect.isGreaterThan
            (RT.DarkInt.toBigInt first)
            0I
            "the fn shows its values"
          Expect.equal
            (bigint (List.length again))
            (RT.DarkInt.toBigInt first)
            "a forced reload replays again"
          Expect.isFalse
            (List.contains (RT.DString "sentinel") again)
            "and the sentinel is gone"
        | other -> failtest $"expected (n, [sentinel], keys), got {other}"
      })


/// The agent's side of the live loop, without the agent: `Live.observe` renders a view headless
/// through `Ui.Text` (the third renderer), taking each fn at its newest version that passes its
/// checks and, when that one raises, the picture from the version before it; `Live.show` names
/// the view a host loop should be on, through the store so the host wakes for it.
let private observeAndShow =
  cliTest
    "observe renders a view headless and show points a host at it"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        let viewLoc = ("(" + locSource [] "LiveObs" + ")")
        let observe () =
          evalUnder
            state
            $"""let o = Darklang.Stdlib.Live.observe Darklang.SCM.Branch.mainBranchId {viewLoc}
(Darklang.Stdlib.Option.isSome o.report, o.rte, o.render)"""
        let unpack (dv : RT.Dval) =
          match dv with
          | RT.DTuple(RT.DBool hasReport, rte, [ RT.DString render ]) ->
            let rte =
              match rte with
              | RT.DEnum(_, _, _, "Some", [ RT.DString e ]) -> Some e
              | _ -> None
            (hasReport, rte, render)
          | other -> failtest $"expected an observation, got {other}"

        do! author "Tests.LiveObs.init" "(): Int64 = 3L"
        do!
          author
            "Tests.LiveObs.update"
            "(m: Int64) (e: Darklang.Cli.Apps.Host.Event<Int64>): Int64 = m"
        do!
          author
            "Tests.LiveObs.render"
            "(m: Int64): Stdlib.Cli.UI.Node.Node<Int64> = Stdlib.Cli.UI.Node.column [ Stdlib.Cli.UI.Node.bold \"Obs\", Stdlib.Cli.UI.Node.table [ \"k\", \"v\" ] [ [ \"count\", Stdlib.toString m ] ], Stdlib.Cli.UI.Node.row [ Stdlib.Cli.UI.Node.text \"a\", Stdlib.Cli.UI.Node.Node.Button(\"go\", 1L) ], Stdlib.Cli.UI.Node.band Stdlib.Cli.UI.Node.Severity.Error \"boom\" ]"

        let! first = observe ()
        let (hasReport, rte, render) = unpack first
        Expect.isFalse hasReport "the newest version passes its checks"
        Expect.equal rte None "and runs"
        Expect.equal
          render
          "Obs\nk      v\n-----  -\ncount  3\na [ go ]\n! boom"
          "the tree as plain text: table rows, a row side by side, a boxed button, a marked band"

        // A save that fails its checks: the previous picture, with the report.
        do!
          author
            "Tests.LiveObs.render"
            "(m: Int64): Stdlib.Cli.UI.Node.Node<Int64> = Stdlib.Cli.UI.Node.text 3L"
        let! broken = observe ()
        let (hasReport, rte, render) = unpack broken
        Expect.isTrue hasReport "the newest version's report is there"
        Expect.equal rte None "nothing raised"
        Expect.stringContains
          render
          "count  3"
          "and the version before it is the picture"

        // A save that raises: the previous picture, with the error.
        do!
          author
            "Tests.LiveObs.render"
            "(m: Int64): Stdlib.Cli.UI.Node.Node<Int64> = Stdlib.Cli.UI.Node.text (Stdlib.toString (m / 0L))"
        let! raised = observe ()
        let (hasReport, rte, render) = unpack raised
        Expect.isFalse hasReport "this version passes its checks"
        match rte with
        | Some e ->
          Expect.stringContains e "divide by 0" "the runtime error is reported"
        | None -> failtest "expected the runtime error"
        Expect.stringContains
          render
          "count  3"
          "and the version before it is the picture"

        // `show` names a view through the store; the host loop switches on its next turn.
        do! author "Tests.LiveOther.init" "(): Int64 = 0L"
        do!
          author
            "Tests.LiveOther.update"
            "(m: Int64) (e: Darklang.Cli.Apps.Host.Event<Int64>): Int64 = m"
        do!
          author
            "Tests.LiveOther.render"
            "(m: Int64): Stdlib.Cli.UI.Node.Node<Int64> = Stdlib.Cli.UI.Node.text \"the other view\""
        let view =
          "Darklang.Cli.Apps.Model.View { name = \"Tests.LiveOther\"; title = \"O\"; init = \"Tests.LiveOther.init\"; update = \"Tests.LiveOther.update\"; render = \"Tests.LiveOther.render\"; every = Stdlib.Option.Option.None; keys = Stdlib.Option.Option.None }"
        let! prepared =
          evalUnder
            state
            $"Darklang.Cli.Apps.Host.prepare Darklang.SCM.Branch.mainBranchId ({view})"
        let session =
          match prepared with
          | RT.DEnum(_, _, _, "Ok", [ s ]) -> s
          | other -> failtest $"the view did not prepare: {other}"
        let driver = loopDriver state
        let! sizeDv =
          evalUnder state "Darklang.Stdlib.Cli.Tui.Size { width = 40; height = 8 }"
        let rowsOf (s : RT.Dval) =
          task {
            let! rows =
              callByName state "Darklang.Cli.Apps.Host.plainRows" [ s; sizeDv ]
            return plainRows rows
          }
        let! before = rowsOf session
        Expect.contains before "the other view" "the host is on the other view"

        // Put the broken render back to a good one first, so the shown view has a frame.
        do!
          author
            "Tests.LiveObs.render"
            "(m: Int64): Stdlib.Cli.UI.Node.Node<Int64> = Stdlib.Cli.UI.Node.text (\"count \" + Stdlib.toString m)"
        let! _ = evalUnder state $"Darklang.Stdlib.Live.show {viewLoc}"
        let! shown = evalUnder state "Darklang.Stdlib.Live.shown ()"
        match shown with
        | RT.DEnum(_, _, _, "Some", [ RT.DRecord(_, _, _, fields) ]) ->
          Expect.equal
            (Map.tryFind "name" fields)
            (Some(RT.DString "LiveObs"))
            "shown reads back"
        | other -> failtest $"expected the shown view, got {other}"

        pushTick driver
        let! session = stepOn driver "Darklang.Cli.Apps.Host.step" [ session ]
        let! after = rowsOf session
        Expect.contains after "count 3" "the host switched to the shown view"
        // The same wake carried the render's save, so the reload's toast wins over "showing".
        Expect.stringContains
          (String.concat " " after)
          "changed:"
          "and the toast says what changed, not that it switched"
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
    storeReadsRequirePackageRead
    approvedSqliteOnTheStoreRuns
    approvalsNameTheirDependencies
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
          pollOnABranchSeesTheBranchsOwnSaves
          serveFollowsEditsOnABranch
          previewOfAServedRequest
          aFixedCalleeIsNotAdoptedThroughItsBrokenDependent
          devErrorPageCarriesTheListener
          devStreamReportsAnEditAfterTheServe
          liveValuesReplayTheLastCall
          liveValuesStayBounded
          hintsLandOnTheRightIdenticalLine
          pipeStagesCarryTheirValues
          previewPicksWhichRunToShow
          pipeStageValuesStayOutsideTheirDelimiters
          observeAndShow ]
    ) ]
