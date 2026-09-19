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
    testSequenced (testList "live" [ serveFollowsEdits; pollAndAffects ]) ]
