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
  cliTest "version and install-status describe this install" (fun state ->
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
  cliTest "config set is what config get reads back" (fun state ->
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
  cliTest "a script can be added, listed and read back" (fun state ->
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
  cliTest "backups now copies the store instead of dying on the guard" (fun state ->
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
  cliTest "backups restore refuses a name it doesn't have" (fun state ->
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
  cliTest "apps list-available names the catalog" (fun state ->
    task {
      do!
        shows
          state
          [ "apps"; "list-available" ]
          "Available apps:"
          "the catalog lists something"
    })

let appsInstalledLists =
  cliTest "apps list answers on an instance with none" (fun state ->
    task { do! sane state [ "apps"; "list" ] "the installed list answers" })

let permissionsLists =
  cliTest "permissions list names the effects the policy covers" (fun state ->
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
  cliTestOnMain
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
  cliTest "withdrawing an approval nobody made says so" (fun state ->
    task {
      do!
        shows
          state
          [ "permissions"; "unapprove"; "Tests.Appr.neverApproved" ]
          "has no approved version"
          "an unmatched name withdraws nothing and says so"
    })


let dbAndTracesAnswer =
  cliTest "db and traces answer without a canvas or a recording" (fun state ->
    task {
      do! sane state [ "db"; "list" ] "db list answers on an empty canvas"
      do! sane state [ "traces"; "stats" ] "traces stats answers"
    })


// ─── the log ──────────────────────────────────────────────────────────────

let opsAndCommitsDescribeTheLog =
  cliTestOnMain "ops, commits and log describe the same log" (fun state ->
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
  cliTestOnMain
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
  cliTestOnMain "constraints and conflicts say nothing is pending" (fun state ->
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
    constraintsAndConflictsReportQuiet ]
