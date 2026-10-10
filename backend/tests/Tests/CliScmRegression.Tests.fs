/// The bugs a review found, kept found.
///
/// Every test here is somebody's reproduction: mostly Ocean's first review round (her numbers are in
/// the doc comments), plus the ones that turned up while fixing those. They are end-to-end on
/// purpose. Each of these bugs was invisible to a green suite, and what made them visible was
/// running the commands and reading what came back, so that is what these do.
///
/// The vocabulary is `Tests.CliDsl`. A test that needs something it does not have uses `runCli`.
module Tests.CliScmRegression

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


// ─── branch isolation ─────────────────────────────────────────────────────

/// Ocean #9. A cascade on a branch discovered dependents through MAIN's version of a name the branch
/// had rebound, then rewrote that name with main's body: the branch's own work, replaced by
/// propagation.
/// `dark squash` collapses a run of my own unpushed commits, and STOPS at a peer's.
///
/// The second half is the safety property and the reason the command has the shape it does. An op
/// that arrived over the wire carries its AUTHOR's commit hash, deliberately, so the receiver files
/// it under that rather than under its own import. A squash selecting its range any other way (a
/// stamp window, a parent walk over ops, "everything since X") would re-stamp a peer's op under
/// mine. That does not LOSE the op; it silently reattributes somebody else's work, which is worse,
/// because nothing afterwards contradicts it.
///
/// The peer's commit is built with `PackageOps.adoptCommits`, which is what a pull uses for exactly
/// this, rather than with hand-written SQL: a test that fabricates the state itself can be wrong
/// about the state.
/// The other half of the selection: a squash stops at a commit the relay already has.
///
/// Separate from the peer test beside it, and deliberately so: removing the pushed gate leaves that
/// one green, which I measured. `sync_pushed` records per OP, and an op's id is its content hash, so
/// a squash does not make a pushed op un-pushed; it changes which commit the op NAMES, and the relay
/// has already filed it under the old one and will never hear again, because a pushed op does not
/// travel twice. Squashing across that line leaves the two permanently disagreeing.
let private squashStopsAtAPushedCommit =
  instanceTest "squash stops at a commit whose ops have been pushed" (fun state ->
    task {
      do! start state

      do! fn state "Tests.Sqp.a" "() : Int64 = 1L"
      do! commit state "mine a"
      do! fn state "Tests.Sqp.b" "() : Int64 = 2L"
      do! commit state "mine b"
      do! fn state "Tests.Sqp.c" "() : Int64 = 3L"
      do! commit state "mine c"
      do! fn state "Tests.Sqp.d" "() : Int64 = 4L"
      do! commit state "mine d"

      // Mark `mine b`'s op as gone to a relay, which is all `sync_pushed` is.
      let markPushed =
        "Darklang.SCM.Commits.recent 4L "
        + "|> Stdlib.List.filter (fun c -> c.message == \"mine b\") "
        + "|> Stdlib.List.map (fun c -> Darklang.SCM.Commits.opIdsIn c.hash) "
        + "|> Stdlib.List.flatten "
        + "|> Stdlib.List.map (fun o -> "
        + "Stdlib.Sqlite.mustExec (Darklang.SCM.localDb ()) "
        + "\"INSERT OR IGNORE INTO sync_pushed (relay, op_id) VALUES (@p0, @p1)\" "
        + "[ \"http://test-relay\", o ])"

      let! _ = runCliPlain state [ "eval"; markPushed ]

      let! out = runCliPlain state [ "squash"; "the unpushed ones" ]

      Expect.stringContains
        out
        "squashed 2 commits"
        $"it collapses `mine d` and `mine c` and stops at the pushed one, got: {out}"

      let! after = runCliPlain state [ "commits" ]
      Expect.stringContains
        after
        "mine b"
        $"the pushed commit survives, got: {after}"
      Expect.stringContains
        after
        "mine a"
        $"and so does everything under it, got: {after}"
    })


let private squashStopsAtAPeersCommit =
  instanceTest
    "squash collapses my unpushed commits and stops at a peer's"
    (fun state ->
      task {
        do! start state

        // Authored and left in the draft, which is the state a pull's ops arrive in:
        // `adoptCommits` only files an op that has no commit yet.
        do! fn state "Tests.Sq.peerWork" "() : Int64 = 1L"

        // Hand my op to a commit somebody else wrote, the way a pull does.
        let peerHash = "peer0000000000ff"

        let adopt =
          "Darklang.SCM.PackageOps.adoptCommits "
          + "[ Darklang.SCM.PackageOps.BranchBundleCommit { hash = \""
          + peerHash
          + "\""
          + ", message = \"peer work\", author = \"peer-instance\""
          + ", originTs = \"2020-01-01T00:00:00.000Z\", parent = \"\" } ] "
          + "(Darklang.SCM.PackageOps.draftOpIdsFor Darklang.SCM.Branch.mainBranchId "
          + "|> Stdlib.List.map (fun o -> (o, \""
          + peerHash
          + "\")))"

        let! _ = runCliPlain state [ "eval"; adopt ]

        do! fn state "Tests.Sq.a" "() : Int64 = 2L"
        do! commit state "mine a"
        do! fn state "Tests.Sq.b" "() : Int64 = 3L"
        do! commit state "mine b"

        let! out = runCliPlain state [ "squash"; "just mine" ]

        Expect.stringContains
          out
          "squashed 2 commits"
          $"it collapses my two and stops at the peer's, got: {out}"

        let! after = runCliPlain state [ "commits" ]
        Expect.stringContains
          after
          "peer work"
          $"the peer's commit survives, got: {after}"

        let! stillOwned =
          runCliPlain
            state
            [ "eval"
              "Darklang.SCM.Commits.opIdsIn \""
              + peerHash
              + "\""
              + " |> Stdlib.List.length" ]

        Expect.stringContains
          (stillOwned.Trim())
          "1"
          $"and its op was not re-stamped under mine, got: {stillOwned}"

        do! evals state "Tests.Sq.a ()" "2" "the ops the squash moved still answer"
        do! evals state "Tests.Sq.b ()" "3" "both of them"
        do! evals state "Tests.Sq.peerWork ()" "1" "and so does the peer's"
      })


let private propagationLeavesBranchWorkAlone =
  instanceTest
    "a cascade on a branch does not overwrite what the branch rebound"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Casc.base" "() : Int64 = 20L"
        do! fn state "Tests.Casc.caller" "() : Int64 = Tests.Casc.base ()"
        do! commit state "casc base"

        do! switch state "cascbr"
        do! fn state "Tests.Casc.caller" "() : Int64 = 777L"
        do! commit state "branch caller"

        // The edit whose cascade used to reach main's caller and bind main's body here.
        do! fn state "Tests.Casc.base" "() : Int64 = 30L"
        do!
          evals state "Tests.Casc.caller ()" "777" "the branch keeps its own caller"

        do! onMain state
        do!
          evals
            state
            "Tests.Casc.caller ()"
            "20"
            "and main still calls its own base"
        do! archiveBranches state [ "cascbr" ]
      })

/// Ocean #10. Rebase moved a branch's bases onto main's `locations`, which carry main's UNCOMMITTED
/// draft, so a branch could be rebased onto work main had not committed and might still discard.
let private rebaseIgnoresMainsDraft =
  instanceTest
    "rebase moves onto the parent's committed state, not its draft"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Reb.root" "() : Int64 = 111L"
        do! commit state "reb v1"

        do! switch state "rebbr"
        do! fn state "Tests.Reb.user" "() : Int64 = Tests.Reb.root ()"
        do! commit state "branch user"

        // Main edits the dependency and does NOT commit it.
        do! onMain state
        do! fn state "Tests.Reb.root" "() : Int64 = 222L"
        do! dirty state "main's edit is a draft"

        do! switch state "rebbr"
        do! rebase state "rebbr"

        // Both answers come from the same committed version. The bug left the branch calling 222
        // through its caller while `root` still read 111: inconsistent with itself.
        do!
          evals state "Tests.Reb.root ()" "111" "the branch sees the committed root"
        do! evals state "Tests.Reb.user ()" "111" "and its caller agrees with it"

        do! start state
        do! archiveBranches state [ "rebbr" ]
      })

/// Ocean #11. Archiving a parent took its work out from under its children: their code stopped
/// resolving and their listing showed a parent that is not in this store.
let private archiveRefusesToOrphanAChild =
  instanceTest
    "archiving a parent with live children is refused, and names them"
    (fun state ->
      task {
        do! switch state "orphanpar"
        do! fn state "Tests.Orph.p" "() : Int64 = 1L"
        do! commit state "parent work"
        do! run state [ "branch"; "create"; "orphankid" ]

        do! onMain state
        do!
          refuses
            state
            [ "branch"; "archive"; "orphanpar"; "-y" ]
            "cannot archive"
            "archived branch"
            "a parent with a live child is refused"
        do!
          shows
            state
            [ "branch"; "archive"; "orphanpar"; "-y" ]
            "orphankid"
            "and the child is named, so you know what to do"

        // Archiving the child is the advice the refusal gives; then the parent goes.
        do! run state [ "branch"; "archive"; "orphankid"; "-y" ]
        do!
          shows
            state
            [ "branch"; "archive"; "orphanpar"; "-y" ]
            "archived"
            "with the child gone, the parent archives"
      })

/// Not one of Ocean's, but the same family as her branch-isolation findings: a `Deprecate` authored
/// on a branch never folded, so the branch went on calling the item live and only a merge made the
/// deprecation visible anywhere.
let private deprecateIsVisibleOnItsBranch =
  instanceTest
    "a deprecation authored on a branch shows there, and travels on merge"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.DepBr.old" "() : Int64 = 1L"
        do! commit state "depbr base"

        do! switch state "depbr"
        do! deprecate state "Tests.DepBr.old"
        do!
          showsAnyCase
            state
            [ "view"; "Tests.DepBr.old" ]
            "deprecat"
            "the branch sees its own deprecation"

        do! onMain state
        do!
          lacksAnyCase
            state
            [ "view"; "Tests.DepBr.old" ]
            "deprecat"
            "main does not, until it is merged"

        do! switch state "depbr"
        do! commit state "deprecate old"
        do! onMain state
        do! merge state "depbr"
        do!
          showsAnyCase
            state
            [ "view"; "Tests.DepBr.old" ]
            "deprecat"
            "and the merge carries it to main"
      })


// ─── the draft commands ───────────────────────────────────────────────────

/// Ocean #1. `discard <name>` on a branch dropped every op for that name, committed ones included,
/// so the branch lost its last committed version and `status` then called it clean.
let private branchDiscardKeepsCommittedWork =
  instanceTest
    "discard <name> on a branch spares what the branch already committed"
    (fun state ->
      task {
        do! start state
        do! switch state "discardkeep"
        do! fn state "Tests.DiscardKeep.f" "() : Int64 = 1L"
        do! commit state "f v1 on branch"
        do! evals state "Tests.DiscardKeep.f ()" "1" "the committed version runs"

        // A second, UNCOMMITTED edit: this is what discard is allowed to take.
        do! fn state "Tests.DiscardKeep.f" "() : Int64 = 2L"
        do! discardName state "Tests.DiscardKeep.f"

        do!
          evals
            state
            "Tests.DiscardKeep.f ()"
            "1"
            "the draft edit is gone and the committed version is back"
        do!
          lacks
            state
            [ "eval"; "Tests.DiscardKeep.f ()" ]
            "not found"
            "the name itself survives the discard"

        do! onMain state
        do! archiveBranches state [ "discardkeep" ]
      })

/// Ocean #14. `--include=` selected namings by content hash, so an unrelated name that happened to
/// hold an identical body rode along and was reported as needed by what you named.
let private partialCommitTakesNamesNotBodies =
  instanceTest "--include= leaves an unrelated name that shares a body" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Part.public" "() : Int64 = 987L"
      do! fn state "Tests.Part.private" "() : Int64 = 987L"

      // The preview lists the whole draft, which is right; what must not appear is the
      // "also committed" line, which is where the unrelated name used to be reported as needed.
      do!
        lacks
          state
          [ "commit"; "public only"; "--include=Tests.Part.public"; "-y" ]
          "also committed"
          "nothing rides along on an identical body"

      // And the proof it was not committed silently: it is still a draft, and it is the one left.
      do! dirty state "the unrelated name is still uncommitted"
      do!
        shows state [ "status" ] "1 item changed" "exactly one name is left behind"
      do!
        shows
          state
          [ "diff" ]
          "Tests.Part.private"
          "and it is that name specifically"
      do! start state
    })

/// A dependency's hash can be committed while the name used by its caller is
/// still new. Select that naming without dragging unrelated aliases along.
let private partialCommitTakesNewNameForCommittedContent =
  instanceTest
    "--include= takes a needed new name for committed content, leaving its alias"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.SharedCommit.committed" "() : Int64 = 19327L"
        do! commit state "existing dependency content"
        do! fn state "Tests.SharedCommit.dep" "() : Int64 = 19327L"
        do! fn state "Tests.SharedCommit.unrelated" "() : Int64 = 19327L"
        do!
          fn
            state
            "Tests.SharedCommit.caller"
            "() : Int64 = Tests.SharedCommit.dep () + 1L"

        do!
          commitOnly
            state
            "caller and its dependency name"
            "Tests.SharedCommit.caller"
        do! evals state "Tests.SharedCommit.caller ()" "19328" "the caller runs"
        do!
          shows
            state
            [ "status" ]
            "1 item changed"
            "only the unrelated name is left"
        let! remaining = runCliPlain state [ "diff" ]
        Expect.stringContains
          remaining
          "Tests.SharedCommit.unrelated"
          "the unrelated alias stays in the draft"
        Expect.isFalse
          (remaining.Contains "Tests.SharedCommit.dep")
          "the required name was committed"
        Expect.isFalse
          (remaining.Contains "Tests.SharedCommit.caller")
          "the selected caller was committed"
        do! start state
      })

/// Ocean #15. `undo` writes a `Decision`, and `discard <name>` only recognised `SetName`, so it
/// reported the change dropped and changed nothing.
let private discardSeesWhatUndoWrote =
  instanceTest "discard <name> drops what undo staged" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Undo.f" "() : Int64 = 1L"
      do! commit state "undo v1"
      do! fn state "Tests.Undo.f" "() : Int64 = 2L"
      do! commit state "undo v2"

      do! run state [ "undo"; "Tests.Undo.f" ]
      do! evals state "Tests.Undo.f ()" "1" "undo staged the older version"

      do! discardName state "Tests.Undo.f"
      do!
        evals
          state
          "Tests.Undo.f ()"
          "2"
          "discarding it puts the committed version back"
      do! clean state "and the draft is empty"
    })

/// Ocean #16. `discard` decided the draft was empty by counting changed NAME bindings, so a draft
/// holding only a decision (a pin, a deprecation, an ack) read as empty and was left in place.
let private discardCountsOpsNotNames =
  instanceTest "discard sees a draft that holds only a decision" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Dec.f" "() : Int64 = 1L"
      do! commit state "dec base"

      // A pin binds no name, so this draft has an op and no changed names.
      do! deprecate state "Tests.Dec.f"
      do!
        refuses
          state
          [ "discard"; "-y" ]
          "discarded"
          "nothing to discard"
          "a decision-only draft is not empty"
    })


// ─── merge ────────────────────────────────────────────────────────────────

/// Ocean #12. A branch whose commit was REFUSED by the at-rest type check could still be merged,
/// landing code that does not typecheck on main with nothing recorded. Merge asks what commit asks,
/// and refuses uncommitted work outright.
let private mergeIsGatedLikeCommit =
  instanceTest "merge refuses type errors and uncommitted work" (fun state ->
    task {
      do! start state
      do! switch state "mergegate"
      do! fn state "Tests.MergeGate.bad" "() : Int64 = \"oops\""
      do!
        shows
          state
          [ "commit"; "bad"; "-y" ]
          "cannot commit"
          "commit refuses it, as it always did"

      // Uncommitted: the first gate, and the one that answers "why can we merge WIP at all".
      do! onMain state
      do!
        refuses
          state
          [ "merge"; "mergegate" ]
          "uncommitted"
          "Merged"
          "merge refuses uncommitted work"

      // Committed past the gate on purpose, and the merge still refuses on the type error.
      do! switch state "mergegate"
      do! run state [ "commit"; "bad"; "-y"; "--allow-type-errors" ]
      do! onMain state
      do!
        refuses
          state
          [ "merge"; "mergegate" ]
          "at-rest type check"
          "Merged"
          "merge refuses definite type errors"

      // The deliberate way past is the same one commit has.
      do!
        shows
          state
          [ "merge"; "mergegate"; "--allow-type-errors"; "-y" ]
          "Merged"
          "typed out, it merges"

      do! archiveBranches state [ "mergegate" ]
    })


// ─── rename ───────────────────────────────────────────────────────────────

/// Ocean #4 and #5. The workbench's rename authored a lone `SetName` and leaned on a fold heuristic
/// this PR deleted, so it added a second name and kept the first; and there was no CLI rename at all.
/// A rename is two ops: the old name ends, the same content binds at the new one.
let private renameMovesANameNotItsContent =
  instanceTest "rename moves a name and leaves its callers alone" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Rn.old" "() : Int64 = 42L"
      do! fn state "Tests.Rn.caller" "() : Int64 = Tests.Rn.old ()"
      do! commit state "rn base"

      do!
        shows
          state
          [ "rename"; "Tests.Rn.old"; "Tests.Rn.renamed" ]
          "renamed"
          "it says what it did"
      do! evals state "Tests.Rn.renamed ()" "42" "the new name holds the content"
      do! evals state "Tests.Rn.caller ()" "42" "the caller is untouched"
      do! notFound state "Tests.Rn.old ()" "and the old name is gone"

      do! start state
    })

/// The two refusals, which are the reason a rename cannot quietly take something off the shelf.
let private renameRefusesTheBadCases =
  instanceTest "rename refuses a missing name and an occupied one" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Rn2.a" "() : Int64 = 1L"
      do! fn state "Tests.Rn2.b" "() : Int64 = 2L"
      do! commit state "rn2 base"

      do!
        refuses
          state
          [ "rename"; "Tests.Rn2.nope"; "Tests.Rn2.x" ]
          "nothing here is named"
          "renamed"
          "renaming what does not exist is refused"

      do!
        refuses
          state
          [ "rename"; "Tests.Rn2.a"; "Tests.Rn2.b" ]
          "already names something"
          "renamed"
          "and renaming onto a live name is refused"

      // Neither refusal moved anything.
      do! evals state "Tests.Rn2.a ()" "1" "the source name is untouched"
      do! evals state "Tests.Rn2.b ()" "2" "and so is the occupied one"
      do! start state
    })


// ─── the everyday path ────────────────────────────────────────────────────

/// The loop the whole system exists for, asserted end to end: author, see it live, commit, see it
/// clean, edit, see the cascade, commit again. Nothing exotic. It is here because every bug above
/// was found by someone doing exactly this and reading what came back.
let private theEverydayLoop =
  instanceTest "author, run, commit, edit, cascade, commit" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Day.cents" "(d: Int64) : Int64 = d * 100L"
      do! fn state "Tests.Day.total" "(a: Int64) : Int64 = Tests.Day.cents a"

      // Live on write: no build step between authoring and running it.
      do! evals state "Tests.Day.total 3L" "300" "the new code runs immediately"
      do! dirty state "and it is a draft until committed"

      do! commit state "money helpers"
      do! clean state "committing empties the draft"

      // An edit to a dependency repoints its callers, in the draft, before any commit.
      do! fn state "Tests.Day.cents" "(d: Int64) : Int64 = d * 1000L"
      do! evals state "Tests.Day.total 3L" "3000" "the caller followed the edit"
      do! dirty state "the repoint is staged, not committed"

      do! commit state "cents in mills"
      do! clean state "and committing seals it"
      do!
        evals
          state
          "Tests.Day.total 3L"
          "3000"
          "with the new behaviour still in place"
    })

/// A branch, end to end, from the outside: it starts, it holds work main cannot see, main's work
/// stays visible to it, and merging moves the work over.
let private theBranchLoop =
  instanceTest "a branch holds its own work, sees main's, and merges" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Loop.shared" "() : Int64 = 1L"
      do! commit state "shared, on main"

      do! switch state "looper"
      do! shows state [ "branch" ] "looper" "switch says where you are"
      do! fn state "Tests.Loop.mine" "() : Int64 = Tests.Loop.shared () + 1L"
      do! commit state "branch work"
      do! evals state "Tests.Loop.mine ()" "2" "the branch resolves main's names"

      do! onMain state
      do! notFound state "Tests.Loop.mine ()" "main cannot see the branch's work"

      do! merge state "looper"
      do! evals state "Tests.Loop.mine ()" "2" "until it is merged"
    })

/// The refusals a person meets by typing something reasonable that is not available. Each of these
/// answered wrongly at some point: a fall-through arm that reports success is the failure mode the
/// CLI sweep exists to catch, and these are the specific cases worth pinning.
let private theCommonRefusals =
  instanceTest
    "the everyday refusals say what is wrong, and do not claim success"
    (fun state ->
      task {
        do! start state

        do!
          refuses
            state
            [ "merge"; "no-such-branch-here" ]
            "no branch"
            "Merged"
            "merging a branch that does not exist"

        do!
          refuses
            state
            [ "rebase" ]
            "usage"
            "rebased"
            "a bare rebase on main has nothing to rebase"

        do!
          shows
            state
            [ "discard"; "-y" ]
            "nothing to discard"
            "discarding an empty draft says so"

        do!
          shows
            state
            [ "undo"; "Tests.Nothing.here" ]
            "not"
            "undoing a name that does not exist says so"
      })


let tests : List<Test> =
  [ squashStopsAtAPeersCommit
    squashStopsAtAPushedCommit
    propagationLeavesBranchWorkAlone
    rebaseIgnoresMainsDraft
    archiveRefusesToOrphanAChild
    deprecateIsVisibleOnItsBranch
    branchDiscardKeepsCommittedWork
    partialCommitTakesNamesNotBodies
    partialCommitTakesNewNameForCommittedContent
    discardSeesWhatUndoWrote
    discardCountsOpsNotNames
    mergeIsGatedLikeCommit
    renameMovesANameNotItsContent
    renameRefusesTheBadCases
    theEverydayLoop
    theBranchLoop
    theCommonRefusals ]
