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
let private propagationLeavesBranchWorkAlone =
  cliTestOnMain
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
  cliTestOnMain
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
  cliTestOnMain
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

/// Ocean #13. A merge moved a child's ops and name bases to the parent and left its propagation pins
/// under the child's id, where nothing consults them, so the parent's next edit repointed a caller
/// the child had deliberately pinned.
let private mergeCarriesPins =
  cliTestOnMain "a merged branch's pins follow its ops to the parent" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Pin.base" "() : Int64 = 10L"
      do! fn state "Tests.Pin.caller" "() : Int64 = Tests.Pin.base () + 1L"
      do! commit state "pin base"

      do! switch state "pinbr"
      do! pin state "Tests.Pin.caller"
      do! commit state "pin the caller"

      do! onMain state
      do! shows state [ "merge"; "pinbr" ] "Merged" "the merge lands"

      // The pin is the parent's now, so main's next edit leaves the caller where it is.
      do! fn state "Tests.Pin.base" "() : Int64 = 20L"
      do!
        evals
          state
          "Tests.Pin.caller ()"
          "11"
          "a pinned caller keeps calling the version it was pinned to"

      do! start state
    })

/// Not one of Ocean's, but the same family as her branch-isolation findings: a `Deprecate` authored
/// on a branch never folded, so the branch went on calling the item live and only a merge made the
/// deprecation visible anywhere.
let private deprecateIsVisibleOnItsBranch =
  cliTestOnMain
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
  cliTestOnMain
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
  cliTestOnMain
    "--include= leaves an unrelated name that shares a body"
    (fun state ->
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
          shows
            state
            [ "status" ]
            "1 item changed"
            "exactly one name is left behind"
        do!
          shows
            state
            [ "diff" ]
            "Tests.Part.private"
            "and it is that name specifically"
        do! start state
      })

/// Ocean #15. `undo` writes a `Decision`, and `discard <name>` only recognised `SetName`, so it
/// reported the change dropped and changed nothing.
let private discardSeesWhatUndoWrote =
  cliTestOnMain "discard <name> drops what undo staged" (fun state ->
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
  cliTestOnMain "discard sees a draft that holds only a decision" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Dec.f" "() : Int64 = 1L"
      do! commit state "dec base"

      // A pin binds no name, so this draft has an op and no changed names.
      do! pin state "Tests.Dec.f"
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
  cliTestOnMain "merge refuses type errors and uncommitted work" (fun state ->
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
          [ "merge"; "mergegate"; "--allow-type-errors" ]
          "Merged"
          "typed out, it merges"

      do! archiveBranches state [ "mergegate" ]
    })


// ─── rename ───────────────────────────────────────────────────────────────

/// Ocean #4 and #5. The workbench's rename authored a lone `SetName` and leaned on a fold heuristic
/// this PR deleted, so it added a second name and kept the first; and there was no CLI rename at all.
/// A rename is two ops: the old name ends, the same content binds at the new one.
let private renameMovesANameNotItsContent =
  cliTestOnMain "rename moves a name and leaves its callers alone" (fun state ->
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
  cliTestOnMain "rename refuses a missing name and an occupied one" (fun state ->
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
  cliTestOnMain "author, run, commit, edit, cascade, commit" (fun state ->
    task {
      do! start state
      do!
        fn
          state
          "Tests.Day.cents"
          "(d: Int64) : Int64 = Stdlib.Int64.multiply d 100L"
      do! fn state "Tests.Day.total" "(a: Int64) : Int64 = Tests.Day.cents a"

      // Live on write: no build step between authoring and running it.
      do! evals state "Tests.Day.total 3L" "300" "the new code runs immediately"
      do! dirty state "and it is a draft until committed"

      do! commit state "money helpers"
      do! clean state "committing empties the draft"

      // An edit to a dependency repoints its callers, in the draft, before any commit.
      do!
        fn
          state
          "Tests.Day.cents"
          "(d: Int64) : Int64 = Stdlib.Int64.multiply d 1000L"
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
  cliTestOnMain "a branch holds its own work, sees main's, and merges" (fun state ->
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
  cliTestOnMain
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
  [ propagationLeavesBranchWorkAlone
    rebaseIgnoresMainsDraft
    archiveRefusesToOrphanAChild
    mergeCarriesPins
    deprecateIsVisibleOnItsBranch
    branchDiscardKeepsCommittedWork
    partialCommitTakesNamesNotBodies
    discardSeesWhatUndoWrote
    discardCountsOpsNotNames
    mergeIsGatedLikeCommit
    renameMovesANameNotItsContent
    renameRefusesTheBadCases
    theEverydayLoop
    theBranchLoop
    theCommonRefusals ]
