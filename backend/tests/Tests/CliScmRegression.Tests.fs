/// SCM behaviour that a green suite cannot see, end to end.
///
/// These are end-to-end on purpose: what makes this kind of bug visible is running the commands and
/// reading what came back, so that is what these do.
///
/// The vocabulary is `Tests.CliDsl`. A test that needs something it does not have uses `runCli`.
module Tests.CliScmRegression

open System.Threading.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


// ─── branch isolation ─────────────────────────────────────────────────────


/// A cascade on a branch discovers dependents through the BRANCH's version of a name it has rebound,
/// so propagation never replaces the branch's own work with main's body.
let private propagationLeavesBranchWorkAlone =
  instanceTest
    "a cascade on a branch does not overwrite what the branch rebound"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Casc.base" "() : Int64 = 20"
        do! fn state "Tests.Casc.caller" "(): Int64 = Tests.Casc.base()"
        do! commit state "casc base"

        do! switch state "cascbr"
        do! fn state "Tests.Casc.caller" "() : Int64 = 777"
        do! commit state "branch caller"

        // The edit whose cascade must not reach main's caller and bind main's body here.
        do! fn state "Tests.Casc.base" "() : Int64 = 30"
        do!
          evals state "Tests.Casc.caller()" "777" "the branch keeps its own caller"

        do! onMain state
        do!
          evals state "Tests.Casc.caller()" "20" "and main still calls its own base"
        do! archiveBranches state [ "cascbr" ]
      })

/// Archiving a parent must not take its work out from under its children: their code would stop
/// resolving and their listing would show a parent that is not in this store.
let private archiveRefusesToOrphanAChild =
  instanceTest
    "archiving a parent with live children is refused, and names them"
    (fun state ->
      task {
        do! switch state "orphanpar"
        do! fn state "Tests.Orph.p" "() : Int64 = 1"
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

/// A `Deprecate` authored on a branch folds on that branch: the deprecation is visible there without
/// waiting for a merge.
let private deprecateIsVisibleOnItsBranch =
  instanceTest
    "a deprecation authored on a branch shows there, and travels on merge"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.DepBr.old" "() : Int64 = 1"
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

/// `discard <name>` on a branch drops only that name's draft ops, never committed ones, so the branch
/// keeps its last committed version.
let private branchDiscardKeepsCommittedWork =
  instanceTest
    "discard <name> on a branch spares what the branch already committed"
    (fun state ->
      task {
        do! start state
        do! switch state "discardkeep"
        do! fn state "Tests.DiscardKeep.f" "() : Int64 = 1"
        do! commit state "f v1 on branch"
        do! evals state "Tests.DiscardKeep.f()" "1" "the committed version runs"

        // A second, UNCOMMITTED edit: this is what discard is allowed to take.
        do! fn state "Tests.DiscardKeep.f" "() : Int64 = 2"
        do! discardName state "Tests.DiscardKeep.f"

        do!
          evals
            state
            "Tests.DiscardKeep.f()"
            "1"
            "the draft edit is gone and the committed version is back"
        do!
          lacks
            state
            [ "eval"; "Tests.DiscardKeep.f()" ]
            "not found"
            "the name itself survives the discard"

        do! onMain state
        do! archiveBranches state [ "discardkeep" ]
      })

/// `--include=` selects namings by name, not content hash, so an unrelated name that holds an
/// identical body does not ride along or get reported as needed by what you named.
let private partialCommitTakesNamesNotBodies =
  instanceTest "--include= leaves an unrelated name that shares a body" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Part.public" "() : Int64 = 987"
      do! fn state "Tests.Part.private" "() : Int64 = 987"

      // The preview lists the whole draft, which is right; what must not appear is the
      // "also committed" line, which is where the unrelated name would be reported as needed.
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
        do! fn state "Tests.SharedCommit.committed" "() : Int64 = 19327"
        do! commit state "existing dependency content"
        do! fn state "Tests.SharedCommit.dep" "() : Int64 = 19327"
        do! fn state "Tests.SharedCommit.unrelated" "() : Int64 = 19327"
        do!
          fn
            state
            "Tests.SharedCommit.caller"
            "(): Int64 = Tests.SharedCommit.dep() + 1"

        do!
          commitOnly
            state
            "caller and its dependency name"
            "Tests.SharedCommit.caller"
        do! evals state "Tests.SharedCommit.caller()" "19328" "the caller runs"
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

/// `undo` writes a `Decision`, so `discard <name>` has to recognise that as well as `SetName`, or it
/// reports the change dropped and changes nothing.
let private discardSeesWhatUndoWrote =
  instanceTest "discard <name> drops what undo staged" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Undo.f" "() : Int64 = 1"
      do! commit state "undo v1"
      do! fn state "Tests.Undo.f" "() : Int64 = 2"
      do! commit state "undo v2"

      do! run state [ "undo"; "Tests.Undo.f" ]
      do! evals state "Tests.Undo.f()" "1" "undo staged the older version"

      do! discardName state "Tests.Undo.f"
      do!
        evals
          state
          "Tests.Undo.f()"
          "2"
          "discarding it puts the committed version back"
      do! clean state "and the draft is empty"
    })

/// `discard` decides the draft is empty by counting ops, not changed NAME bindings, so a draft
/// holding only a decision (a pin, a deprecation, an ack) is discarded too.
let private discardCountsOpsNotNames =
  instanceTest "discard sees a draft that holds only a decision" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Dec.f" "() : Int64 = 1"
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

/// A branch whose commit the at-rest type check REFUSES cannot be merged either, or code that does
/// not typecheck lands on main with nothing recorded. Merge asks what commit asks,
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

/// A rename should move the name in one operation, keeping the same content.
let private renameMovesANameNotItsContent =
  instanceTest "rename moves a name and leaves its callers alone" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Rn.old" "() : Int64 = 42"
      do! fn state "Tests.Rn.caller" "(): Int64 = Tests.Rn.old()"
      do! commit state "rn base"

      do!
        shows
          state
          [ "rename"; "Tests.Rn.old"; "Tests.Rn.renamed" ]
          "renamed"
          "it says what it did"
      do! evals state "Tests.Rn.renamed()" "42" "the new name holds the content"
      do! evals state "Tests.Rn.caller()" "42" "the caller is untouched"
      do! notFound state "Tests.Rn.old()" "and the old name is gone"

      do! start state
    })

/// The two refusals, which are the reason a rename cannot quietly take something off the shelf.
let private renameRefusesTheBadCases =
  instanceTest "rename refuses a missing name and an occupied one" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Rn2.a" "() : Int64 = 1"
      do! fn state "Tests.Rn2.b" "() : Int64 = 2"
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
      do! evals state "Tests.Rn2.a()" "1" "the source name is untouched"
      do! evals state "Tests.Rn2.b()" "2" "and so is the occupied one"
      do! start state
    })


// ─── the everyday path ────────────────────────────────────────────────────

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
  [ propagationLeavesBranchWorkAlone
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
    theCommonRefusals ]
