/// The multi-instance behaviour that only DARK can answer for.
///
/// Everything here runs Dark against whichever instance is active, which the F#-only half cannot
/// do: the detector, the merge-event queue, and the reporting that reads its own store.
module Tests.MultiInstanceDark


open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module Seed = LibDB.Seed
module Inserts = LibDB.Inserts
module Branches = LibDB.Branches
module Queries = LibDB.Queries
module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module Hashing = LibSerialization.Hashing.Hashing

open TestUtils.TestUtils



open Tests.MultiInstanceHarness
// --- the Dark half -------------------------------------------------------------------------------
//
// Everything below runs DARK against whichever instance is active, which the F#-only tests above
// cannot do. It works because `Builtin.localDbPath` follows the store swap, so `Stdlib.Sqlite` and
// the F# connection are on the same file.

/// Run <param code> against the ACTIVE instance and return its answer as a string.
let private darkOn (code : string) : Task<string> =
  task {
    match! evalDarkExpr code with
    | Ok dv -> return string dv
    | Error(rte, _) -> return failtest $"the Dark call failed: {rte}\n  code: {code}"
  }

/// Dark source for a pending same-name-different-hash conflict on TwoStore.Cascade,
/// between a local and an incoming candidate, auto-resolved to the incoming one.
let private conflictLiteral
  (id : string)
  (name : string)
  (mine : string)
  (theirs : string)
  : string =
  "Darklang.SCM.Conflicts.Conflict { id = \""
  + id
  + "\"; owner = \"TwoStore\"; "
  + "modules = \"Cascade\"; name = \""
  + name
  + "\"; itemType = \"fn\"; part = \"\"; kind = \"same-name-different-hash\"; "
  + "candidates = [ Darklang.SCM.Conflicts.Candidate { side = \"local\"; hash = \""
  + mine
  + "\"; text = \"\"; originTs = \"\"; author = \"\" }; "
  + "Darklang.SCM.Conflicts.Candidate { side = \"incoming\"; hash = \""
  + theirs
  + "\"; text = \"\"; originTs = \"\"; author = \"\" } ]; "
  + "autoResolvedTo = \""
  + theirs
  + "\"; reason = \"\"; status = \"pending\"; resolvedBy = \"\" }"

/// The Dark conflict detector, run on the receiving store, over two edits to one name made
/// independently on two stores.
///
/// `SCM.Conflicts` decides which side wins and whether the divergence is even a conflict, and it is
/// Dark. If the Dark side did not follow the store swap it would answer about the DEFAULT store
/// whatever a test had activated, plausibly and wrongly, so this asserts the swap itself before any
/// test leans on it.
let private theDarkDetectorSeesTheStoreItIsOn =
  twoStoreTest
    "Dark reads the instance it was pointed at, not the store the process started on"
    (fun a b ->
      task {
        // Two stores, one name, two different bodies, B's authored later.
        activate a
        let! _ =
          receive [ wireOp (setName "seen" "from-a") "2026-01-01T00:00:00.000Z" ]

        activate b
        let! _ =
          receive [ wireOp (setName "seen" "from-b") "2026-01-02T00:00:00.000Z" ]

        // A branch-aware read, in Dark, answers about the store it is on. Deliberately NOT asserted via
        // `identity ()`: a chosen identity survives being copied, by design, so two copies of a store
        // whose name was set by hand legitimately share one, and the assertion would depend on whether
        // anything else in the run had named the dev store.
        let liveHere =
          "Darklang.SCM.PackageOps.liveBindingFor Darklang.SCM.Branch.mainBranchId "
          + "(Darklang.LanguageTools.ProgramTypes.PackageLocation "
          + "{ owner = \"MultiInstance\"; modules = [\"Converge\"]; name = \"seen\" })"
        let! bSees = darkOn liveHere
        let (PT.Hash fromB) = hashOf "from-b"
        Expect.stringContains bSees fromB "B's Dark read sees B's binding"

        activate a
        let! aSees = darkOn liveHere
        let (PT.Hash fromA) = hashOf "from-a"
        Expect.stringContains aSees fromA "and A's sees A's, in the same process"
      })

/// A merge event that arrives BEFORE the branch it merged still lands when the branch shows up.
///
/// `dark pull` (main, which carries the event) and `dark branch pull` (the bundle) are separate
/// commands, and that order is the natural one. Folded against a store with none of the branch's ops
/// tagged, the event has nothing to flip; marking itself applied there would retire it for good, so
/// the merger's main would hold the work, this store would not, and `dark branches` would go on
/// showing the branch as live with nothing saying otherwise on either side.
let private aMergeEventWaitsForItsBranch =
  oneStoreTest
    "a merge event that arrives before its branch applies when the branch lands"
    "b"
    (fun b ->
      task {
        let x = PT.BranchId.Id(System.Guid.NewGuid())
        activate b
        // The branch is REGISTERED here (a peer told us it exists) but holds no ops yet.
        do! Branches.createBranch x "early-event" PT.BranchId.Main
        let op = setName "early" "e1"
        let opId = Inserts.computeOpHash op
        let event =
          PT.PackageOp.BranchEvent(
            x,
            PT.Merged [ opId ],
            "2026-01-02T00:00:00.000Z"
          )

        // Main first: the event folds with nothing to flip.
        let! _ = receive [ wireOp event "2026-01-02T00:00:00.000Z" ]
        let! notYet = boundHash "early"
        Expect.isNone notYet "nothing is live yet: the op it names has not arrived"

        let! stillPending = appliedFlag (string (Inserts.computeOpHash event))
        // 2 is DEFERRED: folded, did nothing, waiting. Not 0, which would make the fold loop chase it
        // forever and raise "did not settle".
        Expect.equal
          stillPending
          2L
          "and the event is deferred, waiting for its branch"

        // Now the bundle. Storing it, re-arming and folding is what `scmImportBranchOps` does.
        let! _ = Branches.storeDeltaOps x [ op ]
        do! Branches.undeferBranchEvents x
        let! _ = Seed.applyUnappliedOps ()

        let! landed = boundHash "early"
        let (PT.Hash e1) = hashOf "e1"
        Expect.equal
          landed
          (Some e1)
          "the merge lands the moment its branch's ops do"

        let! nowApplied = appliedFlag (string (Inserts.computeOpHash event))
        Expect.equal
          nowApplied
          1L
          "and the event is applied once it has done its work"
      })

/// A store that reconnects from nothing meets every merge event before every branch. The event for
/// a branch it has never heard of folds to nothing and marks itself done, correctly; when the bundle
/// then registers the branch, the event has to run again or the branch reads as live and empty for
/// good. Seen on a real machine: two merged branches listed as live with 0 ops after a wipe and a
/// reconnect.
let aMergeEventForALaterBranchStillApplies =
  oneStoreTest
    "a merge event that predates any knowledge of its branch applies once the bundle lands"
    "b"
    (fun b ->
      task {
        let x = PT.BranchId.Id(System.Guid.NewGuid())
        activate b
        // No `createBranch`: this store has never heard of x when the event arrives.
        let op = setName "from-nowhere" "n1"
        let event =
          PT.PackageOp.BranchEvent(
            x,
            PT.Merged [ Inserts.computeOpHash op ],
            "2026-01-02T00:00:00.000Z"
          )
        let! _ = receive [ wireOp event "2026-01-02T00:00:00.000Z" ]
        let! applied = appliedFlag (string (Inserts.computeOpHash event))
        Expect.equal
          applied
          1L
          "an event for an unknown branch is applied and done, not parked"

        // The bundle: the branch registers, its ops land inert, and the import re-arms the event.
        do! Branches.createBranch x "late-branch" PT.BranchId.Main
        let! _ = Branches.storeDeltaOps x [ op ]
        do! Branches.undeferBranchEvents x
        let! _ = Seed.applyUnappliedOps ()

        let! landed = boundHash "from-nowhere"
        let (PT.Hash n1) = hashOf "n1"
        Expect.equal
          landed
          (Some n1)
          "the merged work is live on main once its branch arrives"
      })

/// The other half of the rule: an event for a branch this store has never heard of folds to nothing
/// and STAYS applied. Waiting for ops that will never come would re-decode it at every startup.
let private anEventForAnUnknownBranchDoesNotWait =
  oneStoreTest
    "a merge event for a branch this store never had is applied and done"
    "b"
    (fun b ->
      task {
        activate b
        let stranger = PT.BranchId.Id(System.Guid.NewGuid())
        let op = setName "not-ours" "n1"
        let event =
          PT.PackageOp.BranchEvent(
            stranger,
            PT.Merged [ Inserts.computeOpHash op ],
            "2026-01-02T00:00:00.000Z"
          )
        let! _ = receive [ wireOp event "2026-01-02T00:00:00.000Z" ]

        let! applied = appliedFlag (string (Inserts.computeOpHash event))
        Expect.equal
          applied
          1L
          "a colleague's private branch is none of this store's business"
      })

/// An override cascades the way an edit does.
///
/// Two stores edit one fn; the later stamp wins on both, and the loser overrides back to its own
/// version. Overriding rebinds the NAME. A caller of that fn had followed the winner's version by
/// hash when the pull landed; without a cascade it goes on calling the winner's version while the
/// name says otherwise, on every machine the override reaches. Seen on two real machines: `render`
/// bound the override, `summary` printed the other side's output.
let anOverrideRepointsCallers =
  oneStoreTest
    "overriding a conflict repoints the callers that had followed the loser"
    "a"
    (fun a ->
      task {
        activate a
        // One fn, one caller of it. The test helper authors without the cascade the `fn` verb runs,
        // so each step that would have cascaded in real use does so here by hand, through the same
        // wrapper the verb uses.
        let! shared =
          authorIntoMain (
            "module TwoStore.Cascade\n\n"
            + "let base (x: Int64) : Int64 = x + 1L\n"
            + "let caller (x: Int64) : Int64 = TwoStore.Cascade.base x\n"
          )
        let! _ = LibDB.Inserts.commitAllAsBaseline "shared"
        let (PT.Hash origHash) = hashBoundTo shared "base"
        let repoint (fromHash : string) (toHash : string) =
          darkOn (
            "Darklang.SCM.Propagation.repointDependents Darklang.SCM.Branch.mainBranchId "
            + "(Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"TwoStore\"; modules = [\"Cascade\"]; name = \"base\" }) "
            + "Darklang.LanguageTools.ProgramTypes.ItemKind.Fn "
            + "[ Darklang.LanguageTools.ProgramTypes.Hash.Hash \""
            + fromHash
            + "\" ] "
            + "(Darklang.LanguageTools.ProgramTypes.Hash.Hash \""
            + toHash
            + "\")"
          )

        // "Mine": this store's edit, cascaded. "Theirs": a later edit that arrives by sync and wins
        // by stamp; a real pull would also bring the sender's cascaded caller, so cascade here too.
        let! mine =
          authorIntoMain
            "module TwoStore.Cascade\n\nlet base (x: Int64) : Int64 = x + 10L\n"
        let (PT.Hash mineHash) = hashBoundTo mine "base"
        let! _ = repoint origHash mineHash
        // Their version has to be real content or the caller cannot be repointed at it.
        let! theirsOps =
          authorIntoMain
            "module TwoStore.Cascade\n\nlet base (x: Int64) : Int64 = x + 100L\n"
        let (PT.Hash theirsHash) = hashBoundTo theirsOps "base"
        let! _ = repoint mineHash theirsHash
        let! before = darkOn "TwoStore.Cascade.caller 0L"
        Expect.stringContains
          before
          "100"
          "before the override, the caller follows the winning version"

        // The override, through the real resolve path, back to mine.
        let! _ =
          darkOn (
            "let c = "
            + conflictLiteral "cascade01" "base" mineHash theirsHash
            + "\nDarklang.SCM.PackageOps.settleConflict Darklang.SCM.Branch.mainBranchId c \""
            + mineHash
            + "\""
          )

        let! baseNow = darkOn "TwoStore.Cascade.base 0L"
        Expect.stringContains
          baseNow
          "10L"
          "the name binds my version after the override"
        let! callerNow = darkOn "TwoStore.Cascade.caller 0L"
        Expect.stringContains
          callerNow
          "10L"
          "and the caller follows it, rather than still calling the other version by hash"
        Expect.isFalse
          (callerNow.Contains "100")
          "which means it no longer calls the loser"

        // The cascade also settles the conflicts it makes moot. On two machines the caller's own
        // conflict (each side's cascade had made a different caller) stayed pending after the override
        // rebound the caller to a third version, asking for a choice between two versions nobody ran.
        // A conflict whose live binding is neither candidate is superseded; one whose live binding is a
        // candidate is still a real question.
        let! _ =
          darkOn (
            "Darklang.SCM.Conflicts.record Darklang.SCM.Branch.mainBranchId [ "
            + conflictLiteral "moot01" "caller" "aaaa" "bbbb"
            + "; "
            + conflictLiteral "live01" "base" mineHash theirsHash
            + " ]"
          )
        let! (stillPending : string) =
          darkOn
            "Darklang.SCM.Conflicts.pending () |> Stdlib.List.map (fun c -> c.id)"
        Expect.isFalse
          (stillPending.Contains "moot01")
          $"the caller's conflict, rebound past both candidates, is settled: {stillPending}"
        Expect.stringContains
          stillPending
          "live01"
          $"the one whose live binding is a candidate stays: {stillPending}"
        let! status =
          Sql.query "SELECT status FROM conflicts WHERE id = 'moot01'"
          |> Sql.executeRowAsync (fun read -> read.string "status")
        Expect.equal
          status
          "superseded"
          "and it is recorded as superseded, not deleted"
      })

/// Main sync carries the author's commit, so the same work reads the same on every store.
///
/// A's ops arrive on B by the main channel and are filed under A's commit id and message, not under
/// an import commit of B's; the import commit that landed them is deleted once nothing is left under
/// it. Before, `dark commits` on B read "synced from A" for everything A ever did, under ids B minted.
let mainSyncCarriesTheAuthorsCommit =
  twoStoreTest
    "ops synced on main are filed under the author's commit on the other store"
    (fun a b ->
      task {
        activate a
        let! _ =
          authorIntoMain
            "module TwoStore.Attrib\n\nlet f (x: Int64) : Int64 = x + 7L\n"
        let! aCommit = LibDB.Inserts.commitAllAsBaseline "f, by a"
        // What A would put on the wire for this commit: the ops, and the commit row they name. Through a
        // file rather than a string literal, as `sync export` does; the bundle is JSON with quotes in it.
        let path =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            "dark-attrib-bundle.json"
          )
        let! (written : string) =
          darkOn (
            "let json = Darklang.SCM.Wire.exportOps () |> Stdlib.List.filter (fun o -> o.commit == \""
            + aCommit
            + "\") |> Darklang.SCM.Wire.wireEncode\n"
            + "match Stdlib.Cli.File.writeText \""
            + path
            + "\" json with | Ok _ -> \"written\" | Error e -> e.message"
          )
        Expect.stringContains written "written" $"a wrote its bundle: {written}"

        activate b
        let! (imported : string) =
          darkOn (
            "match Darklang.SCM.Wire.wireDecode (Stdlib.String.toBlob (Builtin.unwrap (Stdlib.Cli.File.readText \""
            + path
            + "\"))) with\n"
            + "| Ok bundle -> (match Darklang.SCM.Wire.importFrom \"peer:a\" bundle.ops bundle.commits with | Ok o -> Stdlib.Int.toString o.imported | Error e -> e)\n"
            + "| Error e -> e"
          )
        Expect.isFalse
          (imported.Contains "not")
          $"the import went through: {imported}"

        let! filedUnder =
          Sql.query
            "SELECT COALESCE(commit_hash, '') AS c FROM package_ops p
             WHERE p.id IN (SELECT op_id FROM locations WHERE owner = 'TwoStore' AND modules = 'Attrib')"
          |> Sql.executeAsync (fun read -> read.string "c")
        Expect.isNonEmpty filedUnder "b has the ops"
        Expect.allEqual
          filedUnder
          aCommit
          "and files them under a's commit id, not its own"

        let! message =
          Sql.query "SELECT message FROM commits WHERE hash = @h"
          |> Sql.parameters [ "h", Sql.string aCommit ]
          |> Sql.executeRowAsync (fun read -> read.string "message")
        Expect.equal message "f, by a" "with a's message"

        let! leftover =
          Sql.query
            "SELECT count(*) AS n FROM commits WHERE message LIKE 'synced from%' OR message LIKE 'imported from%'"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        Expect.equal
          leftover
          0L
          "and the import commit, left with nothing under it, is gone"
      })

/// The two "your decision was superseded" reports check WHO acted, not only what happened.
///
/// A peer's policy with no local one before it supersedes nothing of yours; your own later edit is a
/// change of mind, which closes the record rather than reporting you to yourself. Without either rule
/// the store greets a fresh puller with findings about choices they never made -- which is exactly how
/// it presented, on a store whose whole history had arrived by sync.
let supersededReportsCheckAuthorship =
  oneStoreTest
    "a peer's decision alone, or your own later edit, is not a superseded-decision finding"
    "a"
    (fun a ->
      task {
        activate a
        // A peer's pin arrives with an author; no local choice preceded it.
        let! _ =
          darkOn (
            "let op = Darklang.LanguageTools.ProgramTypes.PackageOp.Decision(\"peerpin01\", "
            + "Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"TwoStore\"; modules = [\"Sup\"]; name = \"f\" }, \"\", "
            + "Darklang.LanguageTools.ProgramTypes.DecisionKind.Propagation Darklang.LanguageTools.ProgramTypes.PropagationPolicy.Pin) in "
            + "Darklang.SCM.PackageOps.add Darklang.SCM.Branch.mainBranchId [ op ]"
          )
        do!
          execSql
            "INSERT INTO op_owners (op_id, owner)
             SELECT id, 'peer-1' FROM package_ops
             WHERE id NOT IN (SELECT op_id FROM op_owners)
               AND substr(op_blob, 9, 1) = X'0B'"
        // A second peer replaces the first: still nobody's surprise but theirs. This is the arm the
        // authorship check exists for -- without it the first peer's choice is recorded as "yours".
        let! _ =
          darkOn (
            "let op = Darklang.LanguageTools.ProgramTypes.PackageOp.Decision(\"peerpin02\", "
            + "Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"TwoStore\"; modules = [\"Sup\"]; name = \"f\" }, \"\", "
            + "Darklang.LanguageTools.ProgramTypes.DecisionKind.Propagation Darklang.LanguageTools.ProgramTypes.PropagationPolicy.Follow) in "
            + "Darklang.SCM.PackageOps.add Darklang.SCM.Branch.mainBranchId [ op ]"
          )
        do!
          execSql
            "INSERT INTO op_owners (op_id, owner)
             SELECT id, 'peer-2' FROM package_ops
             WHERE id NOT IN (SELECT op_id FROM op_owners)
               AND substr(op_blob, 9, 1) = X'0B'"
        let! (policies : string) =
          darkOn
            "Darklang.SCM.PackageOps.supersededPolicies () |> Stdlib.List.length |> Stdlib.Int.toString"
        Expect.equal
          policies
          "DString \"0\""
          $"a peer's first-and-only choice supersedes nothing of yours: {policies}"

        // Your own override, then your own edit: a change of mind, not a finding.
        let! ops =
          authorIntoMain
            "module TwoStore.Sup\n\nlet g (x: Int64) : Int64 = x + 1L\n"
        let (PT.Hash gHash) = hashBoundTo ops "g"
        let! _ =
          darkOn (
            "let op = Darklang.LanguageTools.ProgramTypes.PackageOp.Decision(\"selfov01\", "
            + "Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"TwoStore\"; modules = [\"Sup\"]; name = \"g\" }, \"\", "
            + "Darklang.LanguageTools.ProgramTypes.DecisionKind.Override (Darklang.LanguageTools.ProgramTypes.Reference.PackageFn (Darklang.LanguageTools.ProgramTypes.Hash.Hash \""
            + gHash
            + "\"))) in "
            + "Darklang.SCM.PackageOps.add Darklang.SCM.Branch.mainBranchId [ op ]"
          )
        // The conflicts-table guard is what makes the fold run at all.
        do!
          execSql
            "INSERT INTO conflicts (id, owner, modules, name, item_type, kind, candidates, auto_resolved_to, reason, status, origin_ts, branch_id)
             VALUES ('selfov01', 'TwoStore', 'Sup', 'g', 'fn', 'same-name-different-hash', '[]', '', '', 'overridden',
                     strftime('%Y-%m-%dT%H:%M:%fZ','now'), '00000000-0000-0000-0000-000000000001')"
        let! _ =
          authorIntoMain
            "module TwoStore.Sup\n\nlet g (x: Int64) : Int64 = x + 2L\n"
        let! (overrides : string) =
          darkOn
            "Darklang.SCM.PackageOps.supersededOverrides () |> Stdlib.List.length |> Stdlib.Int.toString"
        Expect.equal
          overrides
          "DString \"0\""
          "editing past your own override is a change of mind, not a finding"
      })

/// An import whose insert fails leaves no commit row behind: the row is minted before the insert
/// (the insert stamps against it), so the failure arm has to take it back out.
let aFailedImportLeavesNoCommit =
  oneStoreTest
    "an import that fails leaves no synced-from commit naming ops that never arrived"
    "a"
    (fun a ->
      task {
        activate a
        let! (before : string) =
          darkOn
            "Stdlib.Sqlite.scalarInt (Stdlib.LocalStore.path ()) \"SELECT count(*) AS n FROM commits\" \"n\" |> Stdlib.Option.withDefault 0L |> Stdlib.Int64.toString"
        // blobHex that is not hex: `scmImportOps` throws inside, and importFrom's error arm runs.
        let! (result : string) =
          darkOn (
            "let op = Darklang.SCM.Wire.SyncOp { id = \"7c9e6679-7425-40de-944b-e07fc1f90ae9\"; blobHex = \"zznothex\"; ts = \"2026-01-01T00:00:00.000Z\"; author = \"peer-1\"; commit = \"\" } in "
            + "match Darklang.SCM.Wire.importFrom \"peer:x\" [ op ] [] with | Ok _ -> \"ok\" | Error e -> \"error\""
          )
        let! (after : string) =
          darkOn
            "Stdlib.Sqlite.scalarInt (Stdlib.LocalStore.path ()) \"SELECT count(*) AS n FROM commits\" \"n\" |> Stdlib.Option.withDefault 0L |> Stdlib.Int64.toString"
        Expect.equal
          after
          before
          $"no commit row survives a failed import (import said: {result})"
      })

let tests =
  testSequenced
  <| testList
    "MultiInstanceDark"
    [ theDarkDetectorSeesTheStoreItIsOn
      aMergeEventWaitsForItsBranch
      aMergeEventForALaterBranchStillApplies
      anEventForAnUnknownBranchDoesNotWait
      anOverrideRepointsCallers
      mainSyncCarriesTheAuthorsCommit
      supersededReportsCheckAuthorship
      aFailedImportLeavesNoCommit ]
