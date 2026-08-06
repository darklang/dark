/// Proof for the branches-as-overlays model (notes/fresh-arch/branches-concurrency.md).
/// A "branch" is its delta ops overlaid on a shared core PackageManager (PM.withExtraOps).
/// Two properties concurrent agents depend on:
///   - ISOLATION FROM CORE: a fn authored on a branch overlay resolves + EXECUTES there, but
///     is invisible to the shared core -- it never leaks into main.
///   - ISOLATION BETWEEN BRANCHES: two overlays over the SAME core see only their own defs;
///     neither can resolve OR fetch the other's, which is what lets N agents run N branches
///     concurrently over a shared read-only core.
module Tests.BranchOverlay

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module PM = LibDB.PackageManager
module HS = LibDB.HashStabilization
module Package = LibParser.Package
module NR = LibParser.NameResolver
module Branches = LibDB.Branches
module Queries = LibDB.Queries
module Seed = LibDB.Seed
module BS = LibSerialization.Binary.Serialization

open TestUtils.TestUtils

// A branch's source: one fn `foo` returning `answer`, computed via a CORE call
// (Stdlib.Int64.add), so executing its body ALSO proves the overlay resolves core names.
let private branchSource (answer : int) : string =
  $"""module Darklang.BranchTestOverlay

let foo (x: Int64) : Int64 = Stdlib.Int64.add {answer - 2}L 2L"""

let private fooLoc : PT.PackageLocation =
  { owner = "Darklang"; modules = [ "BranchTestOverlay" ]; name = "foo" }

// A source whose module name varies, so two branches can author DISTINCT fns.
let private namedSource (modName : string) (answer : int) : string =
  $"""module Darklang.{modName}

let foo (x: Int64) : Int64 = Stdlib.Int64.add {answer - 2}L 2L"""

let private fooLocIn (modName : string) : PT.PackageLocation =
  { owner = "Darklang"; modules = [ modName ]; name = "foo" }

/// Parse a branch source into stabilized ops: the real authoring path, SCC-aware hashes and all.
let private opsFor (source : string) : Task<List<PT.PackageOp>> =
  task {
    let builtins = localBuiltIns pmPT
    let! parsed =
      Package.parse builtins pmPT NR.OnMissing.ThrowError source |> Ply.toTask
    match parsed with
    | Ok ops -> return HS.computeRealHashes ops
    | Error errs ->
      return Exception.raiseInternal "branch parse failed" [ "errs", errs ]
  }

/// Execute the body of `foo` from a set of ops against `pm`. Proves the branch's code runs.
let private runFooBody
  (pm : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : Task<RT.Dval> =
  task {
    let body =
      ops
      |> List.tryPick (fun op ->
        match op with
        | PT.PackageOp.AddFn f -> Some f.body
        | _ -> None)
      |> Option.defaultWith (fun () ->
        Exception.raiseInternal "no AddFn op in branch ops" [])
    let! (state : RT.ExecutionState) = executionStateFor pm false Map.empty
    let rtExpr = PT2RT.Expr.toRT Map.empty 0 None body
    match! Exe.executeExpr state rtExpr with
    | Ok dv -> return dv
    | Error(rte, _) ->
      return Exception.raiseInternal "foo body errored" [ "rte", rte ]
  }


let isolationFromCore =
  testTask "a branch fn resolves + executes on its overlay but is INVISIBLE to core" {
    let! ops = opsFor (branchSource 42)
    let branch = PM.withExtraOps pmPT ops

    let! onBranch = branch.findFn fooLoc |> Ply.toTask
    Expect.isSome onBranch "foo resolves on the branch overlay"
    let! onCore = pmPT.findFn fooLoc |> Ply.toTask
    Expect.isNone onCore "foo does NOT leak into the shared core"

    let! dv = runFooBody branch ops
    Expect.equal dv (RT.DInt64 42L) "the branch fn's code runs -> 42"
  }

let isolationBetweenBranches =
  testTask
    "two overlays over one core see only their own defs (concurrent-branch isolation)" {
    let! opsA = opsFor (branchSource 42)
    let! opsB = opsFor (branchSource 99)
    let branchA = PM.withExtraOps pmPT opsA
    let branchB = PM.withExtraOps pmPT opsB

    let! hA = branchA.findFn fooLoc |> Ply.toTask
    let! hB = branchB.findFn fooLoc |> Ply.toTask
    Expect.isSome hA "A resolves its own foo"
    Expect.isSome hB "B resolves its own foo"
    Expect.notEqual
      hA
      hB
      "different bodies -> different content hashes (the branches diverge)"

    // Neither can fetch the other's def: an overlay holds only its own ops, falling back to core.
    let! aHasB = branchA.getFn (Option.get hB) |> Ply.toTask
    let! bHasA = branchB.getFn (Option.get hA) |> Ply.toTask
    Expect.isNone aHasB "branch A cannot fetch branch B's fn"
    Expect.isNone bHasA "branch B cannot fetch branch A's fn"

    let! dvA = runFooBody branchA opsA
    let! dvB = runFooBody branchB opsB
    Expect.equal dvA (RT.DInt64 42L) "branch A -> 42"
    Expect.equal dvB (RT.DInt64 99L) "branch B -> 99"
  }

/// Wipe every trace of a test branch: its ops, its frontier tags, its per-name bases, its registry
/// row. Called defensively before AND after, since the store is shared. The ONE place in this file
/// allowed to spell these tables out, so a new branch table is handled here once for every test.
let private cleanupBranch (branchId : string) : Task<unit> =
  task {
    let del (sql : string) =
      Sql.query sql
      |> Sql.parameters [ "b", Sql.string branchId ]
      |> Sql.executeStatementAsync

    do!
      del
        "DELETE FROM package_ops WHERE id IN (SELECT op_id FROM op_branches WHERE branch_id = @b)"
    do! del "DELETE FROM op_branches WHERE branch_id = @b"
    do! del "DELETE FROM branch_name_bases WHERE branch_id = @b"
    do! del "DELETE FROM branches WHERE id = @b"
  }

/// Pretend the parent moved every name this branch touched, by staling the recorded bases. Doing it
/// this way rather than actually changing the parent keeps the conflict tests off the shared main
/// projection every other test here reads concurrently.
let private staleNameBases (branchId : string) : Task<unit> =
  Sql.query
    "UPDATE branch_name_bases SET base_hash = 'stalehash' WHERE branch_id = @b"
  |> Sql.parameters [ "b", Sql.string branchId ]
  |> Sql.executeStatementAsync

let storeThenOverlay =
  testTask
    "a branch's ops round-trip through the store (effective=0) and overlay to resolve foo" {
    let branchId = "test-branch-store-1"
    do! cleanupBranch branchId

    do! Branches.createBranch branchId "store-proof" "main"
    let! byName = Branches.branchIdForName "store-proof"
    Expect.equal byName (Some branchId) "branch resolves by its name alias"

    let! ops = opsFor (branchSource 42)
    let! stored = Branches.storeDeltaOps branchId ops
    Expect.isGreaterThan stored 0L "ops stored to the branch frontier"

    // stored effective=0 -> in the log, NOT folded into core, so core can't resolve foo.
    let! onCore = pmPT.findFn fooLoc |> Ply.toTask
    Expect.isNone onCore "foo is NOT folded into main (effective=0)"

    let! loaded = Branches.loadDeltaOps branchId
    let overlay = PM.withExtraOps pmPT loaded
    let! onBranch = overlay.findFn fooLoc |> Ply.toTask
    Expect.isSome onBranch "foo resolves via the branch loaded from the store"

    do! cleanupBranch branchId
  }

/// Count a branch's frontier ops at a given effective flag (cache-free, direct SQL).
let private countEffective (branchId : string) (eff : int) : Task<int64> =
  Sql.query
    "SELECT count(*) AS n FROM package_ops p
     JOIN op_branches b ON b.op_id = p.id
     WHERE b.branch_id = @b AND p.effective = @e"
  |> Sql.parameters [ "b", Sql.string branchId; "e", Sql.int eff ]
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

let markMergedFlipsEffective =
  testTask
    "merge half-1: markMergedEffective flips a branch's ops effective 0->1 (fold does the rest)" {
    let branchId = "test-branch-flip-1"
    do! cleanupBranch branchId

    do! Branches.createBranch branchId "flip-proof" "main"
    let! ops = opsFor (branchSource 42)
    let! _ = Branches.storeDeltaOps branchId ops

    let! pending = countEffective branchId 0
    Expect.isGreaterThan pending 0L "stored ops start effective=0 (branch-pending)"

    let! flipped = Branches.markMergedEffective branchId
    Expect.equal flipped pending "all pending ops flip to effective=1"
    let! stillPending = countEffective branchId 0
    Expect.equal stillPending 0L "none left effective=0"

    // (the fold -- Seed.applyUnappliedOps -- is what then brings foo into main; run in a fresh
    // process by `dark merge`, not here, so this test never pollutes the shared main projection.)
    do! cleanupBranch branchId
  }

/// Concurrent `resolveOrCreate` for one name yields ONE branch. The case `DARK_BRANCH` makes
/// ordinary: several agents start in their own shells with the same branch name exported and all
/// reach for it at once. If each minted its own, they would author into different branches while
/// believing they shared one.
let concurrentCreateYieldsOneBranch =
  testTask "racing to create one branch name produces one branch" {
    let name = "race-one-name"
    let! existing = Branches.idForName name
    match existing with
    | Some id -> do! cleanupBranch id
    | None -> ()

    let! results =
      Array.init 8 (fun _ -> Branches.resolveOrCreate name "main")
      |> System.Threading.Tasks.Task.WhenAll

    let ids = results |> Array.map fst |> Array.distinct
    Expect.equal ids.Length 1 "every caller got the SAME branch id"

    let createdCount = results |> Array.filter snd |> Array.length
    Expect.equal createdCount 1 "and exactly one of them reports having created it"

    // The store agrees, not just the return values.
    let! live =
      Sql.query
        "SELECT count(*) AS n FROM branches
         WHERE name = @name AND archived_at IS NULL AND merged_at IS NULL"
      |> Sql.parameters [ "name", Sql.string name ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.equal live 1L "one live row for the name"

    do! cleanupBranch ids[0]
  }


/// A merged branch stays addressable by name; a switch under that name starts a new one. A UX
/// contract, not an implementation detail: `dark branches` lists merged branches, so every verb
/// that takes a branch name has to accept the names it just printed.
let mergedBranchStaysAddressable =
  testTask "a merged branch resolves by name for reads, but not as a switch target" {
    do! cleanupBranch "mergedName"
    do! cleanupBranch "mergedName2"

    do! Branches.createBranch "mergedName" "reuse-me" "main"
    let! live = Branches.liveIdForName "reuse-me"
    Expect.equal
      live
      (Some "mergedName")
      "live branch resolves for both reads and writes"

    do!
      Sql.query "UPDATE branches SET merged_at = datetime('now') WHERE id = @b"
      |> Sql.parameters [ "b", Sql.string "mergedName" ]
      |> Sql.executeStatementAsync

    let! readSide = Branches.idForName "reuse-me"
    Expect.equal
      readSide
      (Some "mergedName")
      "merged branch still answers a read verb by name"

    let! writeSide = Branches.liveIdForName "reuse-me"
    Expect.isNone
      writeSide
      "but is NOT what `switch <name>` lands on -- its work is already merged"

    let! merged = Branches.isMerged "mergedName"
    Expect.isTrue
      merged
      "and reports itself merged, so `merge` can say so instead of flipping nothing"

    // Reusing the name starts a separate branch, and reads then mean the NEW one.
    do! Branches.createBranch "mergedName2" "reuse-me" "main"
    let! afterReuse = Branches.idForName "reuse-me"
    Expect.equal
      afterReuse
      (Some "mergedName2")
      "the most recent branch wins the name"

    // Archiving is different from merging: it discards the ops, so there is nothing left to address.
    do!
      Sql.query "UPDATE branches SET archived_at = datetime('now') WHERE id = @b"
      |> Sql.parameters [ "b", Sql.string "mergedName2" ]
      |> Sql.executeStatementAsync
    let! afterArchive = Branches.idForName "reuse-me"
    Expect.equal
      afterArchive
      (Some "mergedName")
      "archived branches drop out of name resolution"

    do! cleanupBranch "mergedName"
    do! cleanupBranch "mergedName2"
  }


let processOverlaySelects =
  testTask
    "the branch a process is on resolves through its overlay; leaving it stops that" {
    PM.selectBranch None // ensure clean start (process-global)
    let! before =
      (PM.ptForBranch (PM.currentBranchId ())).findFn fooLoc |> Ply.toTask
    Expect.isNone before "no branch active -> foo unresolved (core only)"

    let! ops = opsFor (branchSource 42)
    PM.selectBranch (Some "overlaySel")
    PM.setBranchOverlay ops
    let! during =
      (PM.ptForBranch (PM.currentBranchId ())).findFn fooLoc |> Ply.toTask
    Expect.isSome during "on the branch -> foo resolves through its overlay"

    // Main is still answered from core alone, with the branch's ops loaded and inert.
    let! fromMain = (PM.ptForBranch None).findFn fooLoc |> Ply.toTask
    Expect.isNone fromMain "and main never sees a branch's binding"

    PM.selectBranch None // leave clean for other tests (process-global)
    let! after = (PM.ptForBranch (PM.currentBranchId ())).findFn fooLoc |> Ply.toTask
    Expect.isNone after "back on main -> foo unresolved again"
  }

let branchesOffBranches =
  testTask "a branch off another sees its parent's frontier (branches off branches)" {
    do! cleanupBranch "boB"
    do! cleanupBranch "boA"
    do! Branches.createBranch "boA" "chain-a" "main"
    do! Branches.createBranch "boB" "chain-b" "boA" // B off A

    let! opsA = opsFor (namedSource "ChainA" 42)
    let! opsB = opsFor (namedSource "ChainB" 99)
    let! _ = Branches.storeDeltaOps "boA" opsA
    let! _ = Branches.storeDeltaOps "boB" opsB

    // B's overlay walks the parent chain: A's frontier + B's own.
    let! bOps = Branches.loadDeltaOps "boB"
    let bOverlay = PM.withExtraOps pmPT bOps
    let! bSeesA = bOverlay.findFn (fooLocIn "ChainA") |> Ply.toTask
    let! bSeesB = bOverlay.findFn (fooLocIn "ChainB") |> Ply.toTask
    Expect.isSome bSeesA "B sees its parent A's fn (branches off branches)"
    Expect.isSome bSeesB "B sees its own fn"

    let! aOps = Branches.loadDeltaOps "boA"
    let aOverlay = PM.withExtraOps pmPT aOps
    let! aSeesA = aOverlay.findFn (fooLocIn "ChainA") |> Ply.toTask
    let! aSeesB = aOverlay.findFn (fooLocIn "ChainB") |> Ply.toTask
    Expect.isSome aSeesA "A sees its own fn"
    Expect.isNone aSeesB "A does NOT see its child B's fn"

    // MERGE B INTO A (parent != main): retag B's frontier onto A. A's overlay now folds B's fn, but
    // main is still untouched (a non-main merge never flips effective / folds into main).
    let! parentOfB = Branches.parentOf "boB"
    Expect.equal parentOfB "boA" "B's parent is A"
    let! merged = Branches.retagFrontierToParent "boB" parentOfB
    Expect.isGreaterThan merged 0L "B had frontier ops to merge"

    let! aOps2 = Branches.loadDeltaOps "boA"
    let aOverlay2 = PM.withExtraOps pmPT aOps2
    let! aNowSeesB = aOverlay2.findFn (fooLocIn "ChainB") |> Ply.toTask
    Expect.isSome aNowSeesB "after merge, A sees B's fn (retagged onto A)"
    // B's OWN frontier tags are gone (moved to A). loadDeltaOps("boB") would still WALK to A, so we
    // check the direct tags, not the chain overlay.
    let! bOwnTags =
      Sql.query "SELECT count(*) AS n FROM op_branches WHERE branch_id = 'boB'"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.equal
      bOwnTags
      0L
      "B's own frontier tags are empty after the retag (its ops are A's now)"
    let! stillNotMain = pmPT.findFn (fooLocIn "ChainB") |> Ply.toTask
    Expect.isNone stillNotMain "merge into a non-main parent does NOT leak into main"

    do! cleanupBranch "boB"
    do! cleanupBranch "boA"
  }

/// Main authoring's WipRefresh must NOT see a branch's ops: `getWipOps` excludes every
/// `op_branches`-tagged op. Let them back into WIP and `discardWipOps` + re-insert folds them
/// into main.
let getWipOpsExcludesBranch =
  testTask
    "getWipOps excludes branch-tagged ops (main authoring can't see branch state)" {
    let branchId = "test-wip-guard"
    do! cleanupBranch branchId
    do! Branches.createBranch branchId "wip-guard" "main"
    let! ops = opsFor (branchSource 42)
    let! _ = Branches.storeDeltaOps branchId ops

    let! wip = Queries.getWipOps ()
    let! total =
      Sql.query "SELECT count(*) AS n FROM package_ops"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    let! branchCount =
      Sql.query "SELECT count(*) AS n FROM op_branches WHERE branch_id = @b"
      |> Sql.parameters [ "b", Sql.string branchId ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    // Every TAGGED op, not just this branch's: `getWipOps` excludes `op_branches` wholesale, so
    // subtracting only our own count assumes we're the only branch in the store, and we aren't.
    // DISTINCT because one op can be tagged to several branches.
    let! taggedCount =
      Sql.query "SELECT count(DISTINCT op_id) AS n FROM op_branches"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    Expect.isGreaterThan branchCount 0L "the branch actually stored ops"
    Expect.equal
      (int64 (List.length wip))
      (total - taggedCount)
      "getWipOps returns every op EXCEPT branch-tagged ones (isolation)"

    do! cleanupBranch branchId
  }

/// `applyUnappliedOps`' final sweep is scoped to `applied=0 AND effective=1`, so merging branch M
/// leaves sibling S's still-pending ops applied=0. A wider sweep marks them applied without folding
/// them, they can never fold afterwards, and S's binding is silently lost by merge order.
let mergeDoesNotConsumeSiblingPendingOps =
  testTask
    "a merge leaves OTHER branches' pending ops applied=0 (applied-flag isolation)" {
    let bS = "test-sweep-sibling"
    let bM = "test-sweep-merging"
    do! cleanupBranch bS
    do! cleanupBranch bM
    do! Branches.createBranch bS "sweep-sibling" "main"
    do! Branches.createBranch bM "sweep-merging" "main"
    // Distinct modules so M's fold pollutes only its own unique name (cleaned up after).
    let! opsS = opsFor (namedSource "SweepSibling" 7)
    let! opsM = opsFor (namedSource "SweepMerging" 8)
    let! _ = Branches.storeDeltaOps bS opsS
    let! _ = Branches.storeDeltaOps bM opsM

    let pendingCount (b : string) : Task<int64> =
      Sql.query
        "SELECT count(*) AS n FROM package_ops
         WHERE applied = 0 AND id IN (SELECT op_id FROM op_branches WHERE branch_id = @b)"
      |> Sql.parameters [ "b", Sql.string b ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    let! sBefore = pendingCount bS
    Expect.isGreaterThan sBefore 0L "sibling S starts with pending (applied=0) ops"

    // Merge M into main: flip its frontier effective=1, then fold (which runs the sweep).
    let! _ = Branches.markMergedEffective bM
    let! _ = Seed.applyUnappliedOps ()

    let! sAfter = pendingCount bS
    Expect.equal
      sAfter
      sBefore
      "S's pending ops are UNTOUCHED by M's merge (still applied=0)"

    do! cleanupBranch bS
    do! cleanupBranch bM
    do!
      Sql.query "DELETE FROM locations WHERE modules = 'SweepMerging'"
      |> Sql.executeStatementAsync
  }

/// Two branches bind the SAME name to DIFFERENT hashes, the second authored LATER. Merging older
/// then newer must land main on the NEWER binding (origin_ts LWW), so convergence does not depend on
/// merge order. Folds through the real path (markMergedEffective + applyUnappliedOps).
let sameNameMergesConvergeToLater =
  testTask
    "merging older-then-newer for one name lands on the NEWER binding (origin_ts LWW)" {
    let bOld = "test-cvg-old"
    let bNew = "test-cvg-new"
    let liveHash () : Task<Option<string>> =
      Sql.query
        "SELECT item_hash FROM locations
         WHERE owner = 'Darklang' AND modules = 'ConvergeWin' AND name = 'foo'
           AND unlisted_at IS NULL"
      |> Sql.executeRowOptionAsync (fun read -> read.string "item_hash")
    let mergeFold (b : string) : Task<unit> =
      task {
        let! _ = Branches.markMergedEffective b
        let! _ = Seed.applyUnappliedOps ()
        return ()
      }
    let setNameHash (ops : List<PT.PackageOp>) : string =
      ops
      |> List.pick (fun op ->
        match op with
        | PT.PackageOp.SetName(_, target, _) ->
          let (PT.Hash h) = target.hash
          Some h
        | _ -> None)

    do! cleanupBranch bOld
    do! cleanupBranch bNew
    do!
      Sql.query "DELETE FROM locations WHERE modules = 'ConvergeWin'"
      |> Sql.executeStatementAsync

    do! Branches.createBranch bOld "cvg-old" "main"
    do! Branches.createBranch bNew "cvg-new" "main"
    // Same module+name (ConvergeWin.foo), different bodies -> different hashes. bOld stored first, so
    // its ops get an EARLIER origin_ts than bNew's (storeDeltaOps stamps now() per call).
    let! opsOld = opsFor (namedSource "ConvergeWin" 5)
    let! opsNew = opsFor (namedSource "ConvergeWin" 6)
    let! _ = Branches.storeDeltaOps bOld opsOld
    let! _ = Branches.storeDeltaOps bNew opsNew
    let newerHash = setNameHash opsNew

    do! mergeFold bOld
    do! mergeFold bNew

    let! live = liveHash ()
    Expect.equal
      live
      (Some newerHash)
      "main lands on the NEWER binding after older-then-newer merge"

    do! cleanupBranch bOld
    do! cleanupBranch bNew
    do!
      Sql.query "DELETE FROM locations WHERE modules = 'ConvergeWin'"
      |> Sql.executeStatementAsync
  }

/// Locks the reload-stable rebase model: nameConflicts flags a name whose main hash diverged from
/// the branch's recorded base; rebase accepts main's state and clears it. Manipulates the base row
/// directly (no fold into main) so the test never pollutes the shared main projection.
let rebaseDetectsAndClearsConflicts =
  testTask "nameConflicts flags a diverged name; rebase clears it" {
    let bid = "test-rebase-gate"
    do! cleanupBranch bid

    do! Branches.createBranch bid "rebase-gate" "main"
    let! ops = opsFor (namedSource "RebaseGate" 5)
    let! _ = Branches.storeDeltaOps bid ops
    do! Branches.recordNameBases bid "main" ops

    // clean before divergence: the name isn't in main, so base "" == main's current "".
    let! c0 = Branches.nameConflicts bid
    Expect.isEmpty c0 "clean before any divergence"

    // simulate main having changed that name since the fork: stale the recorded base.
    do! staleNameBases bid
    let! c1 = Branches.nameConflicts bid
    Expect.isNonEmpty c1 "conflict detected (main's current hash != the stale base)"

    // rebase accepts main -> base := main's current -> no conflict, merge unblocked.
    let! _ = Branches.rebase bid
    let! c2 = Branches.nameConflicts bid
    Expect.isEmpty c2 "rebase cleared the conflict"

    do! cleanupBranch bid
  }

/// Locks the branch-transfer import path (what scmImportBranchOps does): register a branch, store
/// its ops effective=0 + tag, and re-derive the per-name bases against THIS instance's main. The
/// cross-instance invariant is that a branch stays a branch on the receiving side.
let branchTransferImportReDerivesBases =
  testTask
    "importing a branch's ops recreates it isolated + re-derives its per-name bases locally" {
    let dst = "test-xfer-dst"
    do! cleanupBranch dst

    // simulate scmImportBranchOps: register + store ops + re-derive bases.
    do! Branches.createBranch dst "xfer" "main"
    let! ops = opsFor (namedSource "XferTest" 8)
    let! _ = Branches.storeDeltaOps dst ops
    do! Branches.recordNameBases dst "main" ops

    let! loaded = Branches.loadDeltaOps dst
    let overlay = PM.withExtraOps pmPT loaded
    let! resolved = overlay.findFn (fooLocIn "XferTest") |> Ply.toTask
    Expect.isSome resolved "imported branch resolves its fn"
    let! onCore = pmPT.findFn (fooLocIn "XferTest") |> Ply.toTask
    Expect.isNone onCore "and it did NOT leak into core"

    // per-name bases were re-derived (against local main), so merge/rebase work on this instance.
    let! bases = Branches.nameBasesFor dst
    Expect.isNonEmpty bases "per-name bases re-derived on import"

    do! cleanupBranch dst
  }

/// A name a branch binds ONLY by resolving a conflict still needs a per-name base: the base is what
/// the conflict detector needs to prove BOTH sides moved. Without one that name can never conflict
/// again, and `dark diff` renders it as `+ new` rather than a change. `bindingFromOp` counts
/// `Resolve` as a binding, and `recordNameBases` has to agree with it.
let resolveAloneRecordsANameBase =
  testTask "a name bound only by Resolve still gets a per-name base" {
    let b = "test-resolve-base"

    do! cleanupBranch b
    do! Branches.createBranch b "resolve-base" "main"

    // Borrow a real (location, target) off a SetName rather than hand-building a hash: what is under
    // test is which op SHAPE gets counted, not what a Reference looks like.
    let! ops = opsFor (namedSource "ResolveBaseTest" 7)

    let binding =
      ops
      |> List.tryPick (fun op ->
        match op with
        | PT.PackageOp.SetName(loc, target, _) -> Some(loc, target)
        | _ -> None)

    Expect.isSome binding "the fixture produced a SetName to borrow a binding from"
    let (loc, target) = Option.get binding

    do!
      Branches.recordNameBases
        b
        "main"
        [ PT.PackageOp.Resolve("decision-for-the-base-test", loc, target) ]

    let! bases = Branches.nameBasesFor b
    let recorded = bases |> List.map (fun ((_, _, n), _) -> n)
    Expect.equal
      recorded
      [ loc.name ]
      "the Resolve on its own recorded a base for the name"

    do! cleanupBranch b
  }

/// Locks per-name RESOLUTION (scm-spec 7). take-theirs untags the branch's SetName, so its overlay
/// falls back to the parent for that name; keep-mine leaves the branch binding it, re-stamped. Both
/// clear the conflict. The conflict is set up by staling the base, so nothing folds into shared main.
let perNameResolutionMineTheirs =
  testTask
    "resolve take-theirs drops the branch's binding; keep-mine keeps it; both clear the conflict" {
    let bT = "test-resolve-theirs"
    let bM = "test-resolve-mine"
    let fqn = "Darklang.ResolveTest.foo"
    let setupConflict (b : string) =
      task {
        do! cleanupBranch b
        do! Branches.createBranch b "resolve" "main"
        let! ops = opsFor (namedSource "ResolveTest" 5)
        let! _ = Branches.storeDeltaOps b ops
        do! Branches.recordNameBases b "main" ops
        do! staleNameBases b
      }

    do! setupConflict bT
    let! c0 = Branches.nameConflicts bT
    Expect.isNonEmpty c0 "conflict present before take-theirs"
    match! Branches.resolveTakeTheirs bT fqn with
    | Error e -> failtest $"take-theirs failed: {e}"
    | Ok() -> ()
    let! c1 = Branches.nameConflicts bT
    Expect.isEmpty c1 "take-theirs cleared the conflict"
    let! loadedT = Branches.loadDeltaOps bT
    let overlayT = PM.withExtraOps pmPT loadedT
    let! resolvedT = overlayT.findFn (fooLocIn "ResolveTest") |> Ply.toTask
    Expect.isNone
      resolvedT
      "take-theirs: branch no longer binds foo (falls back to the parent)"

    do! setupConflict bM
    match! Branches.resolveKeepMine bM fqn with
    | Error e -> failtest $"keep-mine failed: {e}"
    | Ok() -> ()
    let! c2 = Branches.nameConflicts bM
    Expect.isEmpty c2 "keep-mine cleared the conflict"
    let! loadedM = Branches.loadDeltaOps bM
    let overlayM = PM.withExtraOps pmPT loadedM
    let! resolvedM = overlayM.findFn (fooLocIn "ResolveTest") |> Ply.toTask
    Expect.isSome resolvedM "keep-mine: branch still binds foo"

    do! cleanupBranch bT
    do! cleanupBranch bM
  }

/// Locks revive-on-reuse (createBranch upsert): re-creating an archived/merged branch id clears
/// archived_at + merged_at, so a review queue reused after reject/approve is active and visible
/// again. Parent stays first-write-wins.
let reuseBranchIdRevives =
  testTask
    "createBranch on an archived/merged id revives it; parent stays first-write-wins" {
    let b = "test-revive"
    do! cleanupBranch b
    do! Branches.createBranch b "revive" "main"

    let flagsSet () : Task<int64> =
      Sql.query
        "SELECT count(*) AS n FROM branches
         WHERE id = @b AND (archived_at IS NOT NULL OR merged_at IS NOT NULL)"
      |> Sql.parameters [ "b", Sql.string b ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    // Set the flags directly: this test is about createBranch's revive-on-reuse, and `archive` is
    // Dark now (SCM.Branches), so SQL keeps the setup on the test's actual subject.
    do!
      Sql.query
        "UPDATE branches SET archived_at = datetime('now'), merged_at = datetime('now')
         WHERE id = @b"
      |> Sql.parameters [ "b", Sql.string b ]
      |> Sql.executeStatementAsync
    let! before = flagsSet ()
    Expect.equal before 1L "archived/merged flags are set before reuse"

    do! Branches.createBranch b "revive" "main"
    let! after = flagsSet ()
    Expect.equal
      after
      0L
      "reuse revived the branch (archived_at + merged_at cleared)"

    // re-creating with a DIFFERENT parent must NOT change the recorded parent (first-write-wins).
    do! Branches.createBranch b "revive" "some-other-parent"
    let! parent = Branches.parentOf b
    Expect.equal parent "main" "parent stays first-write-wins across re-creation"

    do! cleanupBranch b
  }

/// ISOLATION: the branch author path folds a value's AddValue CONTENT into package_values (so eval
/// can read its rt_dval), but must NOT fold the SetName, so the NAME never lands in main's
/// `locations`. CORRECTNESS: after the content fold, `evaluateAllValues` must materialise an
/// EXPRESSION body's Dval into rt_dval, or it stays NULL and getValue returns nothing.
let branchValueContentFoldIsolatesName =
  testTask
    "folding a branch value's AddValue content populates package_values but NOT locations" {
    let source =
      "module Darklang.BranchValFoldTest\n\nval vv = Stdlib.Int64.add 3L 4L"
    let! ops = opsFor source
    let addValueOps =
      ops
      |> List.filter (fun op ->
        match op with
        | PT.PackageOp.AddValue _ -> true
        | _ -> false)
    Expect.isNonEmpty addValueOps "the val source produced an AddValue op"
    let valueHash =
      addValueOps
      |> List.pick (fun op ->
        match op with
        | PT.PackageOp.AddValue v ->
          let (PT.Hash h) = v.hash
          Some h
        | _ -> None)

    // Fold ONLY the AddValue (mirrors the branch author path's content-only fold).
    do! LibDB.PackageOpPlayback.applyOps addValueOps

    let! contentCount =
      Sql.query "SELECT count(*) AS n FROM package_values WHERE hash = @h"
      |> Sql.parameters [ "h", Sql.string valueHash ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.isGreaterThan
      contentCount
      0L
      "AddValue folded the content into package_values"

    let! locCount =
      Sql.query
        "SELECT count(*) AS n FROM locations WHERE name = 'vv' AND modules LIKE '%BranchValFoldTest%'"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.equal
      locCount
      0L
      "the branch value's NAME is NOT in main locations (content-only fold keeps names branch-isolated)"

    // `applyOps` stores rt_dval NULL (see PackageOpPlayback.fs), so the branch author path must run
    // `evaluateAllValues` for an EXPRESSION-valued branch value to materialise its Dval.
    let builtins = Builtins.CliHost.Libs.Cli.builtinsToUse ()
    let! _ = Seed.evaluateAllValues builtins PM.rt
    let! (evaluated : Option<RT.PackageValue.PackageValue>) =
      LibDB.RuntimeTypes.Value.get (RT.Hash valueHash) |> Ply.toTask
    match evaluated with
    | Some pv ->
      Expect.equal
        pv.body
        (RT.DInt64 7L)
        "the expression-valued branch value materialised to 3+4=7 in rt_dval (bug #1 correctness guard)"
    | None ->
      Tests.failtest
        "rt_dval was NULL after evaluateAllValues -- expression-valued branch values would error (bug #1 regression)"

    do!
      Sql.query "DELETE FROM package_values WHERE hash = @h"
      |> Sql.parameters [ "h", Sql.string valueHash ]
      |> Sql.executeStatementAsync
  }

let branchExists =
  testTask
    "a branch is found by its registry row OR its op tags, and a typo is found by neither" {
    do! cleanupBranch "beX"

    let! beforeAnything = Branches.exists "beX"
    Expect.isFalse beforeAnything "an unknown branch does not exist"

    // Registered with no ops yet: this is what `dark switch` produces before you author anything, and
    // it has to count, or a fresh branch reads as a typo.
    do! Branches.createBranch "beX" "" "main"
    let! registeredOnly = Branches.exists "beX"
    Expect.isTrue registeredOnly "registered with no ops still exists"

    // Tagged ops with no registry row: this is what a branch bundle from another machine looks like
    // before anything registers it locally. Also has to count.
    do! cleanupBranch "beY"
    let! ops = opsFor (namedSource "BeY" 7)
    let! _ = Branches.storeDeltaOps "beY" ops
    do!
      Sql.query "DELETE FROM branches WHERE id = @b"
      |> Sql.parameters [ "b", Sql.string "beY" ]
      |> Sql.executeStatementAsync
    let! taggedOnly = Branches.exists "beY"
    Expect.isTrue taggedOnly "ops tagged with no registry row still exists"

    let! typo = Branches.exists "beYY"
    Expect.isFalse typo "a prefix of a real branch is not that branch"

    do! cleanupBranch "beX"
    do! cleanupBranch "beY"
  }

let mergeCountsWhatItFlipped =
  testTask
    "markMergedEffective reports ops it flipped, not ops that were already effective" {
    do! cleanupBranch "mcX"
    do! Branches.createBranch "mcX" "" "main"
    let! ops = opsFor (namedSource "McX" 11)
    let! _ = Branches.storeDeltaOps "mcX" ops

    let! pending = countEffective "mcX" 0
    Expect.isGreaterThan pending 0L "stored branch ops start effective=0"

    let! first = Branches.markMergedEffective "mcX"
    Expect.equal first pending "the first merge reports exactly what it flipped"

    // Merging again flips NOTHING -- every tagged op is already effective -- so reporting the tag
    // count would be `MergeOutcome.merged` claiming work it did not do.
    let! second = Branches.markMergedEffective "mcX"
    Expect.equal second 0L "a re-merge reports 0, not the number of tagged ops"

    do! cleanupBranch "mcX"
  }

let importedOpsKeepTheirStamps =
  testTask
    "storeDeltaOpsStamped preserves an incoming op's origin_ts instead of re-stamping it" {
    do! cleanupBranch "stX"
    do! Branches.createBranch "stX" "" "main"

    // A stamp from the far past, which the local authoring clock could never produce.
    let farPast = "2001-02-03T04:05:06.007Z"
    let! ops = opsFor (namedSource "StX" 13)
    let! _ =
      Branches.storeDeltaOpsStamped "stX" (ops |> List.map (fun op -> (op, farPast)))

    let! stamps =
      Sql.query
        "SELECT DISTINCT p.origin_ts AS ts FROM package_ops p
         JOIN op_branches ob ON ob.op_id = p.id WHERE ob.branch_id = @b"
      |> Sql.parameters [ "b", Sql.string "stX" ]
      |> Sql.executeAsync (fun read -> read.string "ts")

    Expect.equal
      stamps
      [ farPast ]
      "every imported op keeps the stamp it arrived with -- re-stamping locally makes the IMPORTER \
       look like the author, so LWW resolves by who imported last rather than who edited last"

    do! cleanupBranch "stX"
  }

let rebuildKeepsBranchPolicy =
  testTask "a projection rebuild re-folds branch-scoped propagation decisions" {
    do! cleanupBranch "bpX"
    do! Branches.createBranch "bpX" "" "main"

    let loc : PT.PackageLocation =
      { owner = "Zz"; modules = [ "RebuildTest" ]; name = "pinned" }
    let decide =
      PT.PackageOp.Decide(
        "propagation",
        loc,
        "pin",
        "deliberate",
        "2026-01-02T00:00:00.000Z"
      )
    let! _ = Branches.storeDeltaOps "bpX" [ decide ]

    let countPolicy () =
      Sql.query
        "SELECT count(*) AS n FROM propagation_policy
         WHERE branch_id = @b AND owner = 'Zz' AND modules = 'RebuildTest' AND name = 'pinned'"
      |> Sql.parameters [ "b", Sql.string "bpX" ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    let! stored = countPolicy ()
    Expect.equal
      stored
      1L
      "authoring the Decide on a branch folds a branch-scoped policy row"

    // A projection rebuild clears this table and re-folds only `effective = 1` ops, and branch ops
    // are `effective = 0` by design, so without an explicit re-fold the row never comes back.
    do! Sql.query "DELETE FROM propagation_policy" |> Sql.executeStatementAsync
    let! cleared = countPolicy ()
    Expect.equal cleared 0L "cleared, as a rebuild would"

    do! Branches.refoldBranchDecides ()
    let! restored = countPolicy ()
    Expect.equal
      restored
      1L
      "the rebuild path re-folds branch decisions from the log"

    do!
      Sql.query
        "DELETE FROM propagation_policy WHERE owner = 'Zz' AND modules = 'RebuildTest'"
      |> Sql.executeStatementAsync
    do! cleanupBranch "bpX"
  }

let branchPMIsPerBranch =
  testTask "ptForBranch answers about a branch this process is NOT on" {
    do! cleanupBranch "pfA"
    do! cleanupBranch "pfB"
    do! Branches.createBranch "pfA" "" "main"
    do! Branches.createBranch "pfB" "" "main"

    let! opsA = opsFor (namedSource "PfA" 42)
    let! opsB = opsFor (namedSource "PfB" 99)
    let! _ = Branches.storeDeltaOps "pfA" opsA
    let! _ = Branches.storeDeltaOps "pfB" opsB

    // Sit on A. This is the state a `dark --branch pfA ...` process boots into.
    PM.selectBranch (Some "pfA")
    Expect.equal (PM.currentBranchId ()) (Some "pfA") "process is on pfA"

    // Asking about a branch we're NOT on is what a process-global overlay alone cannot do, and what
    // the LSP and any daemon will need.
    let! aFromA = (PM.ptForBranch (Some "pfA")).findFn (fooLocIn "PfA") |> Ply.toTask
    Expect.isSome aFromA "on pfA, pfA's fn resolves"

    let! bFromA = (PM.ptForBranch (Some "pfB")).findFn (fooLocIn "PfB") |> Ply.toTask
    Expect.isSome bFromA "while ON pfA, pfB's fn still resolves via ptForBranch"

    let! aFromB = (PM.ptForBranch (Some "pfB")).findFn (fooLocIn "PfA") |> Ply.toTask
    Expect.isNone aFromB "pfB's overlay does not contain pfA's fn"

    let! aFromMain = (PM.ptForBranch None).findFn (fooLocIn "PfA") |> Ply.toTask
    Expect.isNone aFromMain "and main sees neither"

    // Switching is a process operation, not a restart: what `dark switch` needs in the REPL.
    PM.selectBranch (Some "pfB")
    Expect.equal
      (PM.currentBranchId ())
      (Some "pfB")
      "process moved to pfB without restarting"
    let! bAfterSwitch =
      (PM.ptForBranch (PM.currentBranchId ())).findFn (fooLocIn "PfB") |> Ply.toTask
    Expect.isSome bAfterSwitch "the active overlay followed the switch"
    let! aAfterSwitch =
      (PM.ptForBranch (PM.currentBranchId ())).findFn (fooLocIn "PfA") |> Ply.toTask
    Expect.isNone aAfterSwitch "and stopped answering about the branch we left"

    PM.selectBranch None // leave clean for other tests (process-global)
    do! cleanupBranch "pfA"
    do! cleanupBranch "pfB"
  }

let branchNamesResolveButDontShadowMain =
  testTask
    "a branch supplies names for hashes main can't name, and never relabels ones it can" {
    do! cleanupBranch "lnX"
    do! Branches.createBranch "lnX" "" "main"

    let! ops = opsFor (namedSource "LnX" 42)
    let! _ = Branches.storeDeltaOps "lnX" ops

    let branchHash =
      ops
      |> List.tryPick (fun op ->
        match op with
        | PT.PackageOp.SetName(l, target, _) when l.name = "foo" -> Some target.hash
        | _ -> None)
      |> Option.get

    // Main cannot name this hash: a branch's SetNames never fold into `locations`, so without the
    // overlay the caller has nothing to render but 64 hex characters.
    let! fromMain = PM.pt.getFnLocations branchHash |> Ply.toTask
    Expect.isEmpty fromMain "main has no name for a branch-authored hash"

    PM.selectBranch (Some "lnX")
    let onBranch =
      PM.locationsFor (PM.currentBranchId ()) PT.ItemKind.Fn branchHash []
    Expect.equal
      (onBranch |> List.map (fun l -> l.name))
      [ "foo" ]
      "the overlay supplies the name main is missing"

    // ... but main WINS when it has an answer. Identical content is one item, so a hash is routinely
    // live at several names, and preferring the branch's renders a main item under a branch label.
    let mainLoc : PT.PackageLocation =
      { owner = "Darklang"; modules = [ "SomeMainModule" ]; name = "mainName" }
    let withMain =
      PM.locationsFor (PM.currentBranchId ()) PT.ItemKind.Fn branchHash [ mainLoc ]
    Expect.equal
      (withMain |> List.map (fun l -> l.name) |> List.tryHead)
      (Some "mainName")
      "main's name comes first, so a duplicated body is never relabelled to the branch's"

    let asType =
      PM.locationsFor (PM.currentBranchId ()) PT.ItemKind.Type branchHash []
    Expect.isEmpty asType "the overlay answers per kind, not per hash alone"

    PM.selectBranch None
    let offBranch = PM.locationsFor None PT.ItemKind.Fn branchHash []
    Expect.isEmpty offBranch "off the branch, the name is gone again (isolation)"

    do! cleanupBranch "lnX"
  }


/// The three ways a run picks its branch. Each tier is scoped tighter than the one below on purpose:
/// the FLAG is this command, the ENV is this SHELL, the config is this machine. The env tier is what
/// lets several agents work on several branches at once without fighting over the single config key
/// `dark switch` writes.
let branchResolutionOrder =
  testTask "the flag beats DARK_BRANCH beats the stored branch" {
    let pick
      (flag : Option<string>)
      (env : Option<string>)
      (stored : Option<string>)
      =
      match flag with
      | Some f -> Some f
      | None ->
        match env with
        | Some e -> Some e
        | None -> stored

    Expect.equal (pick (Some "f") (Some "e") (Some "s")) (Some "f") "the flag wins"
    Expect.equal (pick None (Some "e") (Some "s")) (Some "e") "then the env"
    Expect.equal (pick None None (Some "s")) (Some "s") "then the stored branch"
    Expect.equal (pick None None None) None "and main is the absence of all three"
  }


/// A merge that does not travel: the merged OPS already cross (they are main ops once merged, and
/// the two mains converge on identical hashes), but without the event the FACT of the merge does
/// not, so a colleague's copy of the branch still lists as live work they could keep authoring on.
let branchEventMarksMerged =
  testTask "a BranchEvent(Merged) op folds to marking that branch merged" {
    let branchId = "test-branch-event-merged"
    do! cleanupBranch branchId
    do! Branches.createBranch branchId "event-proof" "main"

    let! before = Branches.isMerged branchId
    Expect.isFalse before "not merged before the event"

    let op =
      PT.PackageOp.BranchEvent(branchId, PT.Merged, "2026-01-01T00:00:00.000Z")
    let! _ = LibDB.Inserts.insertAndApplyOps [ op ]

    let! after = Branches.isMerged branchId
    Expect.isTrue after "the event folded, so the branch reads as merged"

    // Monotonic: this is what lets the event travel with no stamp on `branches` to arbitrate with, and
    // what makes re-receiving it on a third machine harmless.
    let! _ = LibDB.Inserts.insertAndApplyOps [ op ]
    let! twice = Branches.isMerged branchId
    Expect.isTrue twice "applying it again lands in the same place"

    do! cleanupBranch branchId
  }


/// The receiving side has no obligation to know every branch its peers have. Branch ids travel with
/// a bundle, so the branches you actually share match; the rest are none of this store's business.
let branchEventForUnknownBranchIsIgnored =
  testTask "a BranchEvent for a branch this store has never seen folds to nothing" {
    let op =
      PT.PackageOp.BranchEvent(
        "test-branch-that-does-not-exist",
        PT.Merged,
        "2026-01-01T00:00:00.000Z"
      )

    let! _ = LibDB.Inserts.insertAndApplyOps [ op ]

    let! rows =
      Sql.query "SELECT COUNT(*) as n FROM branches WHERE id = @b"
      |> Sql.parameters [ "b", Sql.string "test-branch-that-does-not-exist" ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.equal rows 0L "no branch was conjured up to receive the event"
  }


/// The fold marks ops applied by PREDICATE, not by id, and folding an op can change that predicate.
///
/// A merge event arriving from another machine flips its branch's frontier to effective=1 mid-fold.
/// An applied=1 sweep running afterwards marks those ops applied without anything having folded
/// them, so the branch reads `[merged]` next to a main that does not have its code. The sweep runs
/// BEFORE the fold for exactly this reason.
let foldDoesNotStrandOpsItMadeEffective =
  testTask
    "an op that makes other ops effective does not leave them applied-but-unfolded" {
    let branchId = "test-branch-stranded"
    do! cleanupBranch branchId
    do! Branches.createBranch branchId "stranded-proof" "main"

    let! ops = opsFor (namedSource "BranchTestStranded" 77)
    let! _ = Branches.storeDeltaOps branchId ops

    let! pending = countEffective branchId 0
    Expect.isGreaterThan pending 0L "the branch's ops start effective=0"

    // The event has to arrive the way a SYNC delivers it: inserted unapplied-and-effective, then
    // folded by `applyUnappliedOps`, which puts the flip and the sweep inside ONE pass. Authoring it
    // locally folds it in its own call, and a later pass picks the branch ops up regardless.
    let event =
      PT.PackageOp.BranchEvent(branchId, PT.Merged, "2026-01-01T00:00:00.000Z")
    let eventId = LibDB.Inserts.computeOpHash event
    let eventBlob = BS.PT.PackageOp.serialize eventId event
    do!
      Sql.query
        "INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts)
         VALUES (@id, @blob, 0, 1, @ts)"
      |> Sql.parameters
        [ "id", Sql.uuid eventId
          "blob", Sql.bytes eventBlob
          "ts", Sql.string "2026-01-01T00:00:00.000Z" ]
      |> Sql.executeStatementAsync

    let! _ = Seed.applyUnappliedOps ()

    // Asserted over the ids captured BEFORE the event, because the event clears the branch tags: a
    // query that looks them up by tag afterwards finds nothing and passes for the wrong reason.
    let ids = ops |> List.map (fun op -> string (LibDB.Inserts.computeOpHash op))
    let! unfolded =
      Sql.query
        $"""SELECT COUNT(*) as n FROM package_ops
            WHERE applied = 0 AND id IN ({ids |> List.map (fun i -> $"'{i}'") |> String.concat ", "})"""
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.equal unfolded 0L "every op the event made effective was actually folded"

    let! inMain = pmPT.findFn (fooLocIn "BranchTestStranded") |> Ply.toTask
    Expect.isSome
      inMain
      "the merged branch's fn resolves on main after the event folded"

    do! cleanupBranch branchId
  }



/// Dark code outside the SCM silos must not query `locations`. It is MAIN's projection and has no
/// `branch_id`, so a read that goes straight to it answers about main while the caller stands on a
/// branch, and answers plausibly. The overlay helpers in `SCM.PackageOps` are the branch-aware way.
///
/// Checked by reading the source, because the failure is invisible at run time on a single-branch
/// store. `matter.dark` is exempt: a relay holds no branches, so main's projection IS its answer.
let noDirectLocationsReadsOutsideTheSilos =
  testTask "only the SCM silos query `locations` from Dark" {
    let root = System.IO.Path.Combine("..", "packages", "darklang")

    let exempt (path : string) : bool =
      let p = path.Replace("\\", "/")
      p.Contains "/scm/" || p.EndsWith "matter.dark"

    let offenders =
      System.IO.Directory.GetFiles(root, "*.dark", System.IO.SearchOption.AllDirectories)
      |> Array.filter (exempt >> not)
      |> Array.filter (fun path ->
        System.IO.File.ReadAllLines path
        |> Array.exists (fun line ->
          let t = line.Trim()
          not (t.StartsWith "//")
          && (t.Contains "FROM locations" || t.Contains "JOIN locations")))
      |> Array.map (fun p -> p.Replace("\\", "/"))
      |> List.ofArray

    Expect.isEmpty
      offenders
      "no Dark file outside packages/darklang/scm may read `locations` directly -- \
       it is main-only, so it answers about main while you stand on a branch. \
       Use the overlay helpers in SCM.PackageOps."
  }

let tests =
  // These mutate the process-global branch overlay AND delete from `package_ops`, either of which
  // can make a concurrent reader see the store mid-change. testSequenced, NOT testSequencedGroup:
  // the group form only stops the tests INSIDE it from running alongside each other, and still runs
  // in the parallel phase next to everything else, which is where the hazard is.
  testSequenced
  <| testList
    "BranchOverlay"
    [ branchResolutionOrder
      isolationFromCore
      branchExists
      mergeCountsWhatItFlipped
      importedOpsKeepTheirStamps
      rebuildKeepsBranchPolicy
      branchPMIsPerBranch
      branchNamesResolveButDontShadowMain
      isolationBetweenBranches
      storeThenOverlay
      mergedBranchStaysAddressable
      concurrentCreateYieldsOneBranch
      markMergedFlipsEffective
      processOverlaySelects
      branchesOffBranches
      getWipOpsExcludesBranch
      mergeDoesNotConsumeSiblingPendingOps
      sameNameMergesConvergeToLater
      rebaseDetectsAndClearsConflicts
      perNameResolutionMineTheirs
      resolveAloneRecordsANameBase
      reuseBranchIdRevives
      branchValueContentFoldIsolatesName
      branchEventMarksMerged
      branchEventForUnknownBranchIsIgnored
      foldDoesNotStrandOpsItMadeEffective
      branchTransferImportReDerivesBases
      noDirectLocationsReadsOutsideTheSilos ]
