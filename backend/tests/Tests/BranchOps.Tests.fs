module Tests.BranchOps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude

open TestUtils.TestUtils
open TestUtils.PTShortcuts

module PT = LibExecution.ProgramTypes
module Branches = LibDB.Branches
module Inserts = LibDB.Inserts
module BranchOpPlayback = LibDB.BranchOpPlayback

open Fumble
open LibDB.Sqlite

module BS = LibSerialization.Binary.Serialization
open LibSerialization.Hashing


// ── Helpers ──────────────────────────────────────────────────────────────

let private loc (name : string) : PT.PackageLocation =
  { owner = "Test"; modules = [ "BranchOps" ]; name = name }

let private makeFn (body : PT.Expr) : PT.PackageFn.PackageFn =
  testPackageFn [] (NEList.singleton "x") PT.TInt64 body

let private countRows (table : string) : Task<int64> =
  Sql.query $"SELECT COUNT(*) as cnt FROM {table}"
  |> Sql.executeRowAsync (fun read -> read.int64 "cnt")


// ── Tests ────────────────────────────────────────────────────────────────

let testBranchOpsEmitted =
  testTask "branch ops: emitted for create, commit, archive" {
    let! opCountBefore = countRows "branch_ops"

    // Create a branch — should emit CreateBranch
    let! (branch : PT.Branch) = Branches.create "test-bo-emit" PT.mainBranchId
    let! opCountAfterCreate = countRows "branch_ops"
    Expect.isGreaterThan
      opCountAfterCreate
      opCountBefore
      "CreateBranch op should be emitted"

    // Add a fn as WIP then commit — should emit CreateCommit
    let fn1 = makeFn (eVar "x")
    let ops =
      [ PT.PackageOp.AddFn fn1
        PT.PackageOp.SetName(loc "bo1", PT.PackageFn fn1.hash) ]
    let! (_insertCount : int64) = Inserts.insertAndApplyOpsAsWip branch.id ops
    let! (commitResult : Result<PT.Hash, string>) =
      Inserts.commitWipOps LibCloud.Account.IDs.darklang branch.id "test commit"
    Expect.isOk commitResult "commit should succeed"
    let! opCountAfterCommit = countRows "branch_ops"
    Expect.isGreaterThan
      opCountAfterCommit
      opCountAfterCreate
      "CreateCommit op should be emitted"

    // Archive — should emit ArchiveBranch
    let! (archiveResult : Result<unit, string>) = Branches.archive branch.id
    Expect.isOk archiveResult "archive should succeed"
    let! opCountAfterArchive = countRows "branch_ops"
    Expect.isGreaterThan
      opCountAfterArchive
      opCountAfterCommit
      "ArchiveBranch op should be emitted"

    // Verify branch is archived (not in list)
    let! activeBranches = Branches.list ()
    let found =
      activeBranches |> List.tryFind (fun (b : PT.Branch) -> b.name = "test-bo-emit")
    Expect.isNone found "archived branch should not appear in list()"

    // But it shows up in listAll
    let! allBranches = Branches.listAll ()
    let foundAll =
      allBranches |> List.tryFind (fun (b : PT.Branch) -> b.name = "test-bo-emit")
    Expect.isSome foundAll "archived branch should appear in listAll()"
  }


let testBranchOpsSerialization =
  testTask "branch ops: round-trip serialization" {
    let ops : List<PT.BranchOp> =
      [ PT.BranchOp.CreateBranch(
          System.Guid.NewGuid(),
          "test-serial",
          Some PT.mainBranchId,
          Some(PT.Hash "abc123")
        )
        PT.BranchOp.CreateCommit(
          PT.Hash "commit1",
          "msg",
          PT.mainBranchId,
          LibCloud.Account.IDs.darklang,
          [ PT.Hash "op1"; PT.Hash "op2" ]
        )
        PT.BranchOp.RebaseBranch(System.Guid.NewGuid(), PT.Hash "newbase")
        PT.BranchOp.MergeBranch(System.Guid.NewGuid(), PT.mainBranchId)
        PT.BranchOp.ArchiveBranch(System.Guid.NewGuid())
        PT.BranchOp.CreateBranch(PT.mainBranchId, "main", None, None) ]

    for op in ops do
      let hash = Hashing.computeBranchOpHash op
      let (PT.Hash hashStr) = hash
      let serialized = BS.PT.BranchOp.serialize hashStr op
      let deserialized = BS.PT.BranchOp.deserialize hashStr serialized
      Expect.equal deserialized op $"round-trip failed for {op}"
  }


let testBranchOpsDeserialization =
  testTask "branch ops: all stored ops can be deserialized" {
    // Read all branch_ops and verify they deserialize correctly
    let! branchOps =
      Sql.query "SELECT id, op_blob FROM branch_ops ORDER BY created_at ASC"
      |> Sql.executeAsync (fun read ->
        let id = read.string "id"
        let blob = read.bytes "op_blob"
        (id, BS.PT.BranchOp.deserialize id blob))

    Expect.isNonEmpty branchOps "should have branch_ops stored"

    // Verify we have the expected op types from test setup
    let hasCreateBranch =
      branchOps
      |> List.exists (fun (_, op) ->
        match op with
        | PT.BranchOp.CreateBranch _ -> true
        | _ -> false)
    Expect.isTrue hasCreateBranch "should have at least one CreateBranch op"

    let hasCreateCommit =
      branchOps
      |> List.exists (fun (_, op) ->
        match op with
        | PT.BranchOp.CreateCommit _ -> true
        | _ -> false)
    Expect.isTrue hasCreateCommit "should have at least one CreateCommit op"

    // Verify the init commit op has the right message
    let initCommitOp =
      branchOps
      |> List.tryFind (fun (_, op) ->
        match op with
        | PT.BranchOp.CreateCommit(_, msg, _, _, _) ->
          msg = "Init: packages loaded from disk"
        | _ -> false)
    Expect.isSome initCommitOp "should have init commit op"
  }


/// Regression: same FQN + same body on two branches used to ghost the
/// second one. The SetName op is content-addressed, and with a bare
/// `id` PK on package_ops, the second branch's identical SetName
/// collided on insert (INSERT OR IGNORE silently dropped it). Result:
/// no `locations` row on branch B, so view/search/tree/eval all act
/// like the fn doesn't exist while `fn` still claimed `✓ Created`.
let testGhostFunctionCrossBranch =
  testTask "package ops: same fn on two branches is visible on both" {
    let fn = makeFn (eVar "x")
    let location = loc "ghost-cross-branch"
    let ops =
      [ PT.PackageOp.AddFn fn; PT.PackageOp.SetName(location, PT.PackageFn fn.hash) ]

    let! (branchA : PT.Branch) = Branches.create "ghost-test-A" PT.mainBranchId
    let! (_ : int64) = Inserts.insertAndApplyOpsAsWip branchA.id ops

    let! (branchB : PT.Branch) = Branches.create "ghost-test-B" PT.mainBranchId
    let! (_ : int64) = Inserts.insertAndApplyOpsAsWip branchB.id ops

    let countLocationsFor (branchId : PT.BranchId) : Task<int64> =
      Sql.query
        """
        SELECT COUNT(*) as cnt FROM locations
        WHERE branch_id = @branch_id
          AND owner = @owner AND modules = @modules AND name = @name
        """
      |> Sql.parameters
        [ "branch_id", Sql.uuid branchId
          "owner", Sql.string location.owner
          "modules", Sql.string (String.concat "." location.modules)
          "name", Sql.string location.name ]
      |> Sql.executeRowAsync (fun read -> read.int64 "cnt")

    let! locA = countLocationsFor branchA.id
    let! locB = countLocationsFor branchB.id
    Expect.equal locA 1L "branch A should have 1 location row"
    Expect.equal
      locB
      1L
      "branch B should have 1 location row (ghost-function regression)"
  }


let testPartialCommit =
  testTask "partial commit: only selected ops commit, the rest stay WIP" {
    let! (branch : PT.Branch) = Branches.create "test-bo-partial" PT.mainBranchId

    // Two independent fns (distinct bodies → distinct hashes), each added as
    // AddFn + SetName: 4 WIP ops, 2 WIP locations.
    let fnA = makeFn (eVar "x")
    let fnB = makeFn (eInt64 1L)
    let ops =
      [ PT.PackageOp.AddFn fnA
        PT.PackageOp.SetName(loc "partialA", PT.PackageFn fnA.hash)
        PT.PackageOp.AddFn fnB
        PT.PackageOp.SetName(loc "partialB", PT.PackageFn fnB.hash) ]
    let! (_ : int64) = Inserts.insertAndApplyOpsAsWip branch.id ops

    let wipLocCount () : Task<int64> =
      Sql.query
        """
        SELECT COUNT(*) as cnt FROM locations
        WHERE branch_id = @branch_id AND commit_hash IS NULL
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branch.id ]
      |> Sql.executeRowAsync (fun read -> read.int64 "cnt")

    let! locsBefore = wipLocCount ()
    Expect.equal locsBefore 2L "both fns' locations are WIP before commit"

    // Select only fnA's two ops (its AddFn + SetName).
    let! wip = LibDB.Queries.getWipOpsWithIds branch.id
    let idsForFn (hash : PT.Hash) =
      wip
      |> List.choose (fun (id, op, _) ->
        match op with
        | PT.PackageOp.AddFn f when f.hash = hash -> Some id
        | PT.PackageOp.SetName(_, target) when target.hash = hash -> Some id
        | _ -> None)
    let selectedIds = idsForFn fnA.hash
    Expect.hasLength selectedIds 2 "should select fnA's AddFn + SetName"

    let! (commitResult : Result<PT.Hash, string>) =
      Inserts.commitWipOpsByIds
        LibCloud.Account.IDs.darklang
        branch.id
        "partial commit: fnA only"
        selectedIds
    Expect.isOk commitResult "partial commit should succeed"

    // fnA's ops + location are committed; fnB's stay WIP.
    let mentions (hash : PT.Hash) (op : PT.PackageOp) : bool =
      match op with
      | PT.PackageOp.AddFn f -> f.hash = hash
      | PT.PackageOp.SetName(_, target) -> target.hash = hash
      | _ -> false

    let! remainingWip = LibDB.Queries.getWipOps branch.id
    Expect.hasLength remainingWip 2 "only fnB's two ops should remain WIP"
    Expect.isFalse
      (remainingWip |> List.exists (mentions fnA.hash))
      "fnA's ops should be committed, not WIP"
    Expect.isTrue
      (remainingWip |> List.exists (mentions fnB.hash))
      "fnB's ops should still be WIP"

    let! locsAfter = wipLocCount ()
    Expect.equal locsAfter 1L "only fnB's location should remain WIP"
  }


let testPartialCommitSameFqn =
  testTask
    "partial commit: same-FQN updates flip only the selected version's location" {
    let! (branch : PT.Branch) = Branches.create "test-bo-partial-fqn" PT.mainBranchId

    // Two versions of the SAME fqn (distinct bodies → distinct hashes), each
    // AddFn + SetName at the same location. The second SetName unlists v1's
    // WIP location row but leaves it commit_hash NULL, so two WIP location
    // rows for one FQN coexist.
    let v1 = makeFn (eVar "x")
    let v2 = makeFn (eInt64 7L)
    let ops =
      [ PT.PackageOp.AddFn v1
        PT.PackageOp.SetName(loc "samefqn", PT.PackageFn v1.hash)
        PT.PackageOp.AddFn v2
        PT.PackageOp.SetName(loc "samefqn", PT.PackageFn v2.hash) ]
    let! (_ : int64) = Inserts.insertAndApplyOpsAsWip branch.id ops

    let wipLocCount () : Task<int64> =
      Sql.query
        """
        SELECT COUNT(*) as cnt FROM locations
        WHERE branch_id = @branch_id AND commit_hash IS NULL
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branch.id ]
      |> Sql.executeRowAsync (fun read -> read.int64 "cnt")

    let! locsBefore = wipLocCount ()
    Expect.equal locsBefore 2L "both versions leave a WIP location row"

    // Commit only v1's ops (AddFn v1 + its SetName). v2's SetName is not
    // selected, so its location row must stay WIP — an FQN-only projection
    // match would wrongly flip it too (this would read 0).
    let! wip = LibDB.Queries.getWipOpsWithIds branch.id
    let v1Ids =
      wip
      |> List.choose (fun (id, op, _) ->
        match op with
        | PT.PackageOp.AddFn f when f.hash = v1.hash -> Some id
        | PT.PackageOp.SetName(_, target) when target.hash = v1.hash -> Some id
        | _ -> None)
    Expect.hasLength v1Ids 2 "should select v1's AddFn + SetName"

    let! (commitResult : Result<PT.Hash, string>) =
      Inserts.commitWipOpsByIds
        LibCloud.Account.IDs.darklang
        branch.id
        "commit v1 only"
        v1Ids
    Expect.isOk commitResult "partial commit should succeed"

    let! locsAfter = wipLocCount ()
    Expect.equal locsAfter 1L "only v2's location row should remain WIP"
  }


let testPartialCommitDeprecationState =
  testTask
    "partial commit: Deprecate commits without flipping a still-WIP Undeprecate" {
    let! (branch : PT.Branch) = Branches.create "test-bo-partial-dep" PT.mainBranchId

    // Add a fn, deprecate it, then undeprecate it. The Undeprecate supersedes
    // the WIP "deprecated" row (unlists it, still commit_hash NULL) and inserts
    // an "undeprecated" row — two WIP deprecation rows for the same (hash, fn).
    let fn1 = makeFn (eVar "x")
    let ops =
      [ PT.PackageOp.AddFn fn1
        PT.PackageOp.SetName(loc "depper", PT.PackageFn fn1.hash)
        PT.PackageOp.Deprecate(
          PT.PackageFn fn1.hash,
          PT.DeprecationKind.Obsolete,
          "obsolete for test"
        )
        PT.PackageOp.Undeprecate(PT.PackageFn fn1.hash) ]
    let! (_ : int64) = Inserts.insertAndApplyOpsAsWip branch.id ops

    let wipDepCount () : Task<int64> =
      Sql.query
        """
        SELECT COUNT(*) as cnt FROM deprecations
        WHERE branch_id = @branch_id AND commit_hash IS NULL
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branch.id ]
      |> Sql.executeRowAsync (fun read -> read.int64 "cnt")

    let! depsBefore = wipDepCount ()
    Expect.equal
      depsBefore
      2L
      "deprecate + undeprecate leave two WIP deprecation rows"

    // Commit everything EXCEPT the Undeprecate op.
    let! wip = LibDB.Queries.getWipOpsWithIds branch.id
    let idsToCommit =
      wip
      |> List.choose (fun (id, op, _) ->
        match op with
        | PT.PackageOp.Undeprecate _ -> None
        | _ -> Some id)
    Expect.hasLength
      idsToCommit
      3
      "commit AddFn + SetName + Deprecate, not Undeprecate"

    let! (commitResult : Result<PT.Hash, string>) =
      Inserts.commitWipOpsByIds
        LibCloud.Account.IDs.darklang
        branch.id
        "commit deprecation only"
        idsToCommit
    Expect.isOk commitResult "partial commit should succeed"

    // The Undeprecate op stays WIP, so its "undeprecated" projection row must
    // too — a (hash, kind)-only match (no state) would flip it (reads 0).
    let! depsAfter = wipDepCount ()
    Expect.equal depsAfter 1L "only the undeprecated row should remain WIP"
  }


let tests =
  testList
    "BranchOps"
    [ testBranchOpsEmitted
      testBranchOpsSerialization
      testBranchOpsDeserialization
      testGhostFunctionCrossBranch
      testPartialCommit
      testPartialCommitSameFqn
      testPartialCommitDeprecationState ]
