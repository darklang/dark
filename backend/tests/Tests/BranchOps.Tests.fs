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


let tests =
  testList
    "BranchOps"
    [ testBranchOpsEmitted
      testBranchOpsSerialization
      testBranchOpsDeserialization
      testGhostFunctionCrossBranch ]
