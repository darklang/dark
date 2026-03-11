module Tests.BranchOps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude

open TestUtils.TestUtils
open TestUtils.PTShortcuts

module PT = LibExecution.ProgramTypes
module Branches = LibPackageManager.Branches
module Inserts = LibPackageManager.Inserts
module BranchOpPlayback = LibPackageManager.BranchOpPlayback

open Fumble
open LibDB.Db

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
    let ops = [ PT.PackageOp.AddFn fn1; PT.PackageOp.SetFnName(fn1.hash, loc "bo1") ]
    let! (_insertCount : int64) = Inserts.insertAndApplyOpsAsWip branch.id ops
    let! (commitResult : Result<PT.Hash, string>) =
      Inserts.commitWipOps branch.id "test commit"
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


let testDbReconstructionFromOps =
  testTask "branch ops: can reconstruct branches/commits from ops alone" {
    // 1. Read all branch_ops
    let! branchOps =
      Sql.query "SELECT id, op_blob FROM branch_ops ORDER BY created_at ASC"
      |> Sql.executeAsync (fun read ->
        let id = read.string "id"
        let blob = read.bytes "op_blob"
        BS.PT.BranchOp.deserialize id blob)

    Expect.isNonEmpty branchOps "should have branch_ops to replay"

    // 2. Clear branches (except migration-created main) and commits
    //    Use a single transaction to delete in FK-safe order
    let _ =
      Sql.executeTransactionSync
        [ "DELETE FROM locations", [ [] ]
          "DELETE FROM package_ops", [ [] ]
          "DELETE FROM commits", [ [] ]
          ("DELETE FROM branches WHERE id != '89282547-e4e6-4986-bcb6-db74bc6a8c0f'",
           [ [] ]) ]

    let! branchCountCleared = countRows "branches"
    Expect.equal branchCountCleared 1L "should only have main branch"
    let! commitCountCleared = countRows "commits"
    Expect.equal commitCountCleared 0L "should have no commits"

    // 3. Replay all branch_ops
    for op in branchOps do
      do! BranchOpPlayback.applyOp op

    // 4. Verify main branch still exists
    let! mainBranch =
      Sql.query
        "SELECT name FROM branches WHERE id = '89282547-e4e6-4986-bcb6-db74bc6a8c0f'"
      |> Sql.executeAsync (fun read -> read.string "name")
    Expect.equal mainBranch [ "main" ] "main branch should exist"

    // 5. Verify commits were reconstructed
    let! reconstructedCommits = countRows "commits"
    Expect.isGreaterThan reconstructedCommits 0L "should have commits after replay"

    // 6. Verify specific commit messages exist
    let! commitMessages =
      Sql.query "SELECT message FROM commits ORDER BY created_at"
      |> Sql.executeAsync (fun read -> read.string "message")
    Expect.contains
      commitMessages
      "Init: packages loaded from disk"
      "should have init commit"
  }


let tests =
  testList
    "BranchOps"
    [ testBranchOpsEmitted; testBranchOpsSerialization; testDbReconstructionFromOps ]
