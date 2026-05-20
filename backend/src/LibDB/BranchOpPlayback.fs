module LibDB.BranchOpPlayback

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
open LibSerialization.Hashing


/// Apply a BranchOp to the branches/commits tables.
/// This is the single source of truth for what each op does —
/// mutation sites call insertAndApply, and replay calls applyOp directly.
let applyOp (op : PT.BranchOp) : Task<unit> =
  task {
    match op with
    | PT.BranchOp.CreateBranch(branchId, name, parentBranchId, baseCommitHash) ->
      let baseCommitHashParam =
        match baseCommitHash with
        | Some(Hash h) -> Sql.string h
        | None -> Sql.dbnull

      let parentIdParam =
        match parentBranchId with
        | Some pid -> Sql.uuid pid
        | None -> Sql.dbnull

      do!
        Sql.query
          """
          INSERT OR IGNORE INTO branches (id, name, parent_branch_id, base_commit_hash, created_at)
          VALUES (@id, @name, @parent_id, @base_commit_hash, datetime('now'))
          """
        |> Sql.parameters
          [ "id", Sql.uuid branchId
            "name", Sql.string name
            "parent_id", parentIdParam
            "base_commit_hash", baseCommitHashParam ]
        |> Sql.executeStatementAsync

    | PT.BranchOp.CreateCommit(commitHash, message, accountId, branchId, _opHashes) ->
      let (Hash commitHashStr) = commitHash
      do!
        Sql.query
          """
          INSERT OR IGNORE INTO commits (hash, message, branch_id, account_id, created_at)
          VALUES (@hash, @message, @branch_id, @account_id, datetime('now'))
          """
        |> Sql.parameters
          [ "hash", Sql.string commitHashStr
            "message", Sql.string message
            "branch_id", Sql.uuid branchId
            "account_id", Sql.uuid accountId ]
        |> Sql.executeStatementAsync

    | PT.BranchOp.RebaseBranch(branchId, newBaseCommitHash) ->
      let (Hash h) = newBaseCommitHash
      do!
        Sql.query
          """
          UPDATE branches SET base_commit_hash = @base_commit_hash WHERE id = @id
          """
        |> Sql.parameters
          [ "id", Sql.uuid branchId; "base_commit_hash", Sql.string h ]
        |> Sql.executeStatementAsync

    | PT.BranchOp.MergeBranch(branchId, intoBranchId) ->
      let mergeStatements =
        let parentParams =
          [ "parent_id", Sql.uuid intoBranchId; "branch_id", Sql.uuid branchId ]
        [ ("""
           UPDATE locations
           SET unlisted_at = datetime('now')
           WHERE branch_id = @parent_id
             AND unlisted_at IS NULL
             AND (owner, modules, name, item_type) IN (
               SELECT owner, modules, name, item_type
               FROM locations
               WHERE branch_id = @branch_id AND unlisted_at IS NULL
             )
           """,
           [ parentParams ])

          ("UPDATE commits SET branch_id = @parent_id WHERE branch_id = @branch_id",
           [ parentParams ])

          // Drop duplicate child ops before moving the remaining rows to the
          // parent; otherwise the (id, branch_id) PK rejects the UPDATE below.
          // This preserves program state because the duplicate op content is
          // identical.
          //
          // History caveat: deleting the child row can make getCommitOps miss
          // that child's commit attribution. The proper fix is to store
          // commit-to-op attribution separately from package_ops.
          ("""
           DELETE FROM package_ops
           WHERE branch_id = @branch_id
             AND id IN (
               SELECT id FROM package_ops WHERE branch_id = @parent_id
             )
           """,
           [ parentParams ])

          ("UPDATE package_ops SET branch_id = @parent_id WHERE branch_id = @branch_id",
           [ parentParams ])

          ("""
           UPDATE locations SET branch_id = @parent_id
           WHERE branch_id = @branch_id AND unlisted_at IS NULL
           """,
           [ parentParams ])

          ("UPDATE branches SET merged_at = datetime('now') WHERE id = @id",
           [ [ "id", Sql.uuid branchId ] ]) ]

      let _ = Sql.executeTransactionSync mergeStatements
      ()

    | PT.BranchOp.ArchiveBranch branchId ->
      do!
        Sql.query "UPDATE branches SET archived_at = datetime('now') WHERE id = @id"
        |> Sql.parameters [ "id", Sql.uuid branchId ]
        |> Sql.executeStatementAsync
  }


/// Insert a BranchOp into the branch_ops table and apply it.
/// Uses INSERT OR IGNORE for idempotency (content-addressed by hash).
let insertAndApply (op : PT.BranchOp) : Task<unit> =
  task {
    let opHash = Hashing.computeBranchOpHash op
    let (Hash hashStr) = opHash
    let opBlob = BS.PT.BranchOp.serialize hashStr op

    // Phase 1: Insert with applied=false
    let! rowsAffected =
      Sql.query
        """
        INSERT OR IGNORE INTO branch_ops (id, op_blob, applied, created_at)
        VALUES (@id, @op_blob, 0, datetime('now'))
        """
      |> Sql.parameters [ "id", Sql.string hashStr; "op_blob", Sql.bytes opBlob ]
      |> Sql.executeNonQueryAsync

    // Phase 2: Apply if newly inserted (skip if duplicate)
    if rowsAffected > 0 then
      do! applyOp op

      // Phase 3: Mark as applied
      try
        do!
          Sql.query "UPDATE branch_ops SET applied = 1 WHERE id = @id"
          |> Sql.parameters [ "id", Sql.string hashStr ]
          |> Sql.executeStatementAsync
      with ex ->
        System.Console.Error.WriteLine(
          $"Warning: Failed to mark BranchOp {hashStr} as applied: {ex.Message}"
        )
  }
