module LibPackageManager.BranchOpPlayback

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Db

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

    | PT.BranchOp.CreateCommit(commitHash, message, branchId, _opHashes, accountId) ->
      let (Hash commitHashStr) = commitHash
      do!
        Sql.query
          """
          INSERT OR IGNORE INTO commits (hash, message, account_id, branch_id, created_at)
          VALUES (@hash, @message, @account_id, @branch_id, datetime('now'))
          """
        |> Sql.parameters
          [ "hash", Sql.string commitHashStr
            "message", Sql.string message
            "account_id", Sql.uuid accountId
            "branch_id", Sql.uuid branchId ]
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
      // Deprecate parent locations at paths that the branch is overwriting
      let! branchLocations =
        Sql.query
          """
          SELECT owner, modules, name, item_type
          FROM locations
          WHERE branch_id = @branch_id
            AND deprecated_at IS NULL
          """
        |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
        |> Sql.executeAsync (fun read ->
          (read.string "owner",
           read.string "modules",
           read.string "name",
           read.string "item_type"))

      for (owner, modules, name, itemType) in branchLocations do
        do!
          Sql.query
            """
            UPDATE locations
            SET deprecated_at = datetime('now')
            WHERE branch_id = @parent_id
              AND owner = @owner
              AND modules = @modules
              AND name = @name
              AND item_type = @item_type
              AND deprecated_at IS NULL
            """
          |> Sql.parameters
            [ "parent_id", Sql.uuid intoBranchId
              "owner", Sql.string owner
              "modules", Sql.string modules
              "name", Sql.string name
              "item_type", Sql.string itemType ]
          |> Sql.executeStatementAsync

      // Move commits, ops, locations to parent
      do!
        Sql.query
          "UPDATE commits SET branch_id = @parent_id WHERE branch_id = @branch_id"
        |> Sql.parameters
          [ "parent_id", Sql.uuid intoBranchId; "branch_id", Sql.uuid branchId ]
        |> Sql.executeStatementAsync

      do!
        Sql.query
          "UPDATE package_ops SET branch_id = @parent_id WHERE branch_id = @branch_id"
        |> Sql.parameters
          [ "parent_id", Sql.uuid intoBranchId; "branch_id", Sql.uuid branchId ]
        |> Sql.executeStatementAsync

      do!
        Sql.query
          """
          UPDATE locations SET branch_id = @parent_id
          WHERE branch_id = @branch_id AND deprecated_at IS NULL
          """
        |> Sql.parameters
          [ "parent_id", Sql.uuid intoBranchId; "branch_id", Sql.uuid branchId ]
        |> Sql.executeStatementAsync

      do!
        Sql.query "UPDATE branches SET merged_at = datetime('now') WHERE id = @id"
        |> Sql.parameters [ "id", Sql.uuid branchId ]
        |> Sql.executeStatementAsync

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
