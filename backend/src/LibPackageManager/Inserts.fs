module LibPackageManager.Inserts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


/// Compute a content-addressed ID for a PackageOp by hashing its serialized content
///
/// TODO this is really hacky.
/// honestly we should
/// - make hashing more legit.
/// - rebrand LibBinarySerialization as LibSerialization
/// - migrate any remaining hacky JSON serialization things there
///   (if LibExecution needs them, maybe ExecutionState needs to/from json fns to be part of that context)
/// - put all sorts of Hashers there -
let computeOpHash (op : PT.PackageOp) : System.Guid =
  use memoryStream = new System.IO.MemoryStream()
  use binaryWriter = new System.IO.BinaryWriter(memoryStream)

  // Serialize just the op content (without an ID)
  LibBinarySerialization.Serializers.PT.PackageOp.write binaryWriter op

  let opBytes = memoryStream.ToArray()
  let hashBytes = System.Security.Cryptography.SHA256.HashData(opBytes)

  // Take first 16 bytes to make a UUID
  System.Guid(hashBytes[0..15])


/// Insert PackageOps into the package_ops table and apply them to projection tables.
/// branchId = branch context
/// commitId = None means WIP (commit_id = NULL), Some id means committed
/// Returns the count of ops actually inserted (duplicates are skipped via INSERT OR IGNORE)
///
/// Uses a two-phase approach for consistency:
/// 1. Insert ops with applied=false
/// 2. Apply ops to projection tables
/// 3. Mark ops as applied=true
///
/// This ensures that if step 2 fails, we can identify unapplied ops and retry/rollback.
let insertAndApplyOps
  (branchId : PT.BranchId)
  (commitId : Option<System.Guid>)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  task {
    if List.isEmpty ops then
      return 0L
    else
      // Phase 1: Insert ops with applied=false
      // Tag all ops in a propagation batch with the same propagation_id.
      // This allows cleanup of all related ops when undoing a propagation.
      let batchPropagationId =
        ops
        |> List.tryPick (fun op ->
          match op with
          | PT.PackageOp.PropagateUpdate(pid, _, _, _, _, _) -> Some pid
          | PT.PackageOp.RevertPropagation(rid, _, _, _, _, _) -> Some rid
          | _ -> None)

      let opsWithIds =
        ops
        |> List.map (fun op ->
          let opId = computeOpHash op
          let opBlob = BinarySerialization.PT.PackageOp.serialize opId op
          (opId, op, opBlob, batchPropagationId))

      let insertStatements =
        opsWithIds
        |> List.map (fun (opId, _op, opBlob, propagationId) ->
          let sql =
            """
            INSERT OR IGNORE INTO package_ops (id, op_blob, branch_id, applied, commit_id, propagation_id)
            VALUES (@id, @op_blob, @branch_id, @applied, @commit_id, @propagation_id)
            """

          let parameters =
            [ "id", Sql.uuid opId
              "op_blob", Sql.bytes opBlob
              "branch_id", Sql.uuid branchId
              "applied", Sql.bool false // Insert as unapplied
              "commit_id", Sql.uuidOrNone commitId
              "propagation_id",
              (match propagationId with
               | Some id -> Sql.uuid id
               | None -> Sql.dbnull) ]

          (sql, [ parameters ]))

      let rowsAffected = insertStatements |> Sql.executeTransactionSync

      // Count how many ops were actually inserted (vs skipped as duplicates)
      let insertedCount = rowsAffected |> List.sumBy int64

      // Identify which ops were actually inserted
      let insertedOpsWithIds =
        List.zip opsWithIds rowsAffected
        |> List.filter (fun (_, affected) -> affected > 0)
        |> List.map fst

      let opsToApply = insertedOpsWithIds |> List.map (fun (_, op, _, _) -> op)
      let insertedOpIds =
        insertedOpsWithIds |> List.map (fun (opId, _, _, _) -> opId)

      do! PackageOpPlayback.applyOps branchId commitId opsToApply

      // Mark ops as applied (non-critical - ops are already applied)
      if not (List.isEmpty insertedOpIds) then
        try
          let updateStatements =
            insertedOpIds
            |> List.map (fun opId ->
              let sql = "UPDATE package_ops SET applied = @applied WHERE id = @id"
              let parameters = [ "applied", Sql.bool true; "id", Sql.uuid opId ]
              (sql, [ parameters ]))

          let _ = updateStatements |> Sql.executeTransactionSync
          ()
        with ex ->
          System.Console.Error.WriteLine(
            $"Warning: Failed to mark {List.length insertedOpIds} ops as applied: {ex.Message}"
          )

      return insertedCount
  }


/// Create a new commit and insert ops with that commit_id
/// Returns the commit ID
let insertAndApplyOpsWithCommit
  (branchId : PT.BranchId)
  (message : string)
  (ops : List<PT.PackageOp>)
  : Task<System.Guid> =
  task {
    let commitId = System.Guid.NewGuid()

    // Create the commit record
    do!
      Sql.query
        """
        INSERT INTO commits (id, message, branch_id, created_at)
        VALUES (@id, @message, @branch_id, datetime('now'))
        """
      |> Sql.parameters
        [ "id", Sql.uuid commitId
          "message", Sql.string message
          "branch_id", Sql.uuid branchId ]
      |> Sql.executeStatementAsync

    // Insert ops with the commit_id
    let! _ = insertAndApplyOps branchId (Some commitId) ops

    return commitId
  }


/// Insert ops as WIP (commit_id = NULL)
/// Returns count of inserted ops
let insertAndApplyOpsAsWip
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  insertAndApplyOps branchId None ops


/// Commit all WIP ops on a branch by creating a new commit and assigning commit_id
/// Returns the commit ID on success
let commitWipOps
  (branchId : PT.BranchId)
  (message : string)
  : Task<Result<System.Guid, string>> =
  task {
    try
      // Check if there are any WIP ops on this branch
      let! wipCount =
        Sql.query
          "SELECT COUNT(*) as cnt FROM package_ops WHERE branch_id = @branch_id AND commit_id IS NULL"
        |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
        |> Sql.executeAsync (fun read -> read.int64 "cnt")

      match wipCount with
      | [ 0L ] -> return Error "Nothing to commit"
      | [ _ ] ->
        let commitId = System.Guid.NewGuid()

        // Execute all three operations atomically:
        // 1. Create commit record
        // 2. Assign WIP ops to this commit
        // 3. Assign WIP locations to this commit
        let statements =
          [ ("""
             INSERT INTO commits (id, message, branch_id, created_at)
             VALUES (@id, @message, @branch_id, datetime('now'))
             """,
             [ [ "id", Sql.uuid commitId
                 "message", Sql.string message
                 "branch_id", Sql.uuid branchId ] ])

            ("""
             UPDATE package_ops
             SET commit_id = @commit_id
             WHERE branch_id = @branch_id AND commit_id IS NULL
             """,
             [ [ "commit_id", Sql.uuid commitId; "branch_id", Sql.uuid branchId ] ])

            ("""
             UPDATE locations
             SET commit_id = @commit_id
             WHERE branch_id = @branch_id AND commit_id IS NULL
             """,
             [ [ "commit_id", Sql.uuid commitId; "branch_id", Sql.uuid branchId ] ]) ]

        let _ = Sql.executeTransactionSync statements

        return Ok commitId
      | _ -> return Error "Unexpected query result"
    with ex ->
      return Error ex.Message
  }


/// Find the committed UUID at a location, checking the current branch first,
/// then falling back to ancestor branches.
/// Returns Ok(uuid, locationIdOpt) where locationIdOpt is Some for same-branch
/// committed locations (that need un-deprecating) or None for ancestor locations
/// (which are already active on the parent).
let findCommittedUUID
  (branchId : PT.BranchId)
  (owner : string)
  (modules : string)
  (name : string)
  (itemType : string)
  : Task<Result<uuid * Option<uuid>, string>> =
  task {
    // First: look for deprecated committed location on current branch
    let! committedLocations =
      Sql.query
        """
        SELECT location_id, item_id
        FROM locations
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND item_type = @item_type
          AND branch_id = @branch_id
          AND commit_id IS NOT NULL
          AND deprecated_at IS NOT NULL
        ORDER BY deprecated_at DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "owner", Sql.string owner
          "modules", Sql.string modules
          "name", Sql.string name
          "item_type", Sql.string itemType
          "branch_id", Sql.uuid branchId ]
      |> Sql.executeAsync (fun read ->
        (read.uuid "location_id", read.uuid "item_id"))

    match committedLocations with
    | (locationId, itemId) :: _ -> return Ok(itemId, Some locationId)
    | [] ->
      // Fall back to ancestor branches for an active committed location.
      // The parent's location was never deprecated by applySetName
      // (statement 2 scopes to branch_id), so it's still active.
      let! branchChain = Branches.getBranchChain branchId
      let ancestors = branchChain |> List.filter (fun id -> id <> branchId)

      if List.isEmpty ancestors then
        return Error "No committed version found to restore"
      else
        let branchParams =
          ancestors |> List.mapi (fun i id -> $"ab_{i}", Sql.uuid id)

        let branchInClause =
          ancestors |> List.mapi (fun i _ -> $"@ab_{i}") |> String.concat ", "

        let! ancestorLocations =
          Sql.query
            $"""
            SELECT item_id
            FROM locations
            WHERE owner = @owner
              AND modules = @modules
              AND name = @name
              AND item_type = @item_type
              AND branch_id IN ({branchInClause})
              AND deprecated_at IS NULL
            LIMIT 1
            """
          |> Sql.parameters (
            [ "owner", Sql.string owner
              "modules", Sql.string modules
              "name", Sql.string name
              "item_type", Sql.string itemType ]
            @ branchParams
          )
          |> Sql.executeAsync (fun read -> read.uuid "item_id")

        match ancestorLocations with
        | itemId :: _ -> return Ok(itemId, None)
        | [] -> return Error "No committed version found to restore"
  }


/// Discard all WIP ops on a branch by deleting them and their effects
/// Returns the count of discarded ops
let discardWipOps (branchId : PT.BranchId) : Task<Result<int64, string>> =
  task {
    try
      // Get count before deleting
      let! wipOps =
        Sql.query
          """
          SELECT id FROM package_ops WHERE branch_id = @branch_id AND commit_id IS NULL
          """
        |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
        |> Sql.executeAsync (fun read -> read.uuid "id")

      let count = int64 (List.length wipOps)

      if count = 0L then
        return Ok 0L
      else
        // Restore committed locations that were deprecated by WIP ops.
        // When a WIP SetName op runs, it deprecates the existing committed location.
        // We need to un-deprecate the most recent committed location for each path
        // that has a WIP location but no active committed location.
        do!
          Sql.query
            """
            UPDATE locations
            SET deprecated_at = NULL
            WHERE location_id IN (
              SELECT committed_loc.location_id
              FROM locations wip_loc
              -- Find the most recently deprecated committed location at the same path
              INNER JOIN locations committed_loc
                ON committed_loc.owner = wip_loc.owner
                AND committed_loc.modules = wip_loc.modules
                AND committed_loc.name = wip_loc.name
                AND committed_loc.item_type = wip_loc.item_type
                AND committed_loc.branch_id = wip_loc.branch_id
                AND committed_loc.commit_id IS NOT NULL
                AND committed_loc.deprecated_at IS NOT NULL
              WHERE wip_loc.branch_id = @branch_id
                AND wip_loc.commit_id IS NULL
                -- Only restore if there's no other active committed location at this path
                AND NOT EXISTS (
                  SELECT 1 FROM locations active
                  WHERE active.owner = wip_loc.owner
                    AND active.modules = wip_loc.modules
                    AND active.name = wip_loc.name
                    AND active.item_type = wip_loc.item_type
                    AND active.branch_id = wip_loc.branch_id
                    AND active.commit_id IS NOT NULL
                    AND active.deprecated_at IS NULL
                )
                -- Pick the most recently deprecated committed location
                AND committed_loc.deprecated_at = (
                  SELECT MAX(c2.deprecated_at)
                  FROM locations c2
                  WHERE c2.owner = wip_loc.owner
                    AND c2.modules = wip_loc.modules
                    AND c2.name = wip_loc.name
                    AND c2.item_type = wip_loc.item_type
                    AND c2.branch_id = wip_loc.branch_id
                    AND c2.commit_id IS NOT NULL
                    AND c2.deprecated_at IS NOT NULL
                )
            )
            """
          |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
          |> Sql.executeStatementAsync

        // Delete WIP locations
        do!
          Sql.query
            "DELETE FROM locations WHERE branch_id = @branch_id AND commit_id IS NULL"
          |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
          |> Sql.executeStatementAsync

        // Delete WIP ops
        do!
          Sql.query
            "DELETE FROM package_ops WHERE branch_id = @branch_id AND commit_id IS NULL"
          |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
          |> Sql.executeStatementAsync

        // Note: We don't delete from package_types/values/functions because
        // they're content-addressed and might be referenced by committed ops.
        // They'll be cleaned up by garbage collection if truly orphaned.

        return Ok count
    with ex ->
      return Error ex.Message
  }
