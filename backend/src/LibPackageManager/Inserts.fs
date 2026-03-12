module LibPackageManager.Inserts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
open LibSerialization.Hashing


/// Compute a content-addressed ID for a PackageOp.
/// Returns a UUID derived from the Hash (first 16 bytes) for DB compatibility.
/// TODO: consider whether package_ops.id should store the full hash instead of a truncated UUID.
let computeOpHash (op : PT.PackageOp) : System.Guid =
  let (Hash h) = Hashing.computeOpHash op
  // Convert hex string back to bytes, take first 16 for UUID
  let hashBytes = System.Convert.FromHexString(h)
  System.Guid(hashBytes[0..15])


/// Insert PackageOps into the package_ops table and apply them to projection tables.
/// branchId = branch context
/// commitHash = None means WIP (commit_hash = NULL), Some id means committed
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
  (commitHash : Option<string>)
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
          let opBlob = BS.PT.PackageOp.serialize opId op
          (opId, op, opBlob, batchPropagationId))

      let insertStatements =
        opsWithIds
        |> List.map (fun (opId, _op, opBlob, propagationId) ->
          let sql =
            """
            INSERT OR IGNORE INTO package_ops (id, op_blob, branch_id, applied, commit_hash, propagation_id)
            VALUES (@id, @op_blob, @branch_id, @applied, @commit_hash, @propagation_id)
            """

          let commitHashParam =
            match commitHash with
            | Some s -> Sql.string s
            | None -> Sql.dbnull

          let parameters =
            [ "id", Sql.uuid opId
              "op_blob", Sql.bytes opBlob
              "branch_id", Sql.uuid branchId
              "applied", Sql.bool false // Insert as unapplied
              "commit_hash", commitHashParam
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

      do! PackageOpPlayback.applyOps branchId commitHash opsToApply

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


/// Create a new commit and insert ops with that commit_hash
/// Returns the commit Hash
let insertAndApplyOpsWithCommit
  (accountId : UserID)
  (branchId : PT.BranchId)
  (message : string)
  (ops : List<PT.PackageOp>)
  : Task<Hash> =
  task {
    // Get parent commit hash
    let! parentHash =
      Sql.query
        """
        SELECT hash FROM commits
        WHERE branch_id = @branch_id
        ORDER BY created_at DESC
        LIMIT 1
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
      |> Sql.executeRowOptionAsync (fun read -> Hash(read.string "hash"))

    // Compute content-addressed commit hash
    let opHashes = ops |> List.map Hashing.computeOpHash
    let commitHash = Hashing.computeCommitHash parentHash opHashes accountId
    let (Hash commitHashStr) = commitHash

    // Record and apply the commit
    do!
      BranchOpPlayback.insertAndApply (
        PT.BranchOp.CreateCommit(commitHash, message, branchId, opHashes, accountId)
      )

    // Insert ops with the commit_hash
    let! _ = insertAndApplyOps branchId (Some commitHashStr) ops

    return commitHash
  }


/// Insert ops as WIP (commit_hash = NULL)
/// Returns count of inserted ops
let insertAndApplyOpsAsWip
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  insertAndApplyOps branchId None ops


/// Commit all WIP ops on a branch by creating a new commit and assigning commit_hash.
/// Commit hash is content-addressed: hash(parentHash + sorted opHashes + accountId).
/// Returns the commit Hash on success.
let commitWipOps
  (accountId : UserID)
  (branchId : PT.BranchId)
  (message : string)
  : Task<Result<Hash, string>> =
  task {
    try
      // Get WIP ops with their hashes
      let! wipOps =
        Sql.query
          """
          SELECT id, op_blob
          FROM package_ops
          WHERE branch_id = @branch_id AND commit_hash IS NULL
          ORDER BY created_at ASC
          """
        |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
        |> Sql.executeAsync (fun read ->
          let opId = read.uuid "id"
          let opBlob = read.bytes "op_blob"
          let op = BS.PT.PackageOp.deserialize opId opBlob
          (opId, op))

      if List.isEmpty wipOps then
        return Error "Nothing to commit"
      else
        // Get parent commit hash (latest commit on this branch)
        let! parentHash =
          Sql.query
            """
            SELECT hash FROM commits
            WHERE branch_id = @branch_id
            ORDER BY created_at DESC
            LIMIT 1
            """
          |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
          |> Sql.executeRowOptionAsync (fun read -> Hash(read.string "hash"))

        // Compute op hashes
        let opHashes = wipOps |> List.map (fun (_, op) -> Hashing.computeOpHash op)

        // Compute content-addressed commit hash (includes accountId for integrity)
        let commitHash = Hashing.computeCommitHash parentHash opHashes accountId
        let (Hash commitHashStr) = commitHash

        // Record and apply the commit
        do!
          BranchOpPlayback.insertAndApply (
            PT.BranchOp.CreateCommit(
              commitHash, message, branchId, opHashes, accountId
            )
          )

        // Assign WIP ops and locations to this commit
        let statements =
          [ ("""
             UPDATE package_ops
             SET commit_hash = @commit_hash
             WHERE branch_id = @branch_id AND commit_hash IS NULL
             """,
             [ [ "commit_hash", Sql.string commitHashStr
                 "branch_id", Sql.uuid branchId ] ])

            ("""
             UPDATE locations
             SET commit_hash = @commit_hash
             WHERE branch_id = @branch_id AND commit_hash IS NULL
             """,
             [ [ "commit_hash", Sql.string commitHashStr
                 "branch_id", Sql.uuid branchId ] ]) ]

        let _ = Sql.executeTransactionSync statements

        return Ok commitHash
    with ex ->
      return Error ex.Message
  }


/// Find the committed Hash at a location, checking the current branch first,
/// then falling back to ancestor branches.
/// Returns Ok(hash, locationIdOpt) where locationIdOpt is Some for same-branch
/// committed locations (that need un-deprecating) or None for ancestor locations
/// (which are already active on the parent).
let findCommittedHash
  (branchId : PT.BranchId)
  (owner : string)
  (modules : string)
  (name : string)
  (itemType : string)
  : Task<Result<Hash * Option<uuid>, string>> =
  task {
    // First: look for deprecated committed location on current branch
    let! committedLocations =
      Sql.query
        """
        SELECT location_id, item_hash
        FROM locations
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND item_type = @item_type
          AND branch_id = @branch_id
          AND commit_hash IS NOT NULL
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
        (read.uuid "location_id", Hash(read.string "item_hash")))

    match committedLocations with
    | (locationId, itemHash) :: _ -> return Ok(itemHash, Some locationId)
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
            SELECT item_hash
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
          |> Sql.executeAsync (fun read -> Hash(read.string "item_hash"))

        match ancestorLocations with
        | itemHash :: _ -> return Ok(itemHash, None)
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
          SELECT id FROM package_ops WHERE branch_id = @branch_id AND commit_hash IS NULL
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
                AND committed_loc.commit_hash IS NOT NULL
                AND committed_loc.deprecated_at IS NOT NULL
              WHERE wip_loc.branch_id = @branch_id
                AND wip_loc.commit_hash IS NULL
                -- Only restore if there's no other active committed location at this path
                AND NOT EXISTS (
                  SELECT 1 FROM locations active
                  WHERE active.owner = wip_loc.owner
                    AND active.modules = wip_loc.modules
                    AND active.name = wip_loc.name
                    AND active.item_type = wip_loc.item_type
                    AND active.branch_id = wip_loc.branch_id
                    AND active.commit_hash IS NOT NULL
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
                    AND c2.commit_hash IS NOT NULL
                    AND c2.deprecated_at IS NOT NULL
                )
            )
            """
          |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
          |> Sql.executeStatementAsync

        // Delete WIP locations
        do!
          Sql.query
            "DELETE FROM locations WHERE branch_id = @branch_id AND commit_hash IS NULL"
          |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
          |> Sql.executeStatementAsync

        // Delete WIP ops
        do!
          Sql.query
            "DELETE FROM package_ops WHERE branch_id = @branch_id AND commit_hash IS NULL"
          |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
          |> Sql.executeStatementAsync

        // Note: We don't delete from package_types/values/functions because
        // they're content-addressed and might be referenced by committed ops.
        // They'll be cleaned up by garbage collection if truly orphaned.

        return Ok count
    with ex ->
      return Error ex.Message
  }
