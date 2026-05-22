module LibDB.Inserts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

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
          | PT.PackageOp.PropagateUpdate(pid, _, _, _, _) -> Some pid
          | PT.PackageOp.RevertPropagation(rid, _, _, _, _) -> Some rid
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
              let sql =
                "UPDATE package_ops SET applied = @applied \
                 WHERE id = @id AND branch_id = @branch_id"
              let parameters =
                [ "applied", Sql.bool true
                  "id", Sql.uuid opId
                  "branch_id", Sql.uuid branchId ]
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
  (accountId : AccountID)
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
    let commitHash = Hashing.computeCommitHash accountId branchId parentHash opHashes
    let (Hash commitHashStr) = commitHash

    // Record and apply the commit
    do!
      BranchOpPlayback.insertAndApply (
        PT.BranchOp.CreateCommit(commitHash, message, accountId, branchId, opHashes)
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


// A commit stamps package_ops plus the projection rows it publishes.
// TODO: subset commits match projection rows by content key, so two WIP ops
// that publish the same projection row cannot be committed or discarded
// independently. The clearest case is re-deprecating an item with a changed
// message/kind: same projection key (item_hash, kind, "deprecated"), different
// op id, so committing one stamps the other's row too. (Also: should we even
// allow deprecating an already-deprecated item?)

/// Every requested id must currently be WIP on the branch. If any id is already
/// committed, discarded, or from another branch, reject the whole commit.
let private validateRequestedIds
  (opIdSet : Set<uuid>)
  (allWip : List<uuid * PT.PackageOp>)
  : Result<unit, string> =
  let wipIdSet = allWip |> List.map fst |> Set.ofList
  let missing = opIdSet |> Set.filter (fun id -> not (Set.contains id wipIdSet))

  if Set.isEmpty missing then
    Ok()
  else
    Error(
      $"{Set.count missing} of {Set.count opIdSet} requested op id(s) are not WIP "
      + "on this branch (they may already be committed, discarded, or belong to "
      + "another branch); nothing was committed."
    )

/// SQL to flip WIP rows for a commit. Full commits use branch-wide updates;
/// subset commits only stamp projection rows derived from the selected ops.
let private projectionStatements
  (commitHashStr : string)
  (branchId : PT.BranchId)
  (allSelected : bool)
  (selectedOps : List<uuid * PT.PackageOp>)
  =
  if allSelected then
    [ ("""
       UPDATE package_ops
       SET commit_hash = @commit_hash
       WHERE branch_id = @branch_id AND commit_hash IS NULL
       """,
       [ [ "commit_hash", Sql.string commitHashStr; "branch_id", Sql.uuid branchId ] ])

      ("""
       UPDATE locations
       SET commit_hash = @commit_hash
       WHERE branch_id = @branch_id AND commit_hash IS NULL
       """,
       [ [ "commit_hash", Sql.string commitHashStr; "branch_id", Sql.uuid branchId ] ])

      ("""
       UPDATE deprecations
       SET commit_hash = @commit_hash
       WHERE branch_id = @branch_id AND commit_hash IS NULL
       """,
       [ [ "commit_hash", Sql.string commitHashStr; "branch_id", Sql.uuid branchId ] ]) ]
  else
    let selectedOpIds = selectedOps |> List.map fst

    // SetName stamps its location row, keyed by FQN and item_hash so a second
    // SetName on the same FQN doesn't drag the prior (unlisted but still WIP)
    // row in.
    let selectedLocations : List<PT.PackageLocation * PT.ItemKind * Hash> =
      selectedOps
      |> List.choose (fun (_, op) ->
        match op with
        | PT.PackageOp.SetName(loc, target) -> Some(loc, target.kind, target.hash)
        | _ -> None)
      |> List.distinct

    // Deprecate/Undeprecate stamps its deprecation row, keyed by (item_hash, kind,
    // state) so committing a Deprecate doesn't drag in a still-WIP Undeprecate
    // of the same item. The "deprecated"/"undeprecated" strings mirror the
    // `state` column written by applyDeprecate / applyUndeprecate. (Two ops
    // projecting the same state for one (hash, kind) are still
    // indistinguishable; see the TODO above.)
    let selectedDeps : List<Hash * PT.ItemKind * string> =
      selectedOps
      |> List.choose (fun (_, op) ->
        match op with
        | PT.PackageOp.Deprecate(target, _, _) ->
          Some(target.hash, target.kind, "deprecated")
        | PT.PackageOp.Undeprecate target ->
          Some(target.hash, target.kind, "undeprecated")
        | _ -> None)
      |> List.distinct

    let packageOpStmts =
      selectedOpIds
      |> List.map (fun opId ->
        ("""
         UPDATE package_ops
         SET commit_hash = @commit_hash
         WHERE id = @id AND branch_id = @branch_id AND commit_hash IS NULL
         """,
         [ [ "commit_hash", Sql.string commitHashStr
             "id", Sql.uuid opId
             "branch_id", Sql.uuid branchId ] ]))

    let locationStmts =
      selectedLocations
      |> List.map (fun (loc, kind, hash) ->
        let modulesStr = String.concat "." loc.modules
        let itemTypeStr = kind.toString ()
        let (Hash hashStr) = hash
        ("""
         UPDATE locations
         SET commit_hash = @commit_hash
         WHERE branch_id = @branch_id
           AND commit_hash IS NULL
           AND owner = @owner
           AND modules = @modules
           AND name = @name
           AND item_type = @item_type
           AND item_hash = @item_hash
         """,
         [ [ "commit_hash", Sql.string commitHashStr
             "branch_id", Sql.uuid branchId
             "owner", Sql.string loc.owner
             "modules", Sql.string modulesStr
             "name", Sql.string loc.name
             "item_type", Sql.string itemTypeStr
             "item_hash", Sql.string hashStr ] ]))

    let deprecationStmts =
      selectedDeps
      |> List.map (fun (hash, kind, state) ->
        let (Hash hashStr) = hash
        let itemKindStr = kind.toString ()
        ("""
         UPDATE deprecations
         SET commit_hash = @commit_hash
         WHERE branch_id = @branch_id
           AND commit_hash IS NULL
           AND item_hash = @item_hash
           AND item_kind = @item_kind
           AND state = @state
         """,
         [ [ "commit_hash", Sql.string commitHashStr
             "branch_id", Sql.uuid branchId
             "item_hash", Sql.string hashStr
             "item_kind", Sql.string itemKindStr
             "state", Sql.string state ] ]))

    packageOpStmts @ locationStmts @ deprecationStmts


/// Commit all WIP ops on a branch by creating a new commit and assigning commit_hash.
/// Commit hash is content-addressed: hash(parentHash + sorted opHashes).
/// Returns the commit Hash on success.
let rec commitWipOps
  (accountId : AccountID)
  (branchId : PT.BranchId)
  (message : string)
  : Task<Result<Hash, string>> =
  // Commit-all is just "commit every WIP op id": gather the ids and defer to
  // commitWipOpsByIds, which takes a branch-wide bulk fast-path when handed
  // the full set (see below). Keeps one commit-construction code path.
  task {
    let! ids =
      Sql.query
        """
        SELECT id
        FROM package_ops
        WHERE branch_id = @branch_id AND commit_hash IS NULL
        ORDER BY created_at ASC
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
      |> Sql.executeAsync (fun read -> read.uuid "id")

    if List.isEmpty ids then
      return Error "Nothing to commit"
    else
      return! commitWipOpsByIds accountId branchId message ids
  }


/// Commit exactly the WIP ops with the given IDs.
///
/// The caller owns selection policy. This function validates that every
/// requested id is still WIP, creates the commit, and stamps the selected ops
/// plus their derived projection rows. If the requested ids cover the whole WIP
/// set, it uses the commit-all projection path.
///
/// CLEANUP: if SCM becomes a shared multi-writer service, move validation,
/// parent lookup, commit creation, and row stamping into one transaction
/// on one connection.
and commitWipOpsByIds
  (accountId : AccountID)
  (branchId : PT.BranchId)
  (message : string)
  (opIds : List<uuid>)
  : Task<Result<Hash, string>> =
  task {
    try
      if List.isEmpty opIds then
        return Error "No ops selected"
      else
        let opIdSet = Set.ofList opIds

        let! allWip =
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

        match validateRequestedIds opIdSet allWip with
        | Error e -> return Error e
        | Ok() ->
          let selectedOps =
            allWip |> List.filter (fun (id, _) -> Set.contains id opIdSet)

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

          let opHashes =
            selectedOps |> List.map (fun (_, op) -> Hashing.computeOpHash op)

          let commitHash =
            Hashing.computeCommitHash accountId branchId parentHash opHashes
          let (Hash commitHashStr) = commitHash

          let branchOp =
            PT.BranchOp.CreateCommit(
              commitHash,
              message,
              accountId,
              branchId,
              opHashes
            )
          let branchOpHash = Hashing.computeBranchOpHash branchOp
          let (Hash branchOpHashStr) = branchOpHash
          let branchOpBlob = BS.PT.BranchOp.serialize branchOpHashStr branchOp

          let branchOpStmt =
            ("""
             INSERT OR IGNORE INTO branch_ops (id, op_blob, applied, created_at)
             VALUES (@id, @op_blob, 1, datetime('now'))
             """,
             [ [ "id", Sql.string branchOpHashStr
                 "op_blob", Sql.bytes branchOpBlob ] ])

          let commitStmt =
            ("""
             INSERT OR IGNORE INTO commits
                 (hash, message, branch_id, account_id, created_at)
             VALUES
                 (@hash, @message, @branch_id, @account_id, datetime('now'))
             """,
             [ [ "hash", Sql.string commitHashStr
                 "message", Sql.string message
                 "branch_id", Sql.uuid branchId
                 "account_id", Sql.uuid accountId ] ])

          // selectedOps is allWip filtered by the requested id set, so equal
          // lengths means every WIP op was selected.
          let allSelected = List.length selectedOps = List.length allWip

          let projStmts =
            projectionStatements commitHashStr branchId allSelected selectedOps

          let statements = [ branchOpStmt; commitStmt ] @ projStmts

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
          AND unlisted_at IS NOT NULL
        ORDER BY unlisted_at DESC
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
              AND unlisted_at IS NULL
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
        // All four writes run in one transaction: un-deprecate the most-
        // recent committed location at any path the WIP layer was hiding,
        // then delete WIP locations/deprecations/ops. A mid-discard crash
        // used to leave WIP rows partially deleted with the un-deprecation
        // already applied (or the inverse); since "discard" can run again
        // on retry, the consequence was orphan WIP rows or active rows
        // that should still have been hidden. WIP-deprecations: their
        // supersession set `unlisted_at` on prior rows; we don't restore
        // those here. The op log is source of truth, so a re-run via
        // `commit` + reload would rebuild state.
        let branchParam = [ [ "branch_id", Sql.uuid branchId ] ]
        let discardStatements =
          [ ("""
             UPDATE locations
             SET unlisted_at = NULL
             WHERE location_id IN (
               SELECT committed_loc.location_id
               FROM locations wip_loc
               INNER JOIN locations committed_loc
                 ON committed_loc.owner = wip_loc.owner
                 AND committed_loc.modules = wip_loc.modules
                 AND committed_loc.name = wip_loc.name
                 AND committed_loc.item_type = wip_loc.item_type
                 AND committed_loc.branch_id = wip_loc.branch_id
                 AND committed_loc.commit_hash IS NOT NULL
                 AND committed_loc.unlisted_at IS NOT NULL
               WHERE wip_loc.branch_id = @branch_id
                 AND wip_loc.commit_hash IS NULL
                 AND NOT EXISTS (
                   SELECT 1 FROM locations active
                   WHERE active.owner = wip_loc.owner
                     AND active.modules = wip_loc.modules
                     AND active.name = wip_loc.name
                     AND active.item_type = wip_loc.item_type
                     AND active.branch_id = wip_loc.branch_id
                     AND active.commit_hash IS NOT NULL
                     AND active.unlisted_at IS NULL
                 )
                 AND committed_loc.unlisted_at = (
                   SELECT MAX(c2.unlisted_at)
                   FROM locations c2
                   WHERE c2.owner = wip_loc.owner
                     AND c2.modules = wip_loc.modules
                     AND c2.name = wip_loc.name
                     AND c2.item_type = wip_loc.item_type
                     AND c2.branch_id = wip_loc.branch_id
                     AND c2.commit_hash IS NOT NULL
                     AND c2.unlisted_at IS NOT NULL
                 )
             )
             """,
             branchParam)

            ("DELETE FROM locations WHERE branch_id = @branch_id AND commit_hash IS NULL",
             branchParam)

            ("DELETE FROM deprecations WHERE branch_id = @branch_id AND commit_hash IS NULL",
             branchParam)

            ("DELETE FROM package_ops WHERE branch_id = @branch_id AND commit_hash IS NULL",
             branchParam) ]

        let _ = Sql.executeTransactionSync discardStatements
        ()

        // Note: We don't delete from package_types/values/functions because
        // they're content-addressed and might be referenced by committed ops.
        // They'll be cleaned up by garbage collection if truly orphaned.

        return Ok count
    with ex ->
      return Error ex.Message
  }
