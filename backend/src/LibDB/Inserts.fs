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


/// A process-monotonic authoring stamp (`origin_ts`): millisecond wall clock, but returns `max(nowMs,
/// last+1ms)` so it never repeats within a batch. Otherwise same-ms ops would tie and the LWW in
/// `applySetName` would break it by content hash — silently reordering local sequential edits (rename v1
/// then v2 could leave v1 winning). Strictly-increasing stamps mean the later edit always wins. Format
/// matches the schema default `strftime('%Y-%m-%dT%H:%M:%fZ')` so it stays lexically comparable across peers.
let private originTsLock = System.Object()
let mutable private lastOriginTs = System.DateTime.MinValue

let nextOriginTs () : string =
  lock originTsLock (fun () ->
    let nowMs =
      let n = System.DateTime.UtcNow
      System.DateTime(
        n.Ticks - (n.Ticks % System.TimeSpan.TicksPerMillisecond),
        System.DateTimeKind.Utc
      )
    let next =
      if nowMs > lastOriginTs then nowMs else lastOriginTs.AddMilliseconds 1.0
    lastOriginTs <- next
    next.ToString(
      "yyyy-MM-ddTHH:mm:ss.fffZ",
      System.Globalization.CultureInfo.InvariantCulture
    ))


/// Insert PackageOps and fold them into the projections. `commitHash = None` = WIP (commit_hash NULL), `Some`
/// = committed. Returns the count actually inserted (duplicates skipped via INSERT OR IGNORE). Insert with
/// applied=false, fold, then mark applied=true — so a mid-fold failure leaves the ops identifiable + retryable.
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

      // Each op gets a strictly-increasing authoring stamp (see `nextOriginTs`), assigned in list order so
      // sequential edits within one wall-clock millisecond are still ordered by creation for the LWW.
      let opsWithIds =
        ops
        |> List.map (fun op ->
          let opId = computeOpHash op
          let opBlob = BS.PT.PackageOp.serialize opId op
          (opId, op, opBlob, batchPropagationId, nextOriginTs ()))

      let insertStatements =
        opsWithIds
        |> List.map (fun (opId, _op, opBlob, propagationId, originTs) ->
          let sql =
            """
            INSERT OR IGNORE INTO package_ops (id, op_blob, branch_id, applied, commit_hash, propagation_id, origin_ts)
            VALUES (@id, @op_blob, @branch_id, @applied, @commit_hash, @propagation_id, @origin_ts)
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
               | None -> Sql.dbnull)
              "origin_ts", Sql.string originTs ]

          (sql, [ parameters ]))

      let rowsAffected = insertStatements |> Sql.executeTransactionSync

      // Count how many ops were actually inserted (vs skipped as duplicates)
      let insertedCount = rowsAffected |> List.sumBy int64

      // Identify which ops were actually inserted
      let insertedOpsWithIds =
        List.zip opsWithIds rowsAffected
        |> List.filter (fun (_, affected) -> affected > 0)
        |> List.map fst

      let opsToApply = insertedOpsWithIds |> List.map (fun (_, op, _, _, _) -> op)
      let insertedOpIds =
        insertedOpsWithIds |> List.map (fun (opId, _, _, _, _) -> opId)

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
        -- rowid tiebreak: on a same-second created_at tie, pick the chain TIP deterministically. A child
        -- commit is always inserted after its parent (commits are sequential locally, branch ops apply in
        -- order on receive), so the tip has the highest rowid on every peer — the same logical commit
        -- everywhere, even though absolute rowids differ. Without it, two peers could base off different
        -- commits and diverge.
        ORDER BY created_at DESC, rowid DESC
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


/// Commit exactly the WIP ops with the given ids (the caller owns selection policy): validate each is still
/// WIP, create the commit, stamp the ops + their projection rows. A full-WIP set uses the commit-all path.
/// CLEANUP: if SCM becomes multi-writer, do validation + commit creation + stamping in one transaction.
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

          // Stamp each committed op with a monotonic COMMIT-order `committed_seq`, so sync (`eventsSince`)
          // pages by commit order, not authoring order (rowid). Single-writer SCM, so MAX+i is race-free.
          let! seqBase =
            Sql.query "SELECT COALESCE(MAX(committed_seq), 0) AS m FROM package_ops"
            |> Sql.executeRowAsync (fun read -> read.int64 "m")
          let seqStmts =
            selectedOps
            |> List.mapi (fun i (opId, _) ->
              ("UPDATE package_ops SET committed_seq = @seq WHERE id = @id AND branch_id = @branch_id",
               [ [ "seq", Sql.int64 (seqBase + int64 (i + 1))
                   "id", Sql.uuid opId
                   "branch_id", Sql.uuid branchId ] ]))

          let statements = [ branchOpStmt; commitStmt ] @ projStmts @ seqStmts

          let _ = Sql.executeTransactionSync statements

          return Ok commitHash
    with ex ->
      return Error ex.Message
  }


/// Find the committed item at a location, checking the current branch first, then falling back to ancestor
/// branches. Keyed by NAME, not (name, kind): one name holds one item, so what's committed there is whatever
/// it is — and the caller needs the kind it FOUND (the name may since have been rebound to another kind),
/// hence returning it rather than taking it as a filter.
/// Returns Ok((hash, kind), locationIdOpt) where locationIdOpt is Some for same-branch committed locations
/// (that need un-deprecating) or None for ancestor locations (which are already active on the parent).
let findCommittedHash
  (branchId : PT.BranchId)
  (owner : string)
  (modules : string)
  (name : string)
  : Task<Result<(Hash * string) * Option<uuid>, string>> =
  task {
    // First: look for deprecated committed location on current branch
    let! committedLocations =
      Sql.query
        """
        SELECT location_id, item_hash, item_type
        FROM locations
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND branch_id = @branch_id
          AND commit_hash IS NOT NULL
          AND unlisted_at IS NOT NULL
        -- rowid tiebreak: unlisted_at is second-resolution; without it a tie restores an arbitrary row,
        -- differing across a re-fold. Highest rowid = the truly-latest committed version.
        ORDER BY unlisted_at DESC, rowid DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "owner", Sql.string owner
          "modules", Sql.string modules
          "name", Sql.string name
          "branch_id", Sql.uuid branchId ]
      |> Sql.executeAsync (fun read ->
        (read.uuid "location_id",
         Hash(read.string "item_hash"),
         read.string "item_type"))

    match committedLocations with
    | (locationId, itemHash, itemType) :: _ ->
      return Ok((itemHash, itemType), Some locationId)
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
            SELECT item_hash, item_type
            FROM locations
            WHERE owner = @owner
              AND modules = @modules
              AND name = @name
              AND branch_id IN ({branchInClause})
              AND unlisted_at IS NULL
            LIMIT 1
            """
          |> Sql.parameters (
            [ "owner", Sql.string owner
              "modules", Sql.string modules
              "name", Sql.string name ]
            @ branchParams
          )
          |> Sql.executeAsync (fun read ->
            (Hash(read.string "item_hash"), read.string "item_type"))

        match ancestorLocations with
        | (itemHash, itemType) :: _ -> return Ok((itemHash, itemType), None)
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
                 AND committed_loc.branch_id = wip_loc.branch_id
                 AND committed_loc.commit_hash IS NOT NULL
                 AND committed_loc.unlisted_at IS NOT NULL
               WHERE wip_loc.branch_id = @branch_id
                 AND wip_loc.commit_hash IS NULL
                 AND wip_loc.source <> 'resolution'
                 AND NOT EXISTS (
                   SELECT 1 FROM locations active
                   WHERE active.owner = wip_loc.owner
                     AND active.modules = wip_loc.modules
                     AND active.name = wip_loc.name
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
                     AND c2.branch_id = wip_loc.branch_id
                     AND c2.commit_hash IS NOT NULL
                     AND c2.unlisted_at IS NOT NULL
                 )
             )
             """,
             branchParam)

            ("DELETE FROM locations WHERE branch_id = @branch_id AND commit_hash IS NULL \
              AND source <> 'resolution'",
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
