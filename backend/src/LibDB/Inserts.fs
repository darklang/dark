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


/// The content-addressed id for a PackageOp. See `Hashing.computeOpRowId`, which every path that mints or
/// looks up an op id shares, including this one.
let computeOpHash (op : PT.PackageOp) : System.Guid = Hashing.computeOpRowId op


/// The authoring stamp. Defined once in `LibDB.OriginTs` -- see there for why it's monotonic and why it
/// must not be duplicated.
let nextOriginTs () : string = OriginTs.next ()


/// Insert PackageOps and fold them into the projections, resolving each op's origin_ts via `tsFor` and its
/// committing commit via `commitFor`. Same contract as `insertAndApplyOps`; the two resolvers let callers
/// PRESERVE existing values (WipRefresh's discard+reinsert) instead of resetting them for every op.
let insertAndApplyOpsWith
  (tsFor : System.Guid -> string)
  (commitFor : System.Guid -> string option)
  (source : string)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  task {
    if List.isEmpty ops then
      return 0L
    else
      // Phase 1: Insert ops with applied=false
      // Each op gets a strictly-increasing authoring stamp (see `nextOriginTs`), assigned in list order so
      // sequential edits within one wall-clock millisecond are still ordered by creation for the LWW.
      let opsWithIds =
        ops
        |> List.map (fun op ->
          let opId = computeOpHash op
          let opBlob = BS.PT.PackageOp.serialize opId op
          (opId, op, opBlob, tsFor opId, commitFor opId))

      let insertStatements =
        opsWithIds
        |> List.map (fun (opId, _op, opBlob, originTs, commitHash) ->
          let sql =
            """
            INSERT OR IGNORE INTO package_ops
              (id, op_blob, applied, origin_ts, commit_hash)
            VALUES (@id, @op_blob, @applied, @origin_ts, @commit_hash)
            """

          let parameters =
            [ "id", Sql.uuid opId
              "op_blob", Sql.bytes opBlob
              "applied", Sql.bool false // Insert as unapplied
              "origin_ts", Sql.string originTs
              "commit_hash",
              (match commitHash with
               | Some h -> Sql.string h
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

      let opsToApply = insertedOpsWithIds |> List.map (fun (_, op, _, _, _) -> op)
      let insertedOpIds =
        insertedOpsWithIds |> List.map (fun (opId, _, _, _, _) -> opId)

      do! PackageOpPlayback.applyOpsFrom source opsToApply

      // Mark ops as applied (non-critical - ops are already applied)
      if not (List.isEmpty insertedOpIds) then
        try
          let updateStatements =
            insertedOpIds
            |> List.map (fun opId ->
              let sql =
                "UPDATE package_ops SET applied = @applied \
                 WHERE id = @id"
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


/// Insert PackageOps and fold them into the projections. Returns the count actually inserted (duplicates
/// skipped via INSERT OR IGNORE). Insert with applied=false, fold, then mark applied=true — so a mid-fold
/// failure leaves the ops identifiable + retryable. (Commit-free: no commit_hash -- every op is live.)
let insertAndApplyOps (ops : List<PT.PackageOp>) : Task<int64> =
  insertAndApplyOpsWith (fun _ -> nextOriginTs ()) (fun _ -> None) "op" ops


/// Insert ops that PROPAGATION authored, marking their bindings as such.
///
/// The bindings are otherwise indistinguishable from ones you typed, which is why `dark commit` can't say
/// which entries you edited and which followed. `locations.source` already carried 'op' vs 'resolution';
/// 'propagation' is the third answer, recorded at the point of repoint because there is nowhere else to
/// derive it from -- a repoint changes only an item's resolved references, and rendering both versions
/// doesn't distinguish them either (an older version's references resolve differently once superseded, so
/// the pretty printer's output is a function of the current store rather than of the item).
let insertAndApplyPropagatedOps (ops : List<PT.PackageOp>) : Task<int64> =
  insertAndApplyOpsWith (fun _ -> nextOriginTs ()) (fun _ -> None) "propagation" ops


/// Insert ops as main WIP (commit-free: no commit_hash; the op is live once folded). Returns count inserted.
let insertAndApplyOpsAsWip (ops : List<PT.PackageOp>) : Task<int64> =
  insertAndApplyOps ops


/// Reinsert WIP ops PRESERVING each op's original origin_ts and committing commit where known (by op id),
/// minting a fresh stamp only for ops with no prior entry (the genuinely-new, re-stabilized versions).
/// This is WipRefresh's path: re-stamping every op in a whole-store reinsert would inflate the clock ~10s
/// into the future and poison the fold's timestamp-LWW for the next update. See getWipOpOriginTs.
let insertAndApplyOpsPreservingTs
  (preserveTs : Map<System.Guid, string>)
  (preserveCommit : Map<System.Guid, string>)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  insertAndApplyOpsWith
    (fun opId ->
      match Map.tryFind opId preserveTs with
      | Some ts -> ts
      | None -> nextOriginTs ())
    // An op whose content didn't change keeps its id, so it keeps the commit it was committed into. A
    // genuinely-new (re-stabilized) op has no prior entry and lands uncommitted -- correctly, since it's a new
    // version you haven't reviewed.
    (fun opId -> Map.tryFind opId preserveCommit)
    // A whole-main rewrite re-folds bindings that were originally authored, propagated or resolved alike,
    // and the op alone doesn't say which. 'op' is the honest default rather than a claim; the alternative
    // is carrying provenance per op through the rewrite, which is section 2's work.
    "op"
    ops


/// Bulk-import synced ops (id, op_blob-as-hex, origin_ts) in ONE transaction, committed into <param commitHash>
/// ("" = leave uncommitted). Arriving ops are somebody else's finished work, not YOUR draft, so an import commits
/// them on the way in -- otherwise the first `dark status` after a pull would report the peer's whole history
/// as things you changed.
/// The perf path
/// for transport: Dark's per-op insert is far too slow for a real log (10k ops = 0.26s here
/// as one transaction), so the decode-hex + bulk INSERT lives in F#. origin_ts is preserved
/// (the LWW stamp). INSERT OR IGNORE dedups by content id. Inserts unapplied; the caller
/// folds. Returns how many were newly inserted.
/// (Ops land effective=1 = they just take effect. The pending/approval gate is a later
/// effort; for now sync moves ops and they apply, like the old sync.)
let importOpsBulk
  (commitHash : string)
  (records : List<string * string * string>)
  : Task<int64> =
  task {
    if List.isEmpty records then
      return 0L
    else
      // Advance our clock past everything in this batch BEFORE anything is stored, so the next thing
      // authored here sorts after what we just learned about. See `OriginTs.observe`: without this a peer
      // whose clock is ahead wins every contested name forever, because our later edits never catch up.
      records |> List.iter (fun (_, _, originTs) -> OriginTs.observe originTs)

      // Build the param rows resiliently: a malformed record (non-uuid id, non-hex blob) must
      // SKIP, not throw -- otherwise one bad record on the wire rejects the whole batch. The
      // good records still import; the bad ones are logged + dropped. (Its blob is validated
      // again at fold time, which also skips bad ops, so nothing malformed reaches a projection.)
      let paramRows =
        records
        |> List.choose (fun (id, blobHex, originTs) ->
          try
            Some
              [ "id", Sql.uuid (System.Guid.Parse id)
                "op_blob", Sql.bytes (System.Convert.FromHexString blobHex)
                "origin_ts", Sql.string originTs
                "commit_hash",
                (if commitHash = "" then Sql.dbnull else Sql.string commitHash) ]
          with ex ->
            System.Console.Error.WriteLine(
              $"importOpsBulk: skipping malformed record id={id}: {ex.Message}"
            )
            None)

      if List.isEmpty paramRows then
        return 0L
      else
        let sql =
          """
          INSERT OR IGNORE INTO package_ops
            (id, op_blob, applied, effective, origin_ts, commit_hash)
          VALUES (@id, @op_blob, 0, 1, @origin_ts, @commit_hash)
          """

        let affected = Sql.executeTransactionSync [ (sql, paramRows) ]
        return affected |> List.sumBy int64
  }


/// RELAY store path: bulk-insert the pushed ops AND record ownership (op_id, owner) in ONE
/// transaction. Unlike importOpsBulk this does NOT fold -- a relay serves op blobs, not
/// projections, so folding everyone's ops would be wasteful + meaningless. The op_owners rows
/// let the relay serve "your stuff" back by identity. Malformed records skipped. Returns count
/// of newly-stored ops. (owner="" stores ops without recording ownership.)
let storeOpsWithOwner
  (owner : string)
  (records : List<string * string * string>)
  : Task<int64> =
  task {
    if List.isEmpty records then
      return 0L
    else
      let valid =
        records
        |> List.choose (fun (id, blobHex, originTs) ->
          try
            Some(
              System.Guid.Parse id,
              System.Convert.FromHexString blobHex,
              originTs
            )
          with ex ->
            System.Console.Error.WriteLine(
              $"storeOpsWithOwner: skipping malformed record id={id}: {ex.Message}"
            )
            None)

      if List.isEmpty valid then
        return 0L
      else
        let opRows =
          valid
          |> List.map (fun (id, blob, ts) ->
            [ "id", Sql.uuid id
              "op_blob", Sql.bytes blob
              "origin_ts", Sql.string ts ])

        // `effective = 0`: in the log, NEVER folded into this store's own main.
        //
        // Storing them queued-for-folding is not enough to keep them out: `growIfNeeded` folds everything
        // `applied = 0 AND effective = 1` on the next startup, so a relay would fold its clients' ops one
        // restart later. A client pushes its whole log, package tree included, and names are bound by
        // last-writer-wins over the whole store -- `Darklang.Matter.router` among them. Anyone who can
        // write to a relay could otherwise change what that relay itself runs.
        //
        // Hosted ops are DATA. The relay serves the blobs back verbatim (neither export query filters on
        // `effective`) and its own code stays whatever its binary seeded.
        let insertOps =
          "INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts)
           VALUES (@id, @op_blob, 0, 0, @origin_ts)"

        let statements =
          if owner = "" then
            [ (insertOps, opRows) ]
          else
            let ownerRows =
              valid
              |> List.map (fun (id, _, _) ->
                [ "op_id", Sql.uuid id; "owner", Sql.string owner ])

            let insertOwners =
              "INSERT OR IGNORE INTO op_owners (op_id, owner) VALUES (@op_id, @owner)"

            [ (insertOps, opRows); (insertOwners, ownerRows) ]

        // one transaction; the ops-insert counts come first (statement order), so truncate to
        // the op rows to report NEW ops (not owner rows).
        let affected = Sql.executeTransactionSync statements
        return affected |> List.truncate (List.length opRows) |> List.sumBy int64
  }


/// Commit every currently-uncommitted MAIN op into one commit. The package RELOAD path uses this: the `.dark`
/// files on disk are the shipped baseline, not your uncommitted draft, so leaving them uncommitted would open
/// every `dark status` on "5,000 items changed".
///
/// The hash is derived from what it commits (message + count + newest stamp), so two instances that reload the
/// same packages compute the same id rather than inventing divergent ones.
///
/// DEV CAVEAT: a reload sweeps a genuine un-committed local draft into the baseline commit too. That's tolerable
/// only because reload is a dev-loop tool; retiring it is the point of package bootstrapping (option C).
let commitAllAsBaseline (message : string) : Task<string> =
  task {
    let! summary =
      Sql.query
        """
        SELECT COUNT(*) AS n, COALESCE(MAX(origin_ts), '') AS latest
        FROM package_ops
        WHERE commit_hash IS NULL AND id NOT IN (SELECT op_id FROM op_branches)
        """
      |> Sql.executeRowAsync (fun read -> (read.int64 "n", read.string "latest"))

    let (count, latest) = summary

    if count = 0L then
      return ""
    else
      let material = $"{message}|{count}|{latest}"

      let hash =
        material
        |> System.Text.Encoding.UTF8.GetBytes
        |> System.Security.Cryptography.SHA256.HashData
        |> System.Convert.ToHexString
        |> fun h -> h.Substring(0, 16).ToLowerInvariant()

      do!
        Sql.query
          "INSERT OR REPLACE INTO commits (hash, message, author, origin_ts)
           VALUES (@hash, @message, 'system', @origin_ts)"
        |> Sql.parameters
          [ "hash", Sql.string hash
            "message", Sql.string message
            "origin_ts", Sql.string (nextOriginTs ()) ]
        |> Sql.executeStatementAsync

      do!
        Sql.query
          "UPDATE package_ops SET commit_hash = @hash
           WHERE commit_hash IS NULL AND id NOT IN (SELECT op_id FROM op_branches)"
        |> Sql.parameters [ "hash", Sql.string hash ]
        |> Sql.executeStatementAsync

      // A commit NAMES the ops it committed; it doesn't snapshot anything. The authoring refresh rewrites main
      // by delete-and-reinsert, and an op whose content changed comes back with a new id and no commit -- so
      // an older baseline can end up naming nothing at all. Those rows are tombstones, and `dark commits`
      // otherwise fills up with commits over an empty set.
      do!
        Sql.query
          "DELETE FROM commits WHERE hash NOT IN
             (SELECT DISTINCT commit_hash FROM package_ops WHERE commit_hash IS NOT NULL)"
        |> Sql.executeStatementAsync

      return hash
  }


/// Delete every main op and its projections. Not a user-facing operation on its own: the caller is expected
/// to re-insert whatever should survive, which is how both the authoring refresh and `discardDraftOps`
/// rewrite main. Returns the count deleted.
let discardWipOps () : Task<Result<int64, string>> =
  task {
    try
      // Get count before deleting
      let! wipOps =
        Sql.query
          """
          SELECT id FROM package_ops
          WHERE id NOT IN (SELECT op_id FROM op_branches)
          """
        |> Sql.executeAsync (fun read -> read.uuid "id")

      let count = int64 (List.length wipOps)

      if count = 0L then
        return Ok 0L
      else
        // Deletes the authored rows: locations that aren't the 'resolution' overlay, plus deprecations
        // and the ops themselves. The op log is the source of truth, so re-inserting re-folds. All in
        // one txn. Branch (op_branches-tagged) ops are EXCLUDED -- they're branch-pending, not main
        // (isolation).
        let noParams = [ [] ]
        let discardStatements =
          [ ("DELETE FROM locations WHERE source <> 'resolution'", noParams)
            ("DELETE FROM deprecations", noParams)
            ("DELETE FROM package_ops WHERE id NOT IN (SELECT op_id FROM op_branches)",
             noParams) ]

        let _ = Sql.executeTransactionSync discardStatements
        ()

        // Note: We don't delete from package_types/values/functions because
        // they're content-addressed and might be referenced by committed ops.
        // They'll be cleaned up by garbage collection if truly orphaned.

        return Ok count
    with ex ->
      return Error ex.Message
  }
