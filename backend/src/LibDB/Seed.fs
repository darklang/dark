/// Package seed: extract and grow.
///
/// A seed is a copy of data.db with the projection tables emptied and its ops marked unapplied (it carries
/// the full schema, so it works directly as a data.db). Export copies data.db, strips derived data, VACUUMs;
/// grow folds the unapplied ops back into the projections + evaluates values — runs on CLI startup, a single
/// SELECT COUNT when nothing's pending.
///
/// The op log (`package_ops`) is canonical; the package tables are regenerable projections folded from it.
/// `applyUnappliedOps` folds pending ops (the `applied` flag is the append/fold seam); `rebuildProjections`
/// drops the projections, marks every op unapplied, and re-folds. So a schema change is safe (drop + re-fold,
/// never touching the log) and a synced peer's ops fold in like a local edit.
module LibDB.Seed

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution
module Blob = LibExecution.Blob
module BS = LibSerialization.Binary.Serialization


// ---------------------
// Export
// ---------------------

/// Export a seed database to the given output path.
/// Copies the full source DB, then strips derived data and archived branches.
let export (outputPath : string) : Task<unit> =
  task {
    let sourcePath = LibConfig.Config.dbPath

    if System.IO.File.Exists outputPath then System.IO.File.Delete outputPath

    // Checkpoint WAL before copying to ensure all data is in the main file
    let sourceConnStr = $"Data Source={sourcePath};Mode=ReadOnly;Cache=Private"
    use sourceConn = new SqliteConnection(sourceConnStr)
    sourceConn.Open()
    use checkpointCmd = sourceConn.CreateCommand()
    checkpointCmd.CommandText <- "PRAGMA wal_checkpoint(TRUNCATE);"
    checkpointCmd.ExecuteNonQuery() |> ignore<int>
    sourceConn.Close()

    System.IO.File.Copy(sourcePath, outputPath)

    let connStr = $"Data Source={outputPath};Mode=ReadWriteCreate;Cache=Private"

    use conn = new SqliteConnection(connStr)
    conn.Open()

    use pragmaCmd = conn.CreateCommand()
    pragmaCmd.CommandText <-
      "PRAGMA journal_mode=WAL; PRAGMA synchronous=NORMAL; PRAGMA busy_timeout=5000;"
    pragmaCmd.ExecuteNonQuery() |> ignore<int>

    use cleanCmd = conn.CreateCommand()
    cleanCmd.CommandText <-
      """
      DELETE FROM locations;
      DELETE FROM package_types;
      DELETE FROM package_values;
      DELETE FROM package_functions;
      DELETE FROM package_dependencies;
      DELETE FROM deprecations;

      DELETE FROM package_ops WHERE branch_id IN (
        SELECT id FROM branches WHERE archived_at IS NOT NULL);
      DELETE FROM commits WHERE branch_id IN (
        SELECT id FROM branches WHERE archived_at IS NOT NULL);
      DELETE FROM branches WHERE archived_at IS NOT NULL;

      -- Execution traces are dev telemetry, never part of a seed. Leaving them in bloats the shipped seed
      -- (trace_fn_calls alone was 268 MB of a 305 MB dev store); strip them so the seed is just canon.
      DELETE FROM trace_fn_calls;
      DELETE FROM traces;

      UPDATE package_ops SET applied = 0;
      UPDATE branch_ops SET applied = 1;
      """
    cleanCmd.ExecuteNonQuery() |> ignore<int>

    use vacuumCmd = conn.CreateCommand()
    vacuumCmd.CommandText <- "VACUUM;"
    vacuumCmd.ExecuteNonQuery() |> ignore<int>

    conn.Close()
  }


// ---------------------
// Grow
// ---------------------

/// Apply all unapplied package_ops in the database.
/// Returns the count of ops applied.
let applyUnappliedOps () : Task<int64> =
  task {
    // Fast check: are there any unapplied ops? Avoids loading blobs when count is 0.
    let! count =
      Sql.query "SELECT COUNT(*) as n FROM package_ops WHERE applied = 0"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    if count = 0L then
      return 0L
    else

      let! unappliedOps =
        Sql.query
          """
        SELECT id, op_blob, branch_id, commit_hash
        FROM package_ops
        WHERE applied = 0
        -- rowid breaks ties: created_at is second-resolution so a batch's ops share it. The fold's final
        -- state is order-independent, but a deterministic replay order keeps re-folds byte-identical.
        ORDER BY created_at ASC, rowid ASC
        """
        |> Sql.executeAsync (fun read ->
          let opId = read.uuid "id"
          let opBlob = read.bytes "op_blob"
          let branchId : PT.BranchId = read.uuid "branch_id"
          let commitHash = read.stringOrNone "commit_hash"
          let op = BS.PT.PackageOp.deserialize opId opBlob
          (opId, op, branchId, commitHash))

      if List.isEmpty unappliedOps then
        return 0L
      else
        let groups =
          unappliedOps
          |> List.groupBy (fun (_, _, branchId, commitHash) ->
            (branchId, commitHash))
          |> Map.toList

        // Apply every group + the applied=1 sweep in ONE transaction with synchronous=OFF: for 9000+ ops
        // this collapses ~20k WAL commits into one (apply phase ~5s → sub-second). Not crash-safe by design —
        // an aborted run leaves applied=0 and the next boot replays; replay isn't byte-idempotent (fresh Guid
        // ids) but the final projection is equivalent (crashed-run rows get superseded like a normal re-add).
        // FK enforcement is off for the load — replaying ops out of topological order trips FKs (e.g. a
        // location before the branch it references) — so we `PRAGMA foreign_key_check` after commit and fail
        // loudly on any real violation.
        use conn = new SqliteConnection(LibDB.Sqlite.connString)
        do! conn.OpenAsync()
        let runRaw (sql : string) : Task<unit> =
          task {
            use cmd = conn.CreateCommand()
            cmd.CommandText <- sql
            let! _ = cmd.ExecuteNonQueryAsync()
            return ()
          }
        // PRAGMAs that affect transaction semantics must run *outside* a
        // transaction. foreign_keys=OFF in particular only takes effect
        // when not in a tx.
        //
        // synchronous=NORMAL (not OFF): OFF lets the writer skip syncing
        // the change-counter update to disk, which poisons the page cache
        // of any concurrent reader on a different connection — that
        // reader then returns SQLITE_CORRUPT ("database disk image is
        // malformed") even though PRAGMA integrity_check is clean. NORMAL
        // keeps the WAL-mode coherence guarantee at the cost of one fsync
        // per checkpoint; the bulk-grow win still comes from collapsing
        // 9000+ commits into one transaction.
        do!
          runRaw
            "PRAGMA journal_mode=WAL; \
             PRAGMA synchronous=NORMAL; \
             PRAGMA busy_timeout=5000; \
             PRAGMA foreign_keys=OFF;"
        let opCount = List.length unappliedOps
        use _bulk = Telemetry.span "seed.applyOps.bulk" [ "ops", string opCount ]
        use tx = conn.BeginTransaction()

        for ((branchId, commitHash), ops) in groups do
          let opsOnly = ops |> List.map (fun (_, op, _, _) -> op)
          do! PackageOpPlayback.applyOpsOnConnection conn branchId commitHash opsOnly

        // Mark all loaded ops applied in a single statement (inside the same
        // outer transaction).
        do! runRaw "UPDATE package_ops SET applied = 1 WHERE applied = 0"

        tx.Commit()

        // Integrity check. `PRAGMA foreign_key_check` runs regardless of
        // the per-connection `foreign_keys` setting — it scans every FK in
        // every table and returns a row per violation (or nothing when the
        // DB is clean). Anything here is a real data bug: either the seed
        // was inconsistent or our op-replay produced dangling refs.
        // Surface it loudly rather than persisting a silently-broken
        // projection.
        //
        // We don't bother flipping `foreign_keys` back on first — the
        // pragma is per-connection-instance, and this connection is about
        // to be closed. The next connection picks up the connection-string
        // default (`Foreign Keys=True`) on its own.
        let violations = ResizeArray<string * string * string * string>()
        use checkCmd = conn.CreateCommand()
        checkCmd.CommandText <- "PRAGMA foreign_key_check"
        use! reader = checkCmd.ExecuteReaderAsync()
        // fantomas can't format `while! reader.ReadAsync() do ...` inside a
        // task CE, so we drive the loop with a mutable flag.
        let mutable keepReading = true
        while keepReading do
          let! hasNext = reader.ReadAsync()
          if hasNext then
            // columns: table, rowid, parent, fkid
            let row =
              reader.GetString(0),
              reader.GetValue(1).ToString(),
              reader.GetString(2),
              reader.GetValue(3).ToString()
            violations.Add(row)
          else
            keepReading <- false
        if violations.Count > 0 then
          let summary =
            violations
            |> Seq.truncate 5
            |> Seq.map (fun (t, r, p, f) -> $"  {t} rowid={r} → {p} (fk_id={f})")
            |> String.concat "\n"
          Exception.raiseInternal
            $"foreign_key_check reported {violations.Count} \
              violation(s) after grow:\n{summary}"
            [ "first_violations", summary ]

        return int64 opCount
  }


/// The committed events after `cursor` (≤ `limit`), the commits they reference, and the new cursor — what a
/// peer serves.
///
/// The cursor is `committed_seq`, NOT `rowid`. `rowid` is an op's AUTHORING order, but an op only becomes
/// syncable when it's COMMITTED, which can happen much later than it was authored (e.g. a WIP op on branch X
/// while branch Y is committed + synced). `committed_seq` is assigned when the op is committed (or received),
/// so it reflects COMMIT order: a late commit of an early-authored op gets a high seq and is served after the
/// cursor has passed the earlier commits — never skipped. Native so a large batch is milliseconds. Event =
/// (id, opBlobHex, branchId, commitHash, originTs); commit = (hash, message, branchId, accountId, createdAt).
let eventsSince
  (cursor : int64)
  (limit : int64)
  : Task<List<string * string * string * string * string> *
    List<string * string * string * string * string> *
    int64>
  =
  task {
    let! opRows =
      Sql.query
        $"SELECT committed_seq AS cseq, id, hex(op_blob) AS blob, branch_id, commit_hash, origin_ts
          FROM package_ops
          WHERE committed_seq > {cursor} AND commit_hash IS NOT NULL
          ORDER BY committed_seq
          LIMIT {limit}"
      |> Sql.executeAsync (fun read ->
        (read.int64 "cseq",
         read.string "id",
         read.string "blob",
         read.string "branch_id",
         read.string "commit_hash",
         read.string "origin_ts"))

    let events =
      opRows |> List.map (fun (_, id, blob, br, ch, ts) -> (id, blob, br, ch, ts))

    let newCursor =
      match opRows with
      | [] -> cursor
      | rows -> rows |> List.map (fun (cseq, _, _, _, _, _) -> cseq) |> List.max

    // Only the commits the batch's ops reference (committed_seq in (cursor, newCursor]) — a bounded event
    // batch carries a bounded set of commits, never the whole commit history.
    let! commits =
      Sql.query
        $"SELECT DISTINCT c.hash AS hash, c.message AS message, c.branch_id AS branch_id,
            c.account_id AS account_id, c.created_at AS created_at
          FROM commits c
          JOIN package_ops o ON o.commit_hash = c.hash
          WHERE o.committed_seq > {cursor} AND o.committed_seq <= {newCursor}"
      |> Sql.executeAsync (fun read ->
        (read.string "hash",
         read.string "message",
         read.string "branch_id",
         read.string "account_id",
         read.string "created_at"))

    return (commits, events, newCursor)
  }

/// The branch ops after `cursor` as (id, opBlobHex, originTs) + the new cursor. Branch ops carry their
/// structure (branch/commit/merge/…) in the blob, so — unlike package events — they need no side metadata
/// beyond the authoring stamp. Ordered by rowid so a receiver applies them in order (CreateBranch first).
let branchOpsSince
  (cursor : int64)
  (limit : int64)
  : Task<List<string * string * string> * int64> =
  task {
    let! rows =
      Sql.query
        $"SELECT rowid AS rid, id, hex(op_blob) AS blob, origin_ts
          FROM branch_ops
          WHERE rowid > {cursor}
          ORDER BY rowid
          LIMIT {limit}"
      |> Sql.executeAsync (fun read ->
        (read.int64 "rid",
         read.string "id",
         read.string "blob",
         read.string "origin_ts"))

    // each event carries origin_ts so the receiver's structural LWW (rebase) converges by creation time
    let events = rows |> List.map (fun (_, id, blob, ts) -> (id, blob, ts))

    let newCursor =
      match rows with
      | [] -> cursor
      | _ -> rows |> List.map (fun (rid, _, _, _) -> rid) |> List.max

    return (events, newCursor)
  }

/// Apply branch ops RECEIVED from a peer: deserialize each blob → BranchOp → insertAndApply (idempotent,
/// content-addressed by hash). Applied in order, so CreateBranch lands before the commits/merges that depend
/// on it. Returns the count processed (branch ops are low-volume; the puller advances a per-peer cursor, so a
/// re-pull doesn't re-count in practice).
let receiveBranchOps (events : List<string * byte[] * string>) : Task<int64> =
  task {
    let mutable applied = 0L

    for (id, opBlob, originTs) in events do
      // Count only NEWLY-applied ops (idempotent on the content-addressed id), so the puller's "Pulled N"
      // is honest on a re-pull / shared-base pull — matching the package-op count.
      let existed =
        Sql.query "SELECT 1 FROM branch_ops WHERE id = @id"
        |> Sql.parameters [ "id", Sql.string id ]
        |> Sql.executeExistsSync
      let op = BS.PT.BranchOp.deserialize id opBlob
      // PRESERVE the peer's origin_ts (not a fresh stamp) so the structural LWW converges the same everywhere
      do! BranchOpPlayback.insertAndApplyWithTs op originTs
      if not existed then applied <- applied + 1L

    return applied
  }

/// Append peer-received events into the local op log, then fold them into the projections. Unlike the
/// local-authoring path, this PRESERVES each op's original `origin_ts` — the timestamp-LWW needs it to
/// converge the same on every instance regardless of arrival order. Idempotent (`INSERT OR IGNORE` on the
/// content-addressed id; only unapplied ops fold). Returns the count NEWLY applied, so a puller reports the
/// real change count.
let receiveOps
  (commits : List<string * string * System.Guid * System.Guid * string>)
  (events : List<System.Guid * byte[] * System.Guid * string * string>)
  : Task<int64> =
  task {
    if List.isEmpty events then
      return 0L
    else
      // Do not let synced data claim the `Darklang` owner used for trusted
      // bundled functions.
      let incomingOps =
        events
        |> List.map (fun (opId, opBlob, _, _, _) ->
          BS.PT.PackageOp.deserialize opId opBlob)
      match Inserts.reservedOwnerViolation incomingOps with
      | Some reason ->
        raise (System.InvalidOperationException $"rejecting synced ops: {reason}")
      | None -> ()

      // Insert the referenced commits FIRST (same transaction, in order) so the ops' commit_hash FK is
      // satisfied — a synced op belongs to a commit that must exist on the receiver. INSERT OR IGNORE dedups.
      // TODO(sync-accounts): a synced commit carries an account_id but accounts don't sync. Today the 5
      // well-known accounts are seeded identically on every instance so it always resolves; FKs are off so a
      // missing one wouldn't throw (it'd insert a dangling account_id). When accounts become dynamic, sync
      // them (or create-on-receive) rather than assuming the author exists locally.
      let commitInserts =
        commits
        |> List.map (fun (hash, message, branchId, accountId, createdAt) ->
          let sql =
            """
          INSERT OR IGNORE INTO commits (hash, message, branch_id, account_id, created_at)
          VALUES (@hash, @message, @branch_id, @account_id, @created_at)
          """
          let ps =
            [ "hash", Sql.string hash
              "message", Sql.string message
              "branch_id", Sql.uuid branchId
              "account_id", Sql.uuid accountId
              "created_at", Sql.string createdAt ]
          (sql, [ ps ]))

      // A received op arrives already committed, so it gets a `committed_seq` on insert (a monotonic
      // COMMIT-order stamp local to this store) — that's what this instance serves onward by. Kept on
      // CONFLICT so a re-pulled op keeps its original seq. Single-writer, so MAX+i is race-free.
      let! seqBase =
        Sql.query "SELECT COALESCE(MAX(committed_seq), 0) AS m FROM package_ops"
        |> Sql.executeRowAsync (fun read -> read.int64 "m")

      let opInserts =
        events
        |> List.mapi (fun i (opId, opBlob, branchId, commitHash, originTs) ->
          // Convergence fix (canonical origin_ts): the op id is content-only, so two instances that
          // independently author the SAME op stamp it with different local `origin_ts`. If we kept
          // first-writer's stamp (INSERT OR IGNORE), a later competing edit could resolve differently on each
          // instance → permanent divergence. Instead reconcile to the MIN stamp (deterministic on every
          // instance), and if that LOWERS an already-applied op's stamp, mark it unapplied so the fold re-runs
          // and the binding's `locations.origin_ts` is refreshed to the reconciled value.
          let sql =
            """
            INSERT INTO package_ops
              (id, op_blob, branch_id, applied, commit_hash, propagation_id, origin_ts, committed_seq)
            VALUES (@id, @op_blob, @branch_id, @applied, @commit_hash, @propagation_id, @origin_ts, @committed_seq)
            ON CONFLICT(id, branch_id) DO UPDATE SET
              origin_ts = MIN(package_ops.origin_ts, excluded.origin_ts),
              applied =
                CASE WHEN excluded.origin_ts < package_ops.origin_ts THEN 0
                    ELSE package_ops.applied END
            """
          let ps =
            [ "id", Sql.uuid opId
              "op_blob", Sql.bytes opBlob
              "branch_id", Sql.uuid branchId
              "applied", Sql.bool false
              "commit_hash", Sql.string commitHash
              "propagation_id", Sql.dbnull
              "origin_ts", Sql.string originTs
              "committed_seq", Sql.int64 (seqBase + int64 (i + 1)) ]
          (sql, [ ps ]))

      let _ = (commitInserts @ opInserts) |> Sql.executeTransactionSync
      // TODO(receive-atomicity): the insert above, detectDivergences below, and applyUnappliedOps run in
      // three separate transactions. A mid-fold throw can leave conflicts recorded for ops that never
      // folded — transiently inconsistent, though it self-heals on the next grow. The fix is to thread a
      // single transaction/connection through detect + fold.
      // Record any divergences BEFORE the fold: an incoming SetName that rebinds a name already bound
      // locally to a different hash is a sync conflict (auto-resolved by LWW). Recorded so it's reviewable.
      do! Conflicts.detectDivergences events
      // Honest change count = ops that actually FOLD — newly inserted, plus any the MIN-reconcile above
      // lowered (marked unapplied) so they re-fold. A pure re-pull folds nothing → 0. (Insert rows-affected
      // can't be used now that the op insert is an upsert: a DO UPDATE counts even a no-op re-pull.)
      let! foldedCount = applyUnappliedOps ()
      // The fold just re-derived bindings by LWW, which CLOBBERS the resolution overlay (a human keep-mine /
      // keep-theirs decision). Restore it, so a manual resolution DOMINATES the auto (LWW) pick — locally and
      // on any peer that synced the resolution in. This is the pull-path counterpart of the reapply that
      // rebuildProjections / growIfNeeded do after their folds; without it a synced-in "keep theirs" is lost
      // the moment the op it overrides folds (esp. when the overlay was applied BEFORE that op, so its own
      // apply was an idempotent no-op), and the two peers DIVERGE. Effective binding = fold[LWW] -> overlay.
      if foldedCount > 0L then do! Resolutions.reapplyAll ()
      return foldedCount
  }


/// The regenerable projections — every table the op-fold writes. `deprecations` is one: it's folded
/// from `Deprecate`/`Undeprecate` ops (its `annotation_blob` reconstructs from the op), so it's
/// regenerable and `export` strips it like the others. NOT `package_blobs` (canonical content —
/// op-playback never writes it), nor the op log / branch / commit / account state.
let projectionTables : List<string> =
  [ "package_functions"
    "package_types"
    "package_values"
    "locations"
    "package_dependencies"
    "deprecations" ]

/// Drop every projection table and re-fold the whole `package_ops` log to rebuild them.
/// Projections are regenerable from the ops — losing one costs only the CPU to
/// re-fold; the op log is the canonical durable state and is never touched here. This is the schema-change /
/// durable-canon path (drop projections, re-fold). Returns the count of ops re-applied.
let rebuildProjections () : Task<int64> =
  task {
    // 1. clear the regenerable projection tables (single source of truth = projectionTables).
    for t in projectionTables do
      do! Sql.query $"DELETE FROM {t}" |> Sql.executeStatementAsync
    // 2. mark all ops unapplied so the fold reprocesses the whole log
    do! Sql.query "UPDATE package_ops SET applied = 0" |> Sql.executeStatementAsync
    // 3. re-fold ops -> projections via the existing playback path
    let! folded = applyUnappliedOps ()
    // 4. re-apply the resolutions overlay — the fold only replays package_ops, so without this a human
    //    override would be lost on rebuild ("effective binding = fold → then apply resolutions").
    do! Resolutions.reapplyAll ()
    return folded
  }


/// Remove seed-time access so a stored package value cannot retain the
/// permissions of the process that created it.
let private stripCapturedAccess = LibExecution.Dval.stripCapturedAccess

/// Evaluate all package values that have NULL rt_dval.
/// Multi-pass: values may depend on other values, so we retry until convergence.
let evaluateAllValues
  (branchId : PT.BranchId)
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  : Task<Result<unit, string list>> =
  task {
    let program : RT.Program = { dbs = Map.empty }

    let notify _ _ _ _ = uply { return () }
    let sendException _ _ _ _ = uply { return () }

    let exeState =
      Execution.createState
        builtins
        pm
        Execution.noTracing
        sendException
        notify
        branchId
        program
      // Seeding is trusted host-side work; guest execution remains deny-all
      // by default.
      |> Execution.setInstancePolicy LibExecution.Permissions.Policy.allowAll

    let maxPasses = 10
    let mutable pass = 0
    let mutable keepGoing = true
    let mutable lastErrors : string list = []

    while keepGoing do
      pass <- pass + 1

      let! unevaluatedValues =
        Sql.query
          """
          SELECT pv.hash, pv.pt_def, l.owner, l.modules, l.name
          FROM package_values pv
          LEFT JOIN locations l ON l.item_hash = pv.hash AND l.unlisted_at IS NULL
          WHERE pv.rt_dval IS NULL
          """
        |> Sql.executeAsync (fun read ->
          let hash = Hash(read.string "hash")
          let ptDef = read.bytes "pt_def"
          let owner = read.stringOrNone "owner" |> Option.defaultValue "?"
          let modules = read.stringOrNone "modules" |> Option.defaultValue ""
          let name = read.stringOrNone "name" |> Option.defaultValue "?"
          let fullName =
            if modules = "" then $"{owner}.{name}" else $"{owner}.{modules}.{name}"
          (hash, ptDef, fullName))

      if List.isEmpty unevaluatedValues then
        keepGoing <- false
        lastErrors <- []
      else if pass > maxPasses then
        keepGoing <- false
        lastErrors <-
          [ $"Gave up after {maxPasses} passes with {List.length unevaluatedValues} values remaining" ]
      else
        let errors = ResizeArray<string>()
        let mutable successCount = 0

        for (valueHash, ptDefBytes, fullName) in unevaluatedValues do
          try
            let ptValue = BS.PT.PackageValue.deserialize valueHash ptDefBytes
            let instrs = PT2RT.Expr.toRT Map.empty 0 None ptValue.body
            let! result = Execution.executeExpr exeState instrs

            match result with
            | Error(rte, _callStack) ->
              let! errorResult = Execution.runtimeErrorToString exeState rte
              let errorMsg =
                match errorResult with
                | Ok(RT.DString s) -> s
                | Ok other -> $"{other}"
                | Error(rte2, _) -> $"(could not stringify error: {rte2})"
              errors.Add(
                $"Value {valueHash} ({fullName}): evaluation failed - {errorMsg}"
              )
            | Ok dval ->
              // Promote any ephemeral blobs inside the value to
              // persistent so we can serialize. Streams remain
              // non-persistable and trip the [isPersistable] guard
              // below with a clear error.
              let! dval = LibExecution.Blob.promote pm.persistBlob dval
              let dval = stripCapturedAccess dval

              if not (LibExecution.Dval.isPersistable dval) then
                let reason =
                  LibExecution.Dval.nonPersistableReason dval
                  |> Option.defaultValue "value is not persistable"
                errors.Add(
                  $"Value {valueHash} ({fullName}): cannot store in val — {reason}"
                )
              else
                let rtHash = PT2RT.Hash.toRT valueHash
                let rtValue : RT.PackageValue.PackageValue =
                  { hash = rtHash; body = dval }
                let (Hash defHash) = valueHash
                let rtDvalBytes = BS.RT.PackageValue.serialize rtHash rtValue
                let valueType = RT.Dval.toValueType dval
                let valueTypeBytes = BS.RT.ValueType.serialize valueType

                do!
                  Sql.query
                    """
                    UPDATE package_values
                    SET rt_dval = @rt_dval, value_type = @value_type
                    WHERE hash = @hash
                    """
                  |> Sql.parameters
                    [ "hash", Sql.string defHash
                      "rt_dval", Sql.bytes rtDvalBytes
                      "value_type", Sql.bytes valueTypeBytes ]
                  |> Sql.executeStatementAsync

                successCount <- successCount + 1
          with ex ->
            errors.Add($"Value {valueHash} ({fullName}): exception - {ex.Message}")

        if successCount = 0 then
          keepGoing <- false
          lastErrors <- errors |> List.ofSeq

    if List.isEmpty lastErrors then return Ok() else return Error lastErrors
  }


/// The grow step for CLI/test startup: apply unapplied ops, generate package ref hashes, evaluate values.
/// On a warm DB it's a single fast SELECT COUNT. `getBuiltins` is a function, not a value, because builtins
/// must be constructed AFTER the hashes exist (construction triggers PackageRefs hash lookups).
let growIfNeeded
  (getBuiltins : unit -> RT.Builtins)
  (pm : RT.PackageManager)
  (log : string -> unit)
  : Task<bool> =
  task {
    use _span = Telemetry.span "seed.growIfNeeded" []
    let! appliedCount =
      Telemetry.timeTask "seed.applyOps" [] (fun () -> applyUnappliedOps ())
    // A store can have every op applied yet still hold unevaluated values (rt_dval NULL) — e.g. after a
    // migration that re-marks ops applied without evaluating, or a store copied/built without a final grow
    // (the test seed does exactly this). Gating evaluation on `appliedCount > 0` alone leaves those values
    // NULL forever, so the value is unusable ("value not found" — or, before the null-safe read in
    // RuntimeTypes.Value.get, an internal NULL crash). Evaluate whenever any value is unevaluated so the
    // store self-heals on startup. Refs only need regenerating when we actually applied new ops.
    let! hasUnevaluatedValues =
      Sql.query
        "SELECT EXISTS(SELECT 1 FROM package_values WHERE rt_dval IS NULL) AS has_null"
      |> Sql.executeRowAsync (fun read -> read.int64 "has_null")
      |> Task.map (fun n -> n > 0L)
    if appliedCount > 0L then
      log $"Growing package DB from ops ({appliedCount} ops to apply)..."
      Telemetry.event "seed.applyOps.count" [ ("count", string appliedCount) ]
      // The incremental fold just re-derived bindings by LWW from the newly-applied ops — which CLOBBERS any
      // resolution overlay (a human / keep-local "keep theirs" decision). Re-apply the overlay, exactly like
      // rebuildProjections does after a full re-fold: without this, a synced-in resolution is silently reverted
      // to the LWW loser on the next grow, so two peers that agreed via a resolution DIVERGE. (Effective
      // binding = fold(package_ops)[LWW] -> then apply resolutions.)
      do!
        Telemetry.timeTask "seed.reapplyResolutions" [] (fun () ->
          Resolutions.reapplyAll ())
      do!
        Telemetry.timeTask "seed.generateRefs" [] (fun () ->
          task {
            do! PackageRefsGenerator.generate ()
            LibExecution.PackageRefs.reloadHashes ()
          })
    if appliedCount > 0L || hasUnevaluatedValues then
      let! _evalResult =
        Telemetry.timeTask "seed.evaluateValues" [] (fun () ->
          evaluateAllValues PT.mainBranchId (getBuiltins ()) pm)
      do!
        Telemetry.timeTask "seed.walCheckpoint" [] (fun () ->
          Sql.query "PRAGMA wal_checkpoint(TRUNCATE);" |> Sql.executeStatementAsync)
      // Announce only when we grew from real op work; a pure self-heal (evaluating stray unevaluated values
      // with no new ops) is silent maintenance — it must not print to stdout, or it pollutes captured CLI
      // output (e.g. a caller comparing exact command output).
      if appliedCount > 0L then log "Package DB ready"
      return true
    else
      return false
  }
