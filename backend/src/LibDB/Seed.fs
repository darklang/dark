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

      -- The builder's BRANCHES are not canon. A branch's ops live in `package_ops` at effective = 0 and are
      -- tagged in `op_branches`, so both halves have to go and the ops have to go FIRST -- dropping the tags
      -- alone would leave untagged effective = 0 ops that no fold ever applies and nothing accounts for.
      -- Otherwise every install grown from this seed opens with somebody else's half-finished work in
      -- `dark branches`.
      DELETE FROM package_ops WHERE id IN (SELECT op_id FROM op_branches);
      DELETE FROM op_branches;
      DELETE FROM branch_name_bases;
      -- All of them: main is not a row here, so there is nothing to preserve.
      DELETE FROM branches;

      -- Ownership is a RELAY's index of which instance pushed which op, and it is per-instance by
      -- definition: a fresh install has never been pushed to. It is also the biggest thing here that nobody
      -- notices, because every reader joins through to `package_ops` and a stale row simply fails to join --
      -- 351,299 rows of a build machine's history were shipping in the seed unremarked.
      DELETE FROM op_owners;
      DELETE FROM relay_branches;

      -- A conflict is a finding about two peers' versions of one name. Same reasoning as `sync_bases`
      -- below: inheriting the builder's would be inheriting an argument between machines you have never met.
      DELETE FROM conflicts;

      -- `type_checked` is deliberately KEPT, unlike every other projection here. It is a set of hashes whose
      -- type surface came back clean, the ops that produced them ship in this same file, and the checker
      -- that filled it is the one in the binary this seed is built for. So it is exactly valid on arrival,
      -- and keeping it saves a fresh install the one-off walk of the whole package tree.

      -- Execution traces are dev telemetry, never part of a seed. Leaving them in bloats the shipped seed
      -- (trace_fn_calls alone was 268 MB of a 305 MB dev store); strip them so the seed is just canon.
      DELETE FROM trace_fn_calls;
      DELETE FROM traces;

      -- ALL of it. `config_v0` is per-install by construction -- the builtin that writes it says "Local +
      -- unsynced" -- so there is nothing in here a stranger should inherit, and an allow-list of what to
      -- keep would be empty.
      --
      -- It was a deny-list of three keys, and a deny-list was never going to hold: the sync keys are named
      -- after the peer (`sync.cursor.<url>`, `sync.head.<url>`, `sync.relay-instance.<url>`), so the set of
      -- keys is not knowable in advance. A seed built here shipped `sync.cursor.http://<my tailnet ip>:9090`
      -- along with `current_branch`, which is how a fresh install ends up announcing that its current branch
      -- is gone before the user has done anything.
      --
      -- What each of those would have cost, since the reasons differ: an INSTANCE ID makes every install
      -- grown from the seed claim to be the machine that built it, and two peers sharing an id cannot sync,
      -- cannot record a conflict against each other, and cannot be told apart in any provenance (found by
      -- hand-running the two-box runbook, where the second machine reported the first machine's id and
      -- every assertion after that was quietly watching one instance talk to itself). A CURSOR makes it
      -- believe it has already pulled ops it has never seen, so it skips them. A CURRENT BRANCH points at a
      -- branch that does not exist. And all of them leak the builder's addresses.
      --
      -- Nothing needs a value here to boot: `entry_point` unset falls back to the shipped CLI, which is
      -- also the recovery path for a bad pointer.
      DELETE FROM config_v0;

      -- A sync base is a RELATIONSHIP with a specific peer. A fresh install has none, and inheriting the
      -- builder's would make it believe it had already agreed with machines it has never met.
      DELETE FROM sync_bases;

      UPDATE package_ops SET applied = 0;
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
/// One pass: fold everything currently unapplied-and-effective. `applyUnappliedOps` repeats this,
/// because folding an op can make OTHER ops effective.
let private applyUnappliedOpsPass () : Task<int64> =
  task {
    // Fast check: are there any unapplied ops? Avoids loading blobs when count is 0.
    let! count =
      Sql.query
        "SELECT COUNT(*) as n FROM package_ops WHERE applied = 0 AND effective = 1"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    if count = 0L then
      return 0L
    else

      // Read the raw (id, blob) rows WITHOUT deserializing in the reader: a malformed op_blob
      // (corrupt / truncated on the wire, or a poisoned push) must not throw here and brick the
      // whole fold -- AND every fold after it, since the op stays applied=0 and gets re-read.
      let! rawOps =
        Sql.query
          """
        SELECT id, op_blob
        FROM package_ops
        WHERE applied = 0 AND effective = 1
        -- effective = 1: only APPROVED ops fold into the live projections (step 4 sync/playback split).
        -- A synced-but-unapproved op stays applied = 0, effective = 0 -- present in the log, never folded,
        -- until approval flips it effective. Default effective = 1 keeps single-user playback unchanged.
        -- rowid breaks ties: created_at is second-resolution so a batch's ops share it. The fold's final
        -- state is order-independent, but a deterministic replay order keeps re-folds byte-identical.
        ORDER BY created_at ASC, rowid ASC
        """
        |> Sql.executeAsync (fun read -> (read.uuid "id", read.bytes "op_blob"))

      // Deserialize per-op; SKIP + log any that fail rather than aborting, so one bad op cannot brick a
      // store. They stay `applied = 0`, which is the truth -- nothing folded them -- and means a build
      // that CAN read them still will. Marking them applied would be faster (they are re-read every fold)
      // and would make an op unreadable by an older binary permanently invisible to a newer one.
      let mutable skipped : List<System.Guid> = []

      let unappliedOps =
        rawOps
        |> List.choose (fun (opId, opBlob) ->
          try
            Some(opId, BS.PT.PackageOp.deserialize opId opBlob)
          with ex ->
            skipped <- opId :: skipped
            System.Console.Error.WriteLine(
              $"applyUnappliedOps: skipping unparseable op {opId}: {ex.Message}"
            )
            None)

      // Excluded from the sweep below by id. Empty in the normal case, so the sweep keeps its plain form.
      let excludeSkipped =
        match skipped with
        | [] -> ""
        | ids ->
          let quoted = ids |> List.map (fun g -> $"'{g}'") |> String.concat ", "
          $" AND id NOT IN ({quoted})"

      if List.isEmpty unappliedOps then
        // Every pending op was unparseable. Nothing folded, so nothing is marked applied and the caller's
        // loop stops on the zero. They are re-read next fold, which is the point: a newer build reads them.
        return 0L
      else
        // Apply all ops + the applied=1 sweep in ONE transaction: for 9000+ ops that collapses ~20k WAL
        // commits into one. Not crash-safe by design -- an aborted run leaves applied=0 and the next boot
        // replays; replay isn't byte-idempotent (fresh Guid ids) but the final projection is equivalent.
        // FK enforcement is off for the load, because replaying ops out of topological order trips FKs, so
        // `PRAGMA foreign_key_check` runs after commit and fails loudly on a real violation.
        use conn = new SqliteConnection(LibDB.Sqlite.connString)
        do! conn.OpenAsync()
        let runRaw (sql : string) : Task<unit> =
          task {
            use cmd = conn.CreateCommand()
            cmd.CommandText <- sql
            let! _ = cmd.ExecuteNonQueryAsync()
            return ()
          }
        // PRAGMAs affecting transaction semantics must run OUTSIDE a transaction; foreign_keys=OFF in
        // particular only takes effect when not in one.
        //
        // synchronous=NORMAL, not OFF: OFF lets the writer skip syncing the change-counter update, which
        // poisons the page cache of a concurrent reader on another connection -- that reader then returns
        // SQLITE_CORRUPT even though `PRAGMA integrity_check` is clean. The bulk-grow win comes from
        // collapsing 9000+ commits into one transaction, not from OFF.
        do!
          runRaw
            "PRAGMA journal_mode=WAL; \
             PRAGMA synchronous=NORMAL; \
             PRAGMA busy_timeout=5000; \
             PRAGMA foreign_keys=OFF;"
        let opCount = List.length unappliedOps
        use _bulk = Telemetry.span "seed.applyOps.bulk" [ "ops", string opCount ]
        use tx = conn.BeginTransaction()

        let opsOnly = unappliedOps |> List.map (fun (_, op) -> op)
        // Mark applied BEFORE folding, in the same transaction so a throw rolls both back and the ops
        // stay retryable.
        //
        // The predicate is not stable across the fold. It must match the SELECT's
        // (applied=0 AND effective=1): a bare `WHERE applied = 0` also marks other branches' pending ops
        // applied, so a later merge never folds their SetName. And FOLDING AN OP CAN MAKE OTHERS
        // EFFECTIVE -- a merge event from another machine flips that branch's frontier mid-fold -- so
        // running afterwards marks ops applied that nothing folded. Running first can only mark the set
        // just read, leaving newly-effective ops for the next pass.
        do!
          runRaw (
            "UPDATE package_ops SET applied = 1 WHERE applied = 0 AND effective = 1"
            + excludeSkipped
          )

        do! PackageOpPlayback.applyOpsOnConnection conn opsOnly

        tx.Commit()

        // `PRAGMA foreign_key_check` runs regardless of the per-connection `foreign_keys` setting, and
        // returns a row per violation. Anything here is a real data bug -- an inconsistent seed, or a
        // replay that produced dangling refs -- so surface it rather than persist a broken projection.
        //
        // No need to flip `foreign_keys` back on: the pragma is per-connection and this one is about to
        // close.
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


/// Fold every op that is unapplied and effective, until there are none left.
///
/// Repeats because folding an op can MAKE other ops effective: a merge event arriving from another machine
/// flips that branch's frontier, and those ops are not in the pass that folded the event. One pass would
/// leave them sitting there until the next command happened to run a fold, which is a store that is
/// correct eventually and wrong in the meantime.
///
/// Terminates because every pass marks what it read as applied (quarantined ops included), so the set
/// strictly shrinks. The bound is a backstop against a future op kind that makes work faster than this
/// drains it, not an expected case; it is deliberately loud rather than silent if it is ever hit.
let applyUnappliedOps () : Task<int64> =
  task {
    let mutable total = 0L
    let mutable pass = 0
    let mutable keepGoing = true

    while keepGoing do
      let! n = applyUnappliedOpsPass ()
      total <- total + n
      pass <- pass + 1

      if n > 0L && pass >= 10 then
        Exception.raiseInternal
          "applyUnappliedOps did not settle: an op kind is making ops effective faster than the fold            applies them"
          [ "passes", pass; "applied", total ]

      keepGoing <- n > 0L

    return total
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
    "deprecations"
    // Folded from `Decide` ops; nothing else writes it. Being here is what makes it genuinely derived
    // rather than a second source of truth about the same decisions.
    "propagation_policy" ]

// NOT here, and `OpsProjections.Tests` is what catches you adding it: `type_checked`. This list is the
// set of projections the FOLD rebuilds, and it is asserted to match exactly what `Seed.export` strips. The
// type-check cache is neither -- nothing re-folds it, and the seed deliberately ships it warm. What governs
// whether one of its rows is still true is the `build_hash` on the row, not a re-fold, so it does not want
// this list's lifecycle.

/// Drop every projection table and re-fold the whole `package_ops` log to rebuild them.
///
/// The op log is canonical and untouched here; projections are a cache over it, so losing one costs only
/// the CPU to re-fold. That claim is what the storage model rests on, and `OpsProjections.Tests` asserts
/// the re-folded result is identical.
///
/// Triggered automatically by a SCHEMA CHANGE: `Migrations.fs` drops the same tables from this same list
/// and marks the log unapplied, and the next `growIfNeeded` re-folds and re-evaluates values.
///
/// No user-facing "rebuild" verb, deliberately. Re-folding cannot fix hashes that moved (those are IN the
/// ops) or a corrupt log (it IS the log), so a button mostly invites people to reach for it when something
/// else is wrong.
///
/// Returns the count of ops re-applied.
let rebuildProjections () : Task<int64> =
  task {
    // 1. clear the regenerable projection tables (single source of truth = projectionTables).
    for t in projectionTables do
      do! Sql.query $"DELETE FROM {t}" |> Sql.executeStatementAsync
    // 2. mark all ops unapplied so the fold reprocesses the whole log
    do! Sql.query "UPDATE package_ops SET applied = 0" |> Sql.executeStatementAsync
    // 3. re-fold ops -> projections via the existing playback path
    let! folded = applyUnappliedOps ()
    // 4. branch-scoped propagation policy, which step 3 can't reach (effective = 0 by design)
    do! Branches.refoldBranchDecides ()
    // Nothing extra to reapply for resolutions: `Resolve` is an op, so re-folding the log rebuilds the
    // `source = 'resolution'` rows in `locations` along with everything else.
    return folded
  }


/// Evaluate all package values that have NULL rt_dval.
/// Multi-pass: values may depend on other values, so we retry until convergence.
let evaluateAllValues
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
        program

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
    // ABI type identities are PINNED: the committed package-ref-hashes.txt is authoritative, loaded by
    // PackageRefs on first access. We deliberately do NOT regenerate refs from the store on boot -- that
    // made the kernel's type identities float on whatever the local store happened to hash to, the brick
    // risk the kernel-hash pinning removes. The generator stays a DEV tool (reload-packages / LocalExec
    // fill), where regenerating produces a reviewable git diff = a deliberate re-pin. See SPEC section 10.
    if appliedCount > 0L || hasUnevaluatedValues then
      let! _evalResult =
        Telemetry.timeTask "seed.evaluateValues" [] (fun () ->
          evaluateAllValues (getBuiltins ()) pm)
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
