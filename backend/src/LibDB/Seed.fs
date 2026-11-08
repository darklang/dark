/// Package seed: extract and grow.
///
/// A seed is a copy of data.db with the projection tables emptied and its ops marked unapplied (it carries
/// the full schema, so it works directly as a data.db). Export copies data.db, strips derived data, VACUUMs;
/// grow folds the unapplied ops back into the projections and evaluates values. Grow runs on CLI startup,
/// and is a single SELECT COUNT when nothing is pending.
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
module BS = LibSerialization.Binary.Serialization


/// Whether this process has already said that the store holds ops it cannot read.
///
/// Said once per process, not once per fold: a single command folds more than once, and the note is
/// context for whatever the command was asked to do, not an event worth repeating before every answer.
let mutable private warnedAboutUnreadableOps = false


// ---------------------
// Export
// ---------------------

/// Fold the write-ahead log into the main database file, so that a plain file copy of it
/// carries everything.
///
/// ReadWrite, not ReadOnly: a checkpoint WRITES (it moves pages into the main file and then
/// truncates the log), so a read-only connection fails it with SQLite error 10, `disk I/O
/// error`. That reads as a failing disk and it is not; it is this connection.
///
/// It hid for a long time because a checkpoint with an EMPTY log has nothing to do and
/// succeeds either way. So the export only breaks once the store has been written to, and
/// then it breaks the release gates, which need a fresh seed.
///
/// Separate from `export` so a test can drive it: `export` always copies `Config.dbPath`,
/// which in a test is the store the whole suite shares, and nothing can point it elsewhere.
let checkpointWal (dbPath : string) : unit =
  use conn =
    new SqliteConnection($"Data Source={dbPath};Mode=ReadWrite;Cache=Private")
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <- "PRAGMA wal_checkpoint(TRUNCATE);"
  cmd.ExecuteNonQuery() |> ignore<int>
  conn.Close()


/// Export a seed database to the given output path: copy the full source DB, then strip everything
/// that belongs to the machine that built it rather than to the package set (see the DELETEs below).
let export (outputPath : string) : Task<unit> =
  task {
    LibExecution.PackageRefs.validate ()

    let sourcePath = LibConfig.Config.dbPath

    if System.IO.File.Exists outputPath then System.IO.File.Delete outputPath

    checkpointWal sourcePath

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
      DELETE FROM package_traits;
      DELETE FROM package_trait_impls;
      DELETE FROM package_tests;
      DELETE FROM package_dependencies;
      DELETE FROM deprecations;
      DELETE FROM location_docs;

      -- The builder's BRANCHES are not canon. Otherwise every install grown from this seed opens with
      -- somebody else's half-finished work in `dark branches`.
      DELETE FROM package_ops WHERE branch_id <> '00000000-0000-0000-0000-000000000001';
      DELETE FROM commits WHERE branch_id <> '00000000-0000-0000-0000-000000000001';
      DELETE FROM branch_name_bases;
      -- All of them: main is not a row here, so there is nothing to preserve.
      DELETE FROM branches;

      -- A conflict is a finding about two versions of one name. Inheriting the builder's would be
      -- inheriting an argument you were not part of.
      DELETE FROM conflicts;

      -- Execution traces are dev telemetry, never part of a seed. They dominate a dev store by size
      -- (`trace_fn_calls` alone runs to hundreds of MB), so strip them and the seed is just canon.
      DELETE FROM trace_fn_calls;
      DELETE FROM trace_fns;
      DELETE FROM trace_loops;
      DELETE FROM trace_blobs;
      DELETE FROM trace_cli_invocations;
      DELETE FROM traces;

      -- ALL of it: `config_v0` is per-install by construction, and nothing in it is needed
      -- to boot (every key has a default).
      --
      -- A deny-list can't work: the keys are not all knowable in advance. Costs if shipped: a
      -- shared INSTANCE ID means two installs that cannot be told apart, and a CURRENT BRANCH
      -- dangles.
      DELETE FROM config_v0;

      -- The reload's trait-call cache is the builder's, keyed on the builder's build, and no
      -- install reloads packages from source.
      DELETE FROM trait_call_resolutions;

      -- The builder's DRAFT: a seed is committed history by definition
      -- (`check-seed-carries-refs` asserts it), and an uncommitted op is instance
      -- state like the rows above. Stripped HERE rather than left to the guard: the
      -- F# suite leaves fixtures in main's draft, so a release build would otherwise
      -- refuse or not depending on what ran last. The bindings need no separate
      -- delete; every projection went above.
      DELETE FROM package_ops WHERE commit_hash IS NULL;

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

/// The pending set: every op not yet folded, as raw (rowid, branch, id, blob) rows, in log order. The
/// fold's half is `foldRead`; `applyUnappliedOpsPass` is the two in sequence. Split so a test can put a
/// concurrent write between them, which is the case the by-row sweep in `foldRead` exists for.
let readPending () : Task<List<int64 * PT.BranchId * System.Guid * byte[]>> =
  task {
    // Fast check: are there any unapplied ops? Avoids loading blobs when count is 0.
    let! count =
      Sql.query "SELECT COUNT(*) as n FROM package_ops WHERE applied = 0"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    if count = 0L then
      return []
    else
      // Read the raw rows WITHOUT deserializing in the reader: a malformed op_blob must not throw here
      // and brick the whole fold -- AND every fold after it, since the op stays applied=0.
      return!
        Sql.query
          "SELECT rowid AS seq, branch_id, id, op_blob FROM package_ops
           WHERE applied = 0 ORDER BY rowid"
        |> Sql.executeAsync (fun read ->
          (read.int64 "seq",
           PT.BranchId.ParseUnsafe(read.string "branch_id"),
           read.uuid "id",
           read.bytes "op_blob"))
  }

/// Drop `package_dependencies`' secondary indexes when the table is empty, and return the SQL that
/// builds them again. A cold start writes every edge in the store, about 40,000 of them, and keeping
/// its secondary indexes up to date row by row costs more than building them once from the finished
/// table. The table's own key stays: the inserts rely on it to skip duplicates.
let private dropDependencyIndexesIfEmpty
  (conn : SqliteConnection)
  : Task<List<string>> =
  task {
    use cmd = conn.CreateCommand()
    cmd.CommandText <-
      "SELECT name, sql FROM sqlite_master
       WHERE type = 'index' AND tbl_name = 'package_dependencies'
         AND sql IS NOT NULL AND sql NOT LIKE 'CREATE UNIQUE%'
         AND NOT EXISTS (SELECT 1 FROM package_dependencies)"
    let! indexes =
      task {
        use! reader = cmd.ExecuteReaderAsync()
        return reader |> Sqlite.readRows (fun r -> (r.GetString 0, r.GetString 1))
      }
    for (name, _) in indexes do
      use drop = conn.CreateCommand()
      drop.CommandText <- $"DROP INDEX {name}"
      let! _ = drop.ExecuteNonQueryAsync()
      ()
    return List.map snd indexes
  }


/// How many ops `foldRead` deserializes and folds at a time.
let private foldChunkSize = 1000

/// Fold the rows `readPending` returned, each into its own branch, and mark exactly those applied.
/// Returns the count folded.
let foldRead
  (rawOps : List<int64 * PT.BranchId * System.Guid * byte[]>)
  : Task<int64> =
  task {
    if List.isEmpty rawOps then
      return 0L
    else

      // Deserialize per-op; SKIP + log any that fail rather than aborting, so one bad op cannot brick a
      // store. They stay `applied = 0`, which is the truth -- nothing folded them -- and means a build
      // that CAN read them still will.
      //
      // A chunk at a time: a cold start folds every op in the store, and all of them deserialized at
      // once is most of its peak memory.
      let mutable skipped = 0
      let mutable folded = 0L

      // Opened at the first op that deserializes, so a store holding only ops this build cannot
      // read never takes the write lock.
      let mutable conn : SqliteConnection = null
      let mutable tx : SqliteTransaction = null
      let mutable droppedIndexes : List<string> = []
      let runRaw (conn : SqliteConnection) (sql : string) : Task<unit> =
        task {
          use cmd = conn.CreateCommand()
          cmd.CommandText <- sql
          let! _ = cmd.ExecuteNonQueryAsync()
          return ()
        }
      use _bulk =
        Telemetry.span "seed.applyOps.bulk" [ "ops", string (List.length rawOps) ]

      try
        for chunk in List.chunkBySize foldChunkSize rawOps do
          // What the last chunk left behind, collected before this one deserializes. Concurrent GC
          // is off (`Cli.fsproj`), so collections are rare during a fold and each chunk's syntax
          // trees were promoted and kept to the end of it: most of a cold start's peak memory.
          if folded > 0L then
            System.GC.Collect(1, System.GCCollectionMode.Forced, true)

          let unappliedOps =
            chunk
            |> List.choose (fun (seq, branch, opId, opBlob) ->
              BS.PT.PackageOp.tryDeserialize opId opBlob
              |> Option.map (fun op -> (seq, branch, op)))
          skipped <- skipped + List.length chunk - List.length unappliedOps

          if not (List.isEmpty unappliedOps) then
            if isNull conn then
              // All ops + the applied=1 sweep in ONE transaction: for 9000+ ops that collapses ~20k
              // WAL commits into one.
              conn <- new SqliteConnection(LibDB.Sqlite.connString)
              do! conn.OpenAsync()
              // synchronous=NORMAL, not OFF: OFF lets the writer skip syncing the change-counter
              // update, which poisons the page cache of a concurrent reader on another connection.
              //
              // 16 MB of page cache rather than SQLite's 2 MB: most of the fold's writes land in
              // indexes keyed on random hashes, and with the default nearly every one of them misses
              // the cache.
              do!
                runRaw
                  conn
                  "PRAGMA journal_mode=WAL; PRAGMA synchronous=NORMAL; PRAGMA busy_timeout=5000;
                   PRAGMA cache_size=-16384;"
              tx <- conn.BeginTransaction()
              // Before the first chunk folds, so the check sees the table as the fold found it.
              let! dropped = dropDependencyIndexesIfEmpty conn
              droppedIndexes <- dropped

            // By ROW, never by predicate: a `serve` committing an op between the read and here
            // matches `applied = 0` too, and a predicate sweep would mark it applied with nothing
            // having folded it.
            for rows in
              unappliedOps
              |> List.map (fun (seq, _, _) -> seq)
              |> List.chunkBySize 500 do
              let listed = rows |> List.map string |> String.concat ", "
              do!
                runRaw
                  conn
                  $"UPDATE package_ops SET applied = 1 WHERE rowid IN ({listed})"

            // Each run of consecutive ops on one branch folds into that branch, in log order.
            let runs =
              unappliedOps
              |> List.fold
                (fun (acc : List<PT.BranchId * List<PT.PackageOp>>) (_, branch, op) ->
                  match acc with
                  | (b, ops) :: rest when b = branch -> (b, op :: ops) :: rest
                  | _ -> (branch, [ op ]) :: acc)
                []
              |> List.rev
              |> List.map (fun (b, ops) -> (b, List.rev ops))
            for (branch, ops) in runs do
              do! PackageOpPlayback.applyOpsOnConnection conn branch ops
            folded <- folded + int64 (List.length unappliedOps)

        if not (isNull tx) then
          for sql in droppedIndexes do
            do! runRaw conn sql
          tx.Commit()
      finally
        if not (isNull tx) then tx.Dispose()
        if not (isNull conn) then conn.Dispose()

      // ONE line, not one per op: these are re-examined on every startup.
      if skipped > 0 && not warnedAboutUnreadableOps then
        warnedAboutUnreadableOps <- true
        NonBlockingConsole.writeErrLine (
          $"note: {skipped} op(s) in this store were written in a format this build cannot "
          + "read, and are being skipped. They are kept, not dropped, so a later build can apply them."
        )

      return folded
  }


/// One pass: fold everything currently unapplied.
let private applyUnappliedOpsPass () : Task<int64> =
  task {
    let! pending = readPending ()
    return! foldRead pending
  }

/// Fold every op that is unapplied. An op this build cannot read stays pending and is not counted.
let applyUnappliedOps () : Task<int64> = applyUnappliedOpsPass ()


/// The regenerable projections: every table the op-fold writes. `deprecations` is one: it's folded
/// from `Deprecate`/`Undeprecate` ops (its `annotation_blob` reconstructs from the op), so it's
/// regenerable and `export` strips it like the others. NOT `package_blobs` (canonical content that
/// op-playback never writes), nor the op log / branch / commit / account state.
let projectionTables : List<string> =
  [ "package_functions"
    "package_types"
    "package_values"
    "package_traits"
    "package_trait_impls"
    "package_tests"
    "locations"
    "package_dependencies"
    "deprecations"
    "location_docs" ]



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
    // Nothing extra to reapply for resolutions: an `Override` decision is an op, so re-folding the log
    // rebuilds the `source = 'resolution'` rows in `locations` along with everything else.
    return folded
  }


/// One value that could not be evaluated. Structured rather than pre-formatted
/// so a caller can tell ITS OWN failures from an unrelated value that was
/// already sitting unevaluated in the store — `scmAddOps` reports only the
/// former, since it evaluates every pending value, not just the ones it added.
type ValueEvaluationError =
  {
    /// `None` for a whole-batch failure (non-convergence) that names no value.
    hash : Option<PT.Hash>
    location : string
    message : string
  }

module ValueEvaluationError =
  let toString (e : ValueEvaluationError) : string =
    match e.hash with
    | None -> e.message
    | Some(PT.Hash h) -> $"Value {h} ({e.location}): {e.message}"


/// The state package values are evaluated in. No policy is installed: a value runs under
/// `Execution.valueAccess` whoever stores it, which allows no effect at all, so neither the
/// instance policy nor anyone's package approvals could change the outcome.
let private valueState
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  : RT.ExecutionState =
  let notify _ _ _ _ = uply { return () }
  let sendException _ _ _ _ = uply { return () }
  Execution.createState builtins pm Execution.noTracing sendException notify

/// A failed evaluation as the message `ValueEvaluationError` carries.
let private failureMessage
  (state : RT.ExecutionState)
  (rte : RT.RuntimeError.Error)
  : Task<string> =
  task {
    let! shown = Execution.runtimeErrorMessage state rte
    return $"evaluation failed - {shown}"
  }

/// What the store keeps for an evaluated value: its bytes saved, so it can be serialized, and
/// no access captured, so a stored closure cannot keep the authority it was made under.
/// Streams cannot be stored, and are refused here with the reason.
let private prepareForStore
  (pm : RT.PackageManager)
  (dval : RT.Dval)
  : Task<Result<RT.Dval, string>> =
  task {
    let! dval = LibExecution.Bytes.promote pm.persistBytes dval
    let dval = LibExecution.Dval.stripCapturedAccess dval
    if LibExecution.Dval.isPersistable dval then
      return Ok dval
    else
      let reason =
        LibExecution.Dval.nonPersistableReason dval
        |> Option.defaultValue "value is not persistable"
      return Error $"cannot store in val — {reason}"
  }

/// Write an evaluated value's result where `getValue` reads it.
///
/// TODO: open question W?.1 of the package-tree data design: whether `AddValue` should carry
/// this result's hash (each machine evaluates and checks it) or the result itself. Until it is
/// settled, every machine that folds the op evaluates the value again, in `evaluateAllValues`.
let private storeResult (valueHash : PT.Hash) (dval : RT.Dval) : Task<unit> =
  let rtHash = PT2RT.Hash.toRT valueHash
  let rtValue = LibExecution.Dval.packageValue rtHash dval
  let (Hash defHash) = valueHash
  Sql.query
    """
    UPDATE package_values
    SET rt_dval = @rt_dval, value_type = @value_type
    WHERE hash = @hash AND rt_dval IS NULL
    """
  |> Sql.parameters
    [ "hash", Sql.string defHash
      "rt_dval", Sql.bytes (BS.RT.PackageValue.serialize rtHash rtValue)
      "value_type", Sql.bytes (BS.RT.ValueType.serialize (RT.Dval.toValueType dval)) ]
  |> Sql.executeStatementAsync

/// Evaluate all package values that have NULL rt_dval.
/// Multi-pass: values may depend on other values, so we retry until convergence.
let evaluateAllValues
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  : Task<Result<unit, List<ValueEvaluationError>>> =
  task {
    let exeState = valueState builtins pm

    let maxPasses = 10

    let unevaluated () =
      Sql.query
        """
        SELECT pv.hash, pv.pt_def, l.owner, l.modules, l.name
        FROM package_values pv
        LEFT JOIN locations l ON l.item_hash = pv.hash AND l.live = 1
        WHERE pv.rt_dval IS NULL
        GROUP BY pv.hash
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

    let evaluate
      (
        valueHash : Hash,
        ptDefBytes : byte[],
        fullName : string
      ) : Ply<Result<unit, ValueEvaluationError>> =
      uply {
        let fail message =
          Error { hash = Some valueHash; location = fullName; message = message }
        try
          let ptValue = BS.PT.PackageValue.deserialize valueHash ptDefBytes
          match! Execution.evaluateValue exeState fullName ptValue.body with
          | Error(rte, _callStack) ->
            let! message = failureMessage exeState rte
            return fail message
          | Ok dval ->
            match! prepareForStore pm dval with
            | Error message -> return fail message
            | Ok dval ->
              do! storeResult valueHash dval
              return Ok()
        with ex ->
          return fail $"exception - {ex.Message}"
      }

    // Stops when nothing is left, when a pass evaluates nothing, or after maxPasses.
    let rec passes (pass : int) : Task<List<ValueEvaluationError>> =
      task {
        let! unevaluatedValues = unevaluated ()
        if List.isEmpty unevaluatedValues then
          return []
        else if pass > maxPasses then
          return
            [ { hash = None
                location = ""
                message =
                  $"Gave up after {maxPasses} passes with "
                  + $"{List.length unevaluatedValues} values remaining" } ]
        else
          let! results =
            Ply.List.mapSequentially evaluate unevaluatedValues |> Ply.toTask
          let errors =
            results
            |> List.choose (fun r ->
              match r with
              | Error e -> Some e
              | Ok() -> None)
          if List.length errors = List.length results then
            return errors
          else
            return! passes (pass + 1)
      }

    let! lastErrors = passes 1

    if List.isEmpty lastErrors then return Ok() else return Error lastErrors
  }


/// The grow step for CLI/test startup: apply unapplied ops, generate package ref hashes, evaluate values.
/// On a warm DB it's a single fast SELECT COUNT. `getBuiltins` is a function, not a value, because builtins
/// must be constructed AFTER the hashes exist (construction triggers PackageRefs hash lookups).
///
/// Values are evaluated as every value is (`evaluateAllValues`), with no effects allowed, so a
/// value folded in from elsewhere cannot do here what it could not do where it was saved.
let growIfNeeded
  (getBuiltins : unit -> RT.Builtins)
  (pm : RT.PackageManager)
  (log : string -> unit)
  : Task<bool> =
  task {
    use _span = Telemetry.span "seed.growIfNeeded" []
    let! appliedCount =
      Telemetry.timeTask "seed.applyOps" [] (fun () -> applyUnappliedOps ())
    // A store can have every op applied yet still hold unevaluated values (rt_dval NULL): after a
    // migration that re-marks ops applied without evaluating, or a store copied/built without a final grow
    // (the test seed does exactly this). Gating evaluation on `appliedCount > 0` alone leaves those values
    // NULL forever, and a NULL `rt_dval` reads as "value not found". Evaluate whenever any value is
    // unevaluated so the store self-heals on startup.
    let! hasUnevaluatedValues =
      Sql.query
        "SELECT EXISTS(SELECT 1 FROM package_values WHERE rt_dval IS NULL) AS has_null"
      |> Sql.executeRowAsync (fun read -> read.int64 "has_null")
      |> Task.map (fun n -> n > 0L)
    if appliedCount > 0L then
      log $"Growing package DB from ops ({appliedCount} ops to apply)..."
      Telemetry.event "seed.applyOps.count" [ ("count", string appliedCount) ]
    // ABI type identities are PINNED: the committed package-ref-hashes.txt is authoritative, loaded by
    // PackageRefs on first access, and nothing here regenerates it. Regenerating on boot would let the
    // kernel's type identities float on whatever the local store happened to hash to. The generator is a
    // DEV tool (reload-packages / LocalExec fill), where regenerating produces a reviewable git diff,
    // which is what a re-pin should be.
    if appliedCount > 0L || hasUnevaluatedValues then
      let! _evalResult =
        Telemetry.timeTask "seed.evaluateValues" [] (fun () ->
          evaluateAllValues (getBuiltins ()) pm)
      do!
        Telemetry.timeTask "seed.walCheckpoint" [] (fun () ->
          Sql.query "PRAGMA wal_checkpoint(TRUNCATE);" |> Sql.executeStatementAsync)
      // Announce only when we grew from real op work; a pure self-heal (evaluating stray unevaluated values
      // with no new ops) is silent maintenance, and must not print to stdout, or it pollutes captured CLI
      // output (e.g. a caller comparing exact command output).
      if appliedCount > 0L then log "Package DB ready"
      return true
    else
      return false
  }
