/// Package seed: extract and grow.
///
/// A seed is a store with main's committed ops and none of them applied (it carries the full schema, so it
/// works directly as a data.db). Export builds it from the schema and copies the ops in; grow folds the unapplied ops back into the projections and evaluates values. Grow runs on CLI startup,
/// and is a single lookup past the fold's mark when nothing is pending.
///
/// The op log (`package_ops`) is canonical; the package tables are regenerable projections folded from it.
/// `applyUnappliedOps` folds pending ops (`AppliedOps` is the append/fold seam); `rebuildProjections`
/// drops the projections, marks every op unapplied, and re-folds. So a schema change is safe (drop + re-fold,
/// never touching the log) and a synced peer's ops fold in like a local edit.
module LibDB.Seed

open System.Threading.Tasks

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
  Telemetry.count "sql.seed"
  cmd.ExecuteNonQuery() |> ignore<int>
  conn.Close()


/// The schema a seed is built from: the CLI's embedded copy when the running program carries one,
/// else `migrations/schema/` on disk. Concatenated in file-name order, as every other reader does.
let private schemaSql () : string =
  let embedded =
    match Option.ofObj (System.Reflection.Assembly.GetEntryAssembly()) with
    | None -> [||]
    | Some assembly ->
      assembly.GetManifestResourceNames()
      |> Array.filter (fun n -> n.StartsWith "schema/" && n.EndsWith ".sql")
      |> Array.sort
      |> Array.map (fun name ->
        use stream = assembly.GetManifestResourceStream name
        use reader = new System.IO.StreamReader(stream)
        reader.ReadToEnd())

  if not (Array.isEmpty embedded) then
    String.concat "\n" embedded
  else
    System.IO.Directory.GetFiles(
      System.IO.Path.Combine(LibConfig.Dirs.migrationsDir, "schema"),
      "*.sql"
    )
    |> Array.sort
    |> Array.map System.IO.File.ReadAllText
    |> String.concat "\n"


/// What a seed carries from the store it is built from: main's committed history and the content it
/// points at. Everything else is the builder's own, or a projection the first run folds again.
let private keptRows : List<string * string> =
  [ "system_migrations_v0", ""
    "accounts_v0", ""
    "package_blobs", ""
    "commits", "WHERE branch_id = '00000000-0000-0000-0000-000000000001'"
    "package_ops",
    "WHERE branch_id = '00000000-0000-0000-0000-000000000001' AND commit_hash IS NOT NULL" ]


/// Build a seed at <param outputPath> from the store at <param sourcePath>: a new file made from the
/// schema, with only `keptRows` copied in. Never a copy of the source file, whose tables keep the
/// shape they were created with.
let exportFrom (sourcePath : string) (outputPath : string) : unit =
  if System.IO.File.Exists outputPath then System.IO.File.Delete outputPath

  checkpointWal sourcePath

  // made here, not by SQLite, so a missing directory names the path; SQLite only says it can't open it
  System.IO.File.WriteAllBytes(outputPath, [||])

  use conn =
    new SqliteConnection(
      $"Data Source={outputPath};Mode=ReadWriteCreate;Cache=Private"
    )
  conn.Open()

  let run (sql : string) : unit =
    use cmd = conn.CreateCommand()
    cmd.CommandText <- sql
    Telemetry.count "sql.seed"
    cmd.ExecuteNonQuery() |> ignore<int>

  let columns (schema : string) (table : string) : List<string> =
    use cmd = conn.CreateCommand()
    cmd.CommandText <- $"SELECT name FROM pragma_table_info('{table}', '{schema}')"
    Telemetry.count "sql.seed"
    use reader = cmd.ExecuteReader()
    [ while reader.Read() do
        yield reader.GetString 0 ]

  run "PRAGMA journal_mode=WAL; PRAGMA synchronous=NORMAL; PRAGMA busy_timeout=5000;"

  // Indexes and triggers come after the rows: the triggers would journal every copied row, and an
  // index is cheaper built once over a full table.
  let isIndexOrTrigger (statement : string) =
    let upper = statement.ToUpperInvariant()
    upper.StartsWith "CREATE INDEX"
    || upper.StartsWith "CREATE UNIQUE INDEX"
    || upper.StartsWith "CREATE TRIGGER"

  let addColumn =
    System.Text.RegularExpressions.Regex(
      @"^ALTER\s+TABLE\s+(\w+)\s+ADD\s+COLUMN\s+(\w+)\s",
      System.Text.RegularExpressions.RegexOptions.IgnoreCase
    )

  let statements = Releases.schemaStatements (schemaSql ())

  for statement in statements do
    if not (isIndexOrTrigger statement) then
      let m = addColumn.Match statement
      if
        not m.Success
        || not (List.contains m.Groups[2].Value (columns "main" m.Groups[1].Value))
      then
        run statement

  let quoted = sourcePath.Replace("'", "''")
  run $"ATTACH DATABASE '{quoted}' AS src"

  for table, where in keptRows do
    let source = columns "src" table
    let shared =
      columns "main" table
      |> List.filter (fun c -> List.contains c source)
      |> String.concat ", "
    if shared <> "" then
      run
        $"INSERT OR IGNORE INTO main.{table} ({shared}) SELECT {shared} FROM src.{table} {where}"

  run "DETACH DATABASE src"

  for statement in statements do
    if isIndexOrTrigger statement then run statement

  run "VACUUM;"
  conn.Close()


/// Export a seed of this store (`Config.dbPath`) to the given output path.
let export (outputPath : string) : unit =
  LibExecution.PackageRefs.validate ()
  exportFrom LibConfig.Config.dbPath outputPath


// ---------------------
// Grow
// ---------------------

/// The pending set: every op not yet folded, as raw (rowid, branch, id, blob) rows, in log order. The
/// fold's half is `foldRead`; `applyUnappliedOpsPass` is the two in sequence. Split so a test can put a
/// concurrent write between them, which is the case `foldRead`'s mark has to get right.
let readPending () : List<int64 * PT.BranchId * System.Guid * byte[]> =
  // Read the raw rows WITHOUT deserializing in the reader: a malformed op_blob must not throw here
  // and brick the whole fold -- AND every fold after it, since the op stays unapplied.
  Sql.query (
    AppliedOps.pending "p.rowid AS seq, p.branch_id, p.id, p.op_blob"
    + " ORDER BY seq"
  )
  |> Sql.executeRows (fun read ->
    (read.int64 "seq",
     PT.BranchId.ParseUnsafe(read.string "branch_id"),
     read.uuid "id",
     read.bytes "op_blob"))

/// The tables a cold start fills from empty, each with the secondary indexes the fold itself reads
/// through, which have to stay.
let private filledByColdStart : List<string * List<string>> =
  [ "package_dependencies", []
    // Every name op looks up what its layer already binds at that name.
    "locations", [ "idx_locations_name"; "idx_locations_history" ]
    "package_functions", []
    "package_tests", [] ]

/// Drop the secondary indexes of each table in `filledByColdStart` that is empty, apart from the ones
/// the fold reads through, and return the SQL that builds them again. A cold start writes every row
/// these tables hold, and keeping their indexes up to date row by row costs more than building them
/// once from the finished tables. Unique indexes and keys stay: the inserts rely on them to skip
/// duplicates.
let private dropIndexesOfEmptyTables (conn : SqliteConnection) : List<string> =
  let perTable =
    filledByColdStart
    |> List.map (fun (table, kept) ->
      use cmd = conn.CreateCommand()
      let keptList = kept |> List.map (fun n -> $"'{n}'") |> String.concat ", "
      cmd.CommandText <-
        $"SELECT name, sql FROM sqlite_master
          WHERE type = 'index' AND tbl_name = '{table}'
            AND sql IS NOT NULL AND substr(sql, 1, 13) <> 'CREATE UNIQUE'
            AND name NOT IN ({keptList})
            AND NOT EXISTS (SELECT 1 FROM {table})"
      Telemetry.count "sql.seed"
      use reader = cmd.ExecuteReader()
      reader |> Sqlite.readRows (fun r -> (r.GetString 0, r.GetString 1)))
  let indexes = List.concat perTable
  for (name, _) in indexes do
    use drop = conn.CreateCommand()
    drop.CommandText <- $"DROP INDEX {name}"
    Telemetry.count "sql.seed"
    let _ = drop.ExecuteNonQuery()
    ()
  List.map snd indexes


/// What each row at <param rowids> says the fold needs: its stamp, whether it is still in the
/// draft, and what wrote it.
let private logRowsOf
  (conn : SqliteConnection)
  (rowids : List<int64>)
  : Map<int64, PackageOpPlayback.OpRow> =
  use cmd = conn.CreateCommand()
  let listed = rowids |> List.map string |> String.concat ", "
  cmd.CommandText <-
    $"SELECT rowid, origin_ts, commit_hash IS NULL, source FROM package_ops
      WHERE rowid IN ({listed})"
  Telemetry.count "sql.seed"
  use reader = cmd.ExecuteReader()
  reader
  |> Sqlite.readRows (fun r ->
    (r.GetInt64 0,
     ({ ts = (if r.IsDBNull 1 then None else Some(r.GetString 1))
        draft = r.GetInt64 2 <> 0L
        source = r.GetString 3 }
     : PackageOpPlayback.OpRow)))
  |> Map.ofList


/// How many ops `foldRead` deserializes and folds at a time. Measured on a cold start: 250 did the
/// least work and peaked lowest; 1,000 peaked about 18 MB higher, and below 150 the collections
/// between chunks start to cost more than they save.
let private foldChunkSize = 250

/// Fold the rows `readPending` returned, each into its own branch, and mark exactly those applied.
/// Returns the count folded.
///
/// The mark moves to the last row read, and the rows this build could not read stay behind it as
/// exceptions. Nothing else can be pending at or below that row: a row written since the read comes
/// after it in the log, and an author that moved the mark in between kept what it passed over.
let foldRead (rawOps : List<int64 * PT.BranchId * System.Guid * byte[]>) : int64 =
  if List.isEmpty rawOps then
    0L
  else

    // Deserialize per-op; SKIP + log any that fail rather than aborting, so one bad op cannot brick a
    // store. They stay unapplied, which is the truth -- nothing folded them -- and means a build
    // that CAN read them still will.
    //
    // A chunk at a time: a cold start folds every op in the store, and all of them deserialized at
    // once is most of its peak memory.
    let mutable skipped : List<int64> = []
    let mutable folded = 0L

    // Opened at the first op that deserializes, so a store holding only ops this build cannot
    // read never takes the write lock.
    let mutable conn : SqliteConnection = null
    let mutable tx : SqliteTransaction = null
    let mutable droppedIndexes : List<string> = []
    let runRaw (conn : SqliteConnection) (sql : string) : unit =
      use cmd = conn.CreateCommand()
      cmd.CommandText <- sql
      Telemetry.count "sql.seed"
      let _ = cmd.ExecuteNonQuery()
      ()
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
            |> Option.map (fun op -> (seq, branch, opId, opBlob, op)))
        if List.length unappliedOps < List.length chunk then
          let read =
            unappliedOps |> List.map (fun (seq, _, _, _, _) -> seq) |> Set.ofList
          skipped <-
            (chunk
             |> List.map (fun (seq, _, _, _) -> seq)
             |> List.filter (fun seq -> not (Set.contains seq read)))
            @ skipped

        if not (List.isEmpty unappliedOps) then
          if isNull conn then
            // All ops + the mark in ONE transaction: for 9000+ ops that collapses ~20k WAL commits
            // into one.
            conn <- new SqliteConnection(LibDB.Sqlite.connString)
            conn.Open()
            // synchronous=NORMAL, not OFF: OFF lets the writer skip syncing the change-counter
            // update, which poisons the page cache of a concurrent reader on another connection.
            //
            // 16 MB of page cache rather than SQLite's 2 MB: most of the fold's writes land in
            // indexes keyed on random hashes, and with the default nearly every one of them misses
            // the cache.
            runRaw
              conn
              "PRAGMA journal_mode=WAL; PRAGMA synchronous=NORMAL; PRAGMA busy_timeout=5000;
               PRAGMA cache_size=-16384;"
            tx <- conn.BeginTransaction()
            // Before the first chunk folds, so the check sees the table as the fold found it.
            let dropped = dropIndexesOfEmptyTables conn
            droppedIndexes <- dropped

          // Read under the write lock, so it is what the row says now.
          let logRows =
            logRowsOf conn (unappliedOps |> List.map (fun (seq, _, _, _, _) -> seq))

          // Each run of consecutive ops on one branch folds into that branch, in log order.
          let runs =
            unappliedOps
            |> List.fold
              (fun
                   (acc : List<PT.BranchId * List<PackageOpPlayback.LoggedOp>>)
                   (seq, branch, opId, opBlob, op) ->
                let logged : PackageOpPlayback.LoggedOp =
                  { op = op
                    blob = opBlob
                    id = opId
                    row =
                      Map.tryFind seq logRows
                      |> Option.defaultValue PackageOpPlayback.OpRow.notInLog }
                match acc with
                | (b, ops) :: rest when b = branch -> (b, logged :: ops) :: rest
                | _ -> (branch, [ logged ]) :: acc)
              []
            |> List.rev
            |> List.map (fun (b, ops) -> (b, List.rev ops))
          let dependencies =
            runs
            |> List.map (fun (branch, ops) ->
              PackageOpPlayback.foldLoggedOnConnection conn branch ops)
          PackageOpPlayback.writeDependencies conn (List.concat dependencies)
          folded <- folded + int64 (List.length unappliedOps)

      if not (isNull tx) then
        for sql in droppedIndexes do
          runRaw conn sql
        use mark = conn.CreateCommand()
        mark.CommandText <- AppliedOps.foldedThrough
        mark.Parameters.AddWithValue(
          "$last",
          rawOps
          |> List.maxBy (fun (seq, _, _, _) -> seq)
          |> (fun (seq, _, _, _) -> seq)
        )
        |> ignore<SqliteParameter>
        mark.Parameters.AddWithValue(
          "$skipped",
          "[" + (skipped |> List.map string |> String.concat ",") + "]"
        )
        |> ignore<SqliteParameter>
        Telemetry.count "sql.seed"
        let _ = mark.ExecuteNonQuery()
        tx.Commit()
    finally
      if not (isNull tx) then tx.Dispose()
      if not (isNull conn) then conn.Dispose()

    // ONE line, not one per op: these are re-examined on every startup.
    if not (List.isEmpty skipped) && not warnedAboutUnreadableOps then
      warnedAboutUnreadableOps <- true
      NonBlockingConsole.writeErrLine (
        $"note: {List.length skipped} op(s) in this store were written in a format this build cannot "
        + "read, and are being skipped. They are kept, not dropped, so a later build can apply them."
      )

    folded


/// One pass: fold everything currently unapplied.
let private applyUnappliedOpsPass () : int64 =
  let pending = readPending ()
  foldRead pending

/// Fold every op that is unapplied. An op this build cannot read stays pending and is not counted.
let applyUnappliedOps () : int64 = applyUnappliedOpsPass ()


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
let rebuildProjections () : int64 =
  // 1. clear the regenerable projection tables (single source of truth = projectionTables).
  for t in projectionTables do
    Sql.query $"DELETE FROM {t}" |> Sql.executeStatement
  // 2. mark all ops unapplied so the fold reprocesses the whole log
  Sql.query AppliedOps.reset |> Sql.executeStatement
  // 3. re-fold ops -> projections via the existing playback path
  let folded = applyUnappliedOps ()
  // Nothing extra to reapply for resolutions: an `Override` decision is an op, so re-folding the log
  // rebuilds the `source = 'resolution'` rows in `locations` along with everything else.
  folded


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
  let notify _ _ _ _ = ()
  let sendException _ _ _ _ = ()
  Execution.createState builtins pm Execution.noTracing sendException notify

/// A failed evaluation as the message `ValueEvaluationError` carries.
let private failureMessage
  (state : RT.ExecutionState)
  (rte : RT.RuntimeError.Error)
  : string =
  let shown = Execution.now (Execution.runtimeErrorMessage state rte)
  $"evaluation failed - {shown}"

/// What the store keeps for an evaluated value: its bytes saved, so it can be serialized, and
/// no access captured, so a stored closure cannot keep the authority it was made under.
/// Streams cannot be stored, and are refused here with the reason.
let private prepareForStore
  (pm : RT.PackageManager)
  (dval : RT.Dval)
  : Result<RT.Dval, string> =
  let dval = LibExecution.Bytes.promote pm.persistBytes dval
  let dval = LibExecution.Dval.stripCapturedAccess dval
  if LibExecution.Dval.isPersistable dval then
    Ok dval
  else
    let reason =
      LibExecution.Dval.nonPersistableReason dval
      |> Option.defaultValue "value is not persistable"
    Error $"cannot store in val — {reason}"

/// Write an evaluated value's result where `getValue` reads it.
///
/// `AddValue` carries neither the result nor its hash, so every machine that folds the op
/// evaluates the value again, in `evaluateAllValues`.
let private storeResult (valueHash : PT.Hash) (dval : RT.Dval) : unit =
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
  |> Sql.executeStatement

/// Evaluate all package values that have NULL rt_dval.
/// Multi-pass: values may depend on other values, so we retry until convergence.
let evaluateAllValues
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  : Result<unit, List<ValueEvaluationError>> =
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
    |> Sql.executeRows (fun read ->
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
    ) : Result<unit, ValueEvaluationError> =
    let fail message =
      Error { hash = Some valueHash; location = fullName; message = message }
    try
      let ptValue = BS.PT.PackageValue.deserialize valueHash ptDefBytes
      match
        Execution.now (Execution.evaluateValue exeState fullName ptValue.body)
      with
      | Error(rte, _callStack) ->
        let message = failureMessage exeState rte
        fail message
      | Ok dval ->
        match prepareForStore pm dval with
        | Error message -> fail message
        | Ok dval ->
          storeResult valueHash dval
          Ok()
    with ex ->
      fail $"exception - {ex.Message}"

  // Stops when nothing is left, when a pass evaluates nothing, or after maxPasses.
  let rec passes (pass : int) : List<ValueEvaluationError> =
    let unevaluatedValues = unevaluated ()
    if List.isEmpty unevaluatedValues then
      []
    else if pass > maxPasses then
      [ { hash = None
          location = ""
          message =
            $"Gave up after {maxPasses} passes with "
            + $"{List.length unevaluatedValues} values remaining" } ]
    else
      let results = List.map evaluate unevaluatedValues
      let errors =
        results
        |> List.choose (fun r ->
          match r with
          | Error e -> Some e
          | Ok() -> None)
      if List.length errors = List.length results then errors else passes (pass + 1)

  let lastErrors = passes 1

  if List.isEmpty lastErrors then Ok() else Error lastErrors


/// The grow step for CLI/test startup: apply unapplied ops, generate package ref hashes, evaluate values.
/// On a warm DB it's a single fast lookup past the mark. `getBuiltins` is a function, not a value, because builtins
/// must be constructed AFTER the hashes exist (construction triggers PackageRefs hash lookups).
///
/// Values are evaluated as every value is (`evaluateAllValues`), with no effects allowed, so a
/// value folded in from elsewhere cannot do here what it could not do where it was saved.
/// Whether <fn growIfNeeded> has anything to do: an op still to apply, or a value not yet
/// evaluated. Asked before taking the upgrade lock, so a start with nothing to grow takes none.
let needsGrowing () : bool =
  let pending = AppliedOps.pending "1"
  Sql.query
    $"SELECT EXISTS({pending})
       OR EXISTS(SELECT 1 FROM package_values WHERE rt_dval IS NULL) AS needed"
  |> Sql.executeRow (fun read -> read.int64 "needed")
  |> fun n -> n > 0L


let growIfNeeded
  (getBuiltins : unit -> RT.Builtins)
  (pm : RT.PackageManager)
  (log : string -> unit)
  : bool =
  use _span = Telemetry.span "seed.growIfNeeded" []
  let appliedCount =
    Telemetry.time "seed.applyOps" [] (fun () -> applyUnappliedOps ())
  // A store can have every op applied yet still hold unevaluated values (rt_dval NULL): after a
  // migration that re-marks ops applied without evaluating, or a store copied/built without a final grow
  // (the test seed does exactly this). Gating evaluation on `appliedCount > 0` alone leaves those values
  // NULL forever, and a NULL `rt_dval` reads as "value not found". Evaluate whenever any value is
  // unevaluated so the store self-heals on startup. Answered from `idx_package_values_unevaluated`,
  // which holds only those values, so a warm start does not read the table to find none.
  let hasUnevaluatedValues =
    Sql.query
      "SELECT EXISTS(SELECT 1 FROM package_values WHERE rt_dval IS NULL) AS has_null"
    |> Sql.executeRow (fun read -> read.int64 "has_null")
    |> (fun n -> n > 0L)
  if appliedCount > 0L then
    log $"Growing package DB from ops ({appliedCount} ops to apply)..."
    Telemetry.event "seed.applyOps.count" [ ("count", string appliedCount) ]
  // ABI type identities are PINNED: the committed package-ref-hashes.txt is authoritative, loaded by
  // PackageRefs on first access, and nothing here regenerates it. Regenerating on boot would let the
  // kernel's type identities float on whatever the local store happened to hash to. The generator is a
  // DEV tool (reload-packages / LocalExec fill), where regenerating produces a reviewable git diff,
  // which is what a re-pin should be.
  if appliedCount > 0L || hasUnevaluatedValues then
    let _evalResult =
      Telemetry.time "seed.evaluateValues" [] (fun () ->
        evaluateAllValues (getBuiltins ()) pm)
    Telemetry.time "seed.walCheckpoint" [] (fun () ->
      Sql.query "PRAGMA wal_checkpoint(TRUNCATE);" |> Sql.executeStatement)
    // Announce only when we grew from real op work; a pure self-heal (evaluating stray unevaluated values
    // with no new ops) is silent maintenance, and must not print to stdout, or it pollutes captured CLI
    // output (e.g. a caller comparing exact command output).
    if appliedCount > 0L then log "Package DB ready"
    true
  else
    false
