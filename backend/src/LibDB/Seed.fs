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
module Permission = LibExecution.Permissions


/// Whether this process has already said that the store holds ops it cannot read.
///
/// Said once per process, not once per fold: a single command folds more than once, and the note is
/// context for whatever the command was asked to do, not an event worth repeating before every answer.
let mutable private warnedAboutUnreadableOps = false

/// Whether this process has already said that the store's format is ahead of this build's. Once,
/// for the same reason: it is context for the command, not an event.
let mutable private warnedAboutFormatSkew = false


// ---------------------
// Export
// ---------------------

/// Export a seed database to the given output path: copy the full source DB, then strip everything
/// that belongs to the machine that built it rather than to the package set (see the DELETEs below).
/// A seed, optionally cut at a COMMIT rather than at now.
///
/// `Some commit` keeps the ops that commit or one of its ancestors names, and drops the rest, so
/// two people fetching the same commit get the same bytes however far the source has moved since.
/// That immutability is what makes the seed cacheable and the pin reproducible.
///
/// Ancestry through `commits.parent`, the same walk `revert` uses: a commit names a point in
/// history, not a set of ops.
let exportAt (outputPath : string) (upToCommit : string option) : Task<unit> =
  task {
    if System.IO.File.Exists outputPath then System.IO.File.Delete outputPath

    // Through SQLite's online-backup API, never a file copy.
    //
    // The store this runs against is LIVE -- a server cuts a seed while serving, and the process
    // holding it has a WAL open -- and `data.db` alone is not the store while recent writes sit in
    // `data.db-wal`. The previous version checkpointed the source first and then copied the file,
    // which cannot work on a long-lived process: the checkpoint went through a ReadOnly connection,
    // so the moment there was actually a WAL to fold in it failed with a disk I/O error.
    //
    // It also reads `Sqlite.connString` rather than the config path, so a test that repoints LibDB
    // at its own store exports THAT store rather than the default one.
    match Backup.toFile outputPath with
    | Error e ->
      Exception.raiseInternal $"could not snapshot the store to cut a seed: {e}" []
    | Ok() -> ()

    // `Pooling=False`: a pooled connection outlives its `Close`, so on a process that cuts more than
    // one seed the second cut is handed a handle to a file the first one has since deleted and
    // replaced, and fails with "attempt to write a readonly database". A cut opens one connection and
    // happens rarely; pooling buys it nothing.
    let connStr =
      $"Data Source={outputPath};Mode=ReadWriteCreate;Cache=Private;Pooling=False"

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
      DELETE FROM package_builtin_deps;
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

      -- Ownership is a RELAY's index of which instance pushed which op: per-instance
      -- by definition, and a fresh install has never been pushed to. Readers join
      -- through package_ops, so stale rows fail to join silently -- and they are
      -- large.
      DELETE FROM op_owners;
      DELETE FROM relay_branches;

      -- A conflict is a finding about two peers' versions of one name. Same reasoning as `sync_bases`
      -- below: inheriting the builder's would be inheriting an argument between machines you have never met.
      DELETE FROM conflicts;

      -- Execution traces are dev telemetry, never part of a seed. They dominate a dev store by size
      -- (`trace_fn_calls` alone runs to hundreds of MB), so strip them and the seed is just canon.
      DELETE FROM trace_fn_calls;
      DELETE FROM traces;

      -- ALL of it: `config_v0` is per-install by construction, and nothing needs a
      -- value to boot (`entry_point` unset falls back to the shipped CLI).
      --
      -- A deny-list can't work: sync keys are named per peer (`sync.cursor.<url>`,
      -- ...), unknowable in advance. Costs if shipped: a shared INSTANCE ID means
      -- two peers that cannot sync or be told apart; a CURSOR skips ops the install
      -- has never seen; a CURRENT BRANCH dangles; all leak the builder's addresses.
      DELETE FROM config_v0;

      -- A sync base is a RELATIONSHIP with a specific peer. A fresh install has none, and inheriting the
      -- builder's would make it believe it had already agreed with machines it has never met.
      DELETE FROM sync_bases;

      -- Same reasoning: what a relay HOLDS of the builder's ops is the builder's relationship with that
      -- relay. Shipped, every fresh install would believe that relay already had its ops and never push.
      DELETE FROM sync_pushed;

      -- The builder's DRAFT: a seed is committed history by definition
      -- (`check-seed-carries-refs` asserts it), and an uncommitted op is instance
      -- state like the rows above. Stripped HERE rather than left to the guard: the
      -- F# suite leaves fixtures in main's draft, so a release build would otherwise
      -- refuse or not depending on what ran last. The bindings need no separate
      -- delete; every projection went above.
      DELETE FROM package_ops
      WHERE commit_hash IS NULL AND id NOT IN (SELECT op_id FROM op_branches);

      UPDATE package_ops SET applied = 0;
      """
    cleanCmd.ExecuteNonQuery() |> ignore<int>

    // Cut at a commit: drop every op the commit's history does not name, and every commit outside
    // that history. Runs AFTER the clean above, so it only ever narrows what that already kept.
    match upToCommit with
    | None -> ()
    | Some commit ->
      use cutCmd = conn.CreateCommand()
      cutCmd.CommandText <-
        """
        CREATE TEMP TABLE seed_ancestry AS
        WITH RECURSIVE ancestry(h) AS (
          SELECT hash FROM commits WHERE hash = $commit
          UNION
          SELECT c.parent FROM commits c JOIN ancestry a ON c.hash = a.h WHERE c.parent <> ''
        )
        SELECT h FROM ancestry;

        DELETE FROM package_ops
        WHERE commit_hash IS NULL OR commit_hash NOT IN (SELECT h FROM seed_ancestry);

        DELETE FROM commits WHERE hash NOT IN (SELECT h FROM seed_ancestry);

        DROP TABLE seed_ancestry;
        """
      cutCmd.Parameters.AddWithValue("$commit", commit) |> ignore<SqliteParameter>
      cutCmd.ExecuteNonQuery() |> ignore<int>

    // `cut_at` names a commit even when the caller asked for no cut, so EVERY seed says what it is
    // a cut of. Read back after the cut, so it is the tip of what the file actually holds rather
    // than the tip of the store it came from.
    let cutAt =
      use tipCmd = conn.CreateCommand()
      tipCmd.CommandText <-
        "SELECT hash FROM commits ORDER BY created_at DESC, rowid DESC LIMIT 1"
      match tipCmd.ExecuteScalar() with
      | null -> ""
      | tip -> string tip

    // The stamp every seed and every store carries, so a store can say which cut it came from and
    // which build made it. Written here because export is the only thing that knows.
    //
    // `format` is the op-blob layout version, which is what a migrator keys on: a store two
    // formats behind needs two steps, and a store from a NEWER format has to be refused rather
    // than misread.
    use stampCmd = conn.CreateCommand()
    stampCmd.CommandText <-
      """
      CREATE TABLE IF NOT EXISTS store_meta (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
      );
      DELETE FROM store_meta;
      INSERT INTO store_meta (key, value) VALUES
        ('format', $format),
        ('cut_at', $cutAt),
        ('kernel', $kernel),
        ('at', $at);
      """
    stampCmd.Parameters.AddWithValue(
      "$format",
      string LibSerialization.Binary.BaseFormat.currentVersion
    )
    |> ignore<SqliteParameter>
    stampCmd.Parameters.AddWithValue("$cutAt", cutAt) |> ignore<SqliteParameter>
    stampCmd.Parameters.AddWithValue("$kernel", LibConfig.Config.buildHash)
    |> ignore<SqliteParameter>
    stampCmd.Parameters.AddWithValue(
      "$at",
      System.DateTime.UtcNow.ToString(
        "o",
        System.Globalization.CultureInfo.InvariantCulture
      )
    )
    |> ignore<SqliteParameter>
    stampCmd.ExecuteNonQuery() |> ignore<int>

    use vacuumCmd = conn.CreateCommand()
    vacuumCmd.CommandText <- "VACUUM;"
    vacuumCmd.ExecuteNonQuery() |> ignore<int>

    // Out of WAL before anyone gets the file. A seed is SHIPPED -- copied, served over HTTP,
    // embedded in a binary -- and in WAL mode the `.db` on its own is not the whole database, so
    // whether it is complete depends on when a checkpoint happened to run. `journal_mode=DELETE`
    // folds the WAL back in and removes it, which makes the one file the whole seed.
    // `LibDB.Sqlite` puts a store back into WAL at open, so nothing downstream loses it.
    use settleCmd = conn.CreateCommand()
    settleCmd.CommandText <-
      "PRAGMA wal_checkpoint(TRUNCATE); PRAGMA journal_mode=DELETE;"
    settleCmd.ExecuteNonQuery() |> ignore<int>

    conn.Close()
  }


/// A seed of main as it stands now.
let export (outputPath : string) : Task<unit> = exportAt outputPath None


// ---------------------
// Grow
// ---------------------

/// The pending set: every op that is effective and not yet folded, as raw (id, blob) rows. The
/// fold's half is `foldRead`; `applyUnappliedOpsPass` is the two in sequence. Split so a test can put a
/// concurrent write between them, which is the case the by-id sweep in `foldRead` exists for.
///
/// TEST SEAM: public for `MultiInstance.Tests`, like `useStoreForTesting`. Nothing else calls the halves.
let readPending () : Task<List<System.Guid * byte[]>> =
  task {
    // Fast check: are there any unapplied ops? Avoids loading blobs when count is 0.
    let! count =
      Sql.query
        "SELECT COUNT(*) as n FROM package_ops WHERE applied = 0 AND effective = 1"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    if count = 0L then
      return []
    else
      // Read the raw (id, blob) rows WITHOUT deserializing in the reader: a malformed op_blob
      // (corrupt / truncated on the wire, or a poisoned push) must not throw here and brick the
      // whole fold -- AND every fold after it, since the op stays applied=0 and gets re-read.
      return!
        Sql.query
          """
        SELECT id, op_blob
        FROM package_ops
        WHERE applied = 0 AND effective = 1
        -- effective = 1: only ops main RUNS fold into the live projections. A branch's op, or one a
        -- client pushed to a relay, sits at applied = 0, effective = 0 -- present in the log, never
        -- folded, until a merge flips it effective.
        -- rowid breaks ties: created_at is second-resolution so a batch's ops share it. The fold's final
        -- state is order-independent, but a deterministic replay order keeps re-folds byte-identical.
        ORDER BY created_at ASC, rowid ASC
        """
        |> Sql.executeAsync (fun read -> (read.uuid "id", read.bytes "op_blob"))
  }

/// Fold the rows `readPending` returned, and mark exactly those applied. Returns the count folded.
/// TEST SEAM, as `readPending`.
let foldRead (rawOps : List<System.Guid * byte[]>) : Task<int64> =
  task {
    if List.isEmpty rawOps then
      return 0L
    else

      // Deserialize per-op; SKIP + log any that fail rather than aborting, so one bad op cannot brick a
      // store. They stay `applied = 0`, which is the truth -- nothing folded them -- and means a build
      // that CAN read them still will. Marking them applied would be faster (they are re-read every fold)
      // and would make an op unreadable by an older binary permanently invisible to a newer one.
      let mutable skipped : List<System.Guid> = []

      let unappliedOps =
        rawOps
        |> List.choose (fun (opId, opBlob) ->
          match BS.PT.PackageOp.tryDeserialize opId opBlob with
          | Some op -> Some(opId, op)
          | None ->
            skipped <- opId :: skipped
            None)

      // ONE line, not one per op. These ops are deliberately left unapplied so a later build can read
      // them, which means they are re-examined on every startup: a line each would put a wall of
      // warnings ahead of every command's real output, on every command, forever.
      match skipped with
      | [] -> ()
      | _ when warnedAboutUnreadableOps -> ()
      | ids ->
        warnedAboutUnreadableOps <- true
        System.Console.Error.WriteLine(
          $"note: {List.length ids} op(s) in this store were written in a format this build cannot "
          + "read, and are being skipped. They are kept, not dropped, so a later build can apply them."
        )

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
        // By ID, never by predicate. The SELECT above ran on another connection, outside this
        // transaction; a `serve` committing an op between it and here matches `applied = 0 AND
        // effective = 1` too, and a predicate sweep marked it applied with nothing having folded it, and
        // nothing ever re-read it. By id it stays applied = 0 and the next pass takes it. The same
        // argument covers the fold's own side effects (a merge event flipping a frontier effective
        // mid-fold) and the skipped, unreadable ops, which are simply not in the list.
        for chunk in unappliedOps |> List.map fst |> List.chunkBySize 500 do
          let quoted = chunk |> List.map (fun g -> $"'{g}'") |> String.concat ", "
          do! runRaw $"UPDATE package_ops SET applied = 1 WHERE id IN ({quoted})"

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
            |> Seq.map (fun (t, r, p, f) -> $"  {t} rowid={r} -> {p} (fk_id={f})")
            |> String.concat "\n"
          Exception.raiseInternal
            $"foreign_key_check reported {violations.Count} \
              violation(s) after grow:\n{summary}"
            [ "first_violations", summary ]

        return int64 opCount
  }


/// One pass: fold everything currently unapplied-and-effective. `applyUnappliedOps` repeats this,
/// because folding an op can make OTHER ops effective.
let private applyUnappliedOpsPass () : Task<int64> =
  task {
    let! pending = readPending ()
    return! foldRead pending
  }

/// Fold every op that is unapplied and effective, until there are none left.
///
/// Repeats because folding an op can MAKE other ops effective: a merge event flips
/// its branch's frontier, and those ops are not in the pass that folded the event.
///
/// Terminates because every pass marks what it folded as applied, so the set strictly shrinks; an op
/// this build cannot read stays pending but counts as nothing folded, so a pass that meets only those
/// returns 0 and stops the loop. The bound is a backstop against a future op kind that makes work
/// faster than this drains it, not an expected case; it is deliberately loud rather than silent if it
/// is ever hit.
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
          "applyUnappliedOps did not settle: an op kind is making ops effective \
           faster than the fold applies them"
          [ "passes", pass; "applied", total ]

      keepGoing <- n > 0L

    return total
  }


/// The regenerable projections: every table the op-fold writes. `deprecations` is one: it's folded
/// from `Deprecate`/`Undeprecate` ops (its `annotation_blob` reconstructs from the op), so it's
/// regenerable and `export` strips it like the others. NOT `package_blobs` (canonical content that
/// op-playback never writes), nor the op log / branch / commit / account state.
let projectionTables : List<string> =
  [ "package_functions"
    "package_types"
    "package_values"
    "locations"
    "package_dependencies"
    "package_builtin_deps"
    "deprecations"
    // Folded from `Decision` ops; nothing else writes it. Being here is what makes it genuinely derived
    // rather than a second source of truth about the same decisions.
    "propagation_policy" ]


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
    // Nothing extra to reapply for resolutions: an `Override` decision is an op, so re-folding the log
    // rebuilds the `source = 'resolution'` rows in `locations` along with everything else.
    return folded
  }


/// Remove seed-time access so a stored package value cannot retain the
/// permissions of the process that created it.
let private stripCapturedAccess = LibExecution.Dval.stripCapturedAccess

/// The authority a batch of package values is evaluated under.
///
/// A value body is code, and evaluating it runs that code. There is exactly one
/// trusted producer — `LocalExec`, building the bundled seed from the
/// checked-in `packages/` tree at build time. Every other caller is handling
/// bodies that may have arrived from a guest (authored through `val`, imported,
/// or synced), and gets `Guest`.
type EvaluationAuthority =
  /// Build-time seed construction from the checked-in `packages/` tree. The
  /// bodies are the ones shipped with the binary, so they carry full authority.
  | TrustedSeed

  /// Bodies of unknown provenance. `configure` turns the fresh evaluation
  /// state into the guest state they run under: the instance policy, the
  /// consumer's package approvals and the bundled exemption, all installed the
  /// same way ordinary guest execution installs them (`PolicyStore.guestState`),
  /// optionally narrowed by a caller's access. A function rather than a value
  /// because the bundled set and the policy file are only read when something
  /// is actually pending; and a function of the STATE, not just of the
  /// access, because a state built by hand kept `Execution.createState`'s
  /// allow-all package lookup and let a pending `val` call functions the
  /// consumer had never approved.
  | Guest of configure : (RT.ExecutionState -> Task<RT.ExecutionState>)

module EvaluationAuthority =
  let private guest
    (accountID : Option<System.Guid>)
    (narrowedBy : Option<Permission.Access>)
    : EvaluationAuthority =
    Guest(fun state ->
      task {
        // The bundled set is supplied by the host, as `Cli.execute` does; the
        // approval lookup denies any non-bundled package it does not know.
        let! bundled = LibDB.ProgramTypes.Fn.hashesOwnedBy "Darklang" |> Ply.toTask
        let state =
          { state with isBundledPackageFn = fun (RT.Hash h) -> bundled.Contains h }
          |> PolicyStore.guestState accountID Permission.Policy.allowAll [] []
        return
          match narrowedBy with
          | None -> state
          | Some caller ->
            { state with
                access = state.access |> Permission.Access.constrainBy caller }
      })

  /// The plain guest case: the operator's instance policy and the consumer's
  /// approvals, and nothing else.
  let underInstancePolicy : EvaluationAuthority = guest None None

  /// Guest code invoked from somewhere that carries its own access, which must
  /// also be honoured.
  ///
  /// The caller's access alone is NOT enough, which is why the instance policy
  /// is applied as well: CLI control code deliberately runs allow-all so it can
  /// manage packages and the policy itself (`Cli.execute`), so inheriting it
  /// would leave `val x = <denied effect>` escaping exactly as before. Being
  /// trusted to STORE a body is not being trusted to lend it authority.
  let underInstancePolicyAnd
    (accountID : Option<System.Guid>)
    (caller : Permission.Access)
    : EvaluationAuthority =
    guest accountID (Some caller)

let private configureFor
  (authority : EvaluationAuthority)
  (state : RT.ExecutionState)
  : Task<RT.ExecutionState> =
  match authority with
  | TrustedSeed ->
    Task.FromResult
      { state with access = Permission.Access.start Permission.Policy.allowAll }
  | Guest configure -> configure state

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

/// The values this store may EVALUATE: not the ones somebody else pushed here.
///
/// Folding an op is inert -- it deserializes and writes projection rows, and runs no guest code.
/// Evaluating a `val` is not: it executes the body. On a client that distinction does not arise,
/// because everything in its store is either its own or something it chose to pull. On a SERVER it
/// is the whole difference between holding somebody's code and running it, and `op_owners` already
/// records which ops arrived by push.
///
/// So: a value with a pushed binding and no local one is folded, browsable and servable in a seed,
/// and never executed here. Whoever fetches it evaluates it on their own machine, under their own
/// policy, which is where that decision belongs.
///
/// "and no local one" matters. Content is shared, so a value this store authored can also arrive by
/// push from a peer who wrote the same thing; anything locally bound is still evaluated.
///
/// `op_owners` is empty on an instance, so both EXISTS clauses are false there and this selects
/// exactly what it selected before.
let private evaluableValues =
  """
  pv.rt_dval IS NULL
  AND NOT (
    EXISTS (SELECT 1 FROM locations hl
             WHERE hl.item_hash = pv.hash
               AND hl.op_id IN (SELECT op_id FROM op_owners))
    AND NOT EXISTS (SELECT 1 FROM locations ll
                     WHERE ll.item_hash = pv.hash
                       AND ll.op_id NOT IN (SELECT op_id FROM op_owners))
  )
  """


/// Evaluate all package values that have NULL rt_dval, under `authority`.
/// Multi-pass: values may depend on other values, so we retry until convergence.
let evaluateAllValues
  (authority : EvaluationAuthority)
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  : Task<Result<unit, List<ValueEvaluationError>>> =
  task {
    let program : RT.Program = { dbs = Map.empty }

    let notify _ _ _ _ = uply { return () }
    let sendException _ _ _ _ = uply { return () }

    // NOT `Policy.allowAll`. A `val` body is guest code like any other:
    // evaluating it under the host's own authority let `val x =
    // Builtin.cliExecute "..."` run commands that the very same expression was
    // denied at `eval`. `stripCapturedAccess` below keeps the *stored* value
    // from retaining authority; this keeps the *evaluation* within the policy
    // and the consumer's approvals.
    let! exeState =
      Execution.createState
        builtins
        pm
        Execution.noTracing
        sendException
        notify
        program
      |> configureFor authority

    let maxPasses = 10
    let mutable pass = 0
    let mutable keepGoing = true
    let mutable lastErrors : List<ValueEvaluationError> = []

    while keepGoing do
      pass <- pass + 1

      let! unevaluatedValues =
        Sql.query
          $"""
          SELECT pv.hash, pv.pt_def, l.owner, l.modules, l.name
          FROM package_values pv
          LEFT JOIN locations l ON l.item_hash = pv.hash AND l.unlisted_at IS NULL
          WHERE {evaluableValues}
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
          [ { hash = None
              location = ""
              message =
                $"Gave up after {maxPasses} passes with "
                + $"{List.length unevaluatedValues} values remaining" } ]
      else
        let errors = ResizeArray<ValueEvaluationError>()
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
                { hash = Some valueHash
                  location = fullName
                  message = $"evaluation failed - {errorMsg}" }
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
                  { hash = Some valueHash
                    location = fullName
                    message = $"cannot store in val — {reason}" }
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
            errors.Add(
              { hash = Some valueHash
                location = fullName
                message = $"exception - {ex.Message}" }
            )

        if successCount = 0 then
          keepGoing <- false
          lastErrors <- errors |> List.ofSeq

    if List.isEmpty lastErrors then return Ok() else return Error lastErrors
  }


/// The grow step for CLI/test startup: apply unapplied ops, generate package ref hashes, evaluate values.
/// On a warm DB it's a single fast SELECT COUNT. `getBuiltins` is a function, not a value, because builtins
/// must be constructed AFTER the hashes exist (construction triggers PackageRefs hash lookups).
///
/// `authority` bounds the value evaluation. The store this runs against is not
/// all bundled — an import or a sync folds in ops from elsewhere, and their
/// values are evaluated by exactly this call — so a startup that trusted its own
/// store would run guest code with the host's authority. Callers pass the
/// instance policy; only build-time seed construction passes `TrustedSeed`.
let growIfNeeded
  (authority : EvaluationAuthority)
  (getBuiltins : unit -> RT.Builtins)
  (pm : RT.PackageManager)
  (log : string -> unit)
  : Task<bool> =
  task {
    use _span = Telemetry.span "seed.growIfNeeded" []

    // Every process that opens the store passes through here, which is the only place a skew
    // between the store's format and this build's is certain to be noticed. `Releases.runPending`
    // says it too, but a shipped binary reaches that only when it has an embedded seed to unpack.
    if not warnedAboutFormatSkew then
      warnedAboutFormatSkew <- true
      Releases.noteFormatSkew ()

    let! appliedCount =
      Telemetry.timeTask "seed.applyOps" [] (fun () -> applyUnappliedOps ())
    // The fold above reads effective=1 only, so branch-scoped Decisions (a branch's propagation
    // pins) never pass through it. After a projection drop (the migrations path defers here),
    // skipping this would silently delete every branch pin; refolding is idempotent and
    // origin_ts-guarded, so run it whenever the fold actually folded something.
    if appliedCount > 0 then do! Branches.refoldBranchDecides ()
    // A store can have every op applied yet still hold unevaluated values (rt_dval NULL): after a
    // migration that re-marks ops applied without evaluating, or a store copied/built without a final grow
    // (the test seed does exactly this). Gating evaluation on `appliedCount > 0` alone leaves those values
    // NULL forever, and a NULL `rt_dval` reads as "value not found". Evaluate whenever any value is
    // unevaluated so the store self-heals on startup.
    let! hasUnevaluatedValues =
      // The same predicate `evaluateAllValues` selects with, or a server would take this branch on
      // every startup for hosted values it is never going to evaluate.
      Sql.query
        $"SELECT EXISTS(SELECT 1 FROM package_values pv WHERE {evaluableValues}) AS has_null"
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
          evaluateAllValues authority (getBuiltins ()) pm)
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
