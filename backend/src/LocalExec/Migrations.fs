/// SQLite schema bootstrap + incremental migrations.
///
/// Two layers:
///
/// 1. `backend/migrations/schema.sql` is the canonical from-scratch
///    shape of every table. We hash the file's bytes and compare
///    against `schema_state_v0.hash`; if they differ (or the table
///    is missing), drop the REGENERABLE PROJECTION tables only
///    (`Seed.projectionTables`) and replay the file. The canonical
///    op log, blobs, branches and commits survive, and the log
///    re-folds into the fresh projections at startup — so a schema
///    change costs CPU, not your authored work.
///
///    The corollary is the trap: because the canonical tables are
///    NOT dropped, `CREATE TABLE IF NOT EXISTS` no-ops on them, and
///    a change to a canonical table's SHAPE (a new column, a new PK)
///    NEVER reaches an existing store from this file. That belongs
///    in a Release step (`LibDB.Releases`), which copy-and-swaps.
///    schema.sql is where the shape is DECLARED; for existing
///    stores, a Release step is what puts it there.
///
/// 2. `backend/migrations/incremental/*.sql` is a directory of
///    per-file additive migrations, run in lexical order and
///    name-dedup'd via `system_migrations_v0`. Use this for layered
///    changes that don't justify a full rebuild — typically data
///    backfills or transforms atop the schema.sql base.
///
/// On startup: schema-bootstrap first (drop projections + replay if
/// the file changed), then run any new incremental migrations
/// against the post-bootstrap DB.
///
/// Note that we don't use Tasks in here because migrations run in
/// order — easier to execute synchronously than have a bunch of code
/// to use tasks and then extra code to ensure the tasks run
/// synchronously.
///
/// CLEANUP maybe move this to LibDB?
module LocalExec.Migrations

open System.IO
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Sqlite
module File = LibCloud.File
module Config = LibCloud.Config

open Prelude


// ---------------------
// Schema-hash bootstrap (kill-and-fill on change)
// ---------------------

let private schemaFile = "schema.sql"


let private computeHash (sql : string) : string =
  use sha = System.Security.Cryptography.SHA256.Create()
  sql |> UTF8.toBytes |> sha.ComputeHash |> System.Convert.ToHexString


let private tableExists (name : string) : bool =
  Sql.query
    "SELECT 1
      FROM sqlite_master
      WHERE type = 'table'
        AND name = @name"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeExistsSync


let private storedHash () : Option<string> =
  if not (tableExists "schema_state_v0") then
    None
  else
    match
      Sql.query "SELECT hash FROM schema_state_v0 WHERE id = 0"
      |> Sql.execute (fun read -> read.string "hash")
    with
    | Ok [ h ] -> Some h
    | Ok [] -> None
    | Ok rows ->
      Exception.raiseInternal
        "Multiple schema_state_v0 rows; expected 0 or 1"
        [ "actual", rows ]
    | Error err -> Exception.raiseInternal $"storedHash: {err}" [ "err", err ]


/// Drop ONLY the regenerable projection tables — never the canonical op log, blobs, branches,
/// commits, or account/user state. This is what lets a schema change keep your work: your authored
/// ops survive; only the cache is rebuilt. The list is `Seed.projectionTables` (single source
/// of truth — the same set the runtime's `rebuildProjections` clears), so it can't drift.
let private dropProjectionTables () : unit =
  // FK off for the drop (a child projection may FK a parent we're keeping); connection-scoped, so
  // the next connection (which replays schema.sql) gets the default back.
  Sql.query "PRAGMA foreign_keys = OFF" |> Sql.executeStatementSync
  for t in LibDB.Seed.projectionTables do
    Sql.query (sprintf "DROP TABLE IF EXISTS \"%s\"" t) |> Sql.executeStatementSync

/// Mark every op unapplied so the next `Seed.growIfNeeded` re-folds the whole log into the freshly
/// recreated projections. Re-folding (with value evaluation) needs the runtime, which the migration
/// phase doesn't have — so we defer the fold to startup, exactly like a fresh seed does.
let private markOpsUnapplied () : unit =
  if tableExists "package_ops" then
    Sql.query "UPDATE package_ops SET applied = 0" |> Sql.executeStatementSync

let private opCount () : int =
  if tableExists "package_ops" then
    match
      Sql.query "SELECT COUNT(*) AS c FROM package_ops"
      |> Sql.execute (fun read -> read.int "c")
    with
    | Ok(c :: _) -> c
    | _ -> 0
  else
    0


let private writeHash (hash : string) : unit =
  Sql.query
    "CREATE TABLE IF NOT EXISTS schema_state_v0
     (id INTEGER PRIMARY KEY, hash TEXT NOT NULL)"
  |> Sql.executeStatementSync
  Sql.query "INSERT OR REPLACE INTO schema_state_v0 (id, hash) VALUES (0, @hash)"
  |> Sql.parameters [ "hash", Sql.string hash ]
  |> Sql.executeStatementSync


let private runSchemaBootstrap () : unit =
  let sql = File.readfile Config.Migrations schemaFile
  let want = computeHash sql

  match storedHash () with
  | Some have when have = want -> ()
  | Some have ->
    // Preserve-and-refold (not kill-and-fill): drop only the regenerable projections; the canonical op
    // log + blobs + branch/commit/account state survive. Replaying schema.sql recreates the dropped
    // projections in their new shape and is a no-op for the surviving canonical tables
    // (CREATE TABLE IF NOT EXISTS). Marking ops unapplied makes the next `growIfNeeded` re-fold them.
    // NOTE: a canonical-table SHAPE change can't go through this path (CREATE IF NOT EXISTS won't
    // alter an existing table) — it needs a data-preserving incremental (the Release migrator).
    let ops = opCount ()
    print
      $"schema.sql changed (hash {have[0..7]} → {want[0..7]}); preserving {ops} op(s), \
        rebuilding projections."
    dropProjectionTables ()
    // A canonical-table SHAPE change (a new column on package_ops, say) lands here as a raw SQLite error
    // like "no such column", because CREATE TABLE IF NOT EXISTS won't alter the existing table. Say what
    // that means and what to do about it, rather than surfacing the bare error: the op log is the source of
    // truth and it's still intact, so this is recoverable -- but only if you export before resetting.
    try
      Sql.query sql |> Sql.executeStatementSync
    with e ->
      print
        "schema.sql changed the SHAPE of a canonical table, which this bootstrap can't apply in place."
      print
        $"  Your {ops} op(s) are intact. Export them (`dark sync export <file>`), delete rundir/data.db,"
      print
        "  then start again and import. (A data-preserving migrator is SPEC section 10, not built.)"
      reraise ()
    markOpsUnapplied ()
    writeHash want
  | None ->
    // A store with no schema-hash stamp (fresh, or predates hash tracking): run schema.sql
    // (CREATE TABLE IF NOT EXISTS creates missing tables and no-ops existing ones), then stamp.
    Sql.query sql |> Sql.executeStatementSync
    writeHash want


// ---------------------
// Per-file incremental migrations (atop the schema.sql base)
// ---------------------
//
// Each file runs once, name-dedup'd via `system_migrations_v0`.
// schema.sql guarantees the table exists, so no separate init step.
// File naming convention: `YYYYMMDD_HHMMSS_<short-tag>.sql`.
// Currently EMPTY by design (the `incremental/` dir holds only README.md): the schema-hash bootstrap +
// the Release migrator cover today's needs. Kept as the seam for a future data-backfill/transform migration.

let private incrementalDir = "incremental"


let private alreadyRunMigrations () : List<string> =
  Sql.query "SELECT name from system_migrations_v0"
  |> Sql.execute (fun read -> read.string "name")
  |> Result.unwrap


let private runSystemMigration (name : string) (sql : string) : unit =
  // Use print instead of Rollbar to avoid serialization issues
  print $"Running migration: {name}"

  // Insert into the string because params don't work here for some reason.
  // On conflict, do nothing because another starting process might be running this migration as well.
  let recordMigrationStmt =
    "INSERT INTO system_migrations_v0
      (name, execution_date, sql)
    VALUES
      (@name, CURRENT_TIMESTAMP, @sql)
    ON CONFLICT(name) DO NOTHING"

  let recordMigrationParams = [ "name", Sql.string name; "sql", Sql.string sql ]

  match String.splitOnNewline sql with
  // allow special "pragma" to skip wrapping in a transaction
  // be VERY careful with this!
  | "--#[no_tx]" :: _ ->
    Sql.query sql |> Sql.executeStatementSync

    Sql.query recordMigrationStmt
    |> Sql.parameters recordMigrationParams
    |> Sql.executeStatementSync
  | _ ->
    let counts =
      Sql.executeTransactionSync
        [ sql, []; recordMigrationStmt, [ recordMigrationParams ] ]

    assertEq "recorded migrations" 1 counts[1]

    ()


let private allMigrations () : List<string> =
  // Get all SQL files under `incremental/`, sorted lexically. The
  // dir is optional — empty / missing means "nothing incremental
  // beyond schema.sql," which is the common case.
  try
    File.lsdir Config.Migrations incrementalDir
    |> List.filter (String.endsWith ".sql")
    |> List.sort
  with _ ->
    []


let private migrationsToRun () : List<string> =
  let alreadyRun = alreadyRunMigrations () |> Set
  allMigrations () |> List.filter (fun name -> not (Set.contains name alreadyRun))


let private runIncrementalMigrations () : unit =
  migrationsToRun ()
  |> List.iter (fun name ->
    let sql = File.readfile Config.Migrations $"{incrementalDir}/{name}"
    runSystemMigration name sql)


// ---------------------
// Entry point
// ---------------------

let run () : unit =
  runSchemaBootstrap ()
  // Release steps come after the bootstrap and before the incremental files. After, because they need the
  // tables to exist and a fresh store to have been given the current shape already; before, because an
  // incremental data migration may depend on a column a release step adds.
  LibDB.Releases.runPending ()
  runIncrementalMigrations ()
