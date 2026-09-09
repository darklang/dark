/// SQLite schema bootstrap.
///
/// `backend/migrations/schema/*.sql` is the canonical from-scratch shape of every table, split by
/// subsystem and concatenated in FILENAME ORDER. We hash that concatenation and compare against
/// `schema_state_v0.hash`; if they differ (or the table is missing), drop the REGENERABLE
/// PROJECTION tables only (`Seed.projectionTables`) and replay. The canonical op log, blobs,
/// branches and commits survive, and the log re-folds into the fresh projections at startup -- so a
/// schema change costs CPU, not your authored work.
///
/// The corollary is the trap: because the canonical tables are NOT dropped, `CREATE TABLE IF NOT
/// EXISTS` no-ops on them, and a change to a canonical table's SHAPE (a new column, a new PK) NEVER
/// reaches an existing store from these files. That belongs in a step in `LibDB.Releases`, which is
/// the ONLY other mechanism -- there used to be a second one (`migrations/incremental/*.sql`) and
/// the rule for choosing between them was nowhere, which is how the same migration got written
/// twice and failed in both directions. A `.sql` file cannot look at the store before acting, and
/// every shape change has to.
///
/// So: `schema/` DECLARES the shape a new store is born with; `LibDB.Releases` is how any change
/// reaches a store that already exists.
///
/// Synchronous on purpose: the bootstrap and the release steps run strictly in order.
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

let private schemaDir = "schema"

/// Every schema file, concatenated in filename order.
///
/// Order is the contract: FK targets have to precede FK sources, across files as well as within
/// one, and the hash is taken over the whole string -- so splitting differently, or renaming a file
/// such that it sorts elsewhere, IS a schema change and the bootstrap treats it as one.
let private schemaSql () : string =
  File.lsdir Config.Migrations schemaDir
  |> List.filter (String.endsWith ".sql")
  |> List.sort
  |> List.map (fun name -> File.readfile Config.Migrations $"{schemaDir}/{name}")
  |> String.concat "\n"


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
  // the next connection (which replays the schema) gets the default back.
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
  let sql = schemaSql ()
  let want = computeHash sql

  match storedHash () with
  | Some have when have = want -> ()
  | Some have ->
    // Preserve-and-refold: drop only regenerable projections, replay the schema,
    // mark ops unapplied for the next `growIfNeeded`. Shape changes to canonical
    // tables can't take this path -- see the module doc.
    let ops = opCount ()
    print
      $"the schema changed (hash {have[0..7]} → {want[0..7]}); preserving {ops} op(s), \
        rebuilding projections."
    dropProjectionTables ()
    // A canonical-table shape change surfaces here as a raw SQLite error; say what
    // it means and how to recover instead of the bare error.
    try
      Sql.query sql |> Sql.executeStatementSync
    with e ->
      print
        "the schema changed the SHAPE of a canonical table, which this bootstrap can't apply in place."
      print
        $"  Your {ops} op(s) are intact. Export them (`dark sync export <file>`), delete rundir/data.db,"
      print
        "  then start again and import. (A data-preserving migrator is designed and not built.)"
      reraise ()
    markOpsUnapplied ()
    writeHash want
  | None ->
    // A store with no schema-hash stamp (fresh, or predates hash tracking): run the schema
    // (CREATE TABLE IF NOT EXISTS creates missing tables and no-ops existing ones), then stamp.
    Sql.query sql |> Sql.executeStatementSync
    writeHash want


// ---------------------
// Entry point
// ---------------------

let run () : unit =
  runSchemaBootstrap ()
  // Release steps come after the bootstrap, because they need the tables to exist and a fresh store
  // to have been given the current shape already -- every step is written to be a no-op against
  // exactly that.
  LibDB.Releases.runPending ()
