/// SQLite schema bootstrap.
///
/// `backend/migrations/schema/*.sql` is the shape of every table, concatenated in FILENAME ORDER. We
/// hash that concatenation and compare against `schema_state_v0.hash`; if they differ (or the table
/// is missing), drop the REGENERABLE PROJECTION tables only (`Seed.projectionTables`) and replay. The
/// canonical op log, blobs, branches and commits survive, and the log re-folds into the fresh
/// projections at startup -- so a schema change to a projection costs CPU, not your authored work.
///
/// A change to a canonical table's shape is not carried: `CREATE TABLE IF NOT EXISTS` leaves the old
/// table as it is, so the store is refused and has to be made again.
module LocalExec.Migrations

open System.IO
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Sqlite
module File = LibConfig.File
module Config = LibConfig.Dirs

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
  |> Sql.executeExists


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
  Sql.query "PRAGMA foreign_keys = OFF" |> Sql.executeStatement
  for t in LibDB.Seed.projectionTables do
    Sql.query (sprintf "DROP TABLE IF EXISTS \"%s\"" t) |> Sql.executeStatement

/// Mark every op unapplied so the next `Seed.growIfNeeded` re-folds the whole log into the freshly
/// recreated projections. Re-folding (with value evaluation) needs the runtime, which the migration
/// phase doesn't have — so we defer the fold to startup, exactly like a fresh seed does.
let private markOpsUnapplied () : unit =
  if tableExists "package_ops_applied_through" then
    Sql.query LibDB.AppliedOps.reset |> Sql.executeStatement

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
  |> Sql.executeStatement
  Sql.query "INSERT OR REPLACE INTO schema_state_v0 (id, hash) VALUES (0, @hash)"
  |> Sql.parameters [ "hash", Sql.string hash ]
  |> Sql.executeStatement


/// Stop at a canonical table whose shape the schema changed: it cannot be carried in place.
let private refuseMismatch (sql : string) (ops : int) : unit =
  match LibDB.StoreSchema.mismatch sql with
  | None -> ()
  | Some reason ->
    print $"this store's tables don't match the schema: {reason}."
    print
      $"  Delete rundir/data.db and build again; its {ops} op(s) are not carried across."
    Exception.raiseInternal "store schema mismatch" [ "reason", reason ]

let private runSchemaBootstrap (sql : string) : unit =
  let want = computeHash sql

  match storedHash () with
  | Some have when have = want -> ()
  | Some have ->
    // Preserve-and-refold: drop only regenerable projections, replay the schema,
    // mark ops unapplied for the next `growIfNeeded`.
    let ops = opCount ()
    print
      $"the schema changed (hash {have[0..7]} → {want[0..7]}); preserving {ops} op(s), \
        rebuilding projections."
    dropProjectionTables ()
    refuseMismatch sql ops
    LibDB.StoreSchema.apply sql
    markOpsUnapplied ()
    writeHash want
  | None ->
    // A store with no schema-hash stamp: create the tables, then stamp.
    refuseMismatch sql (opCount ())
    LibDB.StoreSchema.apply sql
    writeHash want


// ---------------------
// Entry point
// ---------------------

let run () : unit = runSchemaBootstrap (schemaSql ())
