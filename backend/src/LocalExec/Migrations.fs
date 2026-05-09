/// SQLite schema bootstrap.
///
/// One `schema.sql` under `backend/migrations/`. We hash the file's
/// bytes and compare against `schema_state_v0.hash` in the DB; if
/// they differ (or the table is missing), drop every non-system
/// table and replay the file fresh. Per the project memory note
/// on this branch — kill-and-fill is OK; the seed DB is regenerated
/// by the build pipeline, and dev DBs are reproducible from source.
///
/// The old per-file iteration + `system_migrations_v0` name-dedup is
/// gone. A one-time adapter still recognizes pre-cutover DBs whose
/// schema matches the cat-of-13 (`system_migrations_v0` exists,
/// `schema_state_v0` doesn't) and stamps the current hash without
/// dropping data — that path can be deleted once everyone's
/// upgraded.
module LocalExec.Migrations

open System.IO
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Sqlite
module File = LibCloud.File
module Config = LibCloud.Config

open Prelude


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


let private dropAllUserTables () : unit =
  let userTables =
    Sql.query
      "SELECT name FROM sqlite_master
       WHERE type = 'table'
         AND name NOT LIKE 'sqlite_%'
         AND name <> 'schema_state_v0'"
    |> Sql.execute (fun read -> read.string "name")
    |> Result.unwrap
  for t in userTables do
    Sql.query (sprintf "DROP TABLE IF EXISTS \"%s\"" t) |> Sql.executeStatementSync


let private writeHash (hash : string) : unit =
  Sql.query
    "CREATE TABLE IF NOT EXISTS schema_state_v0
     (id INTEGER PRIMARY KEY, hash TEXT NOT NULL)"
  |> Sql.executeStatementSync
  Sql.query "INSERT OR REPLACE INTO schema_state_v0 (id, hash) VALUES (0, @hash)"
  |> Sql.parameters [ "hash", Sql.string hash ]
  |> Sql.executeStatementSync


/// Pre-cutover DBs were migrated by name through `system_migrations_v0`.
/// If we see one with the full set of old migration names already
/// applied, treat it as fully-migrated under the new flow — write the
/// current schema hash so subsequent runs see "up to date" and don't
/// kill-and-fill. Drop this once nobody's pulling pre-2026-05-08 main.
let private adoptLegacyDB (currentHash : string) : bool =
  if not (tableExists "system_migrations_v0") then
    false
  else
    let count =
      match
        Sql.query "SELECT COUNT(*) AS c FROM system_migrations_v0"
        |> Sql.execute (fun read -> read.int "c")
      with
      | Ok [ c ] -> c
      | _ -> 0
    if count >= 13 then
      print
        $"Adopting pre-cutover DB ({count} migrations on record). \
          Stamping schema hash; no data dropped."
      writeHash currentHash
      true
    else
      false


let run () : unit =
  let sql = File.readfile Config.Migrations schemaFile
  let want = computeHash sql

  match storedHash () with
  | Some have when have = want -> ()
  | Some have ->
    print
      $"schema.sql changed (hash {have[0..7]} → {want[0..7]}); \
        kill-and-fill."
    dropAllUserTables ()
    Sql.query sql |> Sql.executeStatementSync
    writeHash want
  | None ->
    if adoptLegacyDB want then
      ()
    else
      Sql.query sql |> Sql.executeStatementSync
      writeHash want
