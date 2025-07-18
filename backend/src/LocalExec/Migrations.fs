/// Our Sqlite DB migrations
///
/// Note that we don't use Tasks in here because these are expected to be run in
/// order, so it's easier to execute synchronously than have a bunch of code to use
/// tasks and then extra code to ensure the tasks are run synchronously.
///
/// CLEANUP maybe move this to LibDB?
module LocalExec.Migrations

open System.IO
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db
module File = LibCloud.File

module Telemetry = LibService.Telemetry
module Config = LibCloud.Config

open Prelude

let isInitialized () : bool =
  Sql.query
    "SELECT 1
      FROM sqlite_master
      WHERE type = 'table'
        AND name = 'system_migrations_v0'"
  |> Sql.executeExistsSync

let initializeMigrationsTable () : unit =
  Sql.query
    "CREATE TABLE IF NOT EXISTS
     system_migrations_v0
     ( name TEXT PRIMARY KEY
     , execution_date TEXT NOT NULL
     , sql TEXT NOT NULL)"
  |> Sql.executeStatementSync


let alreadyRunMigrations () : List<string> =
  Sql.query "SELECT name from system_migrations_v0"
  |> Sql.execute (fun read -> read.string "name")
  |> Result.unwrap

let runSystemMigration (name : string) (sql : string) : unit =
  use span = Telemetry.child "new migration" [ "name", name; "sql", sql ]
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
    // Execute both statements in a transaction
    let counts =
      Sql.executeTransactionSync
        [ sql, []; recordMigrationStmt, [ recordMigrationParams ] ]

    assertEq "recorded migrations" 1 counts[1]

    ()


let allMigrations () : List<string> =
  // Get all SQL files in the migrations directory
  File.lsdir Config.Migrations ""
  |> List.filter (String.endsWith ".sql")
  |> List.sort

let migrationsToRun () =
  let alreadyRun = alreadyRunMigrations () |> Set
  allMigrations () |> List.filter (fun name -> not (Set.contains name alreadyRun))

let run () : unit =
  if not (isInitialized ()) then initializeMigrationsTable ()

  migrationsToRun ()
  |> List.iter (fun name ->
    let sql = File.readfile Config.Migrations name
    runSystemMigration name sql)
