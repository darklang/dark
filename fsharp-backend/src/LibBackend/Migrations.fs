module LibBackend.Migrations


// Migrations

// Note that we don't use Tasks in here because these are expected to be run in
// order, so easier to execute synchronously than have a bunch of code to use
// tasks and then extra code to ensure the tasks are run synchronously.

open Npgsql
open Npgsql.FSharp.Tasks
open Db

open Prelude
open Prelude.Tablecloth

let isInitialized () : bool =
  Sql.query
    "SELECT 1
    FROM pg_class
    WHERE relname = 'system_migrations'"
  |> Sql.executeExists

let initializeMigrationsTable () : unit =
  Sql.query
    "CREATE TABLE IF NOT EXISTS
     system_migrations
     ( name TEXT PRIMARY KEY
     , execution_date TIMESTAMPTZ NOT NULL
     , sql TEXT NOT NULL)"
  |> Sql.executeStatement

let isAlreadyRun (name : string) : bool =
  Sql.query
    "SELECT 1 from system_migrations
     WHERE name = @name"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeExists


let runSystemMigration (name : string) (sql : string) : unit =

  // Insert into the string because params don't work here for some reason.
  // On conflict, do nothing because another starting process might be running this migration as well.
  let doneStmt = "INSERT INTO system_migrations
                  (name, execution_date, sql)
                  VALUES
                  (@name, CURRENT_TIMESTAMP, @sql)
                  ON CONFLICT DO NOTHING"
  let doneParams = [ "name", Sql.string "name"; "sql", Sql.string sql ]

  match String.splitOnNewline sql with
  (* allow special "pragma" to skip wrapping in a transaction *)
  (* be VERY careful with this! *)
  | "--#[no_tx]" :: _ ->
      Sql.query sql |> Sql.executeStatement
      Sql.query doneStmt |> Sql.parameters doneParams |> Sql.executeStatement
  | _ ->
      let _ =
        Db.connect () |> Sql.executeTransaction [ sql, []; doneStmt, [ doneParams ] ]

      ()


let names () : List<string> =
  File.lsdir Config.Migrations ""
  (* endsWith sql filters out, say, the .swp files vim sometimes leaves behind
   * *)
  |> List.filter (String.endsWith ".sql")
  |> List.sort


let run () : unit =
  if (not (isInitialized ())) then initializeMigrationsTable ()

  let migrations = names ()
  // FSTODO migration 11 doesn't work with npgsql
  printfn $"migrarions: {migrations}"

  List.iter
    (fun name ->
      if isAlreadyRun name then
        () // FSTODO Log.infO "migration already run" name
      else
        printfn $"new migration: {name}"
        // FSTODO Log.infO "new migration" name ;
        let sql = File.readfile Config.Migrations name
        runSystemMigration name sql)
    migrations


(* ------------------------- *)
(* Initialization *)
(* ------------------------- *)

let init () : unit = run ()
