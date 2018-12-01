open Core_kernel
open Libcommon

open Libexecution

let is_initialized () =
  Db.exists
    ~name:"migrations_initialized"
    "SELECT 1
    FROM pg_class
    WHERE relname = 'system_migrations';"
    ~params:[]

let initialize_migrations_table () =
  Db.run
    ~name:"initialize_migrations_table"
    "CREATE TABLE IF NOT EXISTS
               system_migrations
               ( name TEXT PRIMARY KEY
               , execution_date TIMESTAMPTZ NOT NULL
               , sql TEXT NOT NULL)"
    ~params:[]

let is_already_run (name) : bool =
  Db.exists
    ~name:"migration.is_already_run"
    "SELECT 1 from system_migrations
     WHERE name = $1"
    ~params:[String name]


let run_system_migration (name: string) (sql:string) : unit =
  (* use printf since passing params seems not to work *)
  let fullsql =
    Printf.sprintf
      "DO
         $do$
           BEGIN
             IF ((SELECT COUNT(*)
                  FROM system_migrations
                  WHERE name = %s) = 0)
             THEN
               %s;
               INSERT INTO system_migrations
               (name, execution_date, sql)
               VALUES
               (%s, CURRENT_TIMESTAMP, %s);
             END IF;
           END
         $do$"
      (Db.escape (String name))
      sql
      (Db.escape (String name))
      (Db.escape (String sql))
  in
  Db.run ~params:[] fullsql ~name:"run_system_migration"

let names () =
  File.lsdir ~root:Migrations ""
  |> List.sort ~compare

let run () : unit =
  if not (is_initialized ())
  then initialize_migrations_table ();

  let migrations = names () in
  List.iter migrations
    ~f:(fun name ->
        if is_already_run name
        then
          Log.infO "migration already run" ~data:name
        else
          (Log.infO "new migration" ~data:name;
           let sql = File.readfile ~root:Migrations name in
           run_system_migration name sql
          ));

  ()

(* ------------------------- *)
(* Initialization *)
(* ------------------------- *)

let init () : unit  =
  run ()

