open Core

module Dbp = Dbprim

let is_initialized () =
  let sql = "SELECT 1
             FROM pg_class
             WHERE relname = 'system_migrations';"
  in
  sql
  |> Db.exists_via_sql ~quiet:true

let initialize_migrations_table () =
  let sql = "CREATE TABLE IF NOT EXISTS
             system_migrations
             ( name TEXT PRIMARY KEY
             , execution_date TIMESTAMPTZ NOT NULL
             , sql TEXT NOT NULL)"
  in
  sql
  |> Db.run_sql ~quiet:false

let is_already_run (name) : bool =
  Printf.sprintf
    "SELECT 1 from system_migrations
     WHERE name = %s"
    (Dbp.string name)
  |> Db.exists_via_sql ~quiet:true


let run_system_migration (name: string) (sql:string) : unit =
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
    (Dbp.string name)
    sql
    (Dbp.string name)
    (Dbp.sql sql)

  |> Db.run_sql ~quiet:true

let names () =
  Util.lsdir ~root:Migrations ""
  |> List.sort ~cmp:compare

let run () : unit =
  if not (is_initialized ())
  then initialize_migrations_table ();

  let migrations = names () in
  List.iter migrations
    ~f:(fun name ->
        if is_already_run name
        then
          Log.infO "migration already run" name
        else
          (Log.infO "new migration" name;
           let sql = Util.readfile ~root:Migrations name in
           run_system_migration name sql
          ));

  ()



(* ------------------------- *)
(* Initialization *)
(* ------------------------- *)

let init () : unit  =
  run ()

