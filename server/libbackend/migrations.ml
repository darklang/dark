open Core_kernel

open Libexecution
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
      (Dbp.string name)
      sql
      (Dbp.string name)
      (Dbp.sql sql)
  in
  Db.run_sql2 ~params:[] fullsql ~name:"run_system_migration"

let names () =
  File.lsdir ~root:Migrations ""
  |> List.sort ~cmp:compare

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

let move_json_oplist_into_db () : unit =
  let hosts = Serialize.json_file_hosts () in
  List.iter hosts
    ~f:(fun host ->
        try
          let ops =
            (try
              (Serialize.load_json_from_disk ~root:Appdata host)
            with e ->
              (Serialize.load_deprecated_undo_json_from_disk
                         ~root:Appdata host))
          in
          match ops with
          | None ->
            Log.erroR "Found a host but couldn't load it" ~data:host;
          | Some o ->
            Serialize.save_json_to_db host o;
            let filename = Serialize.json_unversioned_filename host in
            File.rm ~root:Appdata filename
        with e ->
          Log.erroR "Found an error while moving the oplist into the DB"
            ~params:["host", host; "exn", Exn.to_string e])




(* ------------------------- *)
(* Initialization *)
(* ------------------------- *)

let init () : unit  =
  if Config.postgres_settings.dbname <> "testdb"
  then move_json_oplist_into_db ();
  run ()

