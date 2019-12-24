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


let is_already_run name : bool =
  Db.exists
    ~name:"migration.is_already_run"
    "SELECT 1 from system_migrations
     WHERE name = $1"
    ~params:[String name]


let run_system_migration (name : string) (sql : string) : unit =
  let done_sql =
    (* use printf since passing params seems not to work *)
    Printf.sprintf
      "INSERT INTO system_migrations (name, execution_date, sql)
       VALUES (%s, CURRENT_TIMESTAMP, %s)
       ON CONFLICT DO NOTHING"
      (Db.escape (String name))
      (Db.escape (String sql))
  in
  match String.split_lines sql with
  (* allow special "pragma" to skip wrapping in a transaction *)
  (* be VERY careful with this! *)
  | "--#[no_tx]" :: _ ->
      Db.run ~params:[] sql ~name:"run_system_migration" ;
      Db.run ~params:[] done_sql ~name:"run_system_migration"
  | _ ->
      Db.run
        ~params:[]
        ~name:"run_system_migration"
        (Printf.sprintf
           "DO
           $do$
             BEGIN
               %s;
               %s;
             END
           $do$"
           sql
           done_sql)


let names () = File.lsdir ~root:Migrations "" |> List.sort ~compare

let run () : unit =
  if not (is_initialized ()) then initialize_migrations_table () ;
  let migrations = names () in
  List.iter migrations ~f:(fun name ->
      if is_already_run name
      then Log.infO "migration already run" ~data:name
      else (
        Log.infO "new migration" ~data:name ;
        let sql = File.readfile ~root:Migrations name in
        run_system_migration name sql )) ;
  ()


(* ------------------------- *)
(* Initialization *)
(* ------------------------- *)

let init () : unit = run ()
