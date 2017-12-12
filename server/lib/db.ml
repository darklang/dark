open Core

open Types
open Types.DbT
open Types.RuntimeT

module RT = Runtime

module PG = Postgresql


(* ------------------------- *)
(* frontend stuff *)
(* ------------------------- *)
let dbs_as_env (dbs: db list) : dval_map =
  dbs
  |> List.map ~f:(fun (db: db) -> (db.name, DDB db))
  |> DvalMap.of_alist_exn

let dbs_as_exe_env (dbs: db list) : dval_map =
  dbs_as_env dbs

(* ------------------------- *)
(* actual DB stuff *)
(* ------------------------- *)

let conn =
  new PG.connection ~host:"localhost" ~dbname:"proddb" ~user:"dark" ~password:"eapnsdc" ()

let run_sql (sql: string) : unit =
  Log.pP "sql" sql ~stop:10000;
  ignore (conn#exec ~expect:[PG.Command_ok] sql)

let with_postgres fn =
  try
    fn ()
  with
  | PG.Error e ->
    Exception.internal ("DB error with: " ^ (PG.string_of_error e))


let insert (table: db) (vals: dval_map) : unit =
  let vals = DvalMap.add ~key:"id" ~data:(DInt (Util.create_id ())) vals in
  let names = vals
              |> DvalMap.keys
              |> String.concat ~sep:", "
  in vals
     |> DvalMap.data
     |> List.map ~f:Dval.dval_to_sql
     |> String.concat ~sep:", "
     |> Printf.sprintf "INSERT into \"%s\" (%s) VALUES (%s)"
       table.name names
     |> run_sql


let fetch_all (table: db) : dval =
  let names = table.cols
              |> List.map ~f:Tuple.T2.get1
              |> List.filter_map ~f: hole_to_maybe
              |> (@) ["id"]
  in
  let types = table.cols
              |> List.map ~f:Tuple.T2.get2
              |> List.filter_map ~f: hole_to_maybe
              |> (@) [TID]
  in
  let colnames = names |> String.concat ~sep:", " in
  Printf.sprintf
    "SELECT %s FROM \"%s\""
    colnames table.name
  |> Log.pp "sql"
  |> conn#exec ~expect:PG.[Tuples_ok]
  |> (fun res -> res#get_all_lst)
  |> List.map ~f:(List.map2_exn ~f:Dval.sql_to_dval types)
  |> List.map ~f:(List.zip_exn names)
  |> List.map ~f:(Dval.to_dobj)
  |> DList


let fetch_by table col value =
  let names = table.cols
              |> List.map ~f:Tuple.T2.get1
              |> List.filter_map ~f: hole_to_maybe
              |> (@) ["id"]
  in
  let types = table.cols
              |> List.map ~f:Tuple.T2.get2
              |> List.filter_map ~f: hole_to_maybe
              |> (@) [TID]
  in
  let colnames = names |> String.concat ~sep:", " in
  Printf.sprintf
    "SELECT (%s) FROM \"%s\" WHERE %s = %s"
    colnames table.name col (Dval.dval_to_sql value)
  |> Log.pp "sql"
  |> conn#exec
  |> (fun res -> res#get_all_lst)
  |> List.map ~f:(List.map2_exn ~f:Dval.sql_to_dval types)
  |> List.map ~f:(List.zip_exn names)
  |> List.map ~f:(Dval.to_dobj)
  |> DList

let delete db value =
  ()

let update db value =
  ()

let keys db =
  DList []

(* ------------------------- *)
(* run all table and schema changes as migrations *)
(* ------------------------- *)
let run_migration (migration_id: id) (sql:string) : unit =
  Log.pP "sql" sql;
  Printf.sprintf
    "DO
       $do$
         BEGIN
           IF ((SELECT COUNT(*) FROM migrations WHERE id = %d) = 0)
           THEN
             %s;
             INSERT INTO migrations (id) VALUES (%d);
           END IF;
         END
       $do$;
     COMMIT;" migration_id sql migration_id
  |> run_sql

(* -------------------------
(* SQL for DB *)
 * TODO: all of the SQL here is very very easily SQL injectable.
 * This MUST be fixed before we go to production
 * ------------------------- *)

let create_table_sql (table: string) =
  Printf.sprintf
    "CREATE TABLE IF NOT EXISTS \"%s\" (id SERIAL PRIMARY KEY)"
    table

let add_col_sql (table: string) (name: string) (tipe: tipe) : string =
  Printf.sprintf
    "ALTER TABLE \"%s\" ADD COLUMN %s %s"
    table name (Dval.sql_tipe_for tipe)



(* ------------------------- *)
(* DB schema *)
(* ------------------------- *)

let create_new_db (tlid: tlid) (name: string) =
  run_migration tlid (create_table_sql name)

(* we only add this when it is complete, and we use the ID to mark the
   migration table to know whether it's been done before. *)
let maybe_add_to_actual_db (db: db) (id: id) (col: col) : col =
  (match col with
  | Full name, Full tipe ->
    run_migration id (add_col_sql db.name name tipe)
  | _ ->
    ());
  col


let add_db_col colid typeid (db: db) =
  { db with cols = db.cols @ [(Empty colid, Empty typeid)]}

let set_col_name id name db =
  let set col =
    match col with
    | (Empty hid, tipe) when hid = id -> maybe_add_to_actual_db db id (Full name, tipe)
    | _ -> col in
  { db with cols = List.map ~f:set db.cols }

let set_db_col_type id tipe db =
  let set col =
    match col with
    | (name, Empty hid) when hid = id -> maybe_add_to_actual_db db id (name, Full tipe)
    | _ -> col in
  { db with cols = List.map ~f:set db.cols }





(* ------------------------- *)
(* Some initialization *)
(* ------------------------- *)
let _ =
  run_sql "CREATE TABLE IF NOT EXISTS \"migrations\" (id INT PRIMARY KEY)"
