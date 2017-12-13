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
  |> List.map ~f:(fun (db: db) -> (db.display_name, DDB db))
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

let fetch_via_sql (sql: string) : string list list =
  sql
  |> Log.pp "sql"
  |> conn#exec ~expect:PG.[Tuples_ok]
  |> (fun res -> res#get_all_lst)

let with_postgres fn =
  try
    fn ()
  with
  | PG.Error e ->
    Exception.internal ("DB error with: " ^ (PG.string_of_error e))

let key_names (vals: dval_map) : string =
  vals
  |> DvalMap.keys
  |> String.concat ~sep:", "

let val_names (vals: dval_map) : string =
  vals
  |> DvalMap.data
  |> List.map ~f:Dval.dval_to_sql
  |> String.concat ~sep:", "

(* Turn db rows into list of string/type pairs - removes elements with
 * holes, as they won't have been put in the DB yet *)
let cols_for (db: db) : (string * tipe) list =
  db.cols
  |> List.filter_map ~f:(fun c ->
    match c with
    | Full name, Full tipe ->
      Some (name, tipe)
    | _ ->
      None)
  |> fun l -> ("id", TID) :: l

(* PG returns lists of strings. This converts them to types using the
 * row info provided *)
let to_obj (names : string list) (types: tipe list) (db_strings : string list)
  : dval =
  db_strings
  |> List.map2_exn ~f:Dval.sql_to_dval types
  |> List.zip_exn names
  |> Dval.to_dobj


let insert (db: db) (vals: dval_map) : unit =
  let vals = DvalMap.add ~key:"id" ~data:(DInt (Util.create_id ())) vals
  in Printf.sprintf "INSERT into \"%s\" (%s) VALUES (%s)"
       db.actual_name (key_names vals) (val_names vals)
     |> run_sql


let fetch_all (db: db) : dval =
  let (names, types) = cols_for db |> List.unzip in
  let colnames = names |> String.concat ~sep:", " in
  Printf.sprintf
    "SELECT %s FROM \"%s\""
    colnames db.actual_name
  |> fetch_via_sql
  |> List.map ~f:(to_obj names types)
  |> DList


let fetch_by db (col: string) (dv: dval) : dval =
  let (names, types) = cols_for db |> List.unzip in
  let colnames = names |> String.concat ~sep:", " in
  Printf.sprintf
    "SELECT (%s) FROM \"%s\" WHERE %s = %s"
    colnames db.actual_name col (Dval.dval_to_sql dv)
  |> fetch_via_sql
  |> List.map ~f:(to_obj names types)
  |> DList

let delete db (vals: dval_map) =
  let id = DvalMap.find_exn vals "id" in
  Printf.sprintf "DELETE FROM \"%s\" WHERE id = %s"
    db.actual_name (Dval.dval_to_sql id)
  |> run_sql

let update db (vals: dval_map) =
  let id = DvalMap.find_exn vals "id" in
  let sets = vals
           |> DvalMap.to_alist
           |> List.map ~f:(fun (k,v) ->
               k ^ " = " ^ Dval.dval_to_sql v)
           |> String.concat ~sep:", " in
  Printf.sprintf "UPDATE \"%s\" SET %s WHERE id = %s"
    db.actual_name sets (Dval.dval_to_sql id)
  |> run_sql

(* ------------------------- *)
(* run all db and schema changes as migrations *)
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

let create_table_sql (table_name: string) =
  Printf.sprintf
    "CREATE TABLE IF NOT EXISTS \"%s\" (id SERIAL PRIMARY KEY)"
    table_name

let add_col_sql (table_name: string) (name: string) (tipe: tipe) : string =
  Printf.sprintf
    "ALTER TABLE \"%s\" ADD COLUMN %s %s"
    table_name name (Dval.sql_tipe_for tipe)



(* ------------------------- *)
(* DB schema *)
(* ------------------------- *)

let create_new_db (tlid: tlid) (db: db) =
  run_migration tlid (create_table_sql db.actual_name)

(* we only add this when it is complete, and we use the ID to mark the
   migration table to know whether it's been done before. *)
let maybe_add_to_actual_db (db: db) (id: id) (col: col) : col =
  (match col with
  | Full name, Full tipe ->
    run_migration id (add_col_sql db.actual_name name tipe)
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
