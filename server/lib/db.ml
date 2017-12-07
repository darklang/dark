open Core

open Runtime
open Types

module PG = Postgresql

module RT = Runtime


type row = string or_hole * string or_hole
           [@@deriving eq, show, yojson]

type db = { tlid: tlid
          ; name: string
          ; rows: row list
          } [@@deriving eq, show, yojson]


(* ------------------------- *)
(* frontend stuff *)
(* ------------------------- *)
let dbs_as_env (dbs: db list) : RT.dval_map =
  dbs
  |> List.map ~f:(fun db -> (db.name, RT.DOpaque (new RT.opaque db.name)))
  |> RT.DvalMap.of_alist_exn

let dbs_as_exe_env (dbs: db list) : RT.dval_map =
  dbs_as_env dbs

(* ------------------------- *)
(* actual DB stuff *)
(* ------------------------- *)

let conn =
  new PG.connection ~host:"localhost" ~dbname:"proddb" ~user:"dark" ~password:"eapnsdc" ()

let run_sql (sql: string) : unit =
  Log.pP "sql" sql;
  ignore (conn#exec ~expect:[PG.Command_ok] sql)

let with_postgres (table: opaque) fn =
  try
     let t = table#get in
     fn t
   with
   | PG.Error e ->
     Exception.internal ("DB error with: " ^ (PG.string_of_error e))

let dval2sql (dv: dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> s
  | DFloat f -> string_of_float f
  | DChar c -> Char.to_string c
  | DNull -> "null"
  | _ -> Exception.client "Not obvious how to persist this in the DB"


let insert (table: string) (vals: dval list) : unit =
  vals
  |> List.map ~f:dval2sql
  |> String.concat ~sep:"\", \""
  |> Printf.sprintf "INSERT into \"%s\" VALUES (NULL, \"%s\")" table
  |> run_sql

let fetch_all (table: string) : dval =
  Printf.sprintf
    "SELECT * FROM \"%s\""
    table
  |> Log.pp "sql"
  |> conn#exec
  |> (fun res -> res#get_all_lst)
  |> List.map ~f:(fun row ->
      match row with
      | [key; value] -> (key, value |> parse)
      | l -> Exception.internal ("Expected key,value list, got: " ^
                                 (String.concat ~sep:", " l)))
  |> to_dobj



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

let sql_tipe_for (tipe: string) : string =
  match String.lowercase tipe with
  | "string" -> "text"
  | "title" -> "text"
  | "text" -> "text"
  | "url" -> "text"
  | "date" -> "timestamp with time zone"
  | _ -> failwith ("No tipe for " ^ tipe)


let add_row_sql (table: string) (name: string) (tipe: string) : string =
  Printf.sprintf
    "ALTER TABLE \"%s\" ADD COLUMN %s %s"
    table name (sql_tipe_for tipe)



(* ------------------------- *)
(* DB schema *)
(* ------------------------- *)

let create_new_db (tlid: tlid) (name: string) =
  run_migration tlid (create_table_sql name)

(* we only add this when it is complete, and we use the ID to mark the
   migration table to know whether it's been done before. *)
let maybe_add_to_actual_db (db: db) (id: id) (row: row) : row =
  (match row with
  | Full name, Full tipe ->
    run_migration id (add_row_sql db.name name tipe)
  | _ ->
    ());
  row


let add_db_row rowid typeid (db: db) =
  { db with rows = db.rows @ [(Empty rowid, Empty typeid)]}

let set_row_name id name db =
  let set row =
    match row with
    | (Empty hid, tipe) when hid = id -> maybe_add_to_actual_db db id (Full name, tipe)
    | _ -> row in
  { db with rows = List.map ~f:set db.rows }

let set_db_row_type id tipe db =
  let set row =
    match row with
    | (name, Empty hid) when hid = id -> maybe_add_to_actual_db db id (name, Full tipe)
    | _ -> row in
  { db with rows = List.map ~f:set db.rows }





(* ------------------------- *)
(* Some initialization *)
(* ------------------------- *)
let _ =
  run_sql "CREATE TABLE IF NOT EXISTS \"migrations\" (id INT PRIMARY KEY)"
