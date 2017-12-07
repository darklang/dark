open Core

open Runtime
open Types

module PG = Postgresql


type row = string or_hole * string or_hole
           [@@deriving eq, show, yojson]

type db = { tlid: tlid
          ; name: string
          ; rows: row list
          } [@@deriving eq, show, yojson]


(* ------------------------- *)
(* actual DB stuff *)
(* ------------------------- *)

let conn =
  new PG.connection ~host:"localhost" ~dbname:"proddb" ~user:"dark" ~password:"eapnsdc" ()

let _run_sql (sql: string) : unit =
  ignore (conn#exec ~expect:[PG.Command_ok] sql)

(* ------------------------- *)
(* run all table and schema changes as migrations *)
(* ------------------------- *)
let run_sql (migration_id: id) (sql:string) : unit =
  let id = string_of_int migration_id in
  Log.pP "sql" sql;
  let sql =
    [ "DO"
    ; "$do$"
    ; "BEGIN"
    ; "IF ((SELECT COUNT(*) FROM migrations WHERE id = " ^ id ^ ") = 0) "
    ; "THEN " ^ sql ^ "; INSERT INTO migrations (id) VALUES (" ^ id ^ ");"
    ; "END IF;"
    ; "END"
    ; "$do$;"
    ; "COMMIT;"
    ]
  |> String.concat ~sep:"\n"
  in
  _run_sql sql

(* -------------------------
(* SQL for DB *)
 * TODO: all of the SQL here is very very easily SQL injectable.
 * This MUST be fixed before we go to production
 * ------------------------- *)

let create_table_sql (table: string) =
  "CREATE TABLE IF NOT EXISTS \"" ^ table ^ "\" (id INT)"

let sql_tipe_for (tipe: string) : string =
  match String.lowercase tipe with
  | "string" -> "text"
  | "title" -> "text"
  | "text" -> "text"
  | "url" -> "text"
  | "date" -> "timestamp with time zone"
  | _ -> failwith ("No tipe for " ^ tipe)


let add_row_sql (table: string) (name: string) (tipe: string) : string =
  let sql_tipe = sql_tipe_for tipe in
  "ALTER TABLE \"" ^ table ^ "\" ADD COLUMN " ^ name ^ " " ^ sql_tipe



(* ------------------------- *)
(* DB schema *)
(* ------------------------- *)

let create_new_db (tlid: tlid) (name: string) =
  run_sql tlid (create_table_sql name)

(* we only add this when it is complete, and we use the ID to mark the
   migration table to know whether it's been done before. *)
let maybe_add_to_actual_db (db: db) (id: id) (row: row) : row =
  (match row with
  | Full name, Full tipe ->
    run_sql id (add_row_sql db.name name tipe)
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


