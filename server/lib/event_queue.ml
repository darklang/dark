open Core

open Types
open Types.RuntimeT

(* --------------------------- *)
(* Awful Global State (public) *)
(* --------------------------- *)

let current_scope : string option ref =
  ref None

let set_scope (scope: string) : unit =
  current_scope := Some scope

let unset_scope () : unit =
  current_scope := None

(* ------------------------- *)
(* Public API *)
(* ------------------------- *)

let enqueue (space: string) (name: string) (data: dval) : unit =
  let scope =
    (match !current_scope with
     | Some sc -> sc
     | None -> Exception.internal "Missing Event_queue.current_scope! Time to ditch global mutable state!")
  in
  let serialized_data = Dval.dval_to_json_string data in
  let column_names =
    ["status"; "dequeued_by"; "canvas"; "space"; "name"; "value"]
    |> String.concat ~sep:", "
  in
  let column_values =
    let wrap s = "'" ^ s ^ "'" in
    ["'new'"; "NULL"; wrap scope; wrap space; wrap name; wrap serialized_data]
    |> String.concat ~sep:", "
  in
  (Printf.sprintf "INSERT INTO \"events\" (%s) VALUES (%s)" column_names column_values)
  |> Db.run_sql

let dequeue (space: string) (name: string) : dval =
  DIncomplete

(* ------------------------- *)
(* Some initialization *)
(* ------------------------- *)
let _ =
  let ensure_queue_status_type_exists =
    (* there's no CREATE TYPE IF NOT EXISTS :/ *)
    "DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'queue_status') THEN
        CREATE TYPE queue_status AS ENUM ('new', 'locked', 'done', 'error');
    END IF;
END$$;" in
  let ensure_queue_table_exists =
    "CREATE TABLE IF NOT EXISTS \"events\" (id SERIAL PRIMARY KEY, status queue_status, dequeued_by INT, canvas TEXT NOT NULL, space TEXT NOT NULL, name TEXT NOT NULL, value TEXT NOT NULL)"
  in
  let ensure_dequeue_index_exists =
    "CREATE INDEX IF NOT EXISTS \"idx_dequeue\" ON \"events\" (space, name, canvas, status, id)"
  in
  let ensure_cleanup_index_exists =
    "CREATE INDEX IF NOT EXISTS \"idx_cleanup\" ON \"events\" (dequeued_by)"
  in
  Db.run_sql ensure_queue_status_type_exists;
  Db.run_sql ensure_queue_table_exists;
  Db.run_sql ensure_dequeue_index_exists;
  Db.run_sql ensure_cleanup_index_exists

