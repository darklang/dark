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

let enqueue (scope: string) (space: string) (name: string) (data: dval) : unit =
  ()

let dequeue (scope: string) (space: string) (name: string) : dval =
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
    "CREATE TABLE IF NOT EXISTS \"events\" (id SERIAL PRIMARY KEY, status queue_status, canvas TEXT NOT NULL, space TEXT NOT NULL, name TEXT NOT NULL, value TEXT NOT NULL)"
  in
  let ensure_dequeue_index_exists =
    "CREATE INDEX IF NOT EXISTS \"idx_dequeue\" ON \"events\" (space, name, canvas, status, id)"
  in
  Db.run_sql ensure_queue_status_type_exists;
  Db.run_sql ensure_queue_table_exists;
  Db.run_sql ensure_dequeue_index_exists

