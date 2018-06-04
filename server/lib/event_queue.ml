open Core

open Types
open Types.RuntimeT

module FF = Feature_flag
module Dbp = Dbprim

type t = { id: int
         ; value: dval
         ; retries: int
         ; flag_context: feature_flag
         }

let status_to_enum status : string =
  match status with
  | `OK -> "'done'"
  | `Err -> "'error'"
  | `Incomplete -> "'error'"

let unlock_jobs (dequeuer: int) ~status : unit =
  Printf.sprintf
    "UPDATE \"events\"
     SET status = %s
     WHERE dequeued_by = %s
       AND status = 'locked'"
    (status_to_enum status)
    (Dbp.int dequeuer)
  |> Db.run_sql

(* ------------------------- *)
(* Public API *)
(* ------------------------- *)

let finalize (dequeuer: int) ~status : unit =
  unlock_jobs ~status dequeuer

let enqueue (state: exec_state) (space: string) (name: string) (data: dval) : unit =
  let column_names =
    [ "status"
    ; "dequeued_by"
    ; "canvas_id"
    ; "account_id"
    ; "space"
    ; "name"
    ; "value"
    ; "delay_until"
    ; "flag_context"
    ]
    |> String.concat ~sep:", "
  in
  let column_values =
    [ "'new'"
    ; "NULL"
    ; Dbp.uuid state.canvas_id
    ; Dbp.uuid state.account_id
    ; Dbp.string space
    ; Dbp.string name
    ; Dbp.dval data
    ; "CURRENT_TIMESTAMP"
    ; FF.to_sql state.ff
    ]
    |> String.concat ~sep:", "
  in
  (Printf.sprintf
     "INSERT INTO \"events\"
     (%s)
     VALUES (%s)"
     column_names column_values)
  |> Db.run_sql ~quiet:false

(* This should soon enough do something like:
 * https://github.com/chanks/que/blob/master/lib/que/sql.rb#L4
 * but multiple queries will do fine for now
 *)
let dequeue ~(canvas:Uuid.t) ~(account:Uuid.t) (execution_id: int) (space: string) (name: string) : t option =
  let fetched =
    Printf.sprintf
      "SELECT id, value, retries, flag_context from \"events\"
      WHERE space = %s
        AND name = %s
        AND status = 'new'
        AND delay_until < CURRENT_TIMESTAMP
        AND canvas_id = %s
        AND account_id = %s
      ORDER BY id DESC
             , retries ASC
      LIMIT 1"
      (Dbp.string space)
      (Dbp.string name)
      (Dbp.uuid canvas)
      (Dbp.uuid account)
    |> Db.fetch_via_sql
    |> List.hd
  in
  match fetched with
  | None -> None
  | Some [id; value; retries; flag_context] ->
    Db.run_sql
      (Printf.sprintf
         "UPDATE \"events\"
         SET status = 'locked'
           , dequeued_by = %s
         WHERE id = %s"
         (Dbp.int execution_id)
         id);
    Some { id = int_of_string id
         ; value = Dval.parse value
         ; retries = int_of_string retries
         ; flag_context = FF.from_sql flag_context
         }
  | Some s ->
    Exception.internal
      ("Fetched seemingly impossible shape from Postgres"
       ^ ("[" ^ (String.concat ~sep:", " s) ^ "]"))

let put_back (item: t) ~status : unit =
  let sql =
    match status with
    | `OK ->
      Printf.sprintf
        "UPDATE \"events\"
        SET status = 'done'
        WHERE id = %s"
        (Dbp.int item.id)
    | `Err ->
      if item.retries < 2
      then
        Printf.sprintf
          "UPDATE \"events\"
          SET status = 'new'
            , retries = %s
            , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
          WHERE id = %s"
        (Dbp.int (item.retries + 1))
        (Dbp.int item.id)
      else
        Printf.sprintf
          "UPDATE \"events\"
          SET status = 'error'
          WHERE id = %s"
          (Dbp.int item.id)
    | `Incomplete ->
      Printf.sprintf
        "UPDATE \"events\"
        SET status = 'new'
          , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
        WHERE id = %s"
        (Dbp.int item.id)
  in
  Db.run_sql sql

let finish (item: t) : unit =
  put_back ~status:`OK item



