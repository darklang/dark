open Core

open Types
open Types.RuntimeT

module FF = Feature_flag

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
    (string_of_int dequeuer)
  |> Db.run_sql

(* ------------------------- *)
(* Public API *)
(* ------------------------- *)

let wrap s = "'" ^ s ^ "'"

let finalize (dequeuer: int) ~status : unit =
  unlock_jobs ~status dequeuer

let enqueue (state: exec_state) (space: string) (name: string) (data: dval) : unit =
  let serialized_data = Dval.dval_to_json_string data in
  let column_names =
    [ "status"
    ; "dequeued_by"
    ; "canvas"
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
    ; wrap state.host
    ; wrap space
    ; wrap name
    ; wrap serialized_data
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
  |> Db.run_sql

(* This should soon enough do something like:
 * https://github.com/chanks/que/blob/master/lib/que/sql.rb#L4
 * but multiple queries will do fine for now
 *)
let dequeue (execution_id: int) (host: string) (space: string) (name: string) : t option =
  let fetched =
    Printf.sprintf
      "SELECT id, value, retries, flag_context from \"events\"
      WHERE space = %s
        AND name = %s
        AND canvas = %s
        AND status = 'new'
        AND delay_until < CURRENT_TIMESTAMP
      ORDER BY id DESC
             , retries ASC
      LIMIT 1"
      (wrap space)
      (wrap name)
      (wrap host)
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
         (string_of_int execution_id)
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
  let id = string_of_int item.id in
  let sql =
    match status with
    | `OK ->
      Printf.sprintf
        "UPDATE \"events\"
        SET status = 'done'
        WHERE id = %s"
        id
    | `Err ->
      if item.retries < 2
      then
        Printf.sprintf
          "UPDATE \"events\"
          SET status = 'new'
            , retries = %s
            , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
          WHERE id = %s"
        (string_of_int (item.retries + 1))
        id
      else
        Printf.sprintf
          "UPDATE \"events\"
          SET status = 'error'
          WHERE id = %s"
          id
    | `Incomplete ->
      Printf.sprintf
        "UPDATE \"events\"
        SET status = 'new'
          , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
        WHERE id = %s" id
  in
  Db.run_sql sql

let finish (item: t) : unit =
  put_back item ~status:`OK

