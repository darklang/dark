open Core_kernel
open Libexecution

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
  | `OK -> "done"
  | `Err -> "error"
  | `Incomplete -> "error"

let unlock_jobs (dequeuer: int) ~status : unit =
  Db.run
    ~name:"unlock_jobs"
    "UPDATE \"events\"
     SET status = $1
     WHERE dequeued_by = $2
       AND status = 'locked'"
    ~params:[String (status_to_enum status); Int dequeuer]

(* ------------------------- *)
(* Public API *)
(* ------------------------- *)

let finalize (dequeuer: int) ~status : unit =
  unlock_jobs ~status dequeuer

let enqueue (state: exec_state) (space: string) (name: string) (data: dval) : unit =
  Db.run
    ~name:"enqueue"
     "INSERT INTO events
     (status, dequeued_by, canvas_id, account_id,
      space, name, value, delay_until, flag_context)
     VALUES ('new', NULL, $1, $2, $3, $4, $5, CURRENT_TIMESTAMP, $6)"
     ~params:[ Uuid state.canvas_id
             ; Uuid state.account_id
             ; String space
             ; String name
             ; DvalJson data
             ; String (FF.to_sql state.ff)
             ]

(* This should soon enough do something like:
 * https://github.com/chanks/que/blob/master/lib/que/sql.rb#L4
 * but multiple queries will do fine for now
 *)
let dequeue ~(canvas:Uuidm.t) ~(account:Uuidm.t) (execution_id: int) (space: string) (name: string) : t option =
  let fetched =
    Db.fetch_one_option
      ~name:"dequeue_fetch"
      "SELECT id, value, retries, flag_context FROM events
       WHERE space = $1
         AND name = $2
         AND status = 'new'
         AND delay_until < CURRENT_TIMESTAMP
         AND canvas_id = $3
         AND account_id = $4
       ORDER BY id DESC
              , retries ASC
       LIMIT 1"
      ~params:[ Db.String space
              ; Db.String name
              ; Db.Uuid canvas
              ; Db.Uuid account]
  in
  match fetched with
  | None -> None
  | Some [id; value; retries; flag_context] ->
    Db.run
      ~name:"dequeue_update"
      "UPDATE events
      SET status = 'locked'
        , dequeued_by = $1
      WHERE id = $2"
      ~params:[Int execution_id; String id];
    Some { id = int_of_string id
         ; value = Dval.dval_of_json_string value
         ; retries = int_of_string retries
         ; flag_context = FF.from_sql flag_context
         }
  | Some s ->
    Exception.internal
      ("Fetched seemingly impossible shape from Postgres"
       ^ ("[" ^ (String.concat ~sep:", " s) ^ "]"))

let put_back (item: t) ~status : unit =
  match status with
  | `OK ->
    Db.run
      ~name:"put_back_OK"
      "UPDATE \"events\"
      SET status = 'done'
      WHERE id = $1"
      ~params:[Int item.id]
  | `Err ->
    if item.retries < 2
    then
      Db.run
        ~name:"put_back_Err<2"
        "UPDATE events
        SET status = 'new'
          , retries = $1
          , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
        WHERE id = $2"
        ~params:[Int (item.retries + 1); Int item.id]
    else
      Db.run
        ~name:"put_back_Err>=2"
        "UPDATE events
        SET status = 'error'
        WHERE id = $2"
        ~params:[Int item.id]
  | `Incomplete ->
      Db.run
        ~name:"put_back_Incomplete"
        "UPDATE events
        SET status = 'new'
          , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
        WHERE id = $1"
        ~params:[Int item.id]

let finish (item: t) : unit =
  put_back ~status:`OK item



