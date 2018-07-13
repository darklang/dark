open Core_kernel
open Libexecution

open Types
open Types.RuntimeT

module FF = Feature_flag

type transaction = int
type t = { id: int
         ; value: dval
         ; retries: int
         ; flag_context: feature_flag
         ; canvas_id: Uuidm.t
         ; space: string
         ; name: string
         }

let status_to_enum status : string =
  match status with
  | `OK -> "done"
  | `Err -> "error"
  | `Incomplete -> "error"

(* ------------------------- *)
(* Public API *)
(* ------------------------- *)

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

let dequeue transaction : t option =
  let fetched =
    Db.fetch_one_option
      ~name:"dequeue_fetch"
      "SELECT id, value, retries, flag_context, canvas_id, space, name
       FROM events
       FOR UPDATE SKIP LOCKED
       WHERE delay_until < CURRENT_TIMESTAMP
       AND status = 'new'
       ORDER BY id DESC, retries ASC
       LIMIT 1"
     ~params:[]
  in
  match fetched with
  | None -> None
  | Some [id; value; retries; flag_context; canvas_id; space; name] ->
    Some { id = int_of_string id
         ; value = Dval.dval_of_json_string value
         ; retries = int_of_string retries
         ; flag_context = FF.from_sql flag_context
         ; canvas_id = Uuidm.of_string canvas_id |> Option.value_exn ~message:("Bad UUID: " ^ canvas_id)
         ; space = space
         ; name = name
         }
  | Some s ->
    Exception.internal
      ("Fetched seemingly impossible shape from Postgres"
       ^ ("[" ^ (String.concat ~sep:", " s) ^ "]"))

let begin_transaction () =
  let id = Util.create_id () in
  let _ =
    Db.run
      ~name:"start_transaction"
      ~params:[]
      "BEGIN"
  in
  id

let end_transaction t =
  let _ =
    Db.run
      ~name:"end_transaction"
      ~params:[]
      "COMMIT"
  in
  ()

let with_transaction f =
  let transaction = begin_transaction () in
  let result = f transaction in
  end_transaction transaction;
  result

let put_back transaction (item: t) ~status : unit =
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

let finish transaction (item: t) : unit =
  put_back transaction ~status:`OK item



