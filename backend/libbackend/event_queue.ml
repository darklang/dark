open Core_kernel
open Libexecution
open Types
open Types.RuntimeT

type transaction = id

type t =
  { id : int
  ; value : dval
  ; retries : int
  ; canvas_id : Uuidm.t
  ; host : string
  ; space : string
  ; name : string
  ; modifier : string }

let to_event_desc t = (t.space, t.name, t.modifier)

(* ------------------------- *)
(* Public API *)
(* ------------------------- *)

let enqueue
    ~account_id
    ~canvas_id
    (space : string)
    (name : string)
    (modifier : string)
    (data : dval) : unit =
  Db.run
    ~name:"enqueue"
    "INSERT INTO events
     (status, dequeued_by, canvas_id, account_id,
      space, name, modifier, value, delay_until)
     VALUES ('new', NULL, $1, $2, $3, $4, $5, $6, CURRENT_TIMESTAMP)"
    ~params:
      [ Uuid canvas_id
      ; Uuid account_id
      ; String space
      ; String name
      ; String modifier
      ; RoundtrippableDval data ]


let dequeue transaction : t option =
  let fetched =
    Db.fetch_one_option
      ~name:"dequeue_fetch"
      "SELECT e.id, e.value, e.retries, e.canvas_id, c.name, e.space, e.name, e.modifier
       FROM events AS e
       JOIN canvases AS c ON e.canvas_id = c.id
       WHERE delay_until < CURRENT_TIMESTAMP
       AND status = 'new'
       ORDER BY id DESC, retries ASC
       FOR UPDATE OF e SKIP LOCKED
       LIMIT 1"
      ~params:[]
  in
  match fetched with
  | None ->
      None
  | Some [id; value; retries; canvas_id; host; space; name; modifier] ->
      Some
        { id = int_of_string id
        ; value = Dval.of_internal_roundtrippable_v0 value
        ; retries = int_of_string retries
        ; canvas_id = Util.uuid_of_string canvas_id
        ; host
        ; space
        ; name
        ; modifier }
  | Some s ->
      Exception.internal
        ( "Fetched seemingly impossible shape from Postgres"
        ^ "["
        ^ String.concat ~sep:", " s
        ^ "]" )


let begin_transaction () =
  let id = Util.create_id () in
  ignore (Db.run ~name:"start_transaction" ~params:[] "BEGIN") ;
  id


let end_transaction t =
  ignore (Db.run ~name:"end_transaction" ~params:[] "COMMIT")


let with_transaction f =
  let transaction = begin_transaction () in
  let result = f transaction in
  end_transaction transaction ;
  result


let put_back transaction (item : t) ~status : unit =
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
        WHERE id = $1"
          ~params:[Int item.id]
  | `Incomplete ->
      Db.run
        ~name:"put_back_Incomplete"
        "UPDATE events
        SET status = 'new'
          , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
        WHERE id = $1"
        ~params:[Int item.id]


let finish transaction (item : t) : unit =
  put_back transaction ~status:`OK item
