open Core_kernel
open Libexecution
open Types
open Types.RuntimeT
open Libcommon

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

type queue_action =
  | Enqueue
  | Dequeue
  | None

let show_queue_action qa =
  match qa with Enqueue -> "enqueue" | Dequeue -> "dequeue" | None -> "none"


(* None in case we want to log on a timer or something, not
                     just on enqueue/dequeue *)

let log_queue_size
    (queue_action : queue_action)
    ?(host : string option)
    (canvas_id : string)
    (space : string)
    (name : string)
    (modifier : string) =
  (* Prefixing all of these with queue so we don't overwrite - e.g., 'enqueue'
   * from a handler that has a space, etc *)
  let params =
    [ ("queue_action", show_queue_action queue_action)
    ; ("queue_canvas_id", canvas_id)
    ; ("queue_event_space", space)
    ; ("queue_event_name", name)
    ; ("queue_event_modifier", modifier) ]
  in
  (* host is optional b/c we have it when we dequeue, but not enqueue. We could
   * also do a db lookup here if we decide querying by canvas name in honeycomb
   * is important *)
  let host =
    (* Our Option module seems not to have an or_else type function ("if none,
     * then run this function" *)
    match host with
    | Some host ->
        Some host
    | None ->
        Db.fetch_one
          ~name:"fetch_canvas_name"
          "SELECT name FROM canvases WHERE id = $1"
          ~params:[Uuid (Uuidm.of_string canvas_id |> Option.value_exn)]
        |> List.hd
  in
  (* I suppose we could do a subquery that'd batch these three conditionals, but
   * I'm not sure it's worth the added complexity - we could look at the
   * postgres logs in honeycomb if we wanted to get a sense of how long this
   * takes *)
  (* TODO: what statuses? *)
  let canvas_queue_size =
    Db.fetch_one
      ~name:"canvas queue size"
      ~subject:canvas_id
      "SELECT count(*) FROM events
      WHERE status IN ('new','locked','error') AND canvas_id = $1"
      ~params:[Uuid (Uuidm.of_string canvas_id |> Option.value_exn)]
    |> List.hd_exn
    |> int_of_string
  in
  let worker_queue_size =
    Db.fetch_one
      ~name:"canvas queue size"
      ~subject:canvas_id
      "SELECT count(*) FROM events
      WHERE status IN ('new','locked','error') AND canvas_id = $1
      AND space = $2 AND name = $3 AND modifier = $4"
      ~params:
        [ Uuid (Uuidm.of_string canvas_id |> Option.value_exn)
        ; String space
        ; String name
        ; String modifier ]
    |> List.hd_exn
    |> int_of_string
  in
  let jsonparams =
    [ ("canvas_queue_size", `Int canvas_queue_size)
    ; ("worker_queue_size", `Int worker_queue_size) ]
  in
  let params =
    match host with
    | None ->
        params
    | Some host ->
        ("queue_canvas_name", host) :: params
  in
  Log.infO "queue size" ~params ~jsonparams


let enqueue
    ~account_id
    ~canvas_id
    (space : string)
    (name : string)
    (modifier : string)
    (data : dval) : unit =
  log_queue_size Enqueue (canvas_id |> Uuidm.to_string) space name modifier ;
  Db.run
    ~name:"enqueue"
    "INSERT INTO events
     (status, dequeued_by, canvas_id, account_id,
      space, name, modifier, value, delay_until, enqueued_at)
     VALUES ('new', NULL, $1, $2, $3, $4, $5, $6, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
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
      "SELECT e.id, e.value, e.retries, e.canvas_id, c.name, e.space, e.name, e.modifier,
         (extract(epoch from (CURRENT_TIMESTAMP - enqueued_at)) * 1000) as queue_delay_ms
       FROM events AS e
       JOIN canvases AS c ON e.canvas_id = c.id
       WHERE status = 'scheduled'
       ORDER BY id DESC, retries ASC
       FOR UPDATE OF e SKIP LOCKED
       LIMIT 1"
      ~params:[]
  in
  match fetched with
  | None ->
      None
  | Some
      [ id
      ; value
      ; retries
      ; canvas_id
      ; host
      ; space
      ; name
      ; modifier
      ; queue_delay_ms ] ->
      log_queue_size Dequeue ~host canvas_id space name modifier ;
      let queue_delay =
        queue_delay_ms
        |> float_of_string_opt
        |> Option.map ~f:(fun flt -> [("queue_delay_ms", `Float flt)])
        |> Option.value ~default:[]
      in
      Log.infO
        "queue_delay"
        ~params:
          [ ("host", host)
          ; ("canvas_id", canvas_id)
          ; ("space", space)
          ; ("name", name)
          ; ("modifier", modifier) ]
        ~jsonparams:queue_delay ;
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


type canvas_id = Uuidm.t

type scheduling_rule =
  { id : int
  ; rule_type : string
  ; canvas_id : canvas_id
  ; handler_name : string
  ; event_space : string
  ; created_at : time }

(* TESTS ONLY *)
(* schedule_all bypasses actual scheduling logic and is meant only for allowing
 * testing without running the queue-scheduler process *)
let schedule_all unit : unit =
  Db.run
    ~name:"schedule_all"
    ~params:[]
    "UPDATE events SET status = 'scheduled'
    WHERE status = 'new' AND delay_until <= CURRENT_TIMESTAMP"


(* DARK INTERNAL FN *)
(* Gets all event scheduling rules, as used by the queue-scheduler. *)
let get_all_scheduling_rules unit : scheduling_rule list =
  Db.fetch
    ~name:"get_all_scheduling_rules"
    "SELECT id, rule_type, canvas_id, handler_name, event_space, created_at
    FROM scheduling_rules"
    ~params:[]
  |> List.map ~f:(function
         | [id; rule_type; canvas_id; handler_name; event_space; created_at] ->
             { id = int_of_string id
             ; rule_type
             ; canvas_id = canvas_id |> Uuidm.of_string |> Option.value_exn
             ; handler_name
             ; event_space
             ; created_at = Time.of_string created_at }
         | _ ->
             Exception.internal
               "unexpected results parsing get_all_scheduling_rules" )


let row_to_scheduling_rule row : scheduling_rule =
  match row with
  | [id; rule_type; canvas_id; handler_name; event_space; created_at] ->
      { id = int_of_string id
      ; rule_type
      ; canvas_id = canvas_id |> Uuidm.of_string |> Option.value_exn
      ; handler_name
      ; event_space
      ; created_at = Time.of_string created_at }
  | _ ->
      Exception.internal "unexpected structure parsing scheduling_rule row"


(* DARK INTERNAL FN *)
(* Gets event scheduling rules for the specified canvas *)
let get_scheduling_rules_for_canvas canvas_id : scheduling_rule list =
  Db.fetch
    ~name:"get_scheduling_rules_for_canvas"
    "SELECT id, rule_type, canvas_id, handler_name, event_space, created_at
    FROM scheduling_rules"
    ~params:[]
  |> List.map ~f:row_to_scheduling_rule


let get_scheduling_rules_for_canvas canvas_id : scheduling_rule list =
  Db.fetch
    ~name:"get_scheduling_rules_for_canvas"
    "SELECT id, rule_type, canvas_id, handler_name, event_space, created_at
    FROM scheduling_rules
    WHERE canvas_id = $1
    "
    ~params:[Uuid canvas_id]
  |> List.map ~f:row_to_scheduling_rule


(* DARK INTERNAL FN *)
(* Blocks the given worker from executing by inserting a 'block' scheduling rule *)
let block_worker canvas_id handler_name : unit =
  Db.run
    ~name:"add_scheduling_block"
    ~params:[Uuid canvas_id; String handler_name]
    "INSERT INTO scheduling_rules (rule_type, canvas_id, handler_name, event_space)
    VALUES ('block', $1, $2, 'WORKER')"


(* DARK INTERNAL FN *)
(* Unblocks the given worker by removing any 'block' scheduling rule for the handler *)
let unblock_worker canvas_id handler_name : unit =
  Db.run
    ~name:"remove_scheduling_block"
    ~params:[Uuid canvas_id; String handler_name]
    "DELETE FROM scheduling_rules
    WHERE canvas_id = $1
    AND handler_name = $2
    AND event_space = 'WORKER'
    AND rule_type = 'block'"


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
  let show_status s =
    match s with `OK -> "Ok" | `Err -> "Err" | `Incomplete -> "Incomplete"
  in
  Log.infO
    "event_queue: put_back_transaction"
    ~jsonparams:
      [ ("status", `String (status |> show_status))
      ; ("retries", `Int item.retries) ] ;
  match status with
  | `OK ->
      Db.run
        ~name:"put_back_OK"
        "UPDATE \"events\"
      SET status = 'done', last_processed_at = CURRENT_TIMESTAMP
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
          , last_processed_at = CURRENT_TIMESTAMP
        WHERE id = $2"
          ~params:[Int (item.retries + 1); Int item.id]
      else
        Db.run
          ~name:"put_back_Err>=2"
          "UPDATE events
        SET status = 'error'
          , last_processed_at = CURRENT_TIMESTAMP
        WHERE id = $1"
          ~params:[Int item.id]
  | `Incomplete ->
      Db.run
        ~name:"put_back_Incomplete"
        "UPDATE events
        SET status = 'new'
          , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
          , last_processed_at = CURRENT_TIMESTAMP
        WHERE id = $1"
        ~params:[Int item.id]


let finish transaction (item : t) : unit =
  put_back transaction ~status:`OK item
