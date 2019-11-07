open Core
open Libexecution
open Libexecution.Types.RuntimeT
open Types

(* implementation deliberately hidden to prevent users accidentally
 * passing a variable that'll unify *)
type transaction

type t =
  { id : int
  ; value : RuntimeT.dval
  ; retries : int
  ; canvas_id : Uuidm.t
  ; host : string
  ; space : string
  ; name : string
  ; modifier : string }

val to_event_desc : t -> Stored_event.event_desc

val enqueue :
     account_id:Uuidm.t
  -> canvas_id:Uuidm.t
  -> string
  -> string
  -> string
  -> RuntimeT.dval
  -> unit

val with_transaction :
     (transaction -> (RuntimeT.dval option, Exception.captured) Result.t)
  -> (RuntimeT.dval option, Exception.captured) Result.t

val dequeue : transaction -> t option

val put_back : transaction -> t -> status:[`OK | `Err | `Incomplete] -> unit

val finish : transaction -> t -> unit

val schedule_all : unit -> unit

type canvas_id = Uuidm.t

type scheduling_rule =
  { id : int
  ; rule_type : string
  ; canvas_id : canvas_id
  ; handler_name : string
  ; event_space : string
  ; created_at : time }

val get_all_scheduling_rules : unit -> scheduling_rule list

val get_scheduling_rules_for_canvas : canvas_id -> scheduling_rule list

val block_worker : canvas_id -> string -> unit

val unblock_worker : canvas_id -> string -> unit
