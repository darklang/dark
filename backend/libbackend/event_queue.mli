open Core
open Libexecution
open Libexecution.Types.RuntimeT
open Types
module Span = Libcommon.Telemetry.Span

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
     Span.t
  -> (   Span.t
      -> transaction
      -> (RuntimeT.dval option, Exception.captured) Result.t)
  -> (RuntimeT.dval option, Exception.captured) Result.t

val dequeue : Span.t -> transaction -> t option

val put_back : transaction -> t -> status:[`OK | `Err | `Incomplete] -> unit

val finish : transaction -> t -> unit

val schedule_all : unit -> unit

module Scheduling_rule : sig
  type rule_type =
    | Block
    | Pause

  type t =
    { id : int
    ; rule_type : rule_type
    ; canvas_id : Uuidm.t
    ; handler_name : string
    ; event_space : string
    ; created_at : time }

  val rule_type_of_string : string -> rule_type option

  val rule_type_to_string : rule_type -> string

  val to_dval : t -> RuntimeT.dval
end

module Worker_states : sig
  type state =
    | Running
    | Blocked
    | Paused

  val state_to_string : state -> string

  type t = (string, state, String.comparator_witness) Map.t

  val to_yojson : t -> Yojson.Safe.t

  val find : t -> string -> state option
end

val get_all_scheduling_rules : unit -> Scheduling_rule.t list

val get_scheduling_rules_for_canvas : Uuidm.t -> Scheduling_rule.t list

val get_worker_schedules_for_canvas : Uuidm.t -> Worker_states.t

val block_worker : Uuidm.t -> string -> unit

val unblock_worker : Uuidm.t -> string -> unit

val pause_worker : Uuidm.t -> string -> unit

val unpause_worker : Uuidm.t -> string -> unit
