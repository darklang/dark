open Core
open Libexecution
open Types

module FF = Feature_flag

(* implementation deliberately hidden to prevent users accidentally
 * passing a variable that'll unify *)
type transaction
type t = { id: int
         ; value: RuntimeT.dval
         ; retries: int
         ; flag_context: RuntimeT.feature_flag
         ; canvas_id: Uuidm.t
         ; host: string
         ; space: string
         ; name: string
         ; modifier: string
         }
val to_event_desc : t -> Stored_event.event_desc


val enqueue : RuntimeT.exec_state -> string -> string -> string -> RuntimeT.dval -> unit

val with_transaction : (transaction -> (unit, Exception.captured) Result.t) -> (unit, Exception.captured) Result.t
val dequeue : transaction -> t option
val put_back : transaction -> t -> status:[`OK | `Err | `Incomplete] -> unit
val finish : transaction -> t -> unit
