open Types

module FF = Feature_flag

type t = { id: int
         ; value: RuntimeT.dval
         ; retries: int
         ; flag_context: RuntimeT.feature_flag
         }

val init : unit -> unit
val finalize : int -> status:[`OK | `Err ] -> unit

(* note, neither of these currently obtains any locks so is wholly
 * unsafe for actual use. should be fine for development though *)
val enqueue : RuntimeT.exec_state -> string -> string -> RuntimeT.dval -> unit
val dequeue : int -> string -> string -> string -> t option
val put_back : t -> status:[`OK | `Err | `Incomplete] -> unit
val finish : t -> unit
