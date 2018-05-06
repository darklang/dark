open Types

module FF = Feature_flag

type t = { id: int
         ; value: RuntimeT.dval
         ; retries: int
         ; flag_context: RuntimeT.feature_flag
         }

val finalize : host:string -> int -> status:[`OK | `Err ] -> unit

(* note, neither of these currently obtains any locks so is wholly
 * unsafe for actual use. should be fine for development though *)
val enqueue : RuntimeT.exec_state -> string -> string -> RuntimeT.dval -> unit
val dequeue : host:string -> int -> string -> string -> t option
val put_back : host:string -> t -> status:[`OK | `Err | `Incomplete] -> unit
val finish : host:string -> t -> unit
