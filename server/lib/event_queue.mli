open Types

(* ------------------------- *)
(* Awful Global State *)
(* ------------------------- *)

val current_scope : string option ref
val set_scope : string -> unit
val unset_scope : unit -> unit

(* note, neither of these currently obtains any locks so is wholly
 * unsafe for actual use. should be fine for development though *)
val enqueue : string -> string -> string -> RuntimeT.dval -> unit
val dequeue : string -> string -> string -> RuntimeT.dval
