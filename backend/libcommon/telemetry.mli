open Core_kernel
module ID = Int

module Span : sig
  type t =
    { name : string
    ; service_name : string
    ; span_id : ID.t
    ; trace_id : ID.t
    ; parent_id : ID.t
    ; start_time : Time.t
    ; attributes : (string, Yojson.Safe.t) Hashtbl.t }

  val set_attr : t -> string -> Yojson.Safe.t -> unit

  val set_attrs : t -> (string * Yojson.Safe.t) list -> unit

  val event : ?attrs:(string * Yojson.Safe.t) list -> t -> string -> unit

  val log_params : t -> (string * Yojson.Safe.t) list
end

val with_span :
     Span.t
  -> string
  -> ?attrs:(string * Yojson.Safe.t) list
  -> (Span.t -> 'a)
  -> 'a

val with_root :
  string -> ?attrs:(string * Yojson.Safe.t) list -> (Span.t -> 'a) -> 'a
