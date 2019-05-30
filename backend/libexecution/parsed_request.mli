open Core_kernel

type t

type header = string * string

type query_val = string * string list

val from_request :
  ?allow_unparseable:bool -> header list -> query_val list -> string -> t

val to_dval : t -> Types.RuntimeT.dval

val sample_request : t

(* For testing *)

val parsed_query_string : (string * string list) list -> Types.RuntimeT.dval
