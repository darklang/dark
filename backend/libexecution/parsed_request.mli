open Core_kernel

type 'expr_type t

type header = string * string

type query_val = string * string list

val from_request :
     ?allow_unparseable:bool
  -> Uri.t
  -> header list
  -> query_val list
  -> string
  -> Types.fluid_expr t

val to_dval : 'expr_type t -> 'expr_type Types.RuntimeT.dval

val sample_request : 'expr_type t

(* For testing *)

val parsed_query_string :
  (string * string list) list -> Types.RuntimeT.fluid_dval
