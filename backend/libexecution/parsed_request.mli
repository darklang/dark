open Core_kernel

type t

type header = string * string

type query_val = string * string list

val from_request : header list -> query_val list -> string -> t

val to_dval : t -> Types.RuntimeT.dval

val sample_request : t
