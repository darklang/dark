open Core

(* High-level DB serializers *)
val host : Types.host -> string
val uuid : Uuid.t -> string
val string : string -> string
val int : int -> string
val sql : string -> string
val tlid : Types.tlid -> string
val id : Types.id -> string
val binary : string -> string
val binary_to_string : string -> string
val cast_expression_for : Types.RuntimeT.dval -> string option

(* As a JSON string *)
val dvaljson : Types.RuntimeT.dval -> string
val dvals : Types.RuntimeT.dval list -> string
val dvalmap_jsonb : Types.RuntimeT.dval_map -> string

(* DB plumbing *)
val table : string -> string

