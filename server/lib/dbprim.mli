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

(* As real SQL types *)
val dval : Types.RuntimeT.dval -> string
val dvals : Types.RuntimeT.dval list -> string
(* As a JSON string *)
val dvaljson : Types.RuntimeT.dval -> string

val tipe : Types.RuntimeT.tipe -> string
val tipe_default : Types.RuntimeT.tipe -> string

(* DB plumbing *)
val col : string -> string
val cols : string list -> string
val table : string -> string

