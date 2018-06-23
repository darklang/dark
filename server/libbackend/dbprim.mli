open Core
open Libexecution

(* High-level DB serializers *)
val host : Types.host -> string
val uuid : Uuidm.t -> string
val string : string -> string
val sql : string -> string
val binary_to_string : string -> string
val list : serializer:('a -> string) -> 'a list -> string
val cast_type_for : Types.RuntimeT.dval -> string option
val cast_expression_for : Types.RuntimeT.dval -> string option

(* As a JSON string *)
val dvaljson : Types.RuntimeT.dval -> string

(* exposed to test it *)
val escape_single : string -> string
val escape_double : string -> string
