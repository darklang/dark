open Core

val store : string -> int -> Types.RuntimeT.dval -> unit
val load_all : string -> int -> Types.RuntimeT.dval list
