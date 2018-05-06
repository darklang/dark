open Core
open Types

type t

val username : t -> string
val host : t -> host
val password : t -> string

val all_for_host : host -> t list
val for_username : string -> t option
