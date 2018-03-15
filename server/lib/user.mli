open Core

type t

val username : t -> string
val domain : t -> string
val password : t -> string

val all_for_domain : string -> t list
val for_username : string -> t option
