open Core
open Types

type username = string

val authenticate : auth_domain:string -> username:username -> password:string -> bool
val has_access : auth_domain:string -> username:username  -> bool

val init : unit -> unit
