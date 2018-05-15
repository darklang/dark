open Core
open Types

type username = string

val authenticate : host:host -> username:username -> password:string -> bool
val has_access : host:host -> username:username  -> bool

val init : unit -> unit
