open Core
open Types

type t
type username = string

(* validate username/password of a Dark user *)
val authenticate : username:username -> password:string -> bool

(* can username edit auth_domain, in admin/ui *)
val can_edit : auth_domain:string -> username:username -> bool

(* get the id of the domain, for accounting/storage. Note that we need
 * this even for unauthenticated access, as all data and compute need a
 * Dark user to own them (pay for them, etc). *)
val owner : auth_domain:string -> t option

val init : unit -> unit
