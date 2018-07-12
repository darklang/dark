open Core
open Types

type username = string

(* validate username/password of a Dark user *)
val authenticate : username:username -> password:string -> bool

(* can username edit auth_domain, in admin/ui *)
val can_edit : auth_domain:string -> username:username -> bool

(* For a host, what user do we expect *)
val auth_domain_for : string -> string

(* Get the owner of a host *)
val for_host : string -> Uuidm.t

(* get the id of the domain, for accounting/storage. Note that we need
 * this even for unauthenticated access, as all data and compute need a
 * Dark user to own them (pay for them, etc). *)
val owner : auth_domain:string -> Uuidm.t option

val init : unit -> unit
val init_testing : unit -> unit
