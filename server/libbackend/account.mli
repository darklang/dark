open Core

type username = string

type permissions = CanEdit | CanAccessOperations | NoPermission

(* validate username/password of a Dark user *)
val authenticate : username:username -> password:string -> bool

(* hash a password to the format expected in account documents. *)
val hash_password : string -> string

(* get the permissions that username has in authdomain *)
val get_permissions : auth_domain:string -> username:username -> unit -> permissions

val can_access_operations : username:string ->  bool

val can_edit_canvas : auth_domain:string -> username:string ->  bool

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
