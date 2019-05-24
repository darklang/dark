open Core

type username = string

type user_info =
  { username : username
  ; email : string
  ; name : string
  ; admin : bool }

type permissions =
  | CanEdit
  | CanAccessOperations
  | NoPermission

(* validate username/password of a Dark user *)
val authenticate : username:username -> password:string -> bool

(* hash a password to the format expected in account documents. *)
val hash_password : string -> string

(* get the permissions that username has in authdomain *)
val get_permissions :
  auth_domain:string -> username:username -> unit -> permissions

val can_access_operations : username:string -> bool

val can_edit_canvas : auth_domain:string -> username:string -> bool

(* For a host, what user do we expect *)
val auth_domain_for : string -> string

(* Get the owner of a host *)
val for_host : string -> Uuidm.t option

(* Get the owner of a host *)
val for_host_exn : string -> Uuidm.t

(* Add (or update) a user *)
val upsert_user :
  username:string -> email:string -> name:string -> unit -> string

(* Set whether user is an admin *)
val set_admin : username:string -> bool -> unit

(* Get a user's info *)
val get_user : string -> user_info option

(* Get a list of all users *)
val get_users : unit -> string list

(* Get a username from an ID *)
val username_of_id : Uuidm.t -> string option

(* Get an id from a username *)
val id_of_username : string -> Uuidm.t option

(* get the id of the domain, for accounting/storage. Note that we need
 * this even for unauthenticated access, as all data and compute need a
 * Dark user to own them (pay for them, etc). *)
val owner : auth_domain:string -> Uuidm.t option

val init : unit -> unit

val init_testing : unit -> unit

val user_info_to_yojson : user_info -> Yojson.Safe.json
