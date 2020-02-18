open Core

type username = string

type user_info =
  { username : username
  ; email : string
  ; name : string
  ; admin : bool }

type user_info_and_created_at =
  { username : username
  ; email : string
  ; name : string
  ; admin : bool
  ; created_at : string }

(* validate username/password of a Dark user *)
val authenticate :
  username_or_email:username -> password:string -> string option

val can_access_operations : username:string -> bool

(* For a host, what user do we expect *)
val auth_domain_for : string -> string

(* Get the owner of a host *)
val for_host : string -> Uuidm.t option

(* Get the owner of a host *)
val for_host_exn : string -> Uuidm.t

(* Add (or update) a user, returns Result (password,error) *)
val upsert_user :
     username:string
  -> email:string
  -> name:string
  -> unit
  -> (string, string) Result.t

(* Add a user; return error if we can't b/c username (unique) was taken *)
val insert_user :
     username:string
  -> email:string
  -> name:string
  -> ?segment_metadata:Libexecution.Types.RuntimeT.dval_map
  -> unit
  -> (string, string) Result.t

(* Set whether user is an admin *)
val set_admin : username:string -> bool -> unit

(* Get a user's info *)
val get_user : string -> user_info option

val get_user_and_created_at_and_segment_metadata :
  string -> (user_info_and_created_at * Yojson.Safe.t) option

(* Get a user's info by their email address *)
val get_user_by_email : string -> user_info option

(* Get a list of all users *)
val get_users : unit -> string list

(* Get a username from an ID *)
val username_of_id : Uuidm.t -> string option

(* Get an id from a username *)
val id_of_username : string -> Uuidm.t option

(* Checks if the user is an admin (aka a Dark employee/contractor). Note that
 * we sometimes refer in code to the user as an "admin", this is not that. *)
val is_admin : username:string -> bool

(* get the id of the domain, for accounting/storage. Note that we need
 * this even for unauthenticated access, as all data and compute need a
 * Dark user to own them (pay for them, etc). *)
val owner : auth_domain:string -> Uuidm.t option

val init : unit -> unit

val init_testing : unit -> unit

val user_info_to_yojson : user_info -> Yojson.Safe.t

val user_info_and_created_at_to_yojson :
  user_info_and_created_at -> Yojson.Safe.t

module Testing : sig
  val validate_username : string -> (unit, string) Result.t

  val validate_email : string -> (unit, string) Result.t
end
