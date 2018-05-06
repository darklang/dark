open Core
open Types

module Session : sig
  module Backend = Session_postgresql_lwt
  include (module type of Session)
  include (module type of Session_cohttp_lwt.Make(Backend))
  val backend : Backend.t
  val cookie_key : string
  val of_request : Cohttp_lwt_unix.Request.t -> (t option, S.error) result io
  val new_for_user : User.t -> t io
  val user_for : t -> User.t option
end

val authenticate : host:host -> username:string -> password:string -> User.t option
val has_access : host:host -> user:User.t -> bool

