open Core

module Session : sig
  module Backend = Session_postgresql_lwt

  include module type of Session

  include module type of Session_cohttp_lwt.Make (Backend)

  val backend : Backend.t

  val cookie_key : string

  val of_request : Cohttp_lwt_unix.Request.t -> (t option, S.error) result io

  val new_for_username : Account.username -> t io

  val username_for : t -> Account.username

  val csrf_token_for : t -> string

  val username_of_key : string -> string option
end
