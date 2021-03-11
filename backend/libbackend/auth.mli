open Core

module SessionLwt : sig
  module Backend = Session_postgresql_lwt

  include module type of Session

  include module type of Session_cohttp_lwt.Make (Backend)

  val backend : Backend.t

  val cookie_key : string

  val of_request : Cohttp_lwt_unix.Request.t -> (t option, S.error) result io

  val new_for_username : Account.username -> t io

  val username_of_key : string -> string option

  val random_string : int -> string

  val session_data : string -> string

  val username_for : t -> Account.username

  val csrf_token_for : t -> string
end

module SessionSync : sig
  module Backend = Session_postgresql

  val new_for_username : Account.username -> string

  type session_key_and_csrf_token =
    { sessionKey : string
    ; csrfToken : string }

  val new_for_username_with_csrf_token :
    Account.username -> session_key_and_csrf_token

  val username_of_key : string -> string option
end
