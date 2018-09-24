open Core_kernel

module Session = struct
  module Backend = Session_postgresql_lwt
  include Session
  include Session_cohttp_lwt.Make(Backend)

  let backend = Libservice.Dbconnection.conn
  let cookie_key = "__session"
  let of_request req =
    of_header backend cookie_key (req |> Cohttp_lwt_unix.Request.headers)
  let new_for_username username =
    generate backend username
  let username_for session =
    session.value
  let csrf_token_for session =
    "abc"
end
