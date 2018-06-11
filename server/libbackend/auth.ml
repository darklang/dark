open Core

module Session = struct
  module Backend = Session_postgresql_lwt
  include Session
  include Session_cohttp_lwt.Make(Backend)

  let backend = Dbconnection.conn
  let cookie_key = "__session"
  let of_request req =
    of_header backend cookie_key (req |> Cohttp_lwt_unix.Request.headers)
  let new_for_username username =
    generate backend username
  let username_for session =
    session.value
end


