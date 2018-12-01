open Core_kernel

module Session = struct
  module Backend = Session_postgresql_lwt
  include Session
  include Session_cohttp_lwt.Make(Backend)

  let backend = Libservice.Dbconnection.conn
  let cookie_key = "__session"
  let of_request req =
    of_header backend cookie_key (req |> Cohttp_lwt_unix.Request.headers)

  (* We store two values alongside each other in the session.value:
     one, the username; and two, the current CSRF token. These are
     stored as a JSON map with values "username" and "csrf_token".
   *)
  let new_for_username username =
    generate backend
      (Yojson.to_string
         (`Assoc
            [ "username", `String username
            (* Generate a random CSRF token the same way Session
               does internally *)
            ; "csrf_token", `String
                              (Cstruct.to_string
                                 Nocrypto.(Base64.encode
                                 (Rng.generate 30)))
            ]))

  let username_for session =
    session.value
    |> Yojson.Basic.from_string
    |> Yojson.Basic.Util.member "username"
    |> Yojson.Basic.Util.to_string

  let csrf_token_for session =
    session.value
    |> Yojson.Basic.from_string
    |> Yojson.Basic.Util.member "csrf_token"
    |> Yojson.Basic.Util.to_string
end
