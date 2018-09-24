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

     Because CSRF tokens exist that predate this change (previously,
     just the username was stored, we default username_for to

     We can remove this hacks one week from when this is merged,
     since the session default cookie max-age is one week.
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
    try
      session.value
      |> Yojson.Basic.from_string
      |> Yojson.Basic.Util.member "username"
      |> Yojson.Basic.Util.to_string
    with e ->
      (* If there's not valid json in the database,
         we should assume it's just a username. *)
      session.value

  let csrf_token_for session =
    try
      session.value
      |> Yojson.Basic.from_string
      |> Yojson.Basic.Util.member "csrf_token"
      |> Yojson.Basic.Util.to_string
    with e ->
      (* If there's not valid json in the database,
         this user has not been issued a CSRF token.
         So, we'll just issue a dummy token. *)
      "DUMMY_CSRF"
end
