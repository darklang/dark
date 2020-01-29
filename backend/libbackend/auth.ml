open Core_kernel

module Session = struct
  module Backend = Session_postgresql_lwt
  include Session
  include Session_cohttp_lwt.Make (Backend)

  let backend = Libservice.Dbconnection.conn

  let cookie_key = "__session"

  let of_request req =
    of_header backend cookie_key (req |> Cohttp_lwt_unix.Request.headers)


  let random_string (len : int) : string =
    Cstruct.to_string
      (let open Nocrypto in
      Base64.encode (Rng.generate len))


  let session_data (username : string) : string =
    Yojson.to_string
      (`Assoc
        [ ("username", `String username)
          (* Generate a random CSRF token the same way Session
               does internally *)
        ; ("csrf_token", `String (random_string 30)) ])


  (* We store two values alongside each other in the session.value:
     one, the username; and two, the current CSRF token. These are
     stored as a JSON map with values "username" and "csrf_token".
   *)
  let new_for_username username = generate backend (session_data username)

  (* Like new_for_username above, but without the lwt context, since we don't
   * have lwt in the dark interpreter. *)
  let new_for_username_sync (username : string) : (string, exn) Result.t =
    let session_key = random_string 30 in
    let session_data = session_data username in
    let session_key =
      try
        Db.fetch_one
          ~name:"insert session from dark"
          ~subject:username
          "INSERT INTO session
                      (session_key, expire_date, session_data)
                      VALUES ($1, NOW() + '1 week'::interval, $2)
                      RETURNING session_key"
          ~params:[String session_key; String session_data]
        |> List.hd_exn
        |> Result.Ok
      with e -> Result.fail e
    in
    session_key


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


  let username_of_key (key : string) : string option =
    Db.fetch_one_option
      ~name:"username_of_key"
      "SELECT session_data
       FROM session
       WHERE expire_date > NOW() AND session_key = $1"
      ~params:[Db.String key]
    |> Option.bind ~f:(fun row -> row |> List.hd)
    |> Option.map ~f:(fun session_data ->
           session_data
           |> Yojson.Basic.from_string
           |> Yojson.Basic.Util.member "username"
           |> Yojson.Basic.Util.to_string)
end
