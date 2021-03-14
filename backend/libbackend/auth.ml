open Core_kernel
module Db = Libbackend_basics.Db

module SessionShared = struct
  let backend = Libservice.Dbconnection.conn

  let cookie_key = "__session"

  let random_string (len : int) : string =
    Cstruct.to_string
      (let open Nocrypto in
      Base64.encode (Rng.generate len))


  (* We store two values alongside each other in the session.value: one, the
   * username; and two, the current CSRF token. These are stored as a JSON map
   * with values "username" and "csrf_token". *)
  let session_data (username : string) : string =
    Yojson.to_string
      (`Assoc
        [ ("username", `String username)
          (* Generate a random CSRF token the same way Session
               does internally *)
        ; ("csrf_token", `String (random_string 30)) ])


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

(* Sync, as opposed to lwt *)
module SessionSync = struct
  module Backend = Session_postgresql
  include Session
  include SessionShared

  let new_for_username username =
    Backend.generate backend ~value:(session_data username)


  type session_key_and_csrf_token =
    { sessionKey : string
    ; csrfToken : string }

  let new_for_username_with_csrf_token username =
    let session_data = session_data username in
    let csrfToken =
      session_data
      |> Yojson.Basic.from_string
      |> Yojson.Basic.Util.member "csrf_token"
      |> Yojson.Basic.Util.to_string
    in
    {sessionKey = Backend.generate backend ~value:session_data; csrfToken}
end

module SessionLwt = struct
  module Backend = Session_postgresql_lwt
  include Session
  include SessionShared
  include Session_cohttp_lwt.Make (Backend)

  let of_request req =
    of_header backend cookie_key (req |> Cohttp_lwt_unix.Request.headers)


  let new_for_username username = generate backend (session_data username)

  (* These can't be shared because session is type t, and so varies by backend
   * *)
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
