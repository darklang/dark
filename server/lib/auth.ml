open Core

module Session = struct
  module Backend = Session_postgresql_lwt
  include Session
  include Session_cohttp_lwt.Make(Backend)

  let backend = Db.conn
  let cookie_key = "__session"
  let of_request req =
    of_header backend cookie_key (req |> Cohttp_lwt_unix.Request.headers)
  let new_for_user user =
    generate backend (User.username user)
  let user_for session =
    User.for_username session.value
end

let has_access ~host ~user : bool =
  String.Caseless.equal host (User.host user)
  || (User.host user) = "admin"

let authenticate ~host ~username ~password : User.t option =
  User.all_for_host host
  |> List.append (User.all_for_host "admin")
  |> List.find
    ~f:(fun u -> (User.username u) = username && (User.password u) = password)
