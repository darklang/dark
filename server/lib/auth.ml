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

let has_access ~domain ~user : bool =
  String.Caseless.equal domain (User.domain user)
  || (User.domain user) = "admin"

let authenticate ~domain ~username ~password : User.t option =
  User.all_for_domain domain
  |> List.append (User.all_for_domain "admin")
  |> List.find
    ~f:(fun u -> (User.username u) = username && (User.password u) = password)
