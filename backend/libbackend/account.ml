open Core_kernel
open Libexecution
open Types
module Hash = Sodium.Password_hash.Bytes

let banned_usernames : string list =
  (* originally from https://ldpreload.com/blog/names-to-reserve *)
  (* we allow www, because we have a canvas there *)
  [ "abuse"
  ; "admin"
  ; "administrator"
  ; "autoconfig"
  ; "broadcasthost"
  ; "ftp"
  ; "hostmaster"
  ; "imap"
  ; "info"
  ; "is"
  ; "isatap"
  ; "it"
  ; "localdomain"
  ; "localhost"
  ; "mail"
  ; "mailer-daemon"
  ; "marketing"
  ; "mis"
  ; "news"
  ; "nobody"
  ; "noc"
  ; "noreply"
  ; "no-reply"
  ; "pop"
  ; "pop3"
  ; "postmaster"
  ; "root"
  ; "sales"
  ; "security"
  ; "smtp"
  ; "ssladmin"
  ; "ssladministrator"
  ; "sslwebmaster"
  ; "support"
  ; "sysadmin"
  ; "usenet"
  ; "uucp"
  ; "webmaster"
  ; "wpad" ]
  @ (* original to us *)
    ["billing"; "dev"]


type username = string [@@deriving yojson]

type account =
  { username : username
  ; password : string
  ; email : string
  ; name : string }

type user_info =
  { username : username
  ; email : string
  ; name : string
  ; admin : bool }
[@@deriving yojson]

(************************)
(* Adding *)
(************************)
let validate_username (username : string) : (unit, string) Result.t =
  (* rules: no uppercase, ascii only, must start with letter, other letters can
   * be numbers or underscores. 3-20 characters. *)
  let regex = Re2.create_exn "^[a-z][a-z0-9_]{2,20}$" in
  if Re2.matches regex username
  then Ok ()
  else
    Error
      ( "Invalid username '"
      ^ username
      ^ "', must match /^[a-z][a-z0-9_]{2,20}$/" )


let validate_password ~(username : string) (password : string) :
    (unit, string) Result.t =
  (* rules: must be at least 8 characters *)
  if String.length password > 8
  then Ok ()
  else
    Error
      ( "Invalid password for user '"
      ^ username
      ^ "', must be at least 8 characters" )


let validate_email (email : string) : (unit, string) Result.t =
  (* just checking it's roughly the shape of an email *)
  let regex = Re2.create_exn ".+@.+\\..+" in
  if Re2.matches regex email
  then Ok ()
  else Error ("Invalid email '" ^ email ^ "'")


let validate_account (account : account) : (unit, string) Result.t =
  validate_username account.username
  |> Prelude.Result.or_ (validate_email account.email)
  |> Prelude.Result.or_
       (validate_password ~username:account.username account.password)


let upsert_account ?(validate : bool = true) (account : account) :
    (unit, string) Result.t =
  let result = if validate then validate_account account else Ok () in
  Result.map result ~f:(fun () ->
      Db.run
        ~name:"upsert_account"
        ~subject:account.username
        "INSERT INTO accounts
    (id, username, name, email, admin, password)
    VALUES
    ($1, $2, $3, $4, false, $5)
    ON CONFLICT (username)
    DO UPDATE SET name = EXCLUDED.name,
                  email = EXCLUDED.email,
                  admin = false,
                  password = EXCLUDED.password"
        ~params:
          [ Uuid (Util.create_uuid ())
          ; String account.username
          ; String account.name
          ; String account.email
          ; String account.password ] )


let upsert_account_exn ?(validate : bool = true) (account : account) : unit =
  upsert_account ~validate account
  |> Prelude.Result.ok_or_internal_exception "Cannot upsert account"


let upsert_admin ?(validate : bool = true) (account : account) :
    (unit, string) Result.t =
  Result.map (validate_account account) ~f:(fun () ->
      Db.run
        ~name:"upsert_admin"
        ~subject:account.username
        "INSERT INTO accounts as u
    (id, username, name, email, admin, password)
    VALUES
    ($1, $2, $3, $4, true, $5)
    ON CONFLICT (username)
    DO UPDATE SET name = EXCLUDED.name,
                  email = EXCLUDED.email,
                  admin = true,
                  password = EXCLUDED.password"
        ~params:
          [ Uuid (Util.create_uuid ())
          ; String account.username
          ; String account.name
          ; String account.email
          ; String account.password ] )


let upsert_admin_exn ?(validate : bool = true) (account : account) : unit =
  upsert_admin ~validate account
  |> Prelude.Result.ok_or_internal_exception "Cannot upsert account"


(************************)
(* Querying *)
(************************)
let username_of_id id =
  Db.fetch_one_option
    ~name:"account_of_id"
    ~subject:(Uuidm.to_string id)
    "SELECT username from accounts
     WHERE accounts.id = $1"
    ~params:[Uuid id]
  |> Option.map ~f:List.hd_exn


let id_of_username username : Uuidm.t option =
  Db.fetch_one_option
    ~name:"account_of_username"
    ~subject:username
    "SELECT id from accounts
     WHERE accounts.username = $1"
    ~params:[String username]
  |> Option.map ~f:List.hd_exn
  |> fun x -> match x with Some sid -> Uuidm.of_string sid | None -> None


let get_user username =
  Db.fetch_one_option
    ~name:"get_user"
    ~subject:username
    "SELECT name, email, admin from accounts
     WHERE accounts.username = $1"
    ~params:[String username]
  |> Option.bind ~f:(function
         | [name; email; admin] ->
             Some {username; name; admin = admin = "t"; email}
         | _ ->
             None )


let get_users () =
  Db.fetch ~name:"get_users" "SELECT username from accounts" ~params:[]
  |> List.map ~f:List.hd_exn


let is_admin ~username : bool =
  Db.exists
    ~subject:username
    ~name:"is_admin"
    "SELECT 1 from accounts
     WHERE accounts.username = $1
       AND accounts.admin = true"
    ~params:[String username]


let set_admin ~username (admin : bool) : unit =
  Db.run
    ~name:"set_admin"
    ~subject:username
    "UPDATE accounts SET admin = $1 where username = $2"
    ~params:[Bool admin; String username]


let valid_user ~(username : username) ~(password : string) : bool =
  match
    Db.fetch_one_option
      ~name:"valid_user"
      ~subject:username
      "SELECT password from accounts
           WHERE accounts.username = $1"
      ~params:[String username]
  with
  | None ->
      false
  | Some [db_password] ->
      password
      |> Bytes.of_string
      |> Hash.wipe_to_password
      |> Hash.verify_password_hash (Bytes.of_string (B64.decode db_password))
  | _ ->
      false


let can_access_operations ~(username : username) : bool = is_admin ~username

let authenticate ~(username : username) ~(password : string) : bool =
  valid_user ~username ~password


let hash_password password =
  password
  |> Bytes.of_string
  |> Hash.wipe_to_password
  |> Hash.hash_password Sodium.Password_hash.interactive
  |> Bytes.to_string
  |> B64.encode


let owner ~(auth_domain : string) : Uuidm.t option =
  let auth_domain = String.lowercase auth_domain in
  if List.mem banned_usernames auth_domain ~equal:( = )
  then None
  else
    Db.fetch_one_option
      ~name:"owner"
      ~subject:auth_domain
      "SELECT id from accounts
     WHERE accounts.username = $1"
      ~params:[String auth_domain]
    |> Option.map ~f:List.hd_exn
    |> Option.bind ~f:Uuidm.of_string


let auth_domain_for host : string =
  match String.split host '-' with d :: _ -> d | _ -> host


let for_host (host : string) : Uuidm.t option =
  host |> auth_domain_for |> owner


let for_host_exn (host : string) : Uuidm.t =
  host
  |> for_host
  |> fun o -> Option.value_exn ~message:("No owner found for host " ^ host) o


(************************)
(* Darkinternal functions *)
(************************)

let upsert_user ~(username : string) ~(email : string) ~(name : string) () :
    (string, string) Result.t =
  let plaintext = Util.random_string 16 in
  let password = hash_password plaintext in
  upsert_account {username; email; name; password}
  |> Result.map ~f:(fun () -> plaintext)


let init_testing () : unit =
  upsert_account_exn
    { username = "test_unhashed"
    ; password = "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test@darklang.com"
    ; name = "Dark OCaml Tests with Unhashed Password" } ;
  upsert_account_exn
    { username = "test"
    ; password = hash_password "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test@darklang.com"
    ; name = "Dark OCaml Tests" } ;
  upsert_admin_exn
    { username = "test_admin"
    ; password = hash_password "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test@darklang.com"
    ; name = "Dark OCaml Test Admin" } ;
  ()


let upsert_admins () : unit =
  upsert_admin_exn
    { username = "ian"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkOXd2R3BSYW54Y3llYmdRdU1EMHdUdyRNN1ljWVFQdDk0S29nM1EyM1Q2cHFRZDRlMk9VM3lDTmpreUZ2NGIva1o4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ian@darklang.com"
    ; name = "Ian Connolly" } ;
  upsert_admin_exn
    { username = "paul"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcEQxWXBLOG1aVStnUUJUYXdKZytkQSR3TWFXb1hHOER1UzVGd2NDYzRXQVc3RlZGN0VYdVpnMndvZEJ0QnY1bkdJAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "paul@darklang.com"
    ; name = "Paul Biggar" } ;
  upsert_admin_exn
    { username = "ellen"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcHcxNmRhelJaTGNrYXhZV1psLytXdyRpUHJ1V1NQV2xya1RDZjRDbGlwNTkyaC9tSlZvaTVWSTliRlp0c2xrVmg0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ellen@darklang.com"
    ; name = "Ellen Chisa" } ;
  upsert_admin_exn
    { username = "stefi"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkKzIyM3lwOWU4ZzdvOVFWN1RtbnpYQSQ2b0VsdTBLU0JzendBR3FDb1FUQVoyVGNlcERDaUZ4OE9haFRWbzZMTk9VAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "stefi@darklang.com"
    ; name = "Stefi Petit" } ;
  upsert_admin_exn
    { username = "alice"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkVGllNGtJT3kyMVFjL1dGUnhScC9PdyROMnp1ZVZnczhIcjl0ODZEREN2VFBYMVNHOE1Za1plSUZCSWFzck9aR1J3AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "alice@darklang.com"
    ; name = "Alice Wong" } ;
  upsert_admin_exn
    { username = "ismith"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkbHlXamc0MHA3MWRBZ1kyTmFTTVhIZyRnaWZ1UGpsSnoxMFNUVDlZYWR5Tis1SVovRFVxSXdZeXVtL0Z2TkFOa1ZnAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ismith@darklang.com"
    ; name = "Ian Smith" } ;
  upsert_admin_exn
    { username = "sydney"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkMDJYZzhSS1RQai9JOGppdzI5MTBEUSRJdE0yYnlIK29OL1RIdzFJbC9yNWZBT2RGR0xrUFc3V3MxaVpUUUVFKytjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "sydney@darklang.com"
    ; name = "Sydney Noteboom" } ;
  (* dark contractors *)
  (* upsert_account_exn *)
  (*   { username = "lizzie" *)
  (*   ; password = *)
  (*       "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkWTNueDFWQUFYRWpLMjJGclcwMjU2ZyRYVDQxUGtGNnYyM1E4L0MrSUZITlNXNi8wUGN4TFdEbkRMZ0xVdHN2bHJZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" *)
  (*   ; email = "_@lizzie.io" *)
  (*   ; name = "Lizzie Dixon" } ; *)
  ()


(* accounts to create namespaces for dark canvases *)
let upsert_useful_canvases () : unit =
  upsert_account_exn
    { username = "builtwithdark"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkMUdwN0luSFJEbllrMGw5dnR1NTBzdyRMazhVSUdOZU9tTm5SMVFwbDRHUGs3VHdzRXQwbTQ5QUFTdjJQdlZpd1pjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "Built with Dark" } ;
  upsert_account_exn
    { username = "benchmarking"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkdS9vQml2Uy9pa2ZnUlFEeHYvcVhJdyQxcVNNenExVnE2THdWMElVKyswNDRWbEpsYmk3d1NZaFZySzNXUEIwRkw4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "Dark benchmarking" } ;
  upsert_account_exn
    { username = "www"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkTHhZV0RvZEFvVVBJdmVJeWdTS3E1ZyQ2ejJFV3lJUDgvdTBnMjZ1R0JRaVhEQWZHSHNSU0RNSVRUazAwL2dBUytrAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "WWW user" } ;
  upsert_admin_exn
    { username = "ops"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkZm0zUzhSUXhNQ3loWkI3bTlMRDhzQSRBWDdEbGNGYzIyVDJzb3lLc2V4ODlIdEtBY25uZllDN3VXa2FodVBvdzFvAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "Ops machinery" } ;
  upsert_admin_exn
    ~validate:false
    { username = "sample"
    ; password = ""
    ; email = "nouser@example.com"
    ; name = "Sample Owner" } ;
  upsert_admin_exn
    { username = "korede"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkRGVxb0M0dXJUYkltWWdlYmRidGQxZyRXTHNrRTErTThscmwvRUlIVGoxUFpVVE5nNDdNQ0FqVHZRWHFvMVFjUkI4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "korede@darklang.com"
    ; name = "Korede" }


let upsert_banned_accounts () : unit =
  ignore
    ( banned_usernames
    |> List.map ~f:(fun username ->
           upsert_account_exn
             ~validate:false
             { username
             ; password =
                 "" (* empty string isn't a valid hash, so can't login *)
             ; email = "ops@darklang.com"
             ; name = "Disallowed account" } ) ) ;
  ()


let init () : unit =
  if Config.create_accounts
  then (
    init_testing () ;
    upsert_banned_accounts () ;
    upsert_admins () ;
    upsert_useful_canvases () ;
    () )


module Testing = struct
  let validate_username = validate_username

  let validate_email = validate_email

  let validate_password = validate_password
end
