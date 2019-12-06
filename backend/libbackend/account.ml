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
  ; password : Password.t
  ; email : string
  ; name : string }

type user_info =
  { username : username
  ; email : string
  ; name : string
  ; admin : bool }
[@@deriving yojson]

type user_info_and_created_at =
  { username : username
  ; email : string
  ; name : string
  ; admin : bool
  ; created_at : string }
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


let validate_email (email : string) : (unit, string) Result.t =
  (* just checking it's roughly the shape of an email *)
  let regex = Re2.create_exn ".+@.+\\..+" in
  if Re2.matches regex email
  then Ok ()
  else Error ("Invalid email '" ^ email ^ "'")


let validate_account (account : account) : (unit, string) Result.t =
  validate_username account.username
  |> Prelude.Result.and_ (validate_email account.email)


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
                  password = EXCLUDED.password"
        ~params:
          [ Uuid (Util.create_uuid ())
          ; String account.username
          ; String account.name
          ; String account.email
          ; String (Password.to_bytes account.password) ] )


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
          ; String (Password.to_bytes account.password) ] )


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


let get_user_and_created_at username =
  Db.fetch_one_option
    ~name:"get_user_and_created_at"
    ~subject:username
    "SELECT name, email, admin, created_at from accounts
     WHERE accounts.username = $1"
    ~params:[String username]
  |> Option.bind ~f:(function
         | [name; email; admin; created_at] ->
             Some
               { username
               ; name
               ; admin = admin = "t"
               ; email
               ; created_at =
                   created_at
                   |> Db.date_of_sqlstring
                   |> Core.Time.to_string_iso8601_basic
                        ~zone:Core.Time.Zone.utc }
         | _ ->
             None )


let get_user_by_email email =
  Db.fetch_one_option
    ~name:"get_user_by_email"
    ~subject:email
    "SELECT name, username, admin from accounts
     WHERE accounts.email = $1"
    ~params:[String email]
  |> Option.bind ~f:(function
         | [name; username; admin] ->
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


(* Any external calls to this should also call Stroller.segment_identify_user;
 * we can't do it here because that sets up a module dependency cycle *)
let set_admin ~username (admin : bool) : unit =
  Db.run
    ~name:"set_admin"
    ~subject:username
    "UPDATE accounts SET admin = $1 where username = $2"
    ~params:[Bool admin; String username]


(* Returns None if no valid user, or Some username _from the db_ if valid. Note:
 * the input username may also be an email address.
 *
 * No need to detect which and SQL differently; no valid username contains a
 * '@', and every valid email address does. [If you say 'uucp bang path', I will
 * laugh and then tell you to give me a real email address.] *)
let valid_user ~(username : username) ~(password : string) : string option =
  match
    Db.fetch_one_option
      ~name:"valid_user"
      ~subject:username
      "SELECT username, password from accounts
           WHERE accounts.username = $1 OR accounts.email = $1"
      ~params:[String username]
  with
  | Some [db_username; db_password] ->
      if password
         |> Bytes.of_string
         |> Hash.wipe_to_password
         |> Hash.verify_password_hash
              (Bytes.of_string (B64.decode db_password))
      then Some db_username
      else None
  | None | _ ->
      None


let can_access_operations ~(username : username) : bool = is_admin ~username

(* Returns None if no valid user, or Some username _from the db_ if valid. Note:
 * the input username may also be an email address *)
let authenticate ~(username : username) ~(password : string) : string option =
  valid_user ~username ~password


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

(* Any external calls to this should also call Stroller.segment_identify_user;
 * we can't do it here because that sets up a module dependency cycle *)
let upsert_user ~(username : string) ~(email : string) ~(name : string) () :
    (string, string) Result.t =
  let plaintext = Util.random_string 16 in
  let password = Password.from_plaintext plaintext in
  upsert_account {username; email; name; password}
  |> Result.map ~f:(fun () -> plaintext)


let init_testing () : unit =
  upsert_account_exn
    { username = "test_unhashed"
    ; password = Password.from_hash "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test+unhashed@darklang.com"
    ; name = "Dark OCaml Tests with Unhashed Password" } ;
  upsert_account_exn
    { username = "test"
    ; password = Password.from_plaintext "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test@darklang.com"
    ; name = "Dark OCaml Tests" } ;
  upsert_admin_exn
    { username = "test_admin"
    ; password = Password.from_plaintext "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test+admin@darklang.com"
    ; name = "Dark OCaml Test Admin" } ;
  ()


let upsert_admins () : unit =
  upsert_admin_exn
    { username = "ian"
    ; password =
        Password.from_hash
          "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkOXd2R3BSYW54Y3llYmdRdU1EMHdUdyRNN1ljWVFQdDk0S29nM1EyM1Q2cHFRZDRlMk9VM3lDTmpreUZ2NGIva1o4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ian@darklang.com"
    ; name = "Ian Connolly" } ;
  upsert_admin_exn
    { username = "paul"
    ; password =
        Password.from_hash
          "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcEQxWXBLOG1aVStnUUJUYXdKZytkQSR3TWFXb1hHOER1UzVGd2NDYzRXQVc3RlZGN0VYdVpnMndvZEJ0QnY1bkdJAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "paul@darklang.com"
    ; name = "Paul Biggar" } ;
  upsert_admin_exn
    { username = "ellen"
    ; password =
        Password.from_hash
          "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcHcxNmRhelJaTGNrYXhZV1psLytXdyRpUHJ1V1NQV2xya1RDZjRDbGlwNTkyaC9tSlZvaTVWSTliRlp0c2xrVmg0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ellen@darklang.com"
    ; name = "Ellen Chisa" } ;
  upsert_admin_exn
    { username = "alice"
    ; password =
        Password.from_hash
          "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkVGllNGtJT3kyMVFjL1dGUnhScC9PdyROMnp1ZVZnczhIcjl0ODZEREN2VFBYMVNHOE1Za1plSUZCSWFzck9aR1J3AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "alice@darklang.com"
    ; name = "Alice Wong" } ;
  upsert_admin_exn
    { username = "ismith"
    ; password =
        Password.from_hash
          "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkbHlXamc0MHA3MWRBZ1kyTmFTTVhIZyRnaWZ1UGpsSnoxMFNUVDlZYWR5Tis1SVovRFVxSXdZeXVtL0Z2TkFOa1ZnAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ismith@darklang.com"
    ; name = "Ian Smith" } ;
  upsert_admin_exn
    { username = "sydney"
    ; password =
        Password.from_hash
          "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkMDJYZzhSS1RQai9JOGppdzI5MTBEUSRJdE0yYnlIK29OL1RIdzFJbC9yNWZBT2RGR0xrUFc3V3MxaVpUUUVFKytjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "sydney@darklang.com"
    ; name = "Sydney Noteboom" } ;
  upsert_admin_exn
    { username = "korede"
    ; password =
        Password.from_hash
          "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkRGVxb0M0dXJUYkltWWdlYmRidGQxZyRXTHNrRTErTThscmwvRUlIVGoxUFpVVE5nNDdNQ0FqVHZRWHFvMVFjUkI4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "korede@darklang.com"
    ; name = "Korede" } ;
  upsert_admin_exn
    { username = "julian"
    ; password =
        Password.from_hash
          "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkRmFQeGdPZXhaZTFZL2pSYkZ4azFNQSRFR0ZyNEkyeDVqaDIvL243UEIzeUhkcTIwaUZUUi91RXE1RDJkQ0o0eE5BAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "julian@darklang.com"
    ; name = "Julian Ceipek" } ;
  upsert_admin_exn
    { username = "dean"
    ; password =
        Password.from_hash
          "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkWjdFdjJlZ2ZmMnZRaFhjQWlpOWlPZyRrL2F1bGFEU0tra3BQMmNKTHF6S3NaU3d5WXdmWm1pNkQ4Yy96alJrT3YwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "dean@darklang.com"
    ; name = "Dean Strelau" } ;
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
  (* Needed for tests *)
  upsert_admin_exn
    ~validate:false
    { username = "sample"
    ; password = Password.invalid
    ; email = "nouser@example.com"
    ; name = "Sample Owner" }


let upsert_banned_accounts () : unit =
  ignore
    ( banned_usernames
    |> List.map ~f:(fun username ->
           upsert_account_exn
             ~validate:false
             { username
             ; password = Password.invalid
             ; email = "ops+" ^ username ^ "@darklang.com"
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
end
