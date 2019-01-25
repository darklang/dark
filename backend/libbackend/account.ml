open Core_kernel
open Libexecution
open Types
module Hash = Sodium.Password_hash.Bytes

type username = string

type account =
  { username : username
  ; password : string
  ; email : string
  ; name : string }

type user_info =
  { username : username
  ; email : string
  ; name : string }

(************************)
(* Adding *)
(************************)
let upsert_account (account : account) : unit =
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
      ; String account.password ]


let upsert_admin (account : account) : unit =
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
      ; String account.password ]


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
    ~subject:(username)
    "SELECT id from accounts
     WHERE accounts.username = $1"
    ~params:[String username]
  |> Option.map ~f:List.hd_exn
  |> (fun x ->  match x with
        Some sid -> Uuidm.of_string sid
      | None -> None)


let get_user username =
  Db.fetch_one_option
    ~name:"get_user"
    ~subject:username
    "SELECT name, email from accounts
     WHERE accounts.username = $1"
    ~params:[String username]
  |> Option.bind ~f:(function
         | [name; email] ->
             Some {username; name; email}
         | _ ->
             None )


let get_users () =
  Db.fetch ~name:"get_users" "SELECT username from accounts" ~params:[]
  |> List.map ~f:List.hd_exn


(************************)
(* Check access *)
(************************)

let is_admin ~username : bool =
  Db.exists
    ~subject:username
    ~name:"is_admin"
    "SELECT 1 from accounts
     WHERE accounts.username = $1
       AND accounts.admin = true"
    ~params:[String username]


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
  (* Temporarily allow passwords that either equal what's in
     the database, or that hash to what's in the database. *)
  | Some [db_password] ->
      password
      |> Bytes.of_string
      |> Hash.wipe_to_password
      |> Hash.verify_password_hash (Bytes.of_string (B64.decode db_password))
  | _ ->
      false


let can_access_operations ~(username : username) : bool = is_admin ~username

let can_edit_canvas ~(auth_domain : string) ~(username : username) : bool =
  String.Caseless.equal username auth_domain
  || String.Caseless.equal "sample" auth_domain
  || is_admin username


type permissions =
  | CanEdit
  | CanAccessOperations
  | NoPermission

let get_permissions ~(auth_domain : string) ~(username : username) () :
    permissions =
  if can_access_operations ~username
  then CanAccessOperations
  else if can_edit_canvas ~auth_domain ~username
  then CanEdit
  else NoPermission


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
  Db.fetch_one_option
    ~name:"owner"
    ~subject:auth_domain
    "SELECT id from accounts
     WHERE accounts.username = $1"
    ~params:[String (String.lowercase auth_domain)]
  |> Option.map ~f:List.hd_exn
  |> Option.bind ~f:Uuidm.of_string


let auth_domain_for host : string =
  match String.split host '-' with d :: _ -> d | _ -> host


let for_host (host : string) : Uuidm.t =
  host
  |> auth_domain_for
  |> owner
  |> fun o -> Option.value_exn ~message:("No owner found for host " ^ host) o


(************************)
(* Darkinternal functions *)
(************************)

let upsert_user ~(username : string) ~(email : string) ~(name : string) () :
    string =
  let plaintext = Util.random_string 16 in
  let password = hash_password plaintext in
  upsert_account {username; email; name; password} ;
  plaintext


let init_testing () : unit =
  upsert_account
    { username = "test_unhashed"
    ; password = "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test@darklang.com"
    ; name = "Dark OCaml Tests with Unhashed Password" } ;
  upsert_account
    { username = "test"
    ; password = hash_password "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test@darklang.com"
    ; name = "Dark OCaml Tests" } ;
  upsert_admin
    { username = "test_admin"
    ; password = hash_password "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test@darklang.com"
    ; name = "Dark OCaml Test Admin" } ;
  ()


let upsert_admins () : unit =
  upsert_admin
    { username = "ian"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkOXd2R3BSYW54Y3llYmdRdU1EMHdUdyRNN1ljWVFQdDk0S29nM1EyM1Q2cHFRZDRlMk9VM3lDTmpreUZ2NGIva1o4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ian@darklang.com"
    ; name = "Ian Connolly" } ;
  upsert_admin
    { username = "paul"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcEQxWXBLOG1aVStnUUJUYXdKZytkQSR3TWFXb1hHOER1UzVGd2NDYzRXQVc3RlZGN0VYdVpnMndvZEJ0QnY1bkdJAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "paul@darklang.com"
    ; name = "Paul Biggar" } ;
  upsert_admin
    { username = "ellen"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcHcxNmRhelJaTGNrYXhZV1psLytXdyRpUHJ1V1NQV2xya1RDZjRDbGlwNTkyaC9tSlZvaTVWSTliRlp0c2xrVmg0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ellen@darklang.com"
    ; name = "Ellen Chisa" } ;
  upsert_admin
    { username = "stefi"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkKzIyM3lwOWU4ZzdvOVFWN1RtbnpYQSQ2b0VsdTBLU0JzendBR3FDb1FUQVoyVGNlcERDaUZ4OE9haFRWbzZMTk9VAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "stefi@darklang.com"
    ; name = "Stefi Petit" } ;
  upsert_admin
    { username = "alice"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkVGllNGtJT3kyMVFjL1dGUnhScC9PdyROMnp1ZVZnczhIcjl0ODZEREN2VFBYMVNHOE1Za1plSUZCSWFzck9aR1J3AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "alice@darklang.com"
    ; name = "Alice Wong" } ;
  upsert_admin
    { username = "ismith"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkbHlXamc0MHA3MWRBZ1kyTmFTTVhIZyRnaWZ1UGpsSnoxMFNUVDlZYWR5Tis1SVovRFVxSXdZeXVtL0Z2TkFOa1ZnAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ismith@darklang.com"
    ; name = "Ian Smith" } ;
  upsert_admin
    { username = "shamus"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcE9ZT1AwYUpML25xbXBPbWFoRHdBQSR4Q2RlaFZFMEQzeTNOWnRiVEtRUlFwSDUwR1Eydm5MbnZOQ1M0dmJ6VDJRAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "jeremy@darklang.com"
    ; name = "Jeremy Morony" } ;
  (* dark contractors *)
  upsert_account
    { username = "lizzie"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkWTNueDFWQUFYRWpLMjJGclcwMjU2ZyRYVDQxUGtGNnYyM1E4L0MrSUZITlNXNi8wUGN4TFdEbkRMZ0xVdHN2bHJZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "_@lizzie.io"
    ; name = "Lizzie Dixon" } ;
  upsert_account
    { username = "samstokes"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcVViUzB0USt0ZEE5WDBIUm1MME5BQSQydktjVHV2TG9FMFhNOHg3MHJZelNwQUpLT09YeFpKOVFYb2Y3NU9vMmQ0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "me@samstokes.co.uk"
    ; name = "Sam Stokes" } ;
  ()


(* accounts to create namespaces for dark canvases *)
let upsert_useful_canvases () : unit =
  upsert_account
    { username = "builtwithdark"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkMUdwN0luSFJEbllrMGw5dnR1NTBzdyRMazhVSUdOZU9tTm5SMVFwbDRHUGs3VHdzRXQwbTQ5QUFTdjJQdlZpd1pjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "Built with Dark" } ;
  upsert_account
    { username = "benchmarking"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkdS9vQml2Uy9pa2ZnUlFEeHYvcVhJdyQxcVNNenExVnE2THdWMElVKyswNDRWbEpsYmk3d1NZaFZySzNXUEIwRkw4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "Dark benchmarking" } ;
  upsert_account
    { username = "www"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkTHhZV0RvZEFvVVBJdmVJeWdTS3E1ZyQ2ejJFV3lJUDgvdTBnMjZ1R0JRaVhEQWZHSHNSU0RNSVRUazAwL2dBUytrAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "WWW user" } ;
  upsert_admin
    { username = "ops"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkZm0zUzhSUXhNQ3loWkI3bTlMRDhzQSRBWDdEbGNGYzIyVDJzb3lLc2V4ODlIdEtBY25uZllDN3VXa2FodVBvdzFvAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "Ops machinery" } ;
  upsert_admin
    { username = "sample"
    ; password = ""
    ; email = "nouser@example.com"
    ; name = "Sample Owner" }


let init () : unit =
  if Config.create_accounts
  then (
    init_testing () ;
    upsert_admins () ;
    upsert_useful_canvases () ;
    () )
