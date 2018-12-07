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
    ; name = "Dark OCaml Test Admin" }


let init () : unit =
  init_testing () ;
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
    { username = "zane"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkUThFd2wrNW1vRTlSWUFzK1VpOWVxQSQ3NWwyNXFKZ2RWdzFqOEhuRDZsaGJGQkVGVFRFQzRwYlZkTlpPOUVHRlhrAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "zane@darklang.com"
    ; name = "Zane Shannon" } ;
  upsert_admin
    { username = "alice"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkVGllNGtJT3kyMVFjL1dGUnhScC9PdyROMnp1ZVZnczhIcjl0ODZEREN2VFBYMVNHOE1Za1plSUZCSWFzck9aR1J3AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "alice@darklang.com"
    ; name = "Alice Wong" } ;
  upsert_admin
    { username = "jonathan"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEka05jYURTWEdadENHSjhLODBvMDAzZyRuOTFUcGlHZ0NkQ0M0MVZEUGVaVms4M0t2S0RYR0pwaUxxUS9VNjhZbHJRAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "jonathan.laurent@cs.cmu.edu"
    ; name = "Jonathan Laurent" } ;
  upsert_account
    { username = "lizzie"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkWTNueDFWQUFYRWpLMjJGclcwMjU2ZyRYVDQxUGtGNnYyM1E4L0MrSUZITlNXNi8wUGN4TFdEbkRMZ0xVdHN2bHJZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "_@lizzie.io"
    ; name = "Lizzie Dixon" } ;
  upsert_account
    { username = "dabblefox"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkREVrTlVRYlFoMXFwOG9DeTNCQ3hNQSRvM0M1c2VNSkpCclE1dUhVWXdhT21mcFNqRytNQm1SM1hhMTBTRVQwWnEwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "daniel@dabblefox.com"
    ; name = "Daniel Clayson" } ;
  upsert_account
    { username = "lee"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkWlZyMnMrWTNNMDgweDYrckY5a2JVUSRqUWZLdTdvQjcwRXQveFdxQStjNzlCYXJLcFplMkNuVnFxaHJvS0RYZnlBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "lee@ledwards.com"
    ; name = "Lee Edwards" } ;
  upsert_account
    { username = "alexey"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkTFVqQ1lFM2MyS2ZMNXF2aHVaMWkzdyRldGpXS2tzZUw1LzJ1dzExVDV1TnBQaGNmcVBtcDR2cE1YTEdXZ3Y5ZHQ0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "me@alexey.ch"
    ; name = "Alexey Klochay" } ;
  upsert_account
    { username = "steve"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkME1MVk5yNmM4VTVHaGFWZ083ajdaQSQ5ajZmNXFub0RDTENGOGdzZnZ5Slc5ZHVCTlFvMVFpMmdLTldObkIxaGVZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "steveykrouse@gmail.com"
    ; name = "Steve Krouse" } ;
  upsert_account
    { username = "tom"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEka0RPbmhlUnpCVVpPUXB0UC9YSzZXdyQ4OTJsY29MOURRb3FhWFVkcnJhbEFhZG5EOUI2Myt1WHhpclR2ZnVjUnJBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "Tom Rudick"
    ; name = "tmrudick@gmail.com" } ;
  upsert_account
    { username = "justin"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkM2lvSFBobHI2eTQzVTY5NzYyUTNWQSRWWVREY0IveStyRGVUVXlQTDczd1pGT1U4OXNnUHRldFpFeG1xenA3OUpJAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "justinc474@gmail.com"
    ; name = "Justin Cowperthwaite" } ;
  upsert_account
    { username = "jakub"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkWDUvbmovcFhZOWVFSkpZZFZVc1RlZyRONGhOVVFSL0F4bWtCRlBxU2NYU1pLbkF1SG5iUVh2Zkg3UnIxMjltcks0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "jakub.jurovych@gmail.com"
    ; name = "Jakub Jurovych" } ;
  upsert_account
    { username = "pvh"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkSjJ5Z2JINlllTnBOd3c2bnlXTk9YZyRxbWgzYTFWaE9oQUVkYXZOMXBIYWtqT1pTV1VnNnZ6TXBpNUJHMUlHUkcwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "pvh@pvh.ca"
    ; name = "Peter Van Hardenberg" } ;
  upsert_account
    { username = "mmcgrana"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkUnhCWmNRTEhBcW1lblN0UEVBN2NDUSQyNXJUWlV2WXhqNVlub2twNUh4ekIyMzdMMnF0eUdCK0lkU29kOW1jMUQ4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "mmcgrana@gmail.com"
    ; name = "Mark McGranaghan" } ;
  upsert_account
    { username = "darksingleinstance"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkNnMxZzdUbjhDVEVZZTJBd1R3Q0JqUSQydWVtZ0tQMkVuOGRzeFhGdHB2V3YybzdTWm9ZUzIzQ0pyV3dUdkUwMHJZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "Dark Single Instance" } ;
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
    { username = "vlymar"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkSUFaZGN0R0ZlZlF2bkluZ2l5VDFiQSRlL3NuRmRwWHdsT2VabHAvZngzelB3aVR4MnJ2clEza0p1dko5cEZjTTRjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "victorslymar@gmail.com"
    ; name = "Victor Lymar" } ;
  upsert_account
    { username = "listo"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkL2FRRk05WkZ5bjg0WnNSS3dHeEZrZyQzbzJCbmpZUGFnc1I2bUZsYTFkaXNvb003T1J4cnlyYzhJclBEaE9uV2dzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "c@cpo.is"
    ; name = "Chase" } ;
  upsert_account
    { username = "gracey"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkVmlGVjV5VDhFZnpJM01GN2RMdUpYdyRZeVlIOHBXaDR5ekZpSXNZUllDV21GUC9GdlQxeThML1krOE1ZeDYzbUhzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "gracey.wilson@students.olin.edu"
    ; name = "Gracey Wilson" } ;
  (* horizons hackathon people *)
  upsert_account
    { username = "jaroslav"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkaHdmNGhuQnJ1alI2MThWT2hKbFVmQSRUZ1pIL2J6dTFudXpLSU1XZDFrVDVIVVRpYTZpZUs5eTJUTEJWaTBCZ2Z3AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "jaroslav.tran@gmail.com"
    ; name = "Jaroslav Tran" } ;
  upsert_account
    { username = "owen"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkSnp0SlVKaHg1N0xjRERWd0N5bDU5USRhajBPOEw2STIycmtYUU9uQkRIL2VlbG5aOE82enRxTlcveHNBSEZhdmVJAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "owenzhang76@gmail.com"
    ; name = "Owen Zhang" } ;
  upsert_account
    { username = "abraham"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkL0dxU0pXUjFLbGsyTE5wSTlUSmFDZyQyVFVPeUx6bld6WUNMTFV3WEJnaFlvaWZWVmt4ZDlNUHppb2pLb0FMY2tjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "abraham.hamidi01@gmail.com"
    ; name = "Abraham Hamidi" } ;
  upsert_account
    { username = "lydia"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkSVlId3hDT2p2RlJHTkl4LzVObUlQdyQ1M1ZINmJSbUpvRXRCWnJHM3lMM0NzYTlGNk1VRzVIQ1VEYVhUTG9UUmtJAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "xinglydia@gmail.com"
    ; name = "Lydia Xing" } ;
  upsert_account
    { username = "ronil"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkWTRERW5DL0VBSjNwTUNMZnJMVUZ3dyRnT09ZS29FZUJkd1g4K0o0VU9jR3k0aDFMYzBXRjVDR1RnVytNS2pOb1RNAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ronil.awale@gmail.com"
    ; name = "Ronil Awale" } ;
  upsert_account
    { username = "sarayu"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkRkdwZ1grVkJ0V0hINm1YK08vVS90dyRNYklDZVZhblVoa2RRd3BtRVArd29wNzJId0YwZzlWK1VvL0tkbk1UWXBzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "sarayu@namineni.com"
    ; name = "Sarayu Namineni" } ;
  upsert_account
    { username = "kahuang"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkQk9EeGUwK29oUytvUlMydi9sc3huQSR4ZVVZN2tUUjNvR21SVVp6ZTRKRlJmZVVLOEFDUllxbTNtbDRlbzJxNExNAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "kahuang@ucdavis.edu"
    ; name = "Kahuang" } ;
  upsert_account
    { username = "mike"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkTVB3UUJvNVptNFl1YXhmYUxCOHhMQSRRS0M1SWpNTFh6bUVHdk1RYnIyVzBOZThtRzFUandWN3VBQ1VVbzBkbklZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "mikesun20@gmail.com"
    ; name = "Mike Sun" } ;
  upsert_account
    { username = "amit"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkSndkMHVOWjlybGpwQlVSL2hzVElXUSQvbFpxWEwxV3JKNVQzVVUwdldXenRUOGVPdW9hZUp6THRZQTA5UzZQbmxFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "amitsant.2000@gmail.com"
    ; name = "Amit Sant" } ;
  upsert_account
    { username = "rohankanchana"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkUXNLVnVFZzNDOGRNclZ2eFBJalo1USR3YzI3cUg4R3ZqOFpnSGZWRktDLzFSSnRBVW0rYVFYb3RxSVNSRDcxWlFBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "rohankanchana@gmail.com"
    ; name = "rohankanchana" } ;
  upsert_account
    { username = "kyra"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkb0hjUTRKbXBNTm1kNnFKbUJYWDM0USRnVkpwWGFBdzVicUxwWDVOdWIvdVBNaTBZM0llWXhjL0o3cFJVRE1mUDdVAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "kyraskraft@gmail.com"
    ; name = "Kyra Kraft" } ;
  upsert_account
    { username = "david"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkSjhBYlMzMkVVT1pweS9Kd2o4K20wUSQyOFVuMWJaRVZ6dUt2N2tXbUt2bHRtYzJsZkxWckxLNkZ2TFR3cmx4WEswAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "davidrusarm@gmail.com"
    ; name = "David Storozhenko" } ;
  upsert_account
    { username = "thais"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkK0pXOEhGYjBBenJuaWp6WktuMytTdyRuN201SjUxUTQ0T3hOSno4NHZ0cmtkVWMyeGJQU1EvdEIvdy9UWHpvbGdVAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "gonzo424@gmail.com"
    ; name = "Thais Gonzalez" } ;
  upsert_account
    { username = "diego"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkM2dDOVV0d3RWVGtWM2RydEJ3Nm9xZyQrRUNDamFUUXJ4QnNSS2VBTEtjcG1vSWRFWmliWS9VMHFSRmNuNlBmV1Y0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "dibarra@uchicago.edu"
    ; name = "Diego Ibarra" } ;
  upsert_account
    { username = "liam"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkZ1RhOUZoSFRiS0M1OFdmd3FCazlHdyRYVFZBVjFQZHVZeUE4SVNWa1BGanFWVTdKYnpPeUZrOHlUZHV2OG5vTzNBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ldtrampota@gmail.com"
    ; name = "Liam Trampota" } ;
  upsert_account
    { username = "kitan"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkaUVSNGJWQUdscFF3UGxQSGxURHhnQSRMOGZnTkZob0dQK1pSZThKY2dhTzhGa0ltSjVldHhrTzduNkF5MGwwcURFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "kitgarcia@ucdavis.edu"
    ; name = "Kitan Garcia" } ;
  upsert_account
    { username = "rahul"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkVk54d2FHazZyRXNqSkNPMUtRMWFYZyRYbUhpNGV5dFQ0d1JvMDI5WElEYUw0TEhyRlNmeHN4UlhlSFdjY29XS1BzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "mail4rp@gmail.com"
    ; name = "rahul prasad" } ;
  upsert_account
    { username = "perry"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkS0NocjlReHNtWEhsV0wwVXY5RnJ3ZyRUQmFQcGFaOFlXQ3R5ZDNDaWFBbzhDRVIxYkwzb1Q1M1BISU81ZytXaVlnAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "perry.ya@nyu.edu"
    ; name = "perry ya" } ;
  upsert_account
    { username = "howard"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkNmF3OFF0SHZkY1JlRVQ5S1ZHM3BtZyQweklWdjMvS21GOHRBdHBITUNxb3Ntb1l6VVVjOFYzVTZET0tsS0ZDL0tvAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "chonghorizons@gmail.com"
    ; name = "howard chong" } ;
  upsert_account
    { username = "quoc"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcVI4Q1VhWFZrOExVejNrWnFPeW5odyROL3JseEh3NmdWMFlLMTFLamwwcVN3b2N4VXlXK2F5SUJwQzV3VXpmVXVNAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "qnguyen6@binghamton.edu"
    ; name = "quoc nguyen" } ;
  upsert_account
    { username = "humad"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkdWNhK1BLZHpkSXBCNEEyd0VmUjhEUSRVM3RJUDk0VTBRQ0tzM3NNUVJrWnMyYVkxNDdOTGZxYjNzeEl5WHhLaDc0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "humadshah@umass.edu"
    ; name = "humad shah" } ;
  upsert_account
    { username = "sean"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkR0ZRcXhjRHB0Q01lamhUU3FSOGo0dyQvekIxVjNONTNrNmE4eGdoV0dpYVN1SHJQbU42OGxhQjhsS2VQV1dMY2dBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "seandermanyang@gmail.com"
    ; name = "sean derman" } ;
  upsert_account
    { username = "asheesh"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkSGppdHVnOVBjb2FFejRvVmFOMkhFdyR5Z203Q3dIUFNybTZXaUQ4RnYrcmFCOHRJUnVhbFpQVFVud0txQmo4K0w0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "asheeshchopra11@gmail.com"
    ; name = "asheesh chopra" } ;
  upsert_account
    { username = "sajid"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkK2VNdk9QenBOclVZb0dwajd0cG1PQSQwSG9RZUlOckZmM3hReHNTUTV5a3B1Y1RGdU5ROXBYR21GSkRLeW9KQXNNAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "sajsay6@yahoo.com"
    ; name = "Sajid Sayeed" } ;
  upsert_account
    { username = "raj"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkUThWT051VzBLanJiVnN4M3BhQVlpdyRGbGRYbllVbVVMYVh4OEF1aVp4QWlMK0JBVUgxeU9QQmJ4RTBTUk5yWGtjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "raj.sinhaii@hotmail.com"
    ; name = "Raj Sinha" } ;
  upsert_account
    { username = "juan"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkRmVEaitDMjc4ZnIraytGMnhoT05IQSRBZ0tMd2UyaS9BU093L0dkMmdZVGc4ajRPRHdsTlhWOEFtMjVRKzhsQlNrAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "jddonneys@gmail.com"
    ; name = "Juan Diego Donneys" } ;
  upsert_account
    { username = "tim"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcFhsLzhvQXJybjl5MFJVeEU0bWZ0dyRLZ1JpQUNMZm4xaG45TldWWnl6d2k1Ym1LYWNEZFJqaVlmMmRhZyt1aVFVAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "tchang2017@gmail.com"
    ; name = "Tim Chang" } ;
  (* end Horizons *)
  upsert_account
    { username = "jen"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkMWZ4MlNBc0tzOUNOYllMWEpSczBCUSRYMXZ3cEgrbVM3cUM4NWZhSEdMaklobUpjNDk0QVpQenhpbFRDdm1zZWlBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "j.aprahamian@gmail.com"
    ; name = "Jen Aprahamian" } ;
  upsert_account
    { username = "ross"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkOGdyN0J6QnpQSWF4S21DdFVzT2tZZyRIZHFlUG9ydnpLSzlHZlB3VWRDN2VmN3NJZit2WGpWRDNod0pDS0xKSCtVAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ross.campbell.ucsd@gmail.com"
    ; name = "Ross Campbell" } ;
  upsert_account
    { username = "www"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkTHhZV0RvZEFvVVBJdmVJeWdTS3E1ZyQ2ejJFV3lJUDgvdTBnMjZ1R0JRaVhEQWZHSHNSU0RNSVRUazAwL2dBUytrAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "WWW user" } ;
  upsert_account
    { username = "elliot"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkV1BrajJVWTd0eG1BYzFYd21NMHo5ZyRCcGFuOVBUSDZyRHBzenRybVZSOWF0ZUVZKytRY29uU2x6SjVjc3dvdERrAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "elliot@venturemedias.com"
    ; name = "Elliot Nash" } ;
  upsert_account
    { username = "danielle"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkZlhpcVdvc1FFbGVSN1B1K0xhZHl6QSQyU1FoZFgva2M0YUNqcjZFQ1VxMDBmN2RpeDB0bzBOM3hHRWl3ME5SblJFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "morrilldanielle@gmail.com"
    ; name = "Danielle Morill" } ;
  upsert_account
    { username = "kevin"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkTHdKMmlackdZem9MTkFCRE54U0gydyRBcjJsQ0NoNzJlTGRQbjgrTVNZbFpKOFNIKzF2eE1oL1RUbnF3SVUxNkpNAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "morrillkevin@gmail.com"
    ; name = "Kevin Morill" } ;
  upsert_account
    { username = "kate"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkbEt3VWw0Y05vU0U4MWNyQmtyOUF5QSRxMllZaXJYdjhwdmZxKzZMUVM4WmsrUnJUb3I3YXQ4aU1MN2w5Mkl1NlpjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "kate.heddleston@gmail.com"
    ; name = "Kate Heddleston" } ;
  upsert_account
    { username = "aaron"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkZ1BmcDM5OStrTVR4VEdEU2dpamFmQSR0SjFsM3JtRUU1THhzc2ZYZXUyREVCblZIbXR2NHRLK3p1dEhvR0xJWHlvAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "aaronjlevy@gmail.com"
    ; name = "Aaron Levy" } ;
  upsert_account
    { username = "logan"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkQnh1UUlqRmxjd2JCdjhZcXJpdWxDdyRVVTZyZFhKYzAvSEJDN0pFeXVEWk1DdjdFbm9ZVHdjZWZTZHlkazhwMEVBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "loganm1881@gmail.com"
    ; name = "Logan McPhail" } ;
  upsert_account
    { username = "darragh"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkaFlQYjZ3RGtrTVk5SzNpdzhJUmNMZyRCV0VaU0hrd1FKVG1wZEZhL0s1ZEI5VG8zcEQ3dkNzeXBlaHlRdm1PdjZRAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "darragh.buckley@gmail.com"
    ; name = "Darragh Buckley" } ;
  upsert_account
    { username = "jacob"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkSkhyRjlrbEgzUWpjdkZCamg3dGtMQSRvdFVJTjF2UCs4d21FYWFVU1ZCR1ZSRWJ4TTRlZ3diYWtQakZGSDVEL0kwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "wenger.jacob@gmail.com"
    ; name = "Jacob Wenger" } ;
  upsert_admin
    { username = "ops"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkZm0zUzhSUXhNQ3loWkI3bTlMRDhzQSRBWDdEbGNGYzIyVDJzb3lLc2V4ODlIdEtBY25uZllDN3VXa2FodVBvdzFvAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "ops@darklang.com"
    ; name = "Ops machinery" } ;
  upsert_account
    { username = "russ"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkWE1uc0pGVUhsNnZ4R1dSSDlEdXl0USQrVkpvcS9IaUVGMGxaeWpmSmowakNFK2ViQTVXNWplMlFzZUpIcE8xR3hnAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "russ@rainforestqa.com"
    ; name = "Russell Smith" } ;
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
  upsert_account
    { username = "samstokes"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcVViUzB0USt0ZEE5WDBIUm1MME5BQSQydktjVHV2TG9FMFhNOHg3MHJZelNwQUpLT09YeFpKOVFYb2Y3NU9vMmQ0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "me@samstokes.co.uk"
    ; name = "Sam Stokes" } ;
  upsert_admin
    { username = "sample"
    ; password = ""
    ; email = "nouser@example.com"
    ; name = "Sample Owner" } ;
  upsert_account
    { username = "jim"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkbGxyeFlHb0NrRFNlTFNQNENYcFNIQSRTMEc5NWc0VTd1TUVoSU84OSt6VkN4Q016TnUrOWU3M0Vaa1pMZzd3VjFvAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "james.t.switzer@gmail.com"
    ; name = "James T Swifter" } ;
  upsert_account
    { username = "eamo"
    ; password =
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkVGZUMElBZEVDU1g4TFEyeVNLVy9LQSRyNm5CV2l2ZWJmbk4xdUtHMk1CSVlraDJvRnd5SGh4clJaYUQ4L2hQa3ZNAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "eamo@eamo.net"
    ; name = "Eamon Leonard" } ;
  ()
