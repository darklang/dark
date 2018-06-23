open Core_kernel
open Libexecution

open Types

module Dbp = Dbprim

type username = string

type account = { username: username
               ; password: string
               ; email: string
               ; name: string
               }

(************************)
(* Adding *)
(************************)
let upsert_account (account:account) : unit =
  Db.run
    ~name:"upsert_account"
    "INSERT INTO accounts
    (id, username, name, email, admin, password)
    VALUES
    ($1, $2, $3, $4, false, $5)
    ON CONFLICT (username)
    DO UPDATE SET name = EXCLUDED.name,
                  email = EXCLUDED.email,
                  admin = false,
                  password = EXCLUDED.password"
    ~params:[ Uuid (Util.create_uuid ())
            ; String account.username
            ; String account.name
            ; String account.email
            ; String account.password]

let upsert_admin (account:account) : unit =
  Db.run
    ~name:"upsert_admin"
    "INSERT INTO accounts as u
    (id, username, name, email, admin, password)
    VALUES
    ($1, $2, $3, $4, true, $5)
    ON CONFLICT (username)
    DO UPDATE SET name = EXCLUDED.name,
                  email = EXCLUDED.email,
                  admin = true,
                  password = EXCLUDED.password"
    ~params:[ Uuid (Util.create_uuid ())
            ; String account.username
            ; String account.name
            ; String account.email
            ; String account.password]


(************************)
(* Check access *)
(************************)

let is_admin ~username : bool =
  Printf.sprintf
    "SELECT 1 from accounts
     WHERE accounts.username = %s
       AND accounts.admin = true"
    (Dbp.string username)
  |> Db.exists_via_sql ~quiet:true

let valid_user ~(username:username) ~(password:string) : bool =
  Printf.sprintf
    "SELECT 1 from accounts
      WHERE accounts.username = %s
        AND accounts.password = %s"
    (Dbp.string username)
    (Dbp.string password)
  |> Db.exists_via_sql ~quiet:true


let can_edit ~(auth_domain:string) ~(username:username) : bool =
  String.Caseless.equal username auth_domain
  || is_admin ~username

let authenticate ~(username:username) ~(password:string) : bool =
  valid_user ~username ~password

let owner ~(auth_domain:string) : Uuidm.t option =
  Db.fetch_one_option
    ~name:"owner"
    "SELECT id from accounts
     WHERE accounts.username = $1"
    ~params:[ String (String.lowercase auth_domain)]
  |> Option.map ~f:List.hd_exn
  |> Option.bind ~f:Uuidm.of_string

let auth_domain_for host : string =
  match String.split host '-' with
  | d :: _ -> d
  | _ -> host


let init_testing () : unit =
  upsert_account
    { username = "test"
    ; password = "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "test@darklang.com"
    ; name = "Dark OCaml Tests"}

let init () : unit =
  init_testing ();
  upsert_admin
    { username = "ian"
    ; password = "look"
    ; email = "ian@darklang.com"
    ; name = "Ian Connolly"};
  upsert_admin
    { username = "paul"
    ; password = "what"
    ; email = "paul@darklang.com"
    ; name = "Paul Biggar"};
  upsert_admin
    { username = "ellen"
    ; password = "you"
    ; email = "ellen@darklang.com"
    ; name = "Ellen Chisa"};
  upsert_admin
    { username = "stefi"
    ; password = "made"
    ; email = "stefi@darklang.com"
    ; name = "Stefi Petit"};
  upsert_admin
    { username = "zane"
    ; password = "me do"
    ; email = "zane@darklang.com"
    ; name = "Zane Shannon"};
  upsert_account
    { username = "dabblefox"
    ; password = "alk92''[ponvhi4"
    ; email = "daniel@dabblefox.com"
    ; name = "Daniel Clayson"};
  upsert_account
    { username = "lee"
    ; password = "klma923wels92l{]as]"
    ; email = "lee@ledwards.com"
    ; name = "Lee Edwards"};
  upsert_account
    { username = "alexey"
    ; password =  "sd][3[mlvkm9034j09"
    ; email = "me@alexey.ch"
    ; name = "Alexey Klochay"};
  upsert_account
    { username = "steve"
    ; password = "fUt;re0vcodes"
    ; email = "steveykrouse@gmail.com"
    ; name = "Steve Krouse"};
  upsert_account
    { username = "tom"
    ; password = "ksef[1=s;dier]2"
    ; email = "tmrudick@gmail.com"
    ; name = "Tom Rudick"};
  upsert_account
    { username = "justin"
    ; password = "kasdf91-=3rm492"
    ; email = "justinc474@gmail.com"
    ; name = "Justin Cowperthwaite"};
  upsert_account
    { username = "jakub"
    ; password = "239a,we[weefjfie"
    ; email = "jakub.jurovych@gmail.com"
    ; name = "Jakub Jurovych"};
  upsert_account
    { username = "pvh"
    ; password = "skli93df;fl4is,"
    ; email = "pvh@pvh.ca"
    ; name = "Peter Van Hardenberg"};
   upsert_account
    { username = "mmcgrana"
    ; password = "sjklmdvn4;s;[s;2[d"
    ; email = "mmcgrana@gmail.com"
    ; name = "Mark McGranaghan"};
  upsert_account
    { username = "darksingleinstance"
    ; password = "jas902ksd]23dl49v"
    ; email = "ops@darklang.com"
    ; name = "Dark Single Instance"};
  upsert_account
    { username = "builtwithdark"
    ; password = "jas902ksd]23dl49v"
    ; email = "ops@darklang.com"
    ; name = "Built with Dark"};
  upsert_account
    { username = "benchmarking"
    ; password = "lkas;30mfa]]3f]"
    ; email = "ops@darklang.com"
    ; name = "Dark benchmarking"};



  ()

