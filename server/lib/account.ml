open Core
open Types

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
  Printf.sprintf
    "INSERT INTO accounts
    (id, username, name, email, admin, password)
    VALUES
    ('%s'::uuid, '%s', '%s', '%s', false, '%s')
    ON CONFLICT (username)
    DO UPDATE SET name = EXCLUDED.name,
                  email = EXCLUDED.email,
                  admin = false,
                  password = EXCLUDED.password"
    (Uuid.create () |> Uuid.to_string)
    (Db.escape account.username)
    (Db.escape account.name)
    (Db.escape account.email)
    (Db.escape account.password)
  |> Db.run_sql

let upsert_admin (account:account) : unit =
  Printf.sprintf
    "INSERT INTO accounts as u
    (id, username, name, email, admin, password)
    VALUES
    ('%s'::uuid, '%s', '%s', '%s', true, '%s')
    ON CONFLICT (username)
    DO UPDATE SET name = EXCLUDED.name,
                  email = EXCLUDED.email,
                  admin = true,
                  password = EXCLUDED.password"
    (Uuid.create () |> Uuid.to_string)
    (Db.escape account.username)
    (Db.escape account.name)
    (Db.escape account.email)
    (Db.escape account.password)
  |> Db.run_sql


(************************)
(* Check access *)
(************************)

let is_admin ~username : bool =
  Printf.sprintf
    "SELECT 1 from accounts
     WHERE accounts.username = '%s'
       AND accounts.admin = true "
    (Db.escape username)
  |> Db.truth_via_sql ~quiet:false

let has_access ~(auth_domain:string) ~(username:username) : bool =
  String.Caseless.equal username auth_domain
  || is_admin ~username


let valid_user ~(username:username) ~(password:string) : bool =
  Printf.sprintf
    "SELECT 1 from accounts
      WHERE accounts.username = '%s'
        AND accounts.password = '%s'"
    (Db.escape username)
    (Db.escape password)
  |> Db.truth_via_sql ~quiet:true

let authenticate ~(auth_domain:string) ~(username:username) ~(password:string) : bool =
  valid_user ~username ~password
  && has_access ~auth_domain ~username


let init () : unit =
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
  upsert_admin
    { username = "tests"
    ; password = "fVm2CUePzGKCwoEQQdNJktUQ"
    ; email = "ops@darklang.com"
    ; name = "Test Account"};
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
  ()
