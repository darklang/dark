open Core
open Types

type username = string

type user = { username: string
            ; password: string
            ; email: string
            ; name: string
            }

(************************)
(* Adding *)
(************************)
let save_org (org: string) : unit =
  Printf.sprintf
    "INSERT INTO orgs
    (id, name)
    VALUES
    ('%s'::uuid, '%s')
    ON CONFLICT (name) DO NOTHING
    "
    (Uuid.create () |> Uuid.to_string)
    (Db.escape (String.lowercase org))
  |> Db.run_sql

let upsert_user (user:user) : unit =
  Printf.sprintf
    "INSERT INTO users
    (id, username, name, email, admin, password)
    VALUES
    ('%s'::uuid, '%s', '%s', '%s', false, '%s')
    ON CONFLICT (username)
    DO UPDATE SET name = EXCLUDED.name,
                  email = EXCLUDED.email,
                  admin = false,
                  password = EXCLUDED.password"
    (Uuid.create () |> Uuid.to_string)
    (Db.escape user.username)
    (Db.escape user.name)
    (Db.escape user.email)
    (Db.escape user.password)
  |> Db.run_sql

let upsert_admin (user:user) : unit =
  Printf.sprintf
    "INSERT INTO users as u
    (id, username, name, email, admin, password)
    VALUES
    ('%s'::uuid, '%s', '%s', '%s', true, '%s')
    ON CONFLICT (username)
    DO UPDATE SET name = EXCLUDED.name,
                  email = EXCLUDED.email,
                  admin = true,
                  password = EXCLUDED.password"
    (Uuid.create () |> Uuid.to_string)
    (Db.escape user.username)
    (Db.escape user.name)
    (Db.escape user.email)
    (Db.escape user.password)
  |> Db.run_sql

let upsert_org_membership (user:string) (org:string) : unit =
  Printf.sprintf
    "INSERT INTO org_memberships
    (user_id, org_id)
    VALUES
      ( (select id from users where username = '%s')
      , (select id from orgs where name = '%s'))
    ON CONFLICT DO NOTHING"
    (Db.escape user)
    (Db.escape org)
  |> Db.run_sql


(************************)
(* Check access *)
(************************)

let has_access ~host ~(username:username) : bool =
  Printf.sprintf
    "SELECT 1 from users, orgs, org_memberships, canvases
      WHERE users.username = '%s'
        AND
          (canvases.name = '%s'
           AND orgs.id = org_memberships.org_id
           AND canvases.org_id = orgs.id
           AND users.id = org_memberships.user_id)
        OR users.admin = true
        "
    (Db.escape username)
    (Db.escape (String.lowercase host))
  |> Db.truth_via_sql ~quiet:false


let authenticate ~(host:string) ~(username:username) ~(password:string) : bool =
  Printf.sprintf
    "SELECT 1 from users, orgs, org_memberships, canvases
      WHERE users.username = '%s'
        AND users.password = '%s'
        AND
          (canvases.name = '%s'
           AND orgs.id = org_memberships.org_id
           AND canvases.org_id = orgs.id
           AND users.id = org_memberships.user_id)
        OR users.admin = true
        "
    (Db.escape username)
    (Db.escape password)
    (Db.escape (String.lowercase host))
  |> Db.truth_via_sql ~quiet:false


let init () : unit =
  save_org "dabblefox";
  save_org "dark";
  save_org "lee";
  save_org "alexey";
  save_org "steve";
  save_org "tom";
  save_org "interview";
  save_org "tests";
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
  upsert_user
    { username = "daniel"
    ; password = "alk92''[ponvhi4"
    ; email = "daniel@dabblefox.com"
    ; name = "Daniel Clayson"};
  upsert_user
    { username = "lee"
    ; password = "klma923wels92l{]as]"
    ; email = "lee@ledwards.com"
    ; name = "Lee Edwards"};
  upsert_user
    { username = "alexey"
    ; password =  "sd][3[mlvkm9034j09"
    ; email = "me@alexey.ch"
    ; name = "Alexey Klochay"};
  upsert_user
    { username = "steve"
    ; password = "fUt;re0vcodes"
    ; email = "steveykrouse@gmail.com"
    ; name = "Steve Krouse"};
  upsert_user
    { username = "tom"
    ; password = "ksef[1=s;dier]2"
    ; email = "tmrudick@gmail.com"
    ; name = "Tom Rudick"};

  upsert_org_membership "tom" "tom";
  upsert_org_membership "steve" "steve";
  upsert_org_membership "alexey" "alexey";
  upsert_org_membership "lee" "lee";
  upsert_org_membership "daniel" "dabblefox";
  upsert_org_membership "tests" "tests";

  upsert_org_membership "ian" "dark";
  upsert_org_membership "paul" "dark";
  upsert_org_membership "stefi" "dark";
  upsert_org_membership "ellen" "dark";
  upsert_org_membership "zane" "dark";
  ()
