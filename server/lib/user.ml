open Core
open Types

type t = { username: string
         ; host: host
         ; password: string
         }

type t2 = { username: string
          ; password: string
          ; email : string
          ; name : string
          }

let username (user:t) = user.username
let host (user:t) = user.host
let password (user:t) = user.password

let construct ~username ~host ~password : t =
  { username; host; password }


let save_org (org: string) : unit =
  Printf.sprintf
    "INSERT INTO orgs
    (id, name)
    VALUES
    ('%s'::uuid, '%s')
    ON CONFLICT (name) DO NOTHING
    "
    (Uuid.create () |> Uuid.to_string)
    org
  |> Db.run_sql

let upsert_user (user:t2) : unit =
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
    user.username
    user.name
    user.email
    user.password
  |> Db.run_sql

let upsert_admin (user:t2) : unit =
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
    user.username
    user.name
    user.email
    user.password
  |> Db.run_sql

let upsert_org_membership (user:string) (org:string) : unit =
  Printf.sprintf
    "INSERT INTO org_memberships
    (user_id, org_id)
    VALUES
      ( (select id from users where username = '%s')
      , (select id from orgs where name = '%s'))
    ON CONFLICT DO NOTHING"
    user
    org
  |> Db.run_sql


let users =
  (* employees *)
  [["ian"; "admin"; "look"; "ian@darklang.com"; "Ian Connolly"]
  ;["paul";"admin"; "what"; "paul@darklang.com"; "Paul Biggar"]
  ;["ellen"; "admin"; "you"; "ellen@darklang.com"; "Ellen Chisa"]
  ;["stefi"; "admin"; "made"; "stefi@darklang.com"; "Stefi Petit"]
  ;["zane"; "admin"; "me do"; "zane@darklang.com"; "Zane Shannon"]
  ;["tests"; "admin"; "fVm2CUePzGKCwoEQQdNJktUQ"; "ops@darklang.com"; "Test Account"]
  ;["daniel"; "dabblefox"; "alk92''[ponvhi4"; "daniel@dabblefox.com"; "Daniel Clayson"]
  ;["lee"; "lee"; "klma923wels92l{]as]"; "lee@ledwards.com"; "Lee Edwards"]
  ;["alexey"; "alexey"; "sd][3[mlvkm9034j09"; "me@alexey.ch"; "Alexey Klochay"]
  ;["steve"; "steve"; "';si83n']\\spr,3idk"; "steveykrouse@gmail.com"; "Steve Krouse"]
  ;["tom"; "tom"; "ksef[1=s;'dier]2"; "tmrudick@gmail.com"; "Tom Rudick"]

  ]
  |> List.map
       ~f:(function
        | [username; host; password; _email; _name] -> construct ~username ~host ~password
        | _ -> failwith "Needs to be an array of length 3")

let all_for_host host =
  List.filter ~f:(fun u -> String.Caseless.equal u.host host) users

let for_username username =
  List.find ~f:(fun u -> u.username = username) users

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
