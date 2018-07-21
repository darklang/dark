open Core_kernel
open Libexecution

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
    ~params:[ Uuid (Util.create_uuid ())
            ; String account.username
            ; String account.name
            ; String account.email
            ; String account.password]

let upsert_admin (account:account) : unit =
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
    ~params:[ Uuid (Util.create_uuid ())
            ; String account.username
            ; String account.name
            ; String account.email
            ; String account.password]


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

let valid_user ~(username:username) ~(password:string) : bool =
  Db.exists
    ~name:"valid_user"
    ~subject:username
    "SELECT 1 from accounts
      WHERE accounts.username = $1
        AND accounts.password = $2"
    ~params:[String username; Secret password]


let can_edit ~(auth_domain:string) ~(username:username) : bool =
  String.Caseless.equal username auth_domain
  || is_admin ~username

let authenticate ~(username:username) ~(password:string) : bool =
  valid_user ~username ~password

let owner ~(auth_domain:string) : Uuidm.t option =
  Db.fetch_one_option
    ~name:"owner"
    ~subject:auth_domain
    "SELECT id from accounts
     WHERE accounts.username = $1"
    ~params:[ String (String.lowercase auth_domain)]
  |> Option.map ~f:List.hd_exn
  |> Option.bind ~f:Uuidm.of_string

let auth_domain_for host : string =
  match String.split host '-' with
  | d :: _ -> d
  | _ -> host

let for_host (host:string) : Uuidm.t =
  host
  |> auth_domain_for
  |> owner
  |> fun o -> Option.value_exn ~message:("No owner found for host " ^ host) o



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
  upsert_admin
    { username = "alice"
    ; password = "descartes"
    ; email = "alice@darklang.com"
    ; name = "Alice Wong"};
  upsert_admin
    { username = "jonathan"
    ; password = "jonathan"
    ; email = "jonathan.laurent@cs.cmu.edu"
    ; name = "Jonathan Laurent"};
  upsert_account
    { username = "lizzie"
    ; password = "pHLoEmnAWHWWTuotLoCoPzt8CGbdze"
    ; email = "_@lizzie.io"
    ; name = "Lizzie Dixon"};
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
  upsert_account
    { username = "vlymar"
    ; password = "theexpanse"
    ; email = "victorslymar@gmail.com"
    ; name = "Victor Lymar"};
  upsert_account
    { username = "listo"
    ; password = "GxBGXMBMkUoCakh8QpefmykJNMJdVe"
    ; email = "c@cpo.is"
    ; name = "Chase"};
  upsert_account
    { username = "gracey"
    ; password = "wilson"
    ; email = "gracey.wilson@students.olin.edu"
    ; name = "Gracey Wilson"};
  (* horizons hackathon people *)
  upsert_account
    { username = "jaroslav"
    ; password = "jaroslav"
    ; email = "jaroslav.tran@gmail.com"
    ; name = "Jaroslav Tran"};
  upsert_account
    { username = "owen"
    ; password = "owen"
    ; email = "owenzhang76@gmail.com"
    ; name = "Owen Zhang"};
  upsert_account
    { username = "abraham"
    ; password = "abraham"
    ; email = "abraham.hamidi01@gmail.com"
    ; name = "Abraham Hamidi"};
  upsert_account
    { username = "lydia"
    ; password = "lydia"
    ; email = "xinglydia@gmail.com"
    ; name = "Lydia Xing"};
  upsert_account
    { username = "ronil"
    ; password = "ronil"
    ; email = "ronil.awale@gmail.com"
    ; name = "Ronil Awale"};
  upsert_account
    { username = "sarayu"
    ; password = "sarayu"
    ; email = "sarayu@namineni.com"
    ; name = "Sarayu Namineni"};
  upsert_account
    { username = "kahuang"
    ; password = "kahuang"
    ; email = "kahuang@ucdavis.edu"
    ; name = "Kahuang"};
  upsert_account
    { username = "mike"
    ; password = "mike"
    ; email = "mikesun20@gmail.com"
    ; name = "Mike Sun"};
  upsert_account
    { username = "amit"
    ; password = "amit"
    ; email = "amitsant.2000@gmail.com"
    ; name = "Amit Sant"};
  upsert_account
    { username = "rohankanchana"
    ; password = "rohankanchana"
    ; email = "rohankanchana@gmail.com"
    ; name = "rohankanchana"};
  upsert_account
    { username = "kyra"
    ; password = "kyra"
    ; email = "kyraskraft@gmail.com"
    ; name = "Kyra Kraft"};
  upsert_account
    { username = "david"
    ; password = "david"
    ; email = "davidrusarm@gmail.com"
    ; name = "David Storozhenko"};
  upsert_account
    { username = "thais"
    ; password = "thais"
    ; email = "gonzo424@gmail.com"
    ; name = "Thais Gonzalez"};
  upsert_account
    { username = "diego"
    ; password = "diego"
    ; email = "dibarra@uchicago.edu"
    ; name = "Diego Ibarra"};
  upsert_account
    { username = "liam"
    ; password = "liam"
    ; email = "ldtrampota@gmail.com"
    ; name = "Liam Trampota"};
  upsert_account
    { username = "kitan"
    ; password = "kitan"
    ; email = "kitgarcia@ucdavis.edu"
    ; name = "Kitan Garcia"};
  upsert_account
    { username = "rahul"
    ; password = "rahul"
    ; email = "mail4rp@gmail.com"
    ; name = "rahul prasad"};
  upsert_account
    { username = "perry"
    ; password = "perry"
    ; email = "perry.ya@nyu.edu"
    ; name = "perry ya"};
  upsert_account
    { username = "howard"
    ; password = "howard"
    ; email = "chonghorizons@gmail.com"
    ; name = "howard chong"};
  upsert_account
    { username = "quoc"
    ; password = "quoc"
    ; email = "qnguyen6@binghamton.edu"
    ; name = "quoc nguyen"};
  upsert_account
    { username = "humad"
    ; password = "humad"
    ; email = "humadshah@umass.edu"
    ; name = "humad shah"};

  ()

