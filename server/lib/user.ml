open Core
open Types

type t = { username: string
         ; host: host
         ; password: string
         }

let username user = user.username
let host user = user.host
let password user = user.password

let construct ~username ~host ~password =
  { username; host; password }

let users =
  (* employees *)
  [["ian"; "admin"; "look"; "ian@darklang.com"; "Ian Connolly"]
  ;["paul";"admin"; "what"; "paul@darklang.com"; "Paul Biggar"]
  ;["ellen"; "admin"; "you"; "ellen@darklang.com"; "Ellen Chisa"]
  ;["stefi"; "admin"; "made"; "stefi@darklang.com"; "Stefi Petit"]
  ;["zane"; "admin"; "me do"; "zane@darklang.com"; "Zane Shannon"]

  (* testing *)
  ;["tests"; "admin"; "fVm2CUePzGKCwoEQQdNJktUQ"; "ops@darklang.com"; "Test Account"]
  ;["aoife"; "dogtreats"; "notarealdog"; "ian+dog@darklang.com"; "Aoife Connolly"]

  (* customers *)
  ;["daniel"; "dabblefox"; "alk92''[ponvhi4"; "daniel@dabblefox.com"; "Daniel Clayson"]

  (* friends trying it out *)
  ;["lee"; "lee"; "klma923wels92l{]as]"; "lee@ledwards.com"; "Lee Edwards"]
  ;["alexey"; "alexey"; "sd][3[mlvkm9034j09"; "me@alexey.ch"; "Alexey Klochay"]
  ;["steve"; "steve"; "';si83n']\\spr,3idk"; "steveykrouse@gmail.com"; "Steve Krouse"]
  ;["tom"; "tom"; "ksef[1=s;'dier]2"; "tmrudick@gmail.com"; "Tom Rudick"]

  (* interviews *)
  ]
  |> List.map
       ~f:(function
        | [username; host; password; _email; _name] -> construct ~username ~host ~password
        | _ -> failwith "Needs to be an array of length 3")

let all_for_host host =
  List.filter ~f:(fun u -> String.Caseless.equal u.host host) users

let for_username username =
  List.find ~f:(fun u -> u.username = username) users

