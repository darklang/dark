open Core

type t = { username: string
         ; domain: string
         ; password: string
         }

let username user = user.username
let domain user = user.domain
let password user = user.password

let construct ~username ~domain ~password =
  { username; domain; password }

let users =
  (* employees *)
  [["ian"; "admin"; "look"; "ian@darklang.com"]
  ;["paul";"admin"; "what"; "paul@darklang.com"]
  ;["ellen"; "admin"; "you"; "ellen@darklang.com"]
  ;["stefi"; "admin"; "made"; "stefi@darklang.com"]
  ;["zane"; "admin"; "me do"; "zane@darklang.com"]

  (* testing *)
  ;["tests"; "admin"; "fVm2CUePzGKCwoEQQdNJktUQ"; ""]
  ;["aoife"; "dogtreats"; "notarealdog"; "ian+dog@darklang.com"]

  (* customers *)

  (* friends trying it out *)
  ;["lee"; "lee"; "klma923wels92l{]as]"; "lee@ledwards.com"]
  ;["alexey"; "alexey"; "sd][3[mlvkm9034j09"; "me@alexey.ch"]
  ;["steve"; "steve"; "';si83n']\spr,3idk"; "steveykrouse@gmail.com"]

  (* interviews *)
  ]
  |> List.map
       ~f:(function
        | [username; domain; password; _] -> construct ~username ~domain ~password
        | _ -> failwith "Needs to be an array of length 3")

let all_for_domain domain =
  List.filter ~f:(fun u -> String.Caseless.equal u.domain domain) users

let for_username username =
  List.find ~f:(fun u -> u.username = username) users

