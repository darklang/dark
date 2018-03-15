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
  [["ian"; "admin"; "ian"]
  ;["paul";"admin"; "paul"]
  ;["ellen"; "admin"; "ellen"]
  ;["stefi"; "admin"; "stefi"]
  ;["cam"; "admin"; "cam"]
  ;["zane"; "admin"; "zane"]
  ;["tests"; "admin"; "fVm2CUePzGKCwoEQQdNJktUQ"]
  ;["aoife"; "dogtreats"; "aoife"]
  ]
  |> List.map
       ~f:(function
        | [username; domain; password] -> construct ~username ~domain ~password
        | _ -> failwith "Needs to be an array of length 3")

let all_for_domain domain =
  List.filter ~f:(fun u -> String.Caseless.equal u.domain domain) users

let for_username username =
  List.find ~f:(fun u -> u.username = username) users

