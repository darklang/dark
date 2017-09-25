open Core
type exception_data = { short : string
                      ; long : string
                      ; tipe : string
                      ; actual : Yojson.Safe.json
                      ; expected : string
                      ; info : (string * string) list
                      ; workarounds : string list
                      }
exception DarkException of exception_data


let raise_ (tipe:string) ?(actual="") ?(expected="") ?(info=[]) ?(workarounds=[]) ?(long="")
(short: string) =
  raise (DarkException { short = short
                       ; long = long
                       ; tipe = tipe
                       ; actual = `String "<incomplete>"
                       ; expected = expected
                       ; info = info
                       ; workarounds = workarounds
                       })



let internal = raise_ "Dark (server)"
let client = raise_ "Dark (editor)"
let user = raise_ "User"
let api = raise_ "API error"

type excjson = { short : string
               ; long : string
               ; tipe : string
               ; actual : string
               ; expected : string
               ; workarounds : string list
               } [@@deriving yojson]

let exceptiondata_to_excjson (e: exception_data) =
  { short = e.short
  ; long = e.long
  ; tipe = e.tipe
  ; actual = Yojson.Safe.pretty_to_string e.actual
  ; expected = e.expected
  ; workarounds = e.workarounds
  }




let to_yojson_string (e: exception_data) : string =
  let info = `Assoc (List.map ~f:(fun (k,v) -> (k,`String v)) e.info) in
  let json = match e |> exceptiondata_to_excjson |> excjson_to_yojson with
             | `Assoc l -> `Assoc (List.cons ("info", info) l)
             | json -> json in
  Yojson.Safe.pretty_to_string json

(* let to_string (e: exception_data) = *)
(*   e.tipe *)
(*   ^ " error: " *)
(*   ^ e.short *)
(*   ^ " - Long version: " *)
(*   ^ e.long *)
(*   ^ " - Actual: " *)
(*   ^ (Yojson.Safe.to_string e.actual) *)
(*   ^ " - Expected: " *)
(*   ^ e.expected *)
(*   ^ " - extra info:" *)
(*   ^ "{" ^ (e.info |> List.map ~f:(fun (name,value) -> name ^ ": " ^ value) *)
(*                   |> String.concat ~sep:", ") ^ "}" *)
(*   ^ " - possible workarounds: " *)
(*   ^ (String.concat ~sep:", " e.workarounds) *)
(*  *)
