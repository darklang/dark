open Core

type exception_info = (string * string) list [@@deriving yojson, show]

let exception_info_to_yojson info =
  `Assoc (List.map ~f:(fun (k,v) -> (k, `String v)) info)

type exception_data = { short : string
                      ; long : string
                      ; tipe : string
                      ; actual : string
                      ; actual_tipe : string
                      ; expected : string (* might refer to result or actual *)
                      ; result : string
                      ; result_tipe : string
                      ; info : exception_info
                      ; workarounds : string list
                      } [@@deriving yojson, show][@@deriving_inline sexp][@@@deriving.end]

exception DarkException of exception_data [@@deriving_inline sexp][@@@deriving.end]


let raise_ (tipe:string) ?(actual="") ?(expected="") ?(result="") ?(info=[]) ?(workarounds=[]) ?(long="")
(short: string) =
  raise (DarkException { short = short
                       ; long = long
                       ; tipe = tipe
                       ; actual = actual
                       ; actual_tipe = "<nothing>"
                       ; expected = expected
                       ; result = result
                       ; result_tipe = result
                       ; info = info
                       ; workarounds = workarounds
                       })

let internal = raise_ "Dark (server)"
let client = raise_ "Dark (editor)"
let user = raise_ "User"
let api = raise_ "API error"

