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

exception DarkException of exception_data [@@deriving show][@@deriving_inline sexp][@@@deriving.end]

let reraise_after e (fn : Caml.Printexc.raw_backtrace -> unit) =
  let bt = Caml.Printexc.get_raw_backtrace () in
  fn bt;
  Caml.Printexc.raise_with_backtrace e bt

let reraise e  =
  let bt = Caml.Printexc.get_raw_backtrace () in
  Caml.Printexc.raise_with_backtrace e bt

let raise_
    (tipe: string)
    ?(bt:Caml.Printexc.raw_backtrace option=None)
    ?(actual="")
    ?(actual_tipe="unknown tipe")
    ?(expected="")
    ?(result_tipe="unknown tipe")
    ?(result="")
    ?(info=[])
    ?(workarounds=[])
    ?(long="")
    (short: string) =
  let e = { short = short
          ; long = long
          ; tipe = tipe
          ; actual = actual
          ; actual_tipe = actual_tipe
          ; expected = expected
          ; result = result
          ; result_tipe = result_tipe
          ; info = info
          ; workarounds = workarounds
          }
  in
  Log.erroR "exception" e;
  match bt with
  | None -> raise (DarkException e)
  | Some bt -> Caml.Printexc.raise_with_backtrace (DarkException e) bt

let internal = raise_ "Dark (server)"
let client = raise_ "Dark (editor)"
let user = raise_ "User"
let api = raise_ "API error"


