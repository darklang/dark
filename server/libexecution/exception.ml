open Core_kernel

type exception_info = (string * string) list [@@deriving yojson, show]

let exception_info_to_yojson info =
  `Assoc (List.map ~f:(fun (k,v) -> (k, `String v)) info)

type exception_tipe = DarkServer
                    | DarkStorage
                    | DarkClient
                    | DarkRuntime
                    | Dependency
                    | ExternalService
                    | UserCode
                    | Unknown [@@deriving show, eq, yojson, sexp]

let should_log (et: exception_tipe) : bool =
  match et with
  | DarkServer -> true
  | DarkStorage -> true
  | DarkClient -> true
  | DarkRuntime -> true
  | Dependency -> true
  | ExternalService -> true
  | UserCode -> false
  | Unknown -> true

type exception_data = { short : string
                      ; long : string option
                      ; tipe : exception_tipe
                      ; actual : string option
                      ; actual_tipe : string option
                      ; expected : string option (* might refer to result or actual *)
                      ; result : string option
                      ; result_tipe : string option
                      ; info : exception_info
                      ; workarounds : string list
                      } [@@deriving yojson, show][@@deriving_inline sexp][@@@deriving.end]

exception DarkException of exception_data [@@deriving show][@@deriving_inline sexp][@@@deriving.end]

let get_backtrace () =
  Caml.Printexc.get_raw_backtrace ()

let reraise_after e (fn : Caml.Printexc.raw_backtrace -> unit) =
  let bt = get_backtrace () in
  fn bt;
  Caml.Printexc.raise_with_backtrace e bt

let reraise e  =
  let bt = get_backtrace () in
  Caml.Printexc.raise_with_backtrace e bt

let to_string exc =
  match exc with
  | DarkException e ->
    e
    |> exception_data_to_yojson
    |> Yojson.Safe.pretty_to_string
  | e ->
    Log.inspecT "exception" e;
    Exn.to_string e

let log ?bt e =
  let backtrace =
    match bt with
    | Some bt -> bt
    | None -> get_backtrace ()
  in
  Log.erroR ~bt:backtrace "exec_exception" ~params:["exception", to_string e]


let raise_
    (tipe: exception_tipe)
    ?(bt:Caml.Printexc.raw_backtrace option)
    ?(actual:string option)
    ?(actual_tipe:string option)
    ?(expected:string option)
    ?(result_tipe:string option)
    ?(result:string option)
    ?(info=[])
    ?(workarounds=[])
    ?(long:string option)
    (short: string) =
  let e = { short
          ; long
          ; tipe
          ; actual
          ; actual_tipe
          ; expected
          ; result
          ; result_tipe
          ; info
          ; workarounds
          }
  in
  if should_log e.tipe
  then
    Log.erroR "exception" ?bt ~data:(show_exception_data e);
  match bt with
  | None -> raise (DarkException e)
  | Some bt -> Caml.Printexc.raise_with_backtrace (DarkException e) bt

let internal = raise_ DarkServer
let client = raise_ DarkClient
let user = raise_ UserCode
let api = raise_ ExternalService
let storage = raise_ DarkStorage


