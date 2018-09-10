open Core_kernel

(* -------------------- *)
(* Backtraces *)
(* -------------------- *)

type backtrace = Caml.Printexc.raw_backtrace

let get_backtrace () : backtrace =
  Caml.Printexc.get_raw_backtrace ()

let backtrace_to_string (bt: backtrace) : string =
  Caml.Printexc.raw_backtrace_to_string bt

let reraise_after e (fn : backtrace -> unit) =
  let bt = get_backtrace () in
  fn bt;
  Caml.Printexc.raise_with_backtrace e bt

let reraise e  =
  let bt = get_backtrace () in
  Caml.Printexc.raise_with_backtrace e bt


type captured = backtrace * exn


(* -------------------- *)
(* Dark exceptions *)
(* -------------------- *)
type exception_info = (string * string) list [@@deriving yojson, show]

let exception_info_to_yojson info =
  `Assoc (List.map ~f:(fun (k,v) -> (k, `String v)) info)

type exception_tipe = DarkServer (* error while talking to the server *)
                    | DarkStorage (* error in User_db handling *)
                    | DarkClient (* Error made by client *)
                    | UserCode
                    | EndUser
                    [@@deriving show, eq, yojson]

let should_log (et: exception_tipe) : bool =
  match et with
  | DarkServer -> true
  | DarkStorage -> true
  | DarkClient -> true
  | UserCode -> false
  | EndUser -> false

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
                      } [@@deriving yojson, show]

exception DarkException of exception_data [@@deriving show]

let to_string exc =
  match exc with
  | DarkException e ->
    e
    |> exception_data_to_yojson
    |> Yojson.Safe.pretty_to_string
  | e ->
    Exn.to_string e


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
  match bt with
  | None -> raise (DarkException e)
  | Some bt -> Caml.Printexc.raise_with_backtrace (DarkException e) bt

let internal = raise_ DarkServer
let client = raise_ DarkClient
let user = raise_ UserCode
let storage = raise_ DarkStorage
let enduser = raise_ EndUser

let exn_to_string (e: exn) : string =
  match e with
  | DarkException e ->
    "Dark " ^ (show_exception_tipe e.tipe) ^ " Err: " ^ e.short
  | Yojson.Json_error msg ->
    "Json Err: " ^ msg
  | _ ->
    "Unknown Err: " ^ Exn.to_string e

let exn_to_info (e: exn) : Yojson.Safe.json =
  match e with
  | DarkException e ->
    exception_data_to_yojson e
  | _ -> `Null


