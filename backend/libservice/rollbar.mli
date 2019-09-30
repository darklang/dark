open Core
open Lwt

type request_data =
  { body : string
  ; headers : (string * string) list
  ; url : string
  ; http_method : string }

type result =
  [ `Success
  | `Failure
  | `Disabled ]

type err_ctx =
  | Remote of request_data
  | EventQueue
  | CronChecker
  | Push of string
  | Other of string

(* Reports an exn with a backtrace to Rollbar asynchronously *)
val report_lwt :
     ?pp:(exn -> string)
  -> ?inspect:(exn -> Yojson.Safe.t)
  -> exn
  -> Caml.Printexc.raw_backtrace
  -> err_ctx
  -> string
  -> result Lwt.t

val report :
     ?pp:(exn -> string)
  -> ?inspect:(exn -> Yojson.Safe.t)
  -> exn
  -> Caml.Printexc.raw_backtrace
  -> err_ctx
  -> string
  -> result

(* Just in case *)
val last_ditch :
     ?pp:(exn -> string)
  -> ?inspect:(exn -> Yojson.Safe.t)
  -> exn
  -> bt:Caml.Printexc.raw_backtrace
  -> string
  -> string
  -> unit
