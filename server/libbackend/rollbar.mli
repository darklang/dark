open Core
open Lwt
module CRequest = Cohttp_lwt_unix.Request

type result = [`Success | `Failure | `Disabled]
type err_ctx = Remote of CRequest.t * string
             | EventQueue
             | Other of string


(* Reports an exn with a backtrace to Rollbar asynchronously *)
val report_lwt : exn -> Libexecution.Exception.backtrace -> err_ctx -> Libexecution.Types.id -> result Lwt.t
val report : exn -> Libexecution.Exception.backtrace -> err_ctx -> Libexecution.Types.id -> result

(* Just in case *)
val last_ditch : exn -> string -> Libexecution.Types.id -> unit
