open Core
open Lwt
module CRequest = Cohttp_lwt_unix.Request

type result = [`Success | `Failure | `Disabled]
type err_ctx = Remote of CRequest.t
             | EventQueue
             | Tracing

(* Reports an exn with a backtrace to Rollbar asynchronously *)
val report_lwt : exn -> Backtrace.t -> err_ctx -> result Lwt.t
val report : exn -> Backtrace.t -> err_ctx -> result

