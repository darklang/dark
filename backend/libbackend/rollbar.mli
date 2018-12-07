open Core
open Lwt

type result = Libservice.Rollbar.result

type err_ctx = Libservice.Rollbar.err_ctx

(* Reports an exn with a backtrace to Rollbar asynchronously *)
val report_lwt :
  exn -> Caml.Printexc.raw_backtrace -> err_ctx -> string -> result Lwt.t

val report : exn -> Caml.Printexc.raw_backtrace -> err_ctx -> string -> result

(* Just in case *)
val last_ditch :
  exn -> bt:Caml.Printexc.raw_backtrace -> string -> string -> unit
