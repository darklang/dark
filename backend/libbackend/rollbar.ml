open Libexecution

type result = Libservice.Rollbar.result

type err_ctx = Libservice.Rollbar.err_ctx

let exn_to_string (e : exn) : string =
  match e with
  | Postgresql.Error pge ->
      "Postgres error:" ^ Postgresql.string_of_error pge
  | _ ->
      Exception.exn_to_string e


let report =
  Libservice.Rollbar.report ~pp:exn_to_string ~inspect:Exception.exn_to_info


let report_lwt =
  Libservice.Rollbar.report_lwt ~pp:exn_to_string ~inspect:Exception.exn_to_info


let last_ditch =
  Libservice.Rollbar.last_ditch ~pp:exn_to_string ~inspect:Exception.exn_to_info
