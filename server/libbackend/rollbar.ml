open Libexecution

type result = Libservice.Rollbar.result
type err_ctx = Libservice.Rollbar.err_ctx

let report = Libservice.Rollbar.report ~pp:Exception.exn_to_string ~inspect:Exception.exn_to_info
let report_lwt = Libservice.Rollbar.report_lwt ~pp:Exception.exn_to_string ~inspect:Exception.exn_to_info
let last_ditch = Libservice.Rollbar.last_ditch ~pp:Exception.exn_to_string ~inspect:Exception.exn_to_info
