open Core_kernel
open Libcommon
open Lwt

type result =
  [ `Success
  | `Failure
  | `Disabled ]

type request_data =
  { body : string
  ; headers : (string * string) list
  ; url : string
  ; http_method : string }

type err_ctx =
  | Remote of request_data
  | EventQueue
  | CronChecker
  | Push of string
  | Other of string

let error_to_payload
    ~pp
    ~inspect
    (e : exn)
    (bt : Caml.Printexc.raw_backtrace)
    (ctx : err_ctx)
    (execution_id : string) : Yojson.Safe.json =
  let message =
    let interior =
      [ ("body", `String (pp e))
      ; ("raw_trace", `String (Caml.Printexc.raw_backtrace_to_string bt))
      ; ("raw_info", inspect e) ]
      |> fun b -> `Assoc b
    in
    `Assoc [("message", interior)]
  in
  let context =
    match ctx with
    | Remote _ ->
        `String "server"
    | EventQueue ->
        `String "event queue worker"
    | CronChecker ->
        `String "cron event emitter"
    | Push _ ->
        `String "server push"
    | Other str ->
        `String str
  in
  let env = `String Config.rollbar_environment in
  let language = `String "OCaml" in
  let framework = `String "Cohttp" in
  let payload =
    match ctx with
    | Remote request_data ->
        let request =
          let headers =
            request_data.headers |> List.Assoc.map ~f:(fun v -> `String v)
          in
          [ ("url", `String ("https:" ^ request_data.url))
          ; ("method", `String request_data.http_method)
          ; ("headers", `Assoc headers)
          ; ("execution_id", `String execution_id)
          ; ("body", `String request_data.body) ]
          |> fun r -> `Assoc r
        in
        [ ("body", message)
        ; ("environment", env)
        ; ("language", language)
        ; ("framework", framework)
        ; ("context", context)
        ; ("execution_id", `String execution_id)
        ; ("request", request) ]
    | EventQueue | CronChecker ->
        [ ("body", message)
        ; ("environment", env)
        ; ("language", language)
        ; ("framework", framework)
        ; ("execution_id", `String execution_id)
        ; ("context", context) ]
    | Push event ->
        [ ("body", message)
        ; ("environment", env)
        ; ("language", language)
        ; ("framework", framework)
        ; ("execution_id", `String execution_id)
        ; ("context", context)
        ; ("push_event", `String event) ]
    | Other str ->
        [ ("body", message)
        ; ("environment", env)
        ; ("language", language)
        ; ("framework", framework)
        ; ("execution_id", `String execution_id)
        ; ("context", context) ]
  in
  payload |> fun p -> `Assoc p


let create_request
    ~pp
    ~inspect
    (e : exn)
    (bt : Caml.Printexc.raw_backtrace)
    (ctx : err_ctx)
    (execution_id : string) : Curl.t =
  let body =
    [ ("access_token", `String Config.rollbar_server_access_token)
    ; ("data", error_to_payload ~pp ~inspect e bt ctx execution_id) ]
    |> fun b -> `Assoc b |> Yojson.Safe.to_string
  in
  let headers = ["Content-Type: application/json"] in
  let open Curl in
  let c = init () in
  set_url c Config.rollbar_url ;
  set_followlocation c false ;
  set_maxredirs c 1 ;
  set_connecttimeout c 5 ;
  set_timeout c 10 ;
  set_httpheader c headers ;
  set_post c true ;
  set_postfields c body ;
  set_postfieldsize c (String.length body) ;
  c


let code_to_result (code : int) : result =
  if code >= 200 && code < 300 then `Success else `Failure


let empty_inspect e = `Null

(* ------------------------- *)
(* Exported *)
(* ------------------------- *)

let report_lwt
    ?(pp = Exn.to_string)
    ?(inspect = empty_inspect)
    (e : exn)
    (bt : Caml.Printexc.raw_backtrace)
    (ctx : err_ctx)
    (execution_id : string) : result Lwt.t =
  try%lwt
        if not Config.rollbar_enabled
        then return `Disabled
        else
          let c = create_request ~pp ~inspect e bt ctx execution_id in
          ( try%lwt
                  Curl_lwt.perform c
                  >|= function
                  | CURLE_OK ->
                      `Success
                  | other ->
                      Log.erroR
                        "Rollbar err"
                        ~data:(Curl.strerror other)
                        ~params:[("execution_id", Log.dump execution_id)] ;
                      `Failure
            with err ->
              Log.erroR
                "Rollbar err"
                ~data:(Log.dump err)
                ~params:[("execution_id", Log.dump execution_id)] ;
              Lwt.fail err )
            [%lwt.finally
              Curl.cleanup c ;
              return ()]
  with err ->
    Caml.print_endline "UNHANDLED ERROR: rollbar.report_lwt" ;
    Lwt.fail err


let report
    ?(pp = Exn.to_string)
    ?(inspect = empty_inspect)
    (e : exn)
    (bt : Caml.Printexc.raw_backtrace)
    (ctx : err_ctx)
    (execution_id : string) : result =
  try
    if not Config.rollbar_enabled
    then `Disabled
    else
      let c = create_request ~pp ~inspect e bt ctx execution_id in
      Curl.perform c ;
      let result = c |> Curl.get_responsecode |> code_to_result in
      Curl.cleanup c ;
      result
  with err ->
    Caml.print_endline "UNHANDLED ERROR: rollbar.report" ;
    `Failure


let last_ditch
    ?(pp = Exn.to_string)
    ?(inspect = empty_inspect)
    (e : exn)
    ~(bt : Caml.Printexc.raw_backtrace)
    (name : string)
    (execution_id : string) : unit =
  (* Before anything else, get this flushed to logs *)
  Caml.print_endline ("UNHANDLED ERROR: " ^ name ^ " - " ^ pp e) ;
  ignore (report ~pp ~inspect e bt (Other "main") execution_id) ;
  Caml.print_endline (Caml.Printexc.raw_backtrace_to_string bt) ;
  Caml.print_endline (Yojson.Safe.to_string (inspect e))
