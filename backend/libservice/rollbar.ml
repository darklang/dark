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
  | Segment of string
  | Other of string

let string_of_ctx (ctx : err_ctx) =
  match ctx with
  | EventQueue ->
      "queue_worker"
  | CronChecker ->
      "cron_checker"
  | Remote _ ->
      "remote"
  | Push _ ->
      "push"
  | Segment _ ->
      "segment"
  | Other _ ->
      "other"


(* "https://ui.honeycomb.io/dark/datasets/kubernetes-bwd-ocaml?query={\"filters\":[{\"column\":\"rollbar\",\"op\":\"exists\"},{\"column\":\"execution_id\",\"op\":\"=\",\"value\":\"44602511168214071\"}],\"limit\":100,\"time_range\":604800}"
 *)
(* The escaping on this is a bit overkill - it'll do a layer of url-escaping
 * beyond the example above, which we don't need - but the link works *)
let honeycomb_link_of_execution_id (execution_id : string) : string =
  let (query : Yojson.Safe.t) =
    `Assoc
      [ ( "filters"
        , `List
            [ `Assoc
                [ ("column", `String "execution_id")
                ; ("op", `String "=")
                ; ("value", `String execution_id) ] ] )
      ; ("limit", `Int 100)
      ; ("time_range", `Int 604800) ]
    (* 604800 is 7 days *)
  in
  Uri.make
    ~scheme:"https"
    ~host:"ui.honeycomb.io"
    ~path:"/dark/datasets/kubernetes-bwd-ocaml"
    ~query:[("query", [query |> Yojson.Safe.to_string])]
    ()
  |> Uri.to_string


let error_to_payload
    ~pp
    ~inspect
    (e : exn)
    (bt : Caml.Printexc.raw_backtrace)
    (ctx : err_ctx)
    (execution_id : string) : Yojson.Safe.t =
  let e, pageable =
    (* unwrap PageableExn if it is present *)
    match e with
    | Pageable.PageableExn inner_e ->
        (inner_e, true)
    | _ as e ->
        (e, false)
  in
  let message =
    let interior =
      [ ("body", `String (pp e))
      ; ("raw_trace", `String (Caml.Printexc.raw_backtrace_to_string bt))
      ; ("honeycomb", `String (honeycomb_link_of_execution_id execution_id))
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
    | Segment event ->
        `String (sprintf "segment: %s" event)
  in
  let env = `String Config.rollbar_environment in
  let language = `String "OCaml" in
  let framework = `String "Cohttp" in
  let level = if pageable then `String "critical" else `String "error" in
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
        ; ("level", level)
        ; ("environment", env)
        ; ("language", language)
        ; ("framework", framework)
        ; ("context", context)
        ; ("execution_id", `String execution_id)
        ; ("request", request) ]
    | EventQueue | CronChecker ->
        [ ("body", message)
        ; ("level", level)
        ; ("environment", env)
        ; ("language", language)
        ; ("framework", framework)
        ; ("execution_id", `String execution_id)
        ; ("context", context) ]
    | Push event | Segment event ->
        [ ("body", message)
        ; ("level", level)
        ; ("environment", env)
        ; ("language", language)
        ; ("framework", framework)
        ; ("execution_id", `String execution_id)
        ; ("context", context)
        ; ("push_event", `String event) ]
    | Other str ->
        [ ("body", message)
        ; ("level", level)
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
    (execution_id : string) : Curl.t * Buffer.t * Yojson.Safe.t =
  let payload = error_to_payload ~pp ~inspect e bt ctx execution_id in
  let body =
    [ ("access_token", `String Config.rollbar_server_access_token)
    ; ("data", payload) ]
    |> fun b -> `Assoc b |> Yojson.Safe.to_string
  in
  let headers = ["Content-Type: application/json"] in
  let open Curl in
  let responsebuffer = Buffer.create 16384 in
  let responsefn str : int =
    Buffer.add_string responsebuffer str ;
    String.length str
  in
  let c = init () in
  set_url c Config.rollbar_url ;
  set_followlocation c false ;
  set_maxredirs c 1 ;
  set_connecttimeout c 5 ;
  set_timeout c 10 ;
  set_httpheader c headers ;
  set_writefunction c responsefn ;
  set_post c true ;
  set_postfields c body ;
  set_postfieldsize c (String.length body) ;
  (c, responsebuffer, payload)


(* given a response buffer containing a .result.uuid and a payload, log it *)
let log_rollbar (r : Buffer.t) (payload : Yojson.Safe.t) (e : exn) : unit =
  let rollbar_link_of_curl_buffer (r : Buffer.t) : string option =
    let body =
      try Some (Yojson.Safe.from_string (Buffer.contents r)) with e ->
        (* shouldn't happen *)
        Log.erroR "no body from rollbar response" ;
        None
    in
    body
    |> Option.bind ~f:(fun body ->
           try
             body
             |> Yojson.Safe.Util.member "result"
             |> Yojson.Safe.Util.member "uuid"
             |> Yojson.Safe.Util.to_string_option
           with e ->
             let message =
               body
               |> Yojson.Safe.Util.member "message"
               |> Yojson.Safe.Util.to_string_option
               |> Option.value ~default:""
             in
             Log.erroR
               "rollbar response had no .result.uuid"
               ~params:[("message", message)] ;
             None )
    |> Option.bind ~f:(fun uuid ->
           Some ("https://rollbar.com/item/uuid/?uuid=" ^ uuid) )
  in
  let payload =
    match payload with
    | `Assoc tuples ->
        tuples
    | _ ->
        [("error", `String "unexpected payload format")]
  in
  (* rollbar payload includes a request object, but this overwrites the
     * request path data (key "request") set in webserver.ml, which is in a
     * format that honeycomb knows how to deal with. So the key for rollbar's
     * request data to request_obj *)
  let payload =
    payload
    |> List.map ~f:(fun (k, v) ->
           match k with "request" -> ("request_obj", v) | _ -> (k, v) )
  in
  let payload =
    match rollbar_link_of_curl_buffer r with
    | None ->
        payload
    | Some link ->
        ("rollbar", `String link) :: payload
  in
  let payload =
    let is_pageable =
      match e with Pageable.PageableExn _ -> true | _ -> false
    in
    ("is_pageable", `Bool is_pageable) :: payload
  in
  Log.erroR "rollbar" ~jsonparams:payload


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
          let c, r, p = create_request ~pp ~inspect e bt ctx execution_id in
          ( try%lwt
                  Curl_lwt.perform c
                  >|= function
                  | CURLE_OK ->
                      log_rollbar r p e ;
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
      let c, r, p = create_request ~pp ~inspect e bt ctx execution_id in
      Curl.perform c ;
      let result = c |> Curl.get_responsecode |> code_to_result in
      log_rollbar r p e ;
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
