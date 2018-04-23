open Core
open Lwt

module CRequest = Cohttp_lwt_unix.Request

type result = [`Success | `Failure | `Disabled]

type err_ctx = Remote of CRequest.t
             | EventQueue

let exn_to_string (e: exn) : string =
  match e with
  | Exception.DarkException e ->
    "Dark Err: " ^ e.short
  | Yojson.Json_error msg ->
    "Json Err: " ^ msg
  | Postgresql.Error e ->
    "Postgres Err: " ^ Postgresql.string_of_error e
  | _ ->
    "Unknown Err: " ^ Exn.to_string e

let exn_to_info (e: exn) : string =
  match e with
  | Exception.DarkException e ->
    e.info
    |> List.map
      ~f:(fun (l, r) -> "(" ^ l ^ ", " ^ r ^ ")")
    |> String.concat
      ~sep:", "
    |> fun s -> "[" ^ s ^ "]"
  | _ -> ""

let error_to_payload (e: exn) (bt: Backtrace.t) (ctx: err_ctx) : Yojson.Safe.json =
  let message =
    let interior =
      [("body", `String (exn_to_string e))
      ;("raw_trace", `String (Backtrace.to_string bt))
      ;("raw_info", `String (exn_to_info e))]
      |> fun b -> `Assoc b
    in
    `Assoc [("message", interior)]
  in
  let context =
    match ctx with
    | Remote _ -> `String "server"
    | EventQueue -> `String "event queue worker"
  in
  let env = `String Config.rollbar_environment in
  let language = `String "OCaml" in
  let framework = `String "Cohttp" in
  let payload =
    match ctx with
    | Remote req ->
      let request =
        let headers =
          req
          |> CRequest.headers
          |> Cohttp.Header.to_list
          |> List.Assoc.map ~f:(fun v -> `String v)
        in
        [("url", `String ("https:" ^ (req |> CRequest.uri |> Uri.to_string)))
        ;("method", `String (req |> CRequest.meth |> Cohttp.Code.string_of_method))
        ;("headers", `Assoc headers)
        ]
        |> fun r -> `Assoc r
      in
      [("body", message)
      ;("environment", env)
      ;("language", language)
      ;("framework", framework)
      ;("context", context)
      ;("request", request)]
    | EventQueue ->
      [("body", message)
      ;("environment", env)
      ;("language", language)
      ;("framework", framework)
      ;("context", context)]
  in
  payload
  |> fun p -> `Assoc p

let create_request (e: exn) (bt: Backtrace.t) (ctx: err_ctx) : Curl.t =
  let body =
    [("access_token", `String Config.rollbar_server_access_token)
    ;("data", error_to_payload e bt ctx)
    ]
    |> fun b -> `Assoc b
    |> Yojson.Safe.to_string
  in
  let headers =
    ["Content-Type: application/json"]
  in
  let open Curl in
  let c = init () in
  set_url c Config.rollbar_url;
  set_followlocation c false;
  set_maxredirs c 1;
  set_connecttimeout c 5;
  set_timeout c 10;
  set_httpheader c headers;
  set_post c true;
  set_postfields c body;
  set_postfieldsize c (String.length body);
  c

let code_to_result (code: int) : result =
  if code >= 200 && code < 300
  then `Success
  else `Failure

(* ------------------------- *)
(* Exported *)
(* ------------------------- *)

let report_lwt (e: exn) (bt: Backtrace.t) (ctx: err_ctx) : result Lwt.t =
  if (not Config.rollbar_enabled) then return `Disabled else
  let c = create_request e bt ctx in
  begin try%lwt
    Curl_lwt.perform c
    >|= function
    | CURLE_OK -> `Success
    | other ->
        Log.erroR "Rollbar err" (Curl.strerror other);
        `Failure
  with err ->
    Log.erroR "Rollbar err" err;
    Lwt.fail err
  end[%lwt.finally Curl.cleanup c; return ()]

let report (e: exn) (bt: Backtrace.t) (ctx: err_ctx) : result =
  if (not Config.rollbar_enabled) then `Disabled else
  let c = create_request e bt ctx in
  Curl.perform c;
  let result = c
             |> Curl.get_responsecode
             |> code_to_result
  in
  Curl.cleanup c;
  result

