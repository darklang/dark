open Core_kernel
open Lwt
module Cl = Cohttp_lwt
module Clu = Cohttp_lwt_unix
module S = Clu.Server
module CRequest = Clu.Request
module CResponse = Clu.Response
module Client = Clu.Client
module Header = Cohttp.Header
module F = Liblegacyfuzzing.Fuzzing

let respond_json_ok (body : string) : (CResponse.t * Cl.Body.t) Lwt.t =
  let headers =
    Header.of_list
      [ ("Content-Length", String.length body |> string_of_int)
      ; ("Content-Type", "application/json") ]
  in
  S.respond_string ~status:`OK ~body ~headers ()


let server () =
  let stop, stopper = Lwt.wait () in
  let callback (conn : S.conn) (req : CRequest.t) (req_body : Cl.Body.t) :
      (CResponse.t * Cl.Body.t) Lwt.t =
    let%lwt body_string = Cl.Body.to_string req_body in
    let uri = CRequest.uri req in
    let meth = CRequest.meth req in
    let path =
      uri
      |> Uri.path
      |> String.lstrip ~drop:(( = ) '/')
      |> String.rstrip ~drop:(( = ) '/')
      |> String.split ~on:'/'
    in
    print_endline ("got request: " ^ Uri.path uri ^ " and body:\n" ^ body_string) ;
    let fn =
      match path with
      | ["execute"] ->
          Some F.execute
      | ["benchmark"] ->
          Some F.benchmark
      | ["fuzzing"; fnname] ->
        ( match fnname with
        | "of_internal_queryable_v1" ->
            Some F.of_internal_queryable_v1
        | "of_internal_roundtrippable_v0" ->
            Some F.of_internal_roundtrippable_v0
        | "of_unknown_json_v1" ->
            Some F.of_unknown_json_v1
        | "of_unknown_json_v0" ->
            Some F.of_unknown_json_v0
        | "to_developer_repr_v0" ->
            Some F.to_developer_repr_v0
        | "to_enduser_readable_text_v0" ->
            Some F.to_enduser_readable_text_v0
        | "to_hashable_repr" ->
            Some F.to_hashable_repr
        | "to_internal_queryable_v1" ->
            Some F.to_internal_queryable_v1
        | "to_internal_roundtrippable_v0" ->
            Some F.to_internal_roundtrippable_v0
        | "to_pretty_machine_json_v1" ->
            Some F.to_pretty_machine_json_v1
        | "to_safe_pretty_machine_yojson_v1" ->
            Some F.to_safe_pretty_machine_yojson_v1
        | "to_pretty_request_json" ->
            Some F.to_pretty_request_json
        | "to_pretty_response_json" ->
            Some F.to_pretty_response_json
        | "to_url_string" ->
            Some F.to_url_string
        | "dval_to_query" ->
            Some F.dval_to_query
        | "query_to_dval" ->
            Some F.query_to_dval
        | "dval_to_form_encoding" ->
            Some F.dval_to_form_encoding
        | "query_string_to_params" ->
            Some F.query_string_to_params
        | "params_to_query_string" ->
            Some F.params_to_query_string
        | "hash_v0" ->
            Some F.hash_v0
        | "hash_v1" ->
            Some F.hash_v1
        | _ ->
            None )
      | _ ->
          None
    in

    match (meth, fn) with
    | `POST, Some fn ->
      ( try
          let result = body_string |> fn in
          print_endline ("got response:\n" ^ result) ;
          respond_json_ok result
        with e ->
          let headers = Header.init () in
          let message = Libexecution.Exception.exn_to_string e in
          print_endline
            ( "error while calling "
            ^ Uri.to_string uri
            ^ "\n"
            ^ message
            ^ "\n\n" ) ;
          S.respond_string ~status:`Bad_request ~body:message ~headers () )
    | _ ->
        let headers = Header.init () in
        S.respond_string ~status:`Not_found ~body:"" ~headers ()
  in
  S.create
    ~stop
    ~mode:(`TCP (`Port Libservice.Config.legacy_fuzzing_server_port))
    (S.make ~callback ())


let () =
  try
    print_endline "Starting legacy fuzzing server" ;
    (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
    let () = Lwt.async_exception_hook := ignore in
    Libbackend.Init.init ~run_side_effects:false ~run_migrations:false ;
    Libexecution.Libs.init F.fns ;
    ignore (Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= server))
  with e ->
    let bt = Libexecution.Exception.get_backtrace () in
    Libbackend.Rollbar.last_ditch e ~bt "server" "no execution id"
