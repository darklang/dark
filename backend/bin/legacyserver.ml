open Core_kernel
open Lwt
module Cl = Cohttp_lwt
module Clu = Cohttp_lwt_unix
module S = Clu.Server
module CRequest = Clu.Request
module CResponse = Clu.Response
module Client = Clu.Client
module Header = Cohttp.Header
module F = Liblegacy.Fuzzing
module BS = Liblegacy.Serialization

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
    let fn =
      match path with
      | ["execute"] ->
          Some F.execute
      | ["bs"; fnname] ->
        ( match fnname with
        | "user_fn_bin2json" ->
            Some BS.user_fn_bin2json
        | "user_tipe_bin2json" ->
            Some BS.user_tipe_bin2json
        | "handler_bin2json" ->
            Some BS.handler_bin2json
        | "db_bin2json" ->
            Some BS.db_bin2json
        | "oplist_bin2json" ->
            Some BS.oplist_bin2json
        | "pos_bin2json" ->
            Some BS.pos_bin2json
        | "expr_bin2json" ->
            Some BS.expr_bin2json
        | "expr_tlid_pair_bin2json" ->
            Some BS.expr_tlid_pair_bin2json
        | "user_fn_json2bin" ->
            Some BS.user_fn_json2bin
        | "user_tipe_json2bin" ->
            Some BS.user_tipe_json2bin
        | "handler_json2bin" ->
            Some BS.handler_json2bin
        | "db_json2bin" ->
            Some BS.db_json2bin
        | "oplist_json2bin" ->
            Some BS.oplist_json2bin
        | "pos_json2bin" ->
            Some BS.pos_json2bin
        | "expr_json2bin" ->
            Some BS.expr_json2bin
        | "expr_tlid_pair_json2bin" ->
            Some BS.expr_tlid_pair_json2bin
        | _ ->
            None )
      | ["fuzzing"; fnname] ->
        ( match fnname with
        | "of_internal_queryable_v0" ->
            Some F.of_internal_queryable_v0
        | "of_internal_queryable_v1" ->
            Some F.of_internal_queryable_v1
        | "of_internal_roundtrippable_v0" ->
            Some F.of_internal_roundtrippable_v0
        | "of_unknown_json_v1" ->
            Some F.of_unknown_json_v1
        | "to_developer_repr_v0" ->
            Some F.to_developer_repr_v0
        | "to_enduser_readable_text_v0" ->
            Some F.to_enduser_readable_text_v0
        | "to_hashable_repr" ->
            Some F.to_hashable_repr
        | "to_internal_queryable_v0" ->
            Some F.to_internal_queryable_v0
        | "to_internal_queryable_v1" ->
            Some F.to_internal_queryable_v1
        | "to_internal_roundtrippable_v0" ->
            Some F.to_internal_roundtrippable_v0
        | "to_pretty_machine_json_v1" ->
            Some F.to_pretty_machine_json_v1
        | "to_url_string" ->
            Some F.to_url_string
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
          let result = body_string |> fn |> respond_json_ok in
          print_endline ("success calling " ^ Uri.to_string uri) ;
          result
        with e ->
          let headers = Header.init () in
          let message =
            Libexecution.Exception.exn_to_string e
          in
          print_endline ("error while calling " ^ Uri.to_string uri ^ "\n" ^ message) ;
          S.respond_string
            ~status:`Bad_request
            ~body:message
            ~headers
            () )
    | _ ->
        let headers = Header.init () in
        S.respond_string ~status:`Not_found ~body:"" ~headers ()
  in
  (* FSTODO: make port configurable *)
  S.create
    ~stop
    ~mode:(`TCP (`Port Libservice.Config.legacyserver_port))
    (S.make ~callback ())


let () =
  try
    print_endline "Starting legacy server" ;
    (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
    let () = Lwt.async_exception_hook := ignore in
    Libbackend.Init.init ~run_side_effects:false ;
    Libexecution.Libs.init F.fns ;
    Lwt_main.run (server ()) |> ignore
  with e ->
    let bt = Libexecution.Exception.get_backtrace () in
    Libbackend.Rollbar.last_ditch e ~bt "server" "no execution id"
