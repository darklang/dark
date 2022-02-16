open Core_kernel
open Lwt
module Cl = Cohttp_lwt
module Clu = Cohttp_lwt_unix
module S = Clu.Server
module CRequest = Clu.Request
module CResponse = Clu.Response
module Client = Clu.Client
module Header = Cohttp.Header
module BS = Liblegacyserialization.Serialization

let respond_json_ok (body : string) : (CResponse.t * Cl.Body.t) Lwt.t =
  let headers =
    Header.of_list
      [ ("Content-Length", String.length body |> string_of_int)
      ; ("Content-Type", "application/json") ]
  in
  S.respond_string ~status:`OK ~body ~headers ()


let ready = ref false

let shutdown = ref false

let server () =
  let stop, stopper = Lwt.wait () in
  let callback (conn : S.conn) (req : CRequest.t) (req_body : Cl.Body.t) :
      (CResponse.t * Cl.Body.t) Lwt.t =
    let%lwt body_string = Cl.Body.to_string req_body in
    let ch, _ = conn in
    let ip_address : string =
      match Conduit_lwt_unix.endp_of_flow ch with
      | `TCP (ip, port) ->
          Ipaddr.to_string ip
      | _ ->
          assert false
    in
    let uri = CRequest.uri req in
    let meth = CRequest.meth req in
    let path =
      uri
      |> Uri.path
      |> String.lstrip ~drop:(( = ) '/')
      |> String.rstrip ~drop:(( = ) '/')
      |> String.split ~on:'/'
    in
    let headers = Header.of_list [] in
    let fn =
      match path with
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
        | "toplevel_bin2json" ->
            Some BS.toplevel_bin2json
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
      | _ ->
          None
    in
    match (meth, fn) with
    | `POST, Some fn ->
      ( try
          let result = body_string |> fn in
          Libcommon.Log.infO
            "Successfully completed legacyserver call"
            ~data:""
            ~params:
              [ ("uri", Uri.to_string uri)
              ; ("path", Uri.path uri)
              ; ("method", Cohttp.Code.string_of_method meth)
              ; ("length", string_of_int (String.length result))
              ; ("ip_address", ip_address)
              ; ("result", String.sub ~pos:0 ~len:50 result) ] ;
          respond_json_ok result
        with e ->
          let headers = Header.init () in
          let message = Libexecution.Exception.exn_to_string e in
          Libcommon.Log.erroR
            "Error in legacyserver call"
            ~data:message
            ~params:
              [ ("uri", Uri.to_string uri)
              ; ("path", Uri.path uri)
              ; ("method", Cohttp.Code.string_of_method meth)
              ; ("body", body_string) ] ;
          S.respond_string ~status:`Bad_request ~body:message ~headers () )
    | `GET, None ->
      ( match path with
      | ["k8s"; "livenessProbe"] ->
          S.respond_string
            ~status:`OK
            ~body:"Hello internal overlord"
            ~headers
            ()
      | ["k8s"; "readinessProbe"] ->
          let checks = [] in
          ( match checks with
          | [] ->
              if !ready
              then
                S.respond_string
                  ~status:`OK
                  ~body:"Hello internal overlord"
                  ~headers
                  ()
              else (
                Libcommon.Log.infO "Service ready" ;
                ready := true ;
                S.respond_string
                  ~status:`OK
                  ~body:"Hello internal overlord"
                  ~headers
                  () )
          | _ ->
              Libcommon.Log.erroR
                "Failed readiness check"
                ~params:[("checks", String.concat ~sep:"," checks)] ;
              S.respond_string
                ~status:`Bad_request
                ~body:"Sorry internal overlord"
                ~headers
                () )
      (* For GKE graceful termination *)
      | ["k8s"; "pkill"] ->
          if not !shutdown (* note: this is a ref, not a boolean `not` *)
          then (
            shutdown := true ;
            Libcommon.Log.infO
              "shutdown"
              ~data:"Received shutdown request - shutting down"
              ~params:[] ;
            (* k8s gives us 30 seconds, so ballpark 2s for overhead *)
            Lwt_unix.sleep 28.0
            >>= fun _ ->
            Lwt.wakeup stopper () ;
            S.respond_string ~status:`OK ~body:"Terminated" ~headers () )
          else (
            Libcommon.Log.infO
              "shutdown"
              ~data:
                "Received redundant shutdown request - already shutting down" ;
            S.respond_string ~status:`OK ~body:"Terminated" ~headers () )
      | _ ->
          let headers = Header.init () in
          S.respond_string ~status:`Not_found ~body:"" ~headers () )
    | _ ->
        let headers = Header.init () in
        S.respond_string ~status:`Not_found ~body:"" ~headers ()
  in
  S.create
    ~stop
    ~mode:(`TCP (`Port Libservice.Config.legacy_serialization_server_port))
    (S.make ~callback ())


let () =
  try
    Libcommon.Log.infO "Starting legacy server" ;

    (* Below inlined from Libbackend.Init.init. We want to avoid needing the DB. *)
    Printexc.record_backtrace true ;
    Exn.initialize_module () ;
    (* We need this to ensure that infix is correct *)
    Libexecution.Init.init
      Libservice.Config.log_level
      Libservice.Config.log_format
      [] ;
    (* Dark-specific stuff *)
    Libcommon.Log.infO "Libbackend" ~data:"Initialization Complete" ;

    (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
    let () = Lwt.async_exception_hook := ignore in
    ignore (Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= server))
  with e ->
    let bt = Libexecution.Exception.get_backtrace () in
    Libservice.Rollbar.last_ditch e ~bt "server" "no execution id"
