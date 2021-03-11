open Core_kernel
open Libexecution
open Libcommon
module Cl = Cohttp_lwt
module Clu = Cohttp_lwt_unix
module CResponse = Clu.Response
module Config = Libbackend_basics.Config
module Account = Libbackend_basics.Account
module Static_assets = Libbackend_basics.Static_assets
module Event_queue = Libbackend_basics.Event_queue

let status () =
  match Config.stroller_port with
  | None ->
      Lwt.return `Unconfigured
  | Some port ->
      let uri = Uri.make () ~scheme:"http" ~host:"127.0.0.1" ~port ~path:"/" in
      ( try%lwt
          let%lwt resp, _ = Clu.Client.get uri in
          let code = resp |> CResponse.status |> Cohttp.Code.code_of_status in
          match code with
          | 200 ->
              Lwt.return `Healthy
          | _ ->
              Lwt.return (`Unhealthy (string_of_int code))
        with e ->
          let bt = Exception.get_backtrace () in
          let%lwt _ =
            Rollbar.report_lwt
              e
              bt
              (Other "Stroller healthcheck")
              "Stroller healthcheck"
          in
          Lwt.return (`Unhealthy "Exception in stroller healthcheck") )


type heapio_type =
  | Track
  | Identify

let show_heapio_type (st : heapio_type) : string =
  match st with Track -> "track" | Identify -> "identify"


let _payload_for_heapio_event
    ~execution_id ~canvas ~canvas_id (payload : Yojson.Safe.t) : Yojson.Safe.t =
  let timestamp =
    Time.now () |> Core.Time.to_string_iso8601_basic ~zone:Core.Time.Zone.utc
  in
  match payload with
  | `Assoc orig_payload_items ->
      `Assoc
        ( orig_payload_items
        @ ( canvas
          |> Option.map ~f:(fun c ->
                 [ ("canvas", `String c)
                 ; ("organization", `String (Account.auth_domain_for c)) ])
          |> Option.value ~default:[] )
        @ ( canvas_id
          |> Option.map ~f:(fun c ->
                 [("canvas_id", `String (c |> Uuidm.to_string))])
          |> Option.value ~default:[] )
        @ ( execution_id
          |> Option.map ~f:(fun eid ->
                 [("execution_id", `String (eid |> Types.string_of_id))])
          |> Option.value ~default:[] )
        @ [("timestamp", `String timestamp)] )
  | _ ->
      Exception.internal
        "Expected payload to be an `Assoc list, was some other kind of Yojson.Safe.t"


let _log_params_for_heapio ~canvas ~canvas_id ~event ~(user_id : Uuidm.t) :
    (string * string) list =
  [ ("canvas", canvas)
  ; ("canvas_id", canvas_id |> Option.map ~f:Uuidm.to_string)
  ; ("event", event)
  ; ("userid", Some (user_id |> Uuidm.to_string)) ]
  |> List.filter_map ~f:(fun (k, v) ->
         match v with Some v -> Some (k, v) | _ -> None)


let heapio_event
    ?canvas_id
    ?canvas
    ~(user_id : Uuidm.t)
    ?execution_id
    ?event
    (msg_type : heapio_type)
    (payload : Yojson.Safe.t) : unit Lwt.t =
  let log_params = _log_params_for_heapio ~canvas ~canvas_id ~event ~user_id in
  let payload =
    payload |> _payload_for_heapio_event ~execution_id ~canvas ~canvas_id
  in
  match Config.stroller_port with
  | None ->
      Log.infO "stroller not configured, skipping heapio" ~params:log_params ;
      Lwt.return ()
  | Some port ->
      Log.infO "pushing heapio event via stroller" ~params:log_params ;
      let uri =
        Uri.make
          ()
          ~scheme:"http"
          ~host:"127.0.0.1"
          ~port
          ~path:
            (sprintf
               "heapio/%s/%s/event/%s"
               (Uuidm.to_string user_id)
               (msg_type |> show_heapio_type)
               (event |> Option.value ~default:(msg_type |> show_heapio_type)))
      in
      ( try%lwt
          let%lwt resp, _ =
            let payload = payload |> Yojson.Safe.to_string in
            Clu.Client.post uri ~body:(Cl.Body.of_string payload)
          in
          let code = resp |> CResponse.status |> Cohttp.Code.code_of_status in
          Log.infO
            "pushed to heapio via stroller"
            ~jsonparams:[("status", `Int code)]
            ~params:log_params ;
          Lwt.return ()
        with e ->
          let bt = Exception.get_backtrace () in
          let%lwt _ =
            Rollbar.report_lwt
              e
              bt
              (Heapio
                 (event |> Option.value ~default:(msg_type |> show_heapio_type)))
              ( execution_id
              |> Option.map ~f:Types.show_id
              |> Option.value ~default:"no execution_id" )
          in
          Lwt.return () )


let blocking_curl_post (url : string) (body : string) : int * string * string =
  let errorbuf = ref "" in
  let open Curl in
  let responsebuf = Buffer.create 16384 in
  try
    let c = init () in
    set_url c url ;
    set_followlocation c false ;
    set_maxredirs c 1 ;
    set_connecttimeout c 5 ;
    set_timeout c 10 ;
    set_httpheader c [] ;
    set_post c true ;
    set_postfields c body ;
    set_postfieldsize c (String.length body) ;
    perform c ;
    let responsebody = Buffer.contents responsebuf in
    let response = (get_responsecode c, !errorbuf, responsebody) in
    cleanup c ;
    response
  with Curl.CurlException (curl_code, code, s) ->
    let params =
      [ ("url", url)
      ; ("error", Curl.strerror curl_code)
      ; ("curl_code", string_of_int code)
      ; ("response", Buffer.contents responsebuf) ]
    in
    Log.erroR
      ("Internal HTTP error in blocking_curl_post: " ^ strerror curl_code)
      ~params ;
    (code, "", "")


let heapio_event_blocking
    ?canvas_id
    ?canvas
    ~(user_id : Uuidm.t)
    ?execution_id
    ?event
    (msg_type : heapio_type)
    (payload : Yojson.Safe.t) =
  let payload =
    payload |> _payload_for_heapio_event ~execution_id ~canvas ~canvas_id
  in
  let log_params = _log_params_for_heapio ~canvas_id ~canvas ~event ~user_id in
  match Config.stroller_port with
  | None ->
      Log.infO "stroller not configured, skipping heapio" ~params:log_params
  | Some port ->
      Log.infO "pushing heapio event via stroller" ~params:log_params ;
      let path =
        sprintf
          "heapio/%s/%s/event/%s"
          (Uuidm.to_string user_id)
          (msg_type |> show_heapio_type)
          (event |> Option.value ~default:(msg_type |> show_heapio_type))
      in
      let uri = Uri.make () ~scheme:"http" ~host:"127.0.0.1" ~port ~path in
      let payload = payload |> Yojson.Safe.to_string in
      let code, _, _ = blocking_curl_post (uri |> Uri.to_string) payload in
      ( match code with
      | 202 ->
          Log.infO
            "pushed to heapio via stroller"
            ~jsonparams:[("status", `Int code)]
            ~params:log_params
      | _ ->
          Log.erroR
            "failed to push to heapio via stroller"
            ~jsonparams:[("status", `Int code)]
            ~params:log_params ) ;
      ()


let heapio_track
    ~(canvas_id : Uuidm.t)
    ~(user_id : Uuidm.t)
    ~(canvas : string)
    ~(execution_id : Types.id)
    ~(event : string)
    (msg_type : heapio_type)
    (payload : Yojson.Safe.t) : unit Lwt.t =
  heapio_event ~canvas_id ~canvas ~user_id ~execution_id ~event msg_type payload


(* We call this in two contexts: DarkInternal:: fns, and
 * bin/heapio_identify_users.exe. Neither of those is an async/lwt context, so
 * we use the blocking_curl_post instead of Curl_lwt. *)
let heapio_identify_user (username : string) : unit =
  match Account.get_user_and_created_at_and_analytics_metadata username with
  | None ->
      let bt = Exception.get_backtrace () in
      ( match
          Rollbar.report
            (Exception.internal
               "No user found when calling heapio_identify user")
            bt
            (Other "heapio_identify_user")
            "No execution id"
        with
      | `Failure ->
          Log.erroR "Failed to Rollbar.report in heapio_identify_user"
      | _ ->
          () )
  | Some (user_info_and_created_at, heapio_metadata) ->
      let organization =
        username
        |> Authorization.orgs_for
        (* A user's orgs for this purpose do not include orgs it has
         * read-only access to *)
        |> List.filter ~f:(function _, rw -> rw = ReadWrite)
        (* If you have one org, that's your org! If you have no orgs, or
         * more than one, then we just use your username. This is because
         * Heap's properties/traits don't support lists. *)
        |> function [(org_name, _)] -> org_name | _ -> username
      in
      let payload =
        let payload =
          `Assoc
            [ ("username", `String user_info_and_created_at.username)
            ; ("email", `String user_info_and_created_at.email)
            ; ("name", `String user_info_and_created_at.name)
            ; ("admin", `Bool user_info_and_created_at.admin)
            ; ("handle", `String user_info_and_created_at.username)
            ; ("organization", `String organization) ]
        in
        (* We do zero checking of fields in heapio_metadata, but this is ok
         * because it's a field we control, going to a service only we see.
         * If we wanted to harden this later, we could List.filter the
         * heapio_metadata yojson *)
        Yojson.Safe.Util.combine payload heapio_metadata
      in
      heapio_event_blocking
        ~user_id:user_info_and_created_at.id
        Identify
        payload


let push
    ?(execution_id : Types.id option)
    ~(canvas_id : Uuidm.t)
    ~(event : string)
    (payload : string) : unit =
  let canvas_id_str = Uuidm.to_string canvas_id in
  let log_params = [("canvas_id", canvas_id_str); ("event", event)] in
  match Config.stroller_port with
  | None ->
      Log.infO "stroller not configured, skipping push" ~params:log_params
  | Some port ->
      let uri =
        Uri.make
          ()
          ~scheme:"http"
          ~host:"127.0.0.1"
          ~port
          ~path:(sprintf "canvas/%s/events/%s" canvas_id_str event)
      in
      Lwt.async (fun () ->
          try%lwt
            let%lwt resp, _ =
              Clu.Client.post uri ~body:(Cl.Body.of_string payload)
            in
            let code = resp |> CResponse.status |> Cohttp.Code.code_of_status in
            Log.infO
              "pushed via stroller"
              ~jsonparams:[("status", `Int code)]
              ~params:log_params ;
            Lwt.return ()
          with e ->
            let bt = Exception.get_backtrace () in
            let%lwt _ =
              Rollbar.report_lwt
                e
                bt
                (Push event)
                ( execution_id
                |> Option.map ~f:Types.show_id
                |> Option.value ~default:"not in execution" )
            in
            Lwt.return ())


let push_new_trace_id
    ~(execution_id : Types.id)
    ~(canvas_id : Uuidm.t)
    (trace_id : Uuidm.t)
    (tlids : Types.tlid list) =
  let payload = Analysis.to_new_trace_frontend (trace_id, tlids) in
  push ~execution_id ~canvas_id ~event:"new_trace" payload


let push_new_404
    ~(execution_id : Types.id)
    ~(canvas_id : Uuidm.t)
    (fof : Stored_event.four_oh_four) =
  let payload = Analysis.to_new_404_frontend fof in
  push ~execution_id ~canvas_id ~event:"new_404" payload


let push_new_static_deploy
    ~(execution_id : Types.id)
    ~(canvas_id : Uuidm.t)
    (asset : Static_assets.static_deploy) =
  let payload = Analysis.to_new_static_deploy_frontend asset in
  push ~execution_id ~canvas_id ~event:"new_static_deploy" payload


(* For exposure as a DarkInternal function *)
let push_new_event
    ~(execution_id : Types.id)
    ~(canvas_id : Uuidm.t)
    ~(event : string)
    (payload : string) =
  push ~execution_id ~canvas_id ~event payload


let push_worker_states
    ~(execution_id : Types.id)
    ~(canvas_id : Uuidm.t)
    (ws : Event_queue.Worker_states.t) =
  let payload = Analysis.to_worker_schedules_push ws in
  push ~execution_id ~canvas_id ~event:"worker_state" payload
