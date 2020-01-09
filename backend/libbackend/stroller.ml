open Core_kernel
open Libexecution
open Libcommon
module Cl = Cohttp_lwt
module Clu = Cohttp_lwt_unix
module CResponse = Clu.Response

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


type segment_type =
  | Track
  | Identify

let show_segment_type (st : segment_type) : string =
  match st with Track -> "track" | Identify -> "identify"


let _payload_for_segment_event
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


let _log_params_for_segment ~canvas ~canvas_id ~event ~username :
    (string * string) list =
  [ ("canvas", canvas)
  ; ("canvas_id", canvas_id |> Option.map ~f:Uuidm.to_string)
  ; ("event", event)
  ; ("username", Some username) ]
  |> List.filter_map ~f:(fun (k, v) ->
         match v with Some v -> Some (k, v) | _ -> None)


let _segment_event
    ?canvas_id
    ?canvas
    ~(username : string)
    ?execution_id
    ?event
    (msg_type : segment_type)
    (payload : Yojson.Safe.t) : unit Lwt.t =
  let log_params =
    _log_params_for_segment ~canvas ~canvas_id ~event ~username
  in
  let payload =
    payload |> _payload_for_segment_event ~execution_id ~canvas ~canvas_id
  in
  match Config.stroller_port with
  | None ->
      Log.infO "stroller not configured, skipping segment" ~params:log_params ;
      Lwt.return ()
  | Some port ->
      Log.infO "pushing segment event via stroller" ~params:log_params ;
      let uri =
        Uri.make
          ()
          ~scheme:"http"
          ~host:"127.0.0.1"
          ~port
          ~path:
            (sprintf
               "segment/%s/%s/event/%s"
               username
               (msg_type |> show_segment_type)
               (event |> Option.value ~default:(msg_type |> show_segment_type)))
      in
      ( try%lwt
          let%lwt resp, _ =
            let payload = payload |> Yojson.Safe.to_string in
            Clu.Client.post uri ~body:(Cl.Body.of_string payload)
          in
          let code = resp |> CResponse.status |> Cohttp.Code.code_of_status in
          Log.infO
            "pushed to segment via stroller"
            ~jsonparams:[("status", `Int code)]
            ~params:log_params ;
          Lwt.return ()
        with e ->
          let bt = Exception.get_backtrace () in
          let%lwt _ =
            Rollbar.report_lwt
              e
              bt
              (Segment
                 (event |> Option.value ~default:(msg_type |> show_segment_type)))
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


let segment_event_blocking
    ?canvas_id
    ?canvas
    ~(username : string)
    ?execution_id
    ?event
    (msg_type : segment_type)
    (payload : Yojson.Safe.t) =
  let payload =
    payload |> _payload_for_segment_event ~execution_id ~canvas ~canvas_id
  in
  let log_params =
    _log_params_for_segment ~canvas_id ~canvas ~event ~username
  in
  match Config.stroller_port with
  | None ->
      Log.infO "stroller not configured, skipping segment" ~params:log_params
  | Some port ->
      Log.infO "pushing segment event via stroller" ~params:log_params ;
      let path =
        sprintf
          "segment/%s/%s/event/%s"
          username
          (msg_type |> show_segment_type)
          (event |> Option.value ~default:(msg_type |> show_segment_type))
      in
      let uri = Uri.make () ~scheme:"http" ~host:"127.0.0.1" ~port ~path in
      let payload = payload |> Yojson.Safe.to_string in
      let code, _, _ = blocking_curl_post (uri |> Uri.to_string) payload in
      ( match code with
      | 202 ->
          Log.infO
            "pushed to segment via stroller"
            ~jsonparams:[("status", `Int code)]
            ~params:log_params
      | _ ->
          Log.erroR
            "failed to push to segment via stroller"
            ~jsonparams:[("status", `Int code)]
            ~params:log_params ) ;
      ()


let segment_track
    ~(canvas_id : Uuidm.t)
    ~(canvas : string)
    ~(username : string)
    ~(execution_id : Types.id)
    ~(event : string)
    (msg_type : segment_type)
    (payload : Yojson.Safe.t) : unit Lwt.t =
  _segment_event
    ~canvas_id
    ~canvas
    ~username
    ~execution_id
    ~event
    msg_type
    payload


type identify_payload =
  { username : string
  ; email : string
  ; name : string
  ; admin : bool
  ; created_at : string
  ; organization : string }
[@@deriving yojson]

(* We call this in two contexts: DarkInternal:: fns, and
 * bin/segment_identify_users.exe. Neither of those is an async/lwt context, so
 * we use the blocking_curl_post instead of Curl_lwt. *)
let segment_identify_user (username : string) : unit =
  let payload = Account.get_user_and_created_at username in
  let organization =
    payload
    |> Option.map ~f:(fun p ->
           p.username
           |> Authorization.orgs_for
           (* A user's orgs for this purpose do not include orgs it has
            * read-only access to *)
           |> List.filter ~f:(function
                  | _, Read ->
                      false
                  | _, ReadWrite ->
                      true)
           (* If you have one org, that's your org! If you have no orgs, or
            * more than one, then we just use your username. *)
           |> fun orgs ->
           match orgs with [(org_name, _)] -> org_name | _ -> p.username)
  in
  Option.map2 organization payload ~f:(fun organization payload ->
      { username = payload.username
      ; email = payload.email
      ; name = payload.name
      ; admin = payload.admin
      ; created_at = payload.created_at
      ; organization })
  |> Option.map ~f:identify_payload_to_yojson
  |> Option.map ~f:(fun payload ->
         segment_event_blocking ~username Identify payload)
  |> Option.value ~default:()


let push
    ?(execution_id : Types.id option)
    ~(canvas_id : Uuidm.t)
    ~(event : string)
    (payload : string) : unit =
  let canvas_id_str = Uuidm.to_string canvas_id in
  let log_params =
    [("canvas_id", canvas_id_str); ("event", event); ("payload", payload)]
  in
  match Config.stroller_port with
  | None ->
      Log.infO "stroller not configured, skipping push" ~params:log_params
  | Some port ->
      Log.infO "pushing via stroller" ~params:log_params ;
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
