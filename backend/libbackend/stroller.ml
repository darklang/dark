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
              let code =
                resp |> CResponse.status |> Cohttp.Code.code_of_status
              in
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


let push
    ~(execution_id : Types.id)
    ~(canvas_id : Uuidm.t)
    ~(event : string)
    (payload : string) =
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
                let code =
                  resp |> CResponse.status |> Cohttp.Code.code_of_status
                in
                Log.infO
                  "pushed via stroller"
                  ~jsonparams:[("status", `Int code)]
                  ~params:log_params ;
                Lwt.return ()
          with e ->
            let bt = Exception.get_backtrace () in
            let%lwt _ =
              Rollbar.report_lwt e bt (Push event) (Types.show_id execution_id)
            in
            Lwt.return () )


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
