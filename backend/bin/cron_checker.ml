open Core_kernel
open Lwt
open Libcommon

let shutdown = ref false

let () =
  Libbackend.Init.init ~run_side_effects:false ;
  let execution_id = Libexecution.Util.create_id () in
  (* spin up health http server *)
  let run_health_check_server =
    if Array.length Sys.argv >= 2
    then not (Sys.argv.(1) = "--no-health-check")
    else true
  in
  if run_health_check_server
  then
    Lwt.async (fun () ->
        (* We're sharing a ref across threads here, but Health_check writes
        * `true` to that ref regardless of its value and we never write to
        * it, so there's no sharing issue. *)
        try%lwt
              Libcommon.Log.infO
                "cron_checker"
                ~data:"Spinning up health check service" ;
              Libservice.Health_check.run ~shutdown ~execution_id
        with e ->
          Libcommon.Log.erroR
            "cron_checker"
            ~data:"Health check service threw error"
            ~params:[("exn", Libcommon.Log.dump e)] ;
          let bt = Libexecution.Exception.get_backtrace () in
          Lwt.async (fun () ->
              Libbackend.Rollbar.report_lwt
                e
                bt
                CronChecker
                (Libexecution.Types.string_of_id execution_id) ) ;
          fail e ) ;
  let rec cron_checker () =
    let%lwt () = Lwt_unix.sleep 1.0 in
    let result = Libbackend.Cron.check_all_canvases execution_id in
    match result with
    | Ok _ ->
        if not !shutdown then (cron_checker [@tailcall]) () else Lwt.return ()
    | Error (bt, e) ->
        Libcommon.Log.erroR
          "cron_checker"
          ~data:"Uncaught error"
          ~params:[("exn", Libexecution.Exception.exn_to_string e)] ;
        Lwt.async (fun () ->
            Libbackend.Rollbar.report_lwt
              e
              bt
              CronChecker
              (Libexecution.Types.string_of_id execution_id) ) ;
        if not !shutdown then (cron_checker [@tailcall]) () else Lwt.return ()
  in
  Lwt_main.run
    (Log.add_log_annotations
       [ ( "execution_id"
         , `String (Libexecution.Types.string_of_id execution_id) ) ]
       (fun _ -> Nocrypto_entropy_lwt.initialize () >>= cron_checker))
