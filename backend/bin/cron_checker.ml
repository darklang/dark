open Core_kernel
open Lwt
open Libcommon
open Libbackend.Worker_util

let shutdown = ref false

let cron_checker execution_id =
  Libbackend.Init.init ~run_side_effects:false ;
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


let () =
  let execution_id = Libexecution.Util.create_id () in
  let health_check_thread =
    Thread.create
      health_check
      (Libservice.Rollbar.CronChecker, execution_id, shutdown)
  in
  let cron_checker_thread = Thread.create cron_checker execution_id in
  (* If either thread sets the shutdown ref, the other will see it and
   * terminate; block until both have terminated. *)
  Thread.join cron_checker_thread ;
  Thread.join health_check_thread
