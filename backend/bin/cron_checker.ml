open Core_kernel
open Lwt
open Libcommon
open Libbackend.Worker_util

let shutdown = ref false

let cron_checker execution_id =
  let rec cron_checker () =
    let%lwt () = Lwt_unix.sleep 1.0 in
    let result = Libbackend.Cron.check_all_canvases execution_id in
    match result with
    | Ok _ ->
        if not !shutdown then (cron_checker [@tailcall]) () else (exit 0)
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
              (Libexecution.Types.string_of_id execution_id)
            >>= fun _ -> Lwt.return ()) ;
        if not !shutdown then (cron_checker [@tailcall]) () else exit 0
  in
  Lwt_main.run
    (Log.add_log_annotations
       [("execution_id", `String (Libexecution.Types.string_of_id execution_id))]
       (fun _ -> Nocrypto_entropy_lwt.initialize () >>= cron_checker))


let () =
  let execution_id = Libexecution.Util.create_id () in
  Libbackend.Init.init ~run_side_effects:false ;
  (* If either thread sets the shutdown ref, the other will see it and
   * terminate; block until both have terminated. *)
  (* Three cases where we want to exit:
   * - healthcheck worker is instructed to die (/pkill), it sets the shutdown
   *   ref, cron_checker loop terminates
   * - heathcheck worker dies/is killed (unhandled exn), kubernetes will kill
   *   the pod when it fails healthcheck
   * - cron_checker thread dies; it's the main loop, the process exits *)
  ignore (Thread.create (health_check shutdown) ()) ;
  cron_checker execution_id
