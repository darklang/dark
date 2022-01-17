open Core_kernel
open Lwt
open Libcommon
open Libbackend.Worker_util

let shutdown = ref false

let rec cron_checker (pid : int) () =
  let%lwt () = Lwt_unix.sleep 1.0 in
  let result = Libbackend.Cron.check_and_schedule_work_for_all_crons pid in
  match result with
  | Ok _ ->
      if not !shutdown then (cron_checker pid [@tailcall]) () else exit 0
  | Error (bt, e, log_params) ->
      Libcommon.Log.erroR
        "cron_checker"
        ~data:"Uncaught error"
        ~params:[("exn", Libexecution.Exception.exn_to_string e)]
        ~jsonparams:log_params ;
      Lwt.async (fun () ->
          Libbackend.Rollbar.report_lwt
            e
            bt
            CronChecker
            (Telemetry.ID.to_string pid)
          >>= fun _ -> Lwt.return ()) ;
      if not !shutdown then (cron_checker pid [@tailcall]) () else exit 0


let () =
  Libbackend.Init.init ~run_side_effects:false ~run_migrations:false ;
  (* If either thread sets the shutdown ref, the other will see it and
   * terminate; block until both have terminated. *)
  (* Three cases where we want to exit:
   * - healthcheck worker is instructed to die (/pkill), it sets the shutdown
   *   ref, cron_checker loop terminates
   * - heathcheck worker dies/is killed (unhandled exn), kubernetes will kill
   *   the pod when it fails healthcheck
   * - cron_checker thread dies; it's the main loop, the process exits *)
  ignore (Thread.create (health_check shutdown) ()) ;
  let pid = Unix.getpid () in
  Nocrypto_entropy_lwt.initialize () >>= cron_checker pid |> Lwt_main.run
