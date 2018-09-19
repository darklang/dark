open Core_kernel
open Lwt

let shutdown = ref false

let () =
  Libbackend.Init.init ~run_side_effects:false;
  let execution_id = Libexecution.Util.create_id () in
  (* spin up health http server *)
  if (not (Sys.argv.(1) = "--no-health-check")) then
    Lwt.async begin
      fun () ->
        (* We're sharing a ref across threads here, but Health_check writes
        * `true` to that ref regardless of its value and we never write to
        * it, so there's no sharing issue. *)
        try%lwt
          Libservice.Health_check.run ~shutdown ~execution_id
        with e ->
          let bt = Libexecution.Exception.get_backtrace () in
          Lwt.async (fun () ->
              Libbackend.Rollbar.report_lwt e bt (CronChecker)
                (Libexecution.Types.string_of_id execution_id));
          fail e
    end;
  let rec cron_checker () =
    let%lwt () = Lwt_unix.sleep 1.0 in
    let result = Libbackend.Cron.check_all_canvases execution_id in
    match result with
    | Ok _ ->
      if not !shutdown
      then
        (cron_checker [@tailcall]) ()
      else
        Lwt.return ()
    | Error (bt, e) ->
      Libcommon.Log.erroR "cron_checker"
        ~data:"Uncaught error"
        ~params:["execution_id", Libexecution.Types.string_of_id execution_id
                ;"exn", Libexecution.Exception.exn_to_string e
                ];
      Lwt.async (fun () ->
         Libbackend.Rollbar.report_lwt e bt (CronChecker) (Libexecution.Types.string_of_id execution_id));
      if not !shutdown
      then
        (cron_checker [@tailcall]) ()
      else
        Lwt.return ()
  in
  Lwt_main.run (cron_checker ())
