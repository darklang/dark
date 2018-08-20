open Core_kernel
open Lwt

let shutdown = ref false

let () =
  Libbackend.Init.init ~run_side_effects:false;
  let execution_id = Libexecution.Util.create_id () in
  (* spin up health http server *)
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
            Libbackend.Rollbar.report_lwt e bt (CronChecker) (string_of_int execution_id));
        fail e
  end;
  let rec queue_worker () =
    let%lwt () = Lwt_unix.sleep 1.0 in
    let result = Libbackend.Queue_worker.run execution_id in
    match result with
    | Ok _ ->
      if not !shutdown
      then
        (queue_worker [@tailcall]) ()
      else
        Lwt.return ()
    | Error (bt, e) ->
      Libcommon.Log.erroR "queue_worker"
        ~data:"Unhandled exception bubbled to queue worker"
        ~params:["execution_id", string_of_int execution_id
                ;"exn", Libexecution.Exception.exn_to_string e
                ];
      Lwt.async (fun () ->
         Libbackend.Rollbar.report_lwt e bt (EventQueue) (string_of_int execution_id));
      if not !shutdown
      then
        (queue_worker [@tailcall]) ()
      else
        Lwt.return ()
  in
  Lwt_main.run (queue_worker ())
