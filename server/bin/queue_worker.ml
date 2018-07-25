open Core_kernel
open Lwt

let shutdown = ref false

let () =
  Libbackend.Init.init ~run_side_effects:false;
  let execution_id = Libexecution.Util.create_id () in
  (* spin up health http server *)
  Lwt.async
    (fun () ->
       (* We're sharing a ref across threads here, but Health_check writes
        * `true` to that ref regardless of its value and we never write to
        * it, so there's no sharing issue. *)
       catch (fun () -> Libservice.Health_check.run ~shutdown ~execution_id)
         (fun e ->
            let bt = Libexecution.Exception.get_backtrace () in
            Libbackend.Rollbar.report_lwt e bt (EventQueue) (string_of_int execution_id) >>=
            fun _ -> fail e));
  let rec queue_worker () =
    Lwt_unix.sleep 1.0 >>= fun _ ->
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
        ~data:"Uncaught error"
        ~params:["execution_id", string_of_int execution_id
                ;"exn", Libexecution.Exception.exn_to_string e];
      Libbackend.Rollbar.report_lwt e bt (EventQueue) (string_of_int execution_id) >>= fun _ ->
      if not !shutdown
      then
        (queue_worker [@tailcall]) ()
      else
        Lwt.return ()
  in
  Lwt_main.run (queue_worker ())
