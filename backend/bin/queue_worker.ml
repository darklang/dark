open Core_kernel
open Lwt
open Libcommon
open Libbackend.Worker_util

let shutdown = ref false

let queue_worker execution_id =
  Libbackend.Init.init ~run_side_effects:false ;
  let rec queue_worker () =
    let result = Libbackend.Queue_worker.run execution_id in
    match result with
    | Ok None ->
        if not !shutdown
        then
          let%lwt () = Lwt_unix.sleep 1.0 in
          (queue_worker [@tailcall]) ()
        else Lwt.return ()
    | Ok (Some _) ->
        if not !shutdown then (queue_worker [@tailcall]) () else Lwt.return ()
    | Error (bt, e) ->
        Log.erroR
          "queue_worker"
          ~data:"Unhandled exception bubbled to queue worker"
          ~params:
            [ ("execution_id", Libexecution.Types.string_of_id execution_id)
            ; ("exn", Libexecution.Exception.exn_to_string e) ] ;
        Lwt.async (fun () ->
            Libbackend.Rollbar.report_lwt
              e
              bt
              EventQueue
              (Libexecution.Types.string_of_id execution_id)
            >>= fun _ -> Lwt.return ()) ;
        Thread.yield () ;
        if not !shutdown
        then (queue_worker [@tailcall]) ()
        else (
          shutdown := true ;
          Lwt.return () )
  in
  Lwt_main.run
    (Log.add_log_annotations
       [("execution_id", `String (Libexecution.Types.string_of_id execution_id))]
       (fun _ -> Nocrypto_entropy_lwt.initialize () >>= queue_worker))


let () =
  Random.self_init () ;
  let execution_id = Libexecution.Util.create_id () in
  (* If either thread sets the shutdown ref, the other will see it and
   * terminate; block until both have terminated. *)
  let health_check_thread = Thread.create (health_check shutdown) () in
  let queue_worker_thread = Thread.create queue_worker execution_id in
  Thread.join health_check_thread ;
  Thread.join queue_worker_thread
