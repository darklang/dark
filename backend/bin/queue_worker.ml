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
            ( try
                Libbackend.Rollbar.report_lwt
                  e
                  bt
                  EventQueue
                  (Libexecution.Types.string_of_id execution_id)
              with e ->
                Log.erroR
                  "Error in Rollbar.report_lwt in queue worker"
                  ~data:(Libexecution.Exception.exn_to_string e) ;
                Lwt.return (`Failure : Libbackend.Rollbar.result) )
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
  (* Three cases where we want to exit:
   * - healthcheck worker is instructed to die (/pkill), it sets the shutdown
   *   ref, queue_worker loop terminates
   * - heathcheck worker dies/is killed (unhandled exn), kubernetes will kill
   *   the pod when it fails healthcheck
   * - queue_worker thread dies; it's the main loop, the process exits *)
  let _ = Thread.create (health_check shutdown) () in
  queue_worker execution_id
