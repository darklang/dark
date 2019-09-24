open Core_kernel
open Lwt
open Libcommon

let shutdown = ref false

let health_check execution_id =
  Libbackend.Init.init ~run_side_effects:false ;
  (* spin up health http server *)
  let run_health_check_server =
    if Array.length Sys.argv >= 2
    then not (Sys.argv.(1) = "--no-health-check")
    else true
  in
  if run_health_check_server
  then
    Lwt_main.run
      (* We're sharing a ref across threads here, but Health_check writes
        * `true` to that ref regardless of its value and we never write to
        * it, so there's no sharing issue. *)
        ( try%lwt
                Libcommon.Log.infO
                  "queue_worker"
                  ~data:"Spinning up health check service"
                  ~params:
                    [ ( "execution_id"
                      , Libexecution.Types.string_of_id execution_id ) ] ;
                Libservice.Health_check.run ~shutdown ~execution_id
          with e ->
            let execution_id = Libexecution.Util.create_id () in
            Libcommon.Log.erroR
              "queue_worker"
              ~data:"Health check service threw error"
              ~params:
                [ ("execution_id", Libexecution.Types.string_of_id execution_id)
                ; ("exn", Libcommon.Log.dump e) ] ;
            let bt = Libexecution.Exception.get_backtrace () in
            Lwt.async (fun () ->
                Libbackend.Rollbar.report_lwt
                  e
                  bt
                  EventQueue
                  (Libexecution.Types.string_of_id execution_id) ) ;
            fail e )


let queue_worker execution_id =
  Thread.create (fun _ ->
      Libbackend.Init.init ~run_side_effects:false ;
      let rec queue_worker () =
        let%lwt () = Lwt_unix.sleep 1.0 in
        let result = Libbackend.Queue_worker.run execution_id in
        match result with
        | Ok _ ->
            if not !shutdown
            then (queue_worker [@tailcall]) ()
            else Lwt.return ()
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
                  (Libexecution.Types.string_of_id execution_id) ) ;
            if not !shutdown
            then (queue_worker [@tailcall]) ()
            else (
              shutdown := true ;
              Lwt.return () )
      in
      Lwt_main.run
        (Log.add_log_annotations
           [ ( "execution_id"
             , `String (Libexecution.Types.string_of_id execution_id) ) ]
           (fun _ -> Nocrypto_entropy_lwt.initialize () >>= queue_worker)) )


let () =
  let execution_id = Libexecution.Util.create_id () in
  let health_check_thread = Thread.create health_check execution_id in
  let queue_worker_thread = Thread.create queue_worker execution_id in
  (* If either thread sets the shutdown ref, the other will see it and
   * terminate; block until both have terminated. *)
  Thread.join queue_worker_thread ;
  Thread.join health_check_thread
