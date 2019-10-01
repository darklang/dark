open Core_kernel
open Lwt
open Libcommon

(* We share this with the cron_checker *)
let health_check
    ((ctx, execution_id, shutdown) :
      Libservice.Rollbar.err_ctx * Libexecution.Types.id * bool ref) : unit =
  Init.init ~run_side_effects:false ;
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
                Log.infO
                  (Libservice.Rollbar.string_of_ctx ctx)
                  ~data:"Spinning up health check service"
                  ~params:
                    [ ( "execution_id"
                      , Libexecution.Types.string_of_id execution_id ) ] ;
                Libservice.Health_check.run ~shutdown ~execution_id
          with e ->
            let execution_id = Libexecution.Util.create_id () in
            Log.erroR
              (Libservice.Rollbar.string_of_ctx ctx)
              ~data:"Health check service threw error"
              ~params:
                [ ("execution_id", Libexecution.Types.string_of_id execution_id)
                ; ("exn", Log.dump e) ] ;
            let bt = Libexecution.Exception.get_backtrace () in
            Lwt.async (fun () ->
                Rollbar.report_lwt
                  e
                  bt
                  ctx
                  (Libexecution.Types.string_of_id execution_id) ) ;
            fail e )
