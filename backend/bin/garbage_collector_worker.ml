open Core_kernel
open Libcommon
open Libbackend.Worker_util
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libbackend

let shutdown = ref false

let garbage_collector_iteration () =
  Libbackend.Garbage_collection.collect Delete 10000 All


(* Based on cron_checker, though with only one tailcall to loop instead of two
 * possible locations. *)
let rec gc_loop () =
  ( try garbage_collector_iteration ()
    with e ->
      let bt = Libexecution.Exception.get_backtrace () in
      Libcommon.Log.erroR
        "garbage_collector"
        ~data:"Uncaught error"
        ~params:[("exn", Libexecution.Exception.exn_to_string e)] ;
      (* No relevant execution id here *)
      ( Libbackend.Rollbar.report
          e
          bt
          GarbageCollector
          (Telemetry.ID.to_string 0)
      (* If we fail to rollbar, go ahead and crash the  pod *)
      |> function `Success | `Disabled -> () | `Failure -> raise e ) ;
      () ) ;
  if not !shutdown then (gc_loop [@tailcall]) () else exit 0


let () =
  (* If either thread sets the shutdown ref, the other will see it and
   * terminate; block until both have terminated. *)
  (* Three cases where we want to exit:
   * - healthcheck worker is instructed to die (/pkill), it sets the shutdown
   *   ref gc loop terminates
   * - heathcheck worker dies/is killed (unhandled exn), kubernetes will kill
   *   the pod when it fails healthcheck
   * - gc_loop thread dies; it's the main loop, the process exits *)
  ignore (Thread.create (health_check shutdown) ()) ;
  (* I'm not sure we actually yield often enough to satisfy the healthcheck, but
   * I guess we'll find out *)
  gc_loop ()
