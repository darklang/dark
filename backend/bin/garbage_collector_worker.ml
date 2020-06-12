open Core_kernel
open Libcommon
open Libbackend.Worker_util
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libbackend

let shutdown = ref false

(* This is largely the same as the code in the garbage_collector.exe binary.
 * Most notable changes:
 * - No arg parsing
 * - Thread.yield before each canvas' query, to allow the health checker to work
 *)
let garbage_collector_iteration () =
  let (action, action_arg, limit, canvas_arg)
        : Stored_event.trim_events_action * string * int * string =
    (* Using "all" here is a little silly, because we'll never use the
     * single-canvas branch in this binary, but for now lets us keep
     * garbage_collector_worker.ml and garbage_collector.ml as similar as
     * possible. *)
    (Delete, "delete", 10000, "all")
  in
  (* Outer span - one per loop invocation *)
  Telemetry.with_root "garbage_collector" (fun span ->
      (* Get all canvas ids *)
      let canvas_ids_and_names : (Uuidm.t * string) list =
        ( match canvas_arg with
        | "all" ->
            Telemetry.with_span span "get_canvases_for_gc" (fun span ->
                let canvas_ids : (string * string) list =
                  Db.fetch
                    ~name:"canvases"
                    "SELECT id, name FROM canvases"
                    ~params:[]
                  (* List.map over all the rows; then List.hd_exn each row, it's is a single field, canvases.id *)
                  |> List.map ~f:(function
                         | [id; name] ->
                             (id, name)
                         | _ ->
                             Exception.internal "Wrong shape")
                in
                Telemetry.Span.set_attr
                  span
                  "canvas_count"
                  (`Int (List.length canvas_ids)) ;
                canvas_ids)
        | canvas_arg ->
            let canvas_name =
              Libbackend.Canvas.name_for_id
                (canvas_arg |> Uuidm.of_string |> Tc.Option.value_exn)
            in
            [(canvas_arg, canvas_name)] )
        |> List.map ~f:(fun (cid, canvas_name) ->
               (cid |> Uuidm.of_string |> Tc.Option.value_exn, canvas_name))
      in
      Telemetry.Span.set_attr
        span
        "canvas_count"
        (`Int (List.length canvas_ids_and_names)) ;
      (* Map over canvases *)
      let stored_events_v2_row_count =
        Telemetry.with_span span "garbage_collector_stored_events" (fun span ->
            let stored_events_v2_row_count =
              canvas_ids_and_names
              |> List.map ~f:(fun (canvas_id, canvas_name) ->
                     Thread.yield () ;
                     Stored_event.trim_events_for_canvas
                       ~span
                       ~action
                       canvas_id
                       canvas_name
                       limit)
              (* Sum the row_count returned for each canvas and put it in this outer
               * span *)
              |> Tc.List.sum
            in
            Telemetry.Span.set_attr
              span
              "stored_events_v2_row_count"
              (`Int stored_events_v2_row_count) ;
            stored_events_v2_row_count)
      in
      let function_arguments_row_count =
        Telemetry.with_span
          span
          "garbage_collector_function_arguments"
          (fun span ->
            let function_arguments_row_count =
              canvas_ids_and_names
              |> List.map ~f:(fun (canvas_id, canvas_name) ->
                     Thread.yield () ;
                     Stored_function_arguments.trim_arguments_for_canvas
                       span
                       action
                       canvas_id
                       ~canvas_name
                       ~limit)
              |> Tc.List.sum
            in
            Telemetry.Span.set_attr
              span
              "function_arguments_row_count"
              (`Int function_arguments_row_count) ;
            function_arguments_row_count)
      in
      let function_results_v2_row_count =
        Telemetry.with_span
          span
          "garbage_collector_function_results_v2"
          (fun span ->
            let function_results_v2_row_count =
              canvas_ids_and_names
              |> List.map ~f:(fun (canvas_id, canvas_name) ->
                     Thread.yield () ;
                     Stored_function_result.trim_results_for_canvas
                       span
                       action
                       canvas_id
                       ~canvas_name
                       ~limit)
              |> Tc.List.sum
            in
            Telemetry.Span.set_attr
              span
              "function_results_v2_row_count"
              (`Int function_results_v2_row_count) ;
            function_arguments_row_count)
      in
      Telemetry.Span.set_attrs
        span
        [ ("stored_events_v2_row_count", `Int stored_events_v2_row_count)
        ; ("function_arguments_row_count", `Int function_arguments_row_count)
        ; ("function_results_v2_row_count", `Int function_results_v2_row_count)
        ] ;
      ())
  |> ignore


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
