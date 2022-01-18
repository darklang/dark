open Core_kernel
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libcommon
module Db = Libbackend_basics.Db

let collect
    (action : Stored_event.trim_events_action)
    (limit : int)
    (canvas : Stored_event.trim_events_canvases) =
  (* Outer span - one per loop invocation *)
  Telemetry.with_root "garbage_collector" (fun span ->
      (* Get all canvas ids *)
      let canvas_ids_and_names : (Uuidm.t * string) list =
        match canvas with
        | All ->
            Telemetry.with_span span "get_canvases_for_gc" (fun span ->
                let canvas_ids : (Uuidm.t * string) list =
                  Db.fetch
                    ~name:"canvases"
                    "SELECT id, name FROM canvases"
                    ~params:[]
                  (* List.map over all the rows; then List.hd_exn each row, it's is a single field, canvases.id *)
                  |> List.map ~f:(function
                         | [id; name] ->
                             (id |> Uuidm.of_string |> Tc.Option.value_exn, name)
                         | _ ->
                             Exception.internal "Wrong shape")
                in
                Telemetry.Span.set_attr
                  span
                  "canvas_count"
                  (`Int (List.length canvas_ids)) ;
                canvas_ids)
        | JustOne canvas_name ->
            let canvas_id = Canvas.id_for_name canvas_name in
            [(canvas_id, canvas_name)]
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
      let function_results_v3_row_count =
        Telemetry.with_span
          span
          "garbage_collector_function_results_v3"
          (fun span ->
            let function_results_v3_row_count =
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
              "function_results_v3_row_count"
              (`Int function_results_v3_row_count) ;
            function_results_v3_row_count)
      in
      Telemetry.Span.set_attrs
        span
        [ ("stored_events_v2_row_count", `Int stored_events_v2_row_count)
        ; ("function_arguments_row_count", `Int function_arguments_row_count)
        ; ("function_results_v3_row_count", `Int function_results_v3_row_count)
        ] ;
      ())
  |> ignore
