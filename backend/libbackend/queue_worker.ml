open Core_kernel
open Libcommon
open Libexecution
open Types
module RTT = Types.RuntimeT
module C = Canvas
module TL = Toplevel
module Span = Telemetry.Span

let dequeue_and_process execution_id :
    (RTT.dval option, Exception.captured) Result.t =
  Telemetry.with_root
    "dequeue_and_process"
    ~attrs:[("meta.process_id", `Intlit (execution_id |> Int63.to_string))]
    (fun span ->
      Event_queue.with_transaction span (fun parent transaction ->
          let event =
            try Ok (Event_queue.dequeue parent transaction)
            with e ->
              (* exception occurred while dequeuing,
               * no item to put back *)
              let bt = Exception.get_backtrace () in
              Span.event parent "Exception while dequeueing" ;
              Error (bt, e, [])
          in
          event
          |> Result.bind ~f:(fun event ->
                 match event with
                 | None ->
                     Span.set_attr parent "event_queue.no_events" (`Bool true) ;
                     Ok None
                 | Some event ->
                     let c =
                       (* Telemetry.with_span might belong inside
                        * Canvas.load_for_event_with_cache, but then so would the
                        * error handling ... this mmay want a refactor *)
                       Telemetry.with_span
                         parent
                         "Canvas.load_for_event_from_cache"
                         (fun parent ->
                           try
                             Canvas.load_for_event_from_cache event
                             |> Result.map_error ~f:(String.concat ~sep:", ")
                             |> Prelude.Result.ok_or_internal_exception
                                  "Canvas load error"
                             (* This is a little silly, we could just return the error,
                   * maybe? *)
                             |> fun canvas ->
                             Span.set_attr
                               parent
                               "load_event_succeeded"
                               (`Bool true) ;
                             Ok canvas
                           with e ->
                             (* exception occurred when processing an item,
                              * so put it back as an error *)
                             let bt = Exception.get_backtrace () in
                             ignore
                               (Event_queue.put_back
                                  transaction
                                  event
                                  ~status:`Err) ;
                             Span.set_attr
                               parent
                               "event.load_success"
                               (`Bool false) ;
                             Error (bt, e, []))
                     in
                     c
                     |> Result.bind ~f:(fun c ->
                            let host = !c.host in
                            let trace_id = Util.create_uuid () in
                            let canvas_id = !c.id in
                            let desc = Event_queue.to_event_desc event in
                            Log.add_log_annotations
                              [ ("canvas", `String host)
                              ; ("trace_id", `String (Uuidm.to_string trace_id))
                              ; ( "canvas_id"
                                , `String (Uuidm.to_string canvas_id) )
                                (* handler_name/module/method because those are the
                                 * names of the fields in a handler spec, and we
                                 * want these names to be shared between http logs
                                 * and cron/worker logs *)
                              ; ("module", `String event.space)
                              ; ("handler_name", `String event.name)
                              ; ("method", `String event.modifier)
                              ; ("retries", `Int event.retries) ]
                              (fun _ ->
                                try
                                  let event_timestamp =
                                    Stored_event.store_event
                                      ~trace_id
                                      ~canvas_id
                                      desc
                                      event.value
                                  in
                                  let h =
                                    !c.handlers
                                    |> IDMap.data
                                    |> List.filter_map ~f:TL.as_handler
                                    |> List.filter
                                         ~f:(Handler.matches_event_desc desc)
                                    |> List.hd
                                  in
                                  match h with
                                  | None ->
                                      Span.set_attrs
                                        parent
                                        [ ("host", `String host)
                                        ; ("event", `String (Log.dump desc))
                                        ; ( "event_id"
                                          , `String (string_of_int event.id) )
                                        ] ;
                                      let space, name, modifier = desc in
                                      Stroller.push_new_404
                                        ~execution_id
                                        ~canvas_id
                                        ( space
                                        , name
                                        , modifier
                                        , event_timestamp
                                        , trace_id ) ;
                                      Event_queue.put_back
                                        transaction
                                        event
                                        `Incomplete ;
                                      Ok None
                                  | Some h ->
                                      Span.set_attrs
                                        parent
                                        [ ("event", `String (Log.dump desc))
                                        ; ("host", `String host)
                                        ; ( "event_id"
                                          , `String (string_of_int event.id) )
                                        ; ( "handler_id"
                                          , `String (Types.string_of_id h.tlid)
                                          ) ] ;

                                      let result, touched_tlids =
                                        Execution.execute_handler
                                          ~parent:(Some parent)
                                          h
                                          ~execution_id
                                          ~tlid:h.tlid
                                          ~input_vars:[("event", event.value)]
                                          ~dbs:(TL.dbs !c.dbs)
                                          ~user_tipes:
                                            (!c.user_tipes |> IDMap.data)
                                          ~user_fns:
                                            (!c.user_functions |> IDMap.data)
                                          ~package_fns:!c.package_fns
                                          ~secrets:
                                            (Secret.secrets_in_canvas !c.id)
                                          ~account_id:!c.owner
                                          ~store_fn_arguments:
                                            (Stored_function_arguments.store
                                               ~canvas_id
                                               ~trace_id)
                                          ~store_fn_result:
                                            (Stored_function_result.store
                                               ~canvas_id
                                               ~trace_id)
                                          ~canvas_id
                                      in
                                      Stroller.push_new_trace_id
                                        ~execution_id
                                        ~canvas_id
                                        trace_id
                                        (h.tlid :: touched_tlids) ;
                                      let result_tipe (r : RTT.dval) =
                                        match r with
                                        | DResult (ResOk _) ->
                                            "ResOk"
                                        | DResult (ResError _) ->
                                            "ResError"
                                        | DOption (OptJust _) ->
                                            "OptJust"
                                        | DOption OptNothing ->
                                            "OptNothing"
                                        | _ ->
                                            r
                                            |> Dval.tipe_of
                                            |> Dval.tipe_to_string
                                      in
                                      Span.set_attrs
                                        parent
                                        [ ( "result_tipe"
                                          , `String (result_tipe result) )
                                        ; ("event.execution_success", `Bool true)
                                        ] ;
                                      Event_queue.finish transaction event ;
                                      Ok (Some result)
                                with e ->
                                  (* exception occurred when processing an item,
             * so put it back as an error *)
                                  Span.set_attrs
                                    parent
                                    [("event.execution_success", `Bool false)] ;
                                  let bt = Exception.get_backtrace () in
                                  ignore
                                    ( try
                                        Event_queue.put_back
                                          transaction
                                          event
                                          ~status:`Err
                                      with e ->
                                        Span.set_attr
                                          parent
                                          "error.msg"
                                          (`String
                                            (Libexecution.Exception
                                             .exn_to_string
                                               e)) ) ;
                                  let log_params =
                                    List.append
                                      (Log.current_log_annotations ())
                                      (Span.log_params parent)
                                  in
                                  Error (bt, e, log_params))))))


let run (execution_id : Types.id) :
    (RTT.dval option, Exception.captured) Result.t =
  if String.Caseless.equal
       Libservice.Config.postgres_settings.dbname
       "prodclone"
  then (
    Log.erroR "queue_worker" ~data:"Pointing at prodclone; will not dequeue" ;
    Ok None )
  else
    Log.add_log_annotations
      [("start_timer", `Float (Unix.gettimeofday () *. 1000.0))]
      (fun _ -> dequeue_and_process execution_id)
