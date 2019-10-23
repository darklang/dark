open Core_kernel
open Libcommon
open Libexecution
open Types
module RTT = Types.RuntimeT
module C = Canvas
module TL = Toplevel

let dequeue_and_process execution_id :
    (RTT.dval option, Exception.captured) Result.t =
  Log.infO "queue_worker" ~data:"Worker starting" ;
  Event_queue.with_transaction (fun transaction ->
      let event =
        try Ok (Event_queue.dequeue transaction) with e ->
          (* exception occurred while dequeuing,
         * no item to put back *)
          let bt = Exception.get_backtrace () in
          Log.erroR "Exception while dequeueing" ;
          (* execution_id will be in this log *)
          Error (bt, e)
      in
      event
      |> Result.bind ~f:(fun event ->
             match event with
             | None ->
                 Log.infO "queue_worker" ~data:"No events in queue" ;
                 Ok None
             | Some event ->
                 let c =
                   try
                     Canvas.load_for_event event
                     |> Result.map_error ~f:(String.concat ~sep:", ")
                     |> Prelude.Result.ok_or_internal_exception
                          "Canvas load error"
                     (* This is a little silly, we could just return the error,
                   * maybe? *)
                     |> Ok
                   with e ->
                     (* exception occurred when processing an item,
                      * so put it back as an error *)
                     let bt = Exception.get_backtrace () in
                     ignore
                       (Event_queue.put_back transaction event ~status:`Err) ;
                     Log.erroR "Exception while loading canvas" ;
                     (* execution_id will be in this log *)
                     Error (bt, e)
                 in
                 c
                 |> Result.bind ~f:(fun c ->
                        let host = !c.host in
                        let trace_id = Util.create_uuid () in
                        let canvas_id = !c.id in
                        let desc = Event_queue.to_event_desc event in
                        Log.add_log_annotations
                          [ ("host", `String host)
                          ; ("trace_id", `String (Uuidm.to_string trace_id))
                          ; ("canvas_id", `String (Uuidm.to_string canvas_id))
                          ; ("event_space", `String event.space)
                          ; ("event_name", `String event.name)
                          ; ("event_modifier", `String event.modifier)
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
                                  Log.infO
                                    "queue_worker"
                                    ~data:"No handler for event"
                                    ~params:
                                      [ ("host", host)
                                      ; ("event", Log.dump desc)
                                      ; ("event_id", string_of_int event.id) ] ;
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
                                  Log.infO
                                    "queue_worker"
                                    ~data:"Executing handler for event"
                                    ~params:
                                      [ ("event", Log.dump desc)
                                      ; ("host", host)
                                      ; ("event_id", string_of_int event.id)
                                      ; ( "handler_id"
                                        , Types.string_of_id h.tlid ) ] ;
                                  let result, touched_tlids =
                                    Execution.execute_handler
                                      h
                                      ~execution_id
                                      ~tlid:h.tlid
                                      ~input_vars:[("event", event.value)]
                                      ~dbs:(TL.dbs !c.dbs)
                                      ~user_tipes:(!c.user_tipes |> IDMap.data)
                                      ~user_fns:
                                        (!c.user_functions |> IDMap.data)
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
                                  Log.infO
                                    "queue_worker"
                                    ~data:"Successful execution"
                                    ~params:
                                      [ ("host", host)
                                      ; ("event", Log.dump desc)
                                      ; ("event_id", string_of_int event.id)
                                      ; ( "handler_id"
                                        , Types.string_of_id h.tlid )
                                      ; ("result_type", result_tipe result) ] ;
                                  Event_queue.finish transaction event ;
                                  Ok (Some result)
                            with e ->
                              (* exception occurred when processing an item,
             * so put it back as an error *)
                              let bt = Exception.get_backtrace () in
                              ignore
                                (Event_queue.put_back
                                   transaction
                                   event
                                   ~status:`Err) ;
                              Error (bt, e) ) ) ) )


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
