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
      try
        let event = Event_queue.dequeue transaction in
        match event with
        | None ->
            Log.infO "queue_worker" ~data:"No events in queue" ;
            Ok None
        | Some event ->
          ( try
              let c =
                Canvas.load_for_event event
                |> Result.map_error ~f:(String.concat ~sep:", ")
                |> Tc.Result.ok_or_internal_exception "Canvas load error"
              in
              let host = !c.host in
              let desc = Event_queue.to_event_desc event in
              let trace_id = Util.create_uuid () in
              let canvas_id = !c.id in
              let event_timestamp =
                Stored_event.store_event ~trace_id ~canvas_id desc event.value
              in
              let h =
                !c.handlers
                |> IDMap.data
                |> List.filter_map ~f:TL.as_handler
                |> List.filter ~f:(Handler.matches_event_desc desc)
                |> List.hd
              in
              match h with
              | None ->
                  Log.infO
                    "queue_worker"
                    ~data:"No handler for event"
                    ~params:[("host", host); ("event", Log.dump desc)] ;
                  let space, name, modifier = desc in
                  Stroller.push_new_404
                    ~execution_id
                    ~canvas_id
                    (space, name, modifier, event_timestamp, trace_id) ;
                  Event_queue.put_back transaction event `Incomplete ;
                  Ok None
              | Some h ->
                  Log.infO
                    "queue_worker"
                    ~data:"Executing handler for event"
                    ~params:
                      [ ("event", Log.dump desc)
                      ; ("host", host)
                      ; ("handler_id", Types.string_of_id h.tlid) ] ;
                  let result, touched_tlids =
                    Execution.execute_handler
                      h
                      ~execution_id
                      ~tlid:h.tlid
                      ~input_vars:[("event", event.value)]
                      ~dbs:(TL.dbs !c.dbs)
                      ~user_tipes:(!c.user_tipes |> IDMap.data)
                      ~user_fns:(!c.user_functions |> IDMap.data)
                      ~account_id:!c.owner
                      ~canvas_id
                  in
                  Stroller.push_new_trace_id
                    ~execution_id
                    ~canvas_id
                    trace_id
                    (h.tlid :: touched_tlids) ;
                  Log.infO
                    "queue_worker"
                    ~data:"Successful execution"
                    ~params:
                      [ ("host", host)
                      ; ("event", Log.dump desc)
                      ; ("handler_id", Types.string_of_id h.tlid)
                      ; ("result", Dval.show result) ] ;
                  Event_queue.finish transaction event ;
                  Ok (Some result)
            with e ->
              (* exception occurred when processing an item,
             * so put it back as an error *)
              let bt = Exception.get_backtrace () in
              ignore (Event_queue.put_back transaction event ~status:`Err) ;
              Error (bt, e) )
      with e ->
        (* exception occurred while dequeuing,
         * no item to put back *)
        let bt = Exception.get_backtrace () in
        Error (bt, e) )


let run execution_id : (RTT.dval option, Exception.captured) Result.t =
  if String.Caseless.equal
       Libservice.Config.postgres_settings.dbname
       "prodclone"
  then (
    Log.erroR "queue_worker" ~data:"Pointing at prodclone; will not dequeue" ;
    Ok None )
  else dequeue_and_process execution_id
