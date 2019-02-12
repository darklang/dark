open Core_kernel
open Libcommon
open Libexecution
module RTT = Types.RuntimeT
module C = Canvas
module TL = Toplevel

let dequeue_and_process execution_id : (unit, Exception.captured) Result.t =
  Log.infO
    "queue_worker"
    ~data:"Worker starting"
    ~params:[("execution_id", Log.dump execution_id)] ;
  Event_queue.with_transaction (fun transaction ->
      try
        let event = Event_queue.dequeue transaction in
        match event with
        | None ->
            Log.infO
              "queue_worker"
              ~data:"No events in queue"
              ~params:[("execution_id", Log.dump execution_id)] ;
            Ok ()
        | Some event ->
          ( try
              let c = Canvas.load_for_event event in
              let host = !c.host in
              let desc = Event_queue.to_event_desc event in
              let trace_id = Util.create_uuid () in
              let canvas_id = !c.id in
              let event_timestamp =
                Stored_event.store_event ~trace_id ~canvas_id desc event.value
              in
              let h =
                !c.handlers
                |> List.filter_map ~f:TL.as_handler
                |> List.filter ~f:(Handler.matches_event_desc desc)
                |> List.hd
              in
              match h with
              | None ->
                  Log.infO
                    "queue_worker"
                    ~data:"No handler for event"
                    ~params:
                      [ ("execution_id", Log.dump execution_id)
                      ; ("host", host)
                      ; ("event", Log.dump desc) ] ;
                  let space, name, modifier = desc in
                  Stroller.push_new_404
                    ~execution_id
                    ~canvas_id
                    (space, name, modifier, event_timestamp) ;
                  Event_queue.put_back transaction event `Incomplete ;
                  Ok ()
              | Some h ->
                  ignore
                    (Stroller.push_new_trace_id
                       ~execution_id
                       ~canvas_id
                       h.tlid
                       trace_id) ;
                  Log.infO
                    "queue_worker"
                    ~data:"Executing handler for event"
                    ~params:
                      [ ("execution_id", Log.dump execution_id)
                      ; ("event", Log.dump desc)
                      ; ("host", host)
                      ; ("handler_id", Log.dump h.tlid) ] ;
                  let result =
                    Execution.execute_handler
                      h
                      ~execution_id
                      ~tlid:h.tlid
                      ~input_vars:[("event", event.value)]
                      ~dbs:(TL.dbs !c.dbs)
                      ~user_fns:!c.user_functions
                      ~account_id:!c.owner
                      ~canvas_id
                  in
                  ( match result with
                  | RTT.DIncomplete ->
                      Log.infO
                        "queue_worker"
                        ~data:"Got DIncomplete when executing handler"
                        ~params:
                          [ ("execution_id", Log.dump execution_id)
                          ; ("event", Log.dump desc)
                          ; ("host", host)
                          ; ("handler_id", Log.dump h.tlid) ] ;
                      Event_queue.put_back
                        transaction
                        event
                        ~status:`Incomplete
                  | RTT.DError _ ->
                      Log.infO
                        "queue_worker"
                        ~data:"Got DError when executing handler"
                        ~params:
                          [ ("execution_id", Log.dump execution_id)
                          ; ("event", Log.dump desc)
                          ; ("host", host)
                          ; ("handler_id", Log.dump h.tlid) ] ;
                      Event_queue.put_back transaction event ~status:`Err
                  | v ->
                      Log.infO
                        "queue_worker"
                        ~data:"Successful execution"
                        ~params:
                          [ ("execution_id", Log.dump execution_id)
                          ; ("host", host)
                          ; ("event", Log.dump desc)
                          ; ("handler_id", Log.dump h.tlid) ] ;
                      Event_queue.finish transaction event ) ;
                  Ok ()
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


let run execution_id : (unit, Exception.captured) Result.t =
  if String.Caseless.equal
       Libservice.Config.postgres_settings.dbname
       "prodclone"
  then (
    Log.erroR
      "queue_worker"
      ~data:"Pointing at prodclone; will not dequeue"
      ~params:[("execution_id", Log.dump execution_id)] ;
    Ok () )
  else dequeue_and_process execution_id
