open Core_kernel
open Libcommon
open Libexecution

module RTT = Types.RuntimeT
module C = Canvas
module TL = Toplevel

let run execution_id : (unit, Exception.captured) Result.t =
  Log.infO "queue_worker"
    ~data:"Worker starting"
    ~params:["execution_id", Log.dump execution_id];
  Event_queue.with_transaction (fun transaction ->
      try
        let event = Event_queue.dequeue transaction in
        match event with
        | None ->
          Log.infO "queue_worker"
            ~data:"No events in queue"
            ~params:["execution_id", Log.dump execution_id];
          Ok ()
        | Some event ->
          try
            let c = Canvas.load_for_event event in
            let host = !c.host in
            let desc = Event_queue.to_event_desc event in
            Stored_event.store_event !c.id desc event.value;
            let h =
              !c.handlers
              |> List.filter_map ~f:TL.as_handler
              |> List.filter
                ~f:(Handler.matches_event_desc desc)
              |> List.hd
            in
            (match h with
             | None ->
               Log.infO "queue_worker"
                 ~data:"No handler for event"
                 ~params:["execution_id", Log.dump execution_id
                         ;"host", host
                         ;"event", Log.dump desc];
               Event_queue.put_back transaction event `Incomplete;
               Ok ()
             | Some h ->
               Log.infO "queue_worker"
                 ~data:"Executing handler for event"
                 ~params:["execution_id", Log.dump execution_id
                         ;"event", Log.dump desc
                         ;"host", host
                         ;"handler_id", Log.dump h.tlid];
               let dbs = TL.dbs !c.dbs in
               let dbs_env = User_db.dbs_as_exe_env (dbs) in
               let env = Map.set ~key:"event" ~data:(event.value) dbs_env in
               let state = Execution.state_for_execution h.tlid
                   ~c:!c ~execution_id ~env in
               let result = Ast_analysis.execute_handler state h in
               (match result with
                | RTT.DIncomplete ->
                  Log.infO "queue_worker"
                    ~data:"Got DIncomplete when executing handler"
                    ~params:["execution_id", Log.dump execution_id
                            ;"event", Log.dump desc
                            ;"host", host
                            ;"handler_id", Log.dump h.tlid
                            ];
                  Event_queue.put_back transaction event ~status:`Incomplete
                | RTT.DError _ ->
                  Log.infO "queue_worker"
                    ~data:"Got DError when executing handler"
                    ~params:["execution_id", Log.dump execution_id
                            ;"event", Log.dump desc
                            ;"host", host
                            ;"handler_id", Log.dump h.tlid
                            ];
                  Event_queue.put_back transaction event ~status:`Err
                | v ->
                  Log.infO "queue_worker"
                    ~data:"Successful execution"
                    ~params:["execution_id", Log.dump execution_id
                            ;"host", host
                            ;"event", Log.dump desc
                            ;"handler_id", Log.dump h.tlid
                            ];
                  Event_queue.finish transaction event);
               Ok ())
          with
          | e ->
            let bt = Exception.get_backtrace () in
            ignore (Event_queue.put_back transaction event ~status:`Err);
            Error (bt, e)
      with
      | e ->
        let bt = Exception.get_backtrace () in
        Error (bt, e)
    )
