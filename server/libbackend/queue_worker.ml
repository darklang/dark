open Core_kernel
open Libexecution

module RTT = Types.RuntimeT
module C = Canvas
module TL = Toplevel
module FF = Feature_flag

let dequeue_and_evaluate_all () : string =
  (* iterate all non-test canvases *)
  let current_endpoints =
    Serialize.current_hosts ()
    |> List.filter ~f:(fun f -> not (Serialize.is_test f))
  in
  let execution_id = Util.create_id () in
  let results =
    current_endpoints
    |> List.filter_map
      ~f:(fun endp ->
          try
            Some (endp, C.load endp []) (* serialization can fail, attempt first *)
          with
          | e ->
            let bt = Backtrace.Exn.most_recent () in
            Log.erroR ("Deserialization error for host: " ^ endp) e;
            let _ = Rollbar.report e bt EventQueue in
            None)
    |> List.map
      ~f:(fun (endpoint, c) ->
          try
            (* iterate all queues *)
            let crons, queues =
              !c.toplevels
              |> TL.bg_handlers
              |> List.partition_tf ~f:Handler.is_cron
            in
            let cron_results =
              List.filter_map crons
               ~f:(fun cr ->
                    try
                      if Cron.should_execute !c.id cr
                      then
                        let dbs = TL.dbs !c.toplevels in
                        let env = User_db.dbs_as_exe_env dbs in
                        let state = Execution.state_for_execution ~c:!c cr.tlid
                            ~env ~execution_id
                        in
                        Cron.record_execution !c.id cr;
                        let result = Ast_analysis.execute_handler state cr in
                        Some result
                      else
                        None
                    with
                    | e ->
                      let bt = Backtrace.Exn.most_recent () in
                      let _  = Rollbar.report e bt EventQueue in
                      None)
            in
            let queue_results =
              List.filter_map queues
                ~f:(fun q ->
                    let space = Handler.module_for_exn q in
                    let name = Handler.event_name_for_exn q in
                    (match Event_queue.dequeue
                             ~canvas:!c.id ~account:!c.owner
                             execution_id space name with
                     | None -> None
                     | Some event ->
                       (match Handler.event_desc_for q with
                       | Some desc ->
                         Stored_event.store_event !c.id desc event.value
                       | None -> ());
                       let dbs = TL.dbs !c.toplevels in
                       let dbs_env = User_db.dbs_as_exe_env (dbs) in
                       let env = Map.set ~key:"event" ~data:(event.value) dbs_env in
                       let state = Execution.state_for_execution q.tlid
                           ~c:!c ~execution_id ~env in
                       let result = Ast_analysis.execute_handler state q in
                       (match result with
                        | RTT.DIncomplete ->
                          Event_queue.put_back event ~status:`Incomplete
                        | RTT.DError _ ->
                          Event_queue.put_back event ~status:`Err
                        | _ ->
                          Event_queue.finish event);
                       Some result)
                  )
            in
            let results =
              cron_results @ queue_results
            in
            Event_queue.finalize execution_id ~status:`OK;
            match results with
            | [] -> RTT.DIncomplete
            | l -> RTT.DList l
          with
          | e ->
            let bt = Backtrace.Exn.most_recent () in
            let _  = Rollbar.report e bt EventQueue in
            Event_queue.finalize execution_id ~status:`Err;
            RTT.DError (Exn.to_string e)
        )
  in
  results
  |> List.filter
    ~f:(fun r ->
        match r with
        | RTT.DIncomplete -> false
        | _ -> true)
  |> List.map
    ~f:(Dval.dval_to_json_string)
  |> String.concat
    ~sep:", "
