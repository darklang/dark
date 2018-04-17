open Core

module RTT = Types.RuntimeT
module C = Canvas
module TL = Toplevel
module FF = Feature_flag

let dequeue_and_evaluate_all () : string =
  (* iterate all darkfiles *)
  let current_file_ext = "_" ^ Serialize.digest ^ ".dark" in
  let current_endpoints = Serialize.current_filenames ()
                          |> List.map
                            ~f:(fun f ->
                                String.chop_suffix_exn
                                  ~suffix:current_file_ext
                                  f)
  in
  let execution_id = Util.create_id () in
  let results =
    List.map current_endpoints
      ~f:(fun endpoint ->
          let c = C.load endpoint [] in
          try
            (* iterate all queues *)
            let queues = TL.bg_handlers !c.toplevels in
            let results =
              List.filter_map queues
                ~f:(fun q ->
                    let space = Handler.module_for_exn q in
                    let name = Handler.event_name_for_exn q in
                    (match Event_queue.dequeue execution_id endpoint space name with
                     | None -> None
                     | Some event ->
                       (match Handler.event_desc_for q with
                       | Some desc ->
                         Stored_event.store_event endpoint desc event.value
                       | None -> ());
                       let dbs = TL.dbs !c.toplevels in
                       let dbs_env = Db.dbs_as_exe_env (dbs) in
                       let env = Map.set ~key:"event" ~data:(event.value) dbs_env in
                       let state : RTT.exec_state =
                             { ff = event.flag_context
                             ; tlid = q.tlid
                             ; hostname = !c.name
                             ; user_fns = !c.user_functions
                             ; exe_fn_ids = []
                             ; env = env
                             ; dbs = dbs
                             ; id = execution_id
                             } in
                       let result = Handler.execute state q in
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
