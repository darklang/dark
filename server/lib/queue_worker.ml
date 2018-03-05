open Core

module RTT = Types.RuntimeT
module C = Canvas
module TL = Toplevel

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
  let results =
    List.map current_endpoints
      ~f:(fun endpoint ->
          let c = C.load endpoint [] in
          Event_queue.set_scope !c.name;
          try
            (* iterate all queues *)
            let queues = TL.bg_handlers !c.toplevels in
            let results =
              List.filter_map queues
                ~f:(fun q ->
                    let space = Handler.module_for_exn q in
                    let name = Handler.event_name_for_exn q in
                    (match Event_queue.dequeue space name with
                     | None -> None
                     | Some event ->
                       Stored_event.store endpoint q.tlid (event.value);
                       let dbs = TL.dbs !c.toplevels in
                       let dbs_env = Db.dbs_as_exe_env (dbs) in
                       Db.cur_dbs := dbs;
                       let env = Map.set ~key:"event" ~data:(event.value) dbs_env in
                       let result = Handler.execute q env in
                       (match result with
                        | RTT.DIncomplete ->
                          Event_queue.put_back event ~status:`Incomplete;
                          None
                        | RTT.DError _ ->
                          Event_queue.put_back event ~status:`Err;
                          None
                        | _ ->
                          Event_queue.finish event;
                          Some result))
                  )
            in
            Event_queue.unset_scope ~status:`OK;
            match results with
            | [] -> RTT.DIncomplete
            | l -> RTT.DList l
          with
          | e ->
            Event_queue.unset_scope ~status:`Err;
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
