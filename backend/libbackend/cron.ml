open Core_kernel
open Libexecution
open Types.RuntimeT
open Types.RuntimeT.HandlerT
module Log = Libcommon.Log

let last_ran_at (canvas_id : Uuidm.t) (h : handler) : Time.t option =
  Db.fetch_one_option
    ~name:"last_ran_at"
    "SELECT ran_at
       FROM cron_records
       WHERE tlid = $1
       AND canvas_id = $2
       ORDER BY id DESC
       LIMIT 1"
    ~params:[ ID h.tlid; Uuid canvas_id ]
  |> Option.map ~f:List.hd_exn
  |> Option.map ~f:Db.date_of_sqlstring


let parse_interval (h : handler) : Time.Span.t option =
  let open Option in
  Handler.modifier_for h
  >>= fun modif ->
  match String.lowercase modif with
  | "daily" ->
      Time.Span.create ~sign:Sign.Pos ~day:1 () |> Some
  | "weekly" ->
      Time.Span.create ~sign:Sign.Pos ~day:7 () |> Some
  | "fortnightly" ->
      Time.Span.create ~sign:Sign.Pos ~day:14 () |> Some
  | "every 1hr" ->
      Time.Span.create ~sign:Sign.Pos ~hr:1 () |> Some
  | "every 12hrs" ->
      Time.Span.create ~sign:Sign.Pos ~hr:12 () |> Some
  | "every 1min" ->
      Time.Span.create ~sign:Sign.Pos ~min:1 () |> Some
  | _ ->
      None


let should_execute (canvas_id : Uuidm.t) (h : handler) execution_id : bool =
  let open Option in
  match last_ran_at canvas_id h with
  | None ->
      true (* we should always run if we've never run before *)
  | Some lrt ->
      let now = Time.now () in
      ( match parse_interval h with
      | None ->
          let bt = Caml.Printexc.get_raw_backtrace () in
          (* We used to rollbar here, but that breaks other crons! Just log for
         * now, later we'll validate on save or something *)
          Log.erroR
            "Can't parse interval: "
            ~params:
              [ ( "modifier"
                , Handler.modifier_for h
                  |> Option.value ~default:"<empty modifier>" )
              ] ;
          let e =
            Exception.make_exception
              DarkInternal
              ( "Can't parsse interval: "
              ^ ( Handler.modifier_for h
                |> Option.value ~default:"<empty modifier>" ) )
          in
          ignore
            (Rollbar.report
               e
               bt
               Libservice.Rollbar.CronChecker
               (execution_id |> Types.string_of_id)) ;
          false
      | Some interval ->
          (* Example:
       * last_ran_at = 16:00
       * interval: 1 hour
       * now: 16:30
       *
       * therefore:
       *   should_run_after is 17:01
       *
       *   and we should run once now >= should_run_after
       *
       *)
          let should_run_after = Time.add lrt interval in
          (* How long was it between when we expected to run and when we
           * enqueued it to the events queue? (There may be further delay if the
           * events queue is backed up ... ) *)
          Log.debuG
            "cron_delay"
            ~params:
              [ ( "interval"
                , Handler.modifier_for h
                  |> Option.value ~default:"<empty
modifier>" )
              ]
            ~jsonparams:
              [ ( "delay_ms"
                , `Float (Time.Span.to_ms (Time.diff now should_run_after)) )
              ] ;
          now >= should_run_after )


let record_execution (canvas_id : Uuidm.t) (h : handler) : unit =
  Db.run
    "INSERT INTO cron_records
    (tlid, canvas_id)
    VALUES ($1, $2)"
    ~params:[ ID h.tlid; Uuid canvas_id ]
    ~name:"Cron.record_execution"


let check_all_canvases execution_id : (unit, Exception.captured) Result.t =
  let current_endpoints =
    if String.Caseless.equal
         Libservice.Config.postgres_settings.dbname
         "prodclone"
    then (
      Log.erroR
        "cron_checker"
        ~data:"Not running any crons; pointed at prodclone!"
        ~params:[ ("execution_id", Types.string_of_id execution_id) ] ;
      [] )
    else
      Serialize.current_hosts ()
      |> List.filter ~f:(fun f -> not (Serialize.is_test f))
  in
  let stat_canvases = List.length current_endpoints in
  let stat_canvas_errors = ref 0 in
  let stat_crons = ref 0 in
  let stat_events = ref 0 in
  try
    current_endpoints
    |> List.filter_map ~f:(fun endp ->
           (* serialization can fail, attempt first *)
           try
             let c = Canvas.load_cron endp in
             match c with
             | Ok c ->
                 Some (endp, c)
             | Error errs ->
                 incr stat_canvas_errors ;
                 Log.erroR
                   "cron_checker"
                   ~data:"Canvas verification error"
                   ~params:
                     [ ("host", endp)
                     ; ("errs", String.concat ~sep:", " errs)
                     ; ("execution_id", Types.string_of_id execution_id)
                     ] ;
                 None
           with e ->
             let bt = Exception.get_backtrace () in
             incr stat_canvas_errors ;
             Log.erroR
               "cron_checker"
               ~data:"Deserialization error"
               ~bt
               ~params:
                 [ ("host", endp)
                 ; ("exn", Log.dump e)
                 ; ("execution_id", Types.string_of_id execution_id)
                 ] ;
             ignore (Rollbar.report e bt CronChecker (Log.dump execution_id)) ;
             None)
    |> List.iter ~f:(fun (endp, c) ->
           let crons =
             !c.handlers
             |> Types.IDMap.data
             |> List.filter_map ~f:Toplevel.as_handler
             |> List.filter ~f:Handler.is_complete
             |> List.filter ~f:Handler.is_cron
           in
           let cron_count = List.length crons in
           stat_crons := !stat_crons + cron_count ;
           Log.debuG
             "cron_checker"
             ~data:"checking canvas"
             ~params:
               [ ("execution_id", Types.string_of_id execution_id)
               ; ("host", endp)
               ]
             ~jsonparams:[ ("number_of_crons", `Int cron_count) ] ;
           List.iter
             ~f:(fun cr ->
               if should_execute !c.id cr execution_id
               then
                 let space = Handler.module_for_exn cr in
                 let name = Handler.event_name_for_exn cr in
                 let modifier = Handler.modifier_for_exn cr in
                 Log.add_log_annotations
                   [ ("cron", `Bool true) ]
                   (fun _ ->
                     Event_queue.enqueue
                       ~account_id:!c.owner
                       ~canvas_id:!c.id
                       space
                       name
                       modifier
                       DNull ;
                     record_execution !c.id cr ;
                     incr stat_events ;
                     Log.debuG
                       "cron_checker"
                       ~data:"enqueued event"
                       ~params:
                         [ ("execution_id", Types.string_of_id execution_id)
                         ; ("host", endp)
                         ; ("tlid", Types.string_of_id cr.tlid)
                         ; ("event_name", name)
                         ; ("cron_freq", modifier)
                         ]))
             crons)
    |> (fun x ->
         Log.infO
           "cron_checker"
           ~data:"checked"
           ~params:[ ("execution_id", Types.string_of_id execution_id) ]
           ~jsonparams:
             [ ("canvas.checked", `Int stat_canvases)
             ; ("canvas.errors", `Int !stat_canvas_errors)
             ; ("cron.checked", `Int !stat_crons)
             ; ("cron.queued", `Int !stat_events)
             ] ;
         x)
    |> Ok
  with e ->
    let bt = Exception.get_backtrace () in
    Log.infO
      "cron_checker"
      ~data:"errored"
      ~params:[ ("execution_id", Types.string_of_id execution_id) ]
      ~jsonparams:
        [ ("canvas.checked", `Int stat_canvases)
        ; ("canvas.errors", `Int !stat_canvas_errors)
        ; ("cron.checked", `Int !stat_crons)
        ; ("cron.queued", `Int !stat_events)
        ] ;
    Error (bt, e)
