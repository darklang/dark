open Core_kernel
open Libcommon
open Libexecution
open Types.RuntimeT
open Types.RuntimeT.HandlerT
module Log = Libcommon.Log
module Span = Telemetry.Span

let last_ran_at (canvas_id : Uuidm.t) (h : 'expr_type handler) : Time.t option =
  Db.fetch_one_option
    ~name:"last_ran_at"
    "SELECT ran_at
       FROM cron_records
       WHERE tlid = $1
       AND canvas_id = $2
       ORDER BY id DESC
       LIMIT 1"
    ~params:[ID h.tlid; Uuid canvas_id]
  |> Option.map ~f:List.hd_exn
  |> Option.map ~f:Db.date_of_sqlstring


let parse_interval (h : 'expr_type handler) : Time.Span.t option =
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


type execution_check_type =
  { should_execute : bool
  ; scheduled_run_at : Time.t option
  ; interval : Time.Span.t option }

let execution_check
    (parent : Span.t) (canvas_id : Uuidm.t) (h : 'expr_type handler) :
    execution_check_type =
  let open Option in
  let now = Time.now () in
  match last_ran_at canvas_id h with
  | None ->
      {should_execute = true; scheduled_run_at = Some now; interval = None}
      (* we should always run if we've never run before *)
  | Some lrt ->
    ( match parse_interval h with
    | None ->
        let bt = Caml.Printexc.get_raw_backtrace () in
        Log.erroR
          "Can't parse interval: "
          ~params:
            [ ( "modifier"
              , Handler.modifier_for h
                |> Option.value ~default:"<empty modifier>" ) ] ;
        let e =
          Exception.make_exception
            DarkInternal
            ( "Can't parse interval: "
            ^ ( Handler.modifier_for h
              |> Option.value ~default:"<empty modifier>" ) )
        in
        ignore
          (Rollbar.report
             e
             bt
             Libservice.Rollbar.CronChecker
             (Telemetry.ID.to_string parent.trace_id)) ;
        {should_execute = false; scheduled_run_at = None; interval = None}
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
        if now >= should_run_after
        then
          { should_execute = true
          ; scheduled_run_at = Some should_run_after
          ; interval = Some interval }
        else
          { should_execute = false
          ; scheduled_run_at = None
          ; interval = Some interval } )


let record_execution (canvas_id : Uuidm.t) (h : 'expr_type handler) : unit =
  Db.run
    "INSERT INTO cron_records
    (tlid, canvas_id)
    VALUES ($1, $2)"
    ~params:[ID h.tlid; Uuid canvas_id]
    ~name:"Cron.record_execution"


(* load_canvases takes a list of canvas [names] (ie, canvas.host) and returns
 * the full Canvas.canvas for each, along with a list of canvas names that
 * couldn't be loaded for one reason (it errored) or another (it raised). *)
let load_canvases (parent : Telemetry.Span.t) (names : string list) :
    'a Canvas.canvas ref list * string list =
  Telemetry.with_span parent "Cron.load_canvases" (fun span ->
      List.partition_map names ~f:(fun name ->
          (* Load each canvas in chunk, logging and dropping from list on any error *)
          try
            (* serialization can fail, attempt first *)
            match Canvas.load_for_cron_checker_from_cache name with
            | Ok canvas ->
                `Fst canvas
            | Error errs ->
                Log.erroR
                  "cron_checker"
                  ~data:"Canvas verification error"
                  ~params:
                    [ ("host", name)
                    ; ("errs", String.concat ~sep:", " errs)
                    ; ("execution_id", Telemetry.ID.to_string parent.trace_id)
                    ] ;
                `Snd name
          with e ->
            let bt = Exception.get_backtrace () in
            let tid = Telemetry.ID.to_string span.trace_id in
            ignore (Rollbar.report e bt CronChecker tid) ;
            `Snd name))


(* check_and_schedule_work_for_canvas checks all cron handlers on the given
 * [canvas] and if necessary, enqueues work to run each one.
 *
 * Does not add a Telemetry.Span as this is called thousands of times per tick. *)
let check_and_schedule_work_for_canvas
    (parent : Span.t) (canvas : expr Canvas.canvas ref) =
  let crons =
    !canvas.handlers
    |> Types.IDMap.data
    |> List.filter_map ~f:Toplevel.as_handler
    |> List.filter ~f:Handler.is_complete
    |> List.filter ~f:Handler.is_cron
  in

  let cron_count = List.length crons in
  Log.debuG
    "cron_checker"
    ~data:"checking canvas"
    ~params:
      [ ("execution_id", Telemetry.ID.to_string parent.trace_id)
      ; ("canvas", !canvas.host) ]
    ~jsonparams:[("number_of_crons", `Int cron_count)] ;

  List.iter crons ~f:(fun cron ->
      let {should_execute; scheduled_run_at; interval} =
        execution_check parent !canvas.id cron
      in
      if should_execute
      then
        let space = Handler.module_for_exn cron in
        let name = Handler.event_name_for_exn cron in
        let modifier = Handler.modifier_for_exn cron in
        Log.add_log_annotations
          [("cron", `Bool true)]
          (fun _ ->
            Event_queue.enqueue
              ~account_id:!canvas.owner
              ~canvas_id:!canvas.id
              space
              name
              modifier
              DNull ;
            record_execution !canvas.id cron ;

            (* It's a little silly to recalculate now when we just did
             * it in execution_check, but maybe Event_queue.enqueue was
             * slow or something *)
            let now = Time.now () in
            let delay_ms =
              scheduled_run_at
              |> Option.map ~f:(Time.diff now)
              (* For future reference: yes, to_ms returns the span of time
               * in milliseconds, _not_ "the ms part of the span of
               * time". I had to check that it wasn't, like, 10.5s |>
               * to_ms is 500, because 10.5|>to_s is 10. Time-related
               * APIs get tricky that way. *)
              |> Option.map ~f:Time.Span.to_ms
            in
            let delay_log =
              delay_ms |> Option.map ~f:(fun delay -> ("delay", `Float delay))
            in
            let interval_log =
              interval
              (* This is definitely not None, but keep the
               * typechecker happy *)
              (* Floats are IEEE-754, which has a max at 2**31-1;
               * that's 24.85 days worth of milliseconds. Since our
               * longest allowed cron interval is two weeks, this is
               * fine *)
              |> Option.map ~f:(fun interval ->
                     ("interval", `Float (interval |> Time.Span.to_ms)))
            in
            let delay_ratio_log =
              Option.map2 delay_ms interval ~f:(fun delay_ms interval ->
                  let interval = interval |> Time.Span.to_ms in
                  delay_ms /. interval)
              |> Option.map ~f:(fun delay_ratio ->
                     ("delay_ratio", `Float delay_ratio))
            in
            let attrs =
              [ ("canvas_name", `String !canvas.host)
              ; ("tlid", `String (Types.string_of_id cron.tlid))
              ; ("handler_name", `String name)
                (* method here to use the spec-handler name for
                 * consistency with http/worker logs *)
              ; ("method", `String modifier) ]
              @ ([delay_log; interval_log; delay_ratio_log] |> List.filter_opt)
            in
            Span.event parent "cron.enqueue" ~attrs))


(* check_and_schedule_work_for_canvases checks all cron handlers on all the given
 * [canvases] and if necessary, enqueues work to run each one. *)
let check_and_schedule_work_for_canvases
    (parent : Span.t) (canvases : expr Canvas.canvas ref list) : unit =
  Telemetry.with_span parent "check_and_schedule_work_for_canvases" (fun span ->
      List.iter canvases ~f:(check_and_schedule_work_for_canvas span))


(* check_and_schedule_work_for_all_canvases iterates through every (non-test)
 * canvas and checks every cron handler to see if it should be executed,
 * enqueuing work to execute it if necessary. *)
let check_and_schedule_work_for_all_canvases (pid : int) :
    (unit, Exception.captured) Result.t =
  let span = Span.root "Cron.check_and_schedule_work_for_all_canvases" in
  Span.set_attr span "meta.process_id" (`Int pid) ;

  protectx span ~finally:Span.finish ~f:(fun span ->
      let all_canvas_names =
        if String.Caseless.equal
             Libservice.Config.postgres_settings.dbname
             "prodclone"
        then (
          Span.set_attr
            span
            "error.msg"
            (`String "Not running any crons; pointed at prodclone!") ;
          [] )
        else
          (* usually functions should trace their own execution, but
           * current_hosts is used from many places, not all of which have
           * tracing yet. *)
          Telemetry.with_span span "Serialize.current_hosts" (fun _ ->
              Serialize.current_hosts ()
              |> List.filter ~f:(Fn.non Serialize.is_test))
      in

      (* Chunk the canvas name list so that we don't have to load thousands of
       * canvases into memory at once.
       *
       * 1000 was chosen arbitrarily. Please update if data shows this is the wrong number. *)
      let chunks = all_canvas_names |> List.chunks_of ~length:1000 in
      Span.set_attrs
        span
        [ ("canvas.count", `Int (List.length all_canvas_names))
        ; ("chunks.count", `Int (List.length chunks)) ] ;

      let failed_to_load = ref [] in
      try
        List.iter chunks ~f:(fun chunk ->
            let loaded, errors = load_canvases span chunk in
            failed_to_load := List.append !failed_to_load errors ;
            Thread.yield () ;
            check_and_schedule_work_for_canvases span loaded) ;

        let num_errors = List.length !failed_to_load in
        Span.set_attrs span [("canvas.errors", `Int num_errors)] ;
        Ok ()
      with e ->
        let bt = Exception.get_backtrace () in
        Error (bt, e, Log.current_log_annotations ()))
