open Core_kernel
open Libcommon
open Libexecution
open Types.RuntimeT
open Types.RuntimeT.HandlerT
module Log = Libcommon.Log
module Span = Telemetry.Span

(** sum_pairs folds over the list [l] with function [f], summing the
  * values of the returned (int * int) tuple from each call *)
let sum_pairs (l : 'a list) ~(f : 'a -> int * int) : int * int =
  List.fold l ~init:(0, 0) ~f:(fun (a, b) c ->
      let a', b' = f c in
      (a + a', b + b'))


type cron_schedule_data = Serialize.cron_schedule_data

(* TODO: this is a query per cron handler. Might be worth seeing (later) if a we
 * could do this better with a join in the initial query that gets all cron
 * handlers from toplevel_oplists. *)
let last_ran_at (cron : cron_schedule_data) : Time.t option =
  Db.fetch_one_option
    ~name:"last_ran_at"
    "SELECT ran_at
       FROM cron_records
       WHERE tlid = $1
       AND canvas_id = $2
       ORDER BY id DESC
       LIMIT 1"
    ~params:[ID (Int63.of_string cron.tlid); Uuid cron.canvas_id]
  |> Option.map ~f:List.hd_exn
  |> Option.map ~f:Db.date_of_sqlstring


let parse_interval (cron : cron_schedule_data) : Time.Span.t option =
  match String.lowercase cron.modifier with
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

let execution_check (parent : Span.t) (cron : cron_schedule_data) :
    execution_check_type =
  let open Option in
  let now = Time.now () in
  match last_ran_at cron with
  | None ->
      {should_execute = true; scheduled_run_at = Some now; interval = None}
      (* we should always run if we've never run before *)
  | Some lrt ->
    ( match parse_interval cron with
    | None ->
        let bt = Caml.Printexc.get_raw_backtrace () in
        Log.erroR "Can't parse interval: " ~params:[("modifier", cron.modifier)] ;
        let e =
          Exception.make_exception
            DarkInternal
            ("Can't parse interval: " ^ cron.modifier)
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


let record_execution (cron : cron_schedule_data) : unit =
  let tlid = cron.tlid |> Int63.of_string in
  Db.run
    "INSERT INTO cron_records
    (tlid, canvas_id)
    VALUES ($1, $2)"
    ~params:[ID tlid; Uuid cron.canvas_id]
    ~name:"Cron.record_execution"


(** Check if a given cron spec should execute now, and if so, enqueue it.
  *
  * Returns true/false whether the cron was enqueued, so we can count it later *)
let check_and_schedule_work_for_cron
    (parent : Span.t) (cron : cron_schedule_data) : bool =
  let {should_execute; scheduled_run_at; interval} =
    execution_check parent cron
  in
  if should_execute
  then
    let space = "CRON" in
    let name = cron.name in
    let modifier = cron.modifier in
    Log.add_log_annotations
      [("cron", `Bool true)]
      (fun _ ->
        Event_queue.enqueue
          ~account_id:cron.owner
          ~canvas_id:cron.canvas_id
          space
          name
          modifier
          DNull ;
        record_execution cron ;

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
          [ ("canvas_name", `String cron.host)
          ; ("tlid", `String cron.tlid)
          ; ("handler_name", `String name)
            (* method here to use the spec-handler name for
             * consistency with http/worker logs *)
          ; ("method", `String modifier) ]
          @ ([delay_log; interval_log; delay_ratio_log] |> List.filter_opt)
        in
        Span.event parent "cron.enqueue" ~attrs ;
        true)
  else false


(** Given a list of [cron_schedule_data] records, check which ones are due to
  * run, and enqueue them.
  *
  * Returns a tuple of the number of crons (checked * scheduled) *)
let check_and_schedule_work_for_crons
    (parent : Span.t) (crons : cron_schedule_data list) : int * int =
  Telemetry.with_span parent "check_and_schedule_work_for_crons" (fun span ->
      let enqueued_crons =
        crons |> List.map ~f:(check_and_schedule_work_for_cron span)
      in
      (List.length crons, List.length enqueued_crons))


(** check_and_schedule_work_for_all_crons iterates through every (non-deleted)
  * cron toplevel_oplist and checks to see if it should be executed, enqueuing
  * work to execute it if necessary. *)
let check_and_schedule_work_for_all_crons (pid : int) :
    (unit, Exception.captured) Result.t =
  let span = Span.root "Cron.check_and_schedule_work_for_all_crons" in
  Span.set_attr span "meta.process_id" (`Int pid) ;

  protectx span ~finally:Span.finish ~f:(fun span ->
      let all_crons =
        if String.Caseless.equal
             Libservice.Config.postgres_settings.dbname
             "prodclone"
        then (
          Span.set_attr
            span
            "error.msg"
            (`String "Not running any crons; pointed at prodclone!") ;
          [] )
        else Serialize.fetch_active_crons span
      in

      (* Chunk the crons list so that we don't have to load thousands of
       * canvases into memory at once.
       *
       * 1000 was chosen arbitrarily. Please update if data shows this is the wrong number. *)
      let chunks = all_crons |> List.chunks_of ~length:1000 in
      Span.set_attrs
        span
        [ ("crons.count", `Int (List.length all_crons))
        ; ("chunks.count", `Int (List.length chunks)) ] ;

      try
        (* process_chunk loads each canvas by name,
         * then checks and schedules crons *)
        let process_chunk (chunk : cron_schedule_data list) =
          check_and_schedule_work_for_crons span chunk
        in

        let checked, scheduled = sum_pairs chunks ~f:process_chunk in
        Span.set_attrs
          span
          [("crons.checked", `Int checked); ("crons.scheduled", `Int scheduled)] ;
        Ok ()
      with e ->
        let bt = Exception.get_backtrace () in
        Error (bt, e, Log.current_log_annotations ()))
