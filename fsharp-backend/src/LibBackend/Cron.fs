module LibBackend.Cron

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp.Tasks
open Npgsql
open Db

open Prelude
open Prelude.Tablecloth
open Tablecloth
open LibService.Telemetry

module PT = LibBackend.ProgramTypes



// sumPairs folds over the list [l] with function [f], summing the
// values of the returned (int * int) tuple from each call
// let sumPairs (f : 'a -> int * int) (l : 'a list): int * int =
//   List.fold (0, 0) (fun (a, b) c ->
//       let a', b' = f c in
//       (a + a', b + b')) l
//

type CronScheduleData = Serialize.CronScheduleData

// TODO: this is a query per cron handler. Might be worth seeing (later) if a we
// could do this better with a join in the initial query that gets all cron
// handlers from toplevel_oplists.
let lastRanAt (cron : CronScheduleData) : Task<Option<System.DateTime>> =
  Sql.query
    "SELECT ran_at
       FROM cron_records
       WHERE tlid = @tlid
       AND canvas_id = @canvasID
       ORDER BY id DESC
       LIMIT 1"
  |> Sql.parameters [ "tlid", Sql.tlid cron.tlid
                      "canvasID", Sql.uuid cron.canvasID ]
  |> Sql.executeRowOptionAsync
       (fun read -> read.string "ran_at" |> System.DateTime.ofIsoString)


let convertInterval (interval : PT.Handler.CronInterval) : System.TimeSpan =
  // Must use the 5-value constructor as the 3-value constructor doesn't support days
  match interval with
  | PT.Handler.EveryDay -> System.TimeSpan(1, 0, 0, 0, 0)
  | PT.Handler.EveryWeek -> System.TimeSpan(7, 0, 0, 0, 0)
  | PT.Handler.EveryFortnight -> System.TimeSpan(14, 0, 0, 0, 0)
  | PT.Handler.EveryHour -> System.TimeSpan(0, 1, 0, 0, 0)
  | PT.Handler.Every12Hours -> System.TimeSpan(0, 12, 0, 0, 0)
  | PT.Handler.EveryMinute -> System.TimeSpan(0, 0, 1, 0, 0)


type ExecutionCheck =
  { shouldExecute : bool
    scheduledRunAt : Option<System.DateTime>
    interval : Option<System.TimeSpan> }

let executionCheck (cron : CronScheduleData) : Task<ExecutionCheck> =
  task {
    let now = System.DateTime.Now in

    match! lastRanAt cron with
    | None ->
        // we should always run if we've never run before
        return { shouldExecute = true; scheduledRunAt = Some now; interval = None }
    | Some lrt ->
        // Example:
        //   last_ran_at = 16:00
        //   interval: 1 hour
        //   now: 16:30
        //   therefore:
        //     shouldRunAfter is 17:01
        //     and we should run once now >= shouldRunAfter
        let interval = convertInterval cron.interval
        let shouldRunAfter = System.DateTimeOffset(lrt, interval)

        if now >= shouldRunAfter.DateTime then
          return
            { shouldExecute = true
              scheduledRunAt = Some shouldRunAfter.DateTime
              interval = Some interval }
        else
          return
            { shouldExecute = false
              scheduledRunAt = None
              interval = Some interval }
  }


// let record_execution (cron : cron_schedule_data) : unit =
//   let tlid = cron.tlid |> Int63.of_string in
//   Db.run
//     "INSERT INTO cron_records
//     (tlid, canvas_id)
//     VALUES ($1, $2)"
//     ~params:[ID tlid; Uuid cron.canvas_id]
//     ~name:"Cron.record_execution"
//
//
// (** Check if a given cron spec should execute now, and if so, enqueue it.
//   *
//   * Returns true/false whether the cron was enqueued, so we can count it later *)
// let check_and_schedule_work_for_cron
//     (parent : Span.t) (cron : cron_schedule_data) : bool =
//   let {should_execute; scheduled_run_at; interval} =
//     execution_check parent cron
//   in
//   if should_execute
//   then
//     let space = "CRON" in
//     let name = cron.name in
//     let modifier = cron.modifier in
//     Log.add_log_annotations
//       [("cron", `Bool true)]
//       (fun _ ->
//         Event_queue.enqueue
//           ~account_id:cron.owner
//           ~canvas_id:cron.canvas_id
//           space
//           name
//           modifier
//           DNull ;
//         record_execution cron ;
//
//         (* It's a little silly to recalculate now when we just did
//          * it in execution_check, but maybe Event_queue.enqueue was
//          * slow or something *)
//         let now = Time.now () in
//         let delay_ms =
//           scheduled_run_at
//           |> Option.map ~f:(Time.diff now)
//           (* For future reference: yes, to_ms returns the span of time
//            * in milliseconds, _not_ "the ms part of the span of
//            * time". I had to check that it wasn't, like, 10.5s |>
//            * to_ms is 500, because 10.5|>to_s is 10. Time-related
//            * APIs get tricky that way. *)
//           |> Option.map ~f:Time.Span.to_ms
//         in
//         let delay_log =
//           delay_ms |> Option.map ~f:(fun delay -> ("delay", `Float delay))
//         in
//         let interval_log =
//           interval
//           (* This is definitely not None, but keep the
//            * typechecker happy *)
//           (* Floats are IEEE-754, which has a max at 2**31-1;
//            * that's 24.85 days worth of milliseconds. Since our
//            * longest allowed cron interval is two weeks, this is
//            * fine *)
//           |> Option.map ~f:(fun interval ->
//                  ("interval", `Float (interval |> Time.Span.to_ms)))
//         in
//         let delay_ratio_log =
//           Option.map2 delay_ms interval ~f:(fun delay_ms interval ->
//               let interval = interval |> Time.Span.to_ms in
//               delay_ms /. interval)
//           |> Option.map ~f:(fun delay_ratio ->
//                  ("delay_ratio", `Float delay_ratio))
//         in
//         let attrs =
//           [ ("canvas_name", `String cron.host)
//           ; ("tlid", `String cron.tlid)
//           ; ("handler_name", `String name)
//             (* method here to use the spec-handler name for
//              * consistency with http/worker logs *)
//           ; ("method", `String modifier) ]
//           @ ([delay_log; interval_log; delay_ratio_log] |> List.filter_opt)
//         in
//         Span.event parent "cron.enqueue" ~attrs ;
//         true)
//   else false
//
//
// (** Given a list of [cron_schedule_data] records, check which ones are due to
//   * run, and enqueue them.
//   *
//   * Returns a tuple of the number of crons (checked * scheduled) *)
// let check_and_schedule_work_for_crons
//     (parent : Span.t) (crons : cron_schedule_data list) : int * int =
//   Telemetry.with_span parent "check_and_schedule_work_for_crons" (fun span ->
//       let enqueued_crons =
//         crons |> List.map ~f:(check_and_schedule_work_for_cron span)
//       in
//       (List.length crons, List.length enqueued_crons))
//
//
// (** check_and_schedule_work_for_all_crons iterates through every (non-deleted)
//   * cron toplevel_oplist and checks to see if it should be executed, enqueuing
//   * work to execute it if necessary. *)
// let check_and_schedule_work_for_all_crons (pid : int) :
//     (unit, Exception.captured) Result.t =
//   Telemetry.with_root
//     "Cron.check_and_schedule_work_for_all_crons"
//     ~attrs:[("meta.process_id", `Int pid)]
//     (fun span ->
//       let all_crons =
//         if String.Caseless.equal
//              Libservice.Config.postgres_settings.dbname
//              "prodclone"
//         then (
//           Span.set_attr
//             span
//             "error.msg"
//             (`String "Not running any crons; pointed at prodclone!") ;
//           [] )
//         else Serialize.fetch_active_crons span
//       in
//
//       (* Chunk the crons list so that we don't have to load thousands of
//        * canvases into memory at once.
//        *
//        * 1000 was chosen arbitrarily. Please update if data shows this is the wrong number. *)
//       let chunks = all_crons |> List.chunks_of ~length:1000 in
//       Span.set_attrs
//         span
//         [ ("crons.count", `Int (List.length all_crons))
//         ; ("chunks.count", `Int (List.length chunks)) ] ;
//
//       try
//         (* process_chunk loads each canvas by name,
//          * then checks and schedules crons *)
//         let process_chunk (chunk : cron_schedule_data list) =
//           check_and_schedule_work_for_crons span chunk
//         in
//
//         let checked, scheduled = sum_pairs chunks ~f:process_chunk in
//         Span.set_attrs
//           span
//           [("crons.checked", `Int checked); ("crons.scheduled", `Int scheduled)] ;
//         Ok ()
//       with e ->
//         let bt = Exception.get_backtrace () in
//         Error (bt, e, Log.current_log_annotations ()))
//
