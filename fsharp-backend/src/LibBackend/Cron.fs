module LibBackend.Cron

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

open Prelude
open Prelude.Tablecloth
open Tablecloth

module Telemetry = LibService.Telemetry
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes



// sumPairs folds over the list [l] with function [f], summing the
// values of the returned (int * int) tuple from each call
let sumPairs (l : (int * int) list) : int * int =
  List.fold (0, 0) (fun (a', b') (a, b) -> (a + a', b + b')) l


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
  |> Sql.executeRowOptionAsync (fun read -> read.dateTime "ran_at")


let convertInterval (interval : PT.Handler.CronInterval) : System.TimeSpan =
  // Must use the 5-value constructor as the 3-value constructor doesn't support days
  match interval with
  | PT.Handler.EveryDay -> System.TimeSpan(1, 0, 0, 0, 0)
  | PT.Handler.EveryWeek -> System.TimeSpan(7, 0, 0, 0, 0)
  | PT.Handler.EveryFortnight -> System.TimeSpan(14, 0, 0, 0, 0)
  | PT.Handler.EveryHour -> System.TimeSpan(0, 1, 0, 0, 0)
  | PT.Handler.Every12Hours -> System.TimeSpan(0, 12, 0, 0, 0)
  | PT.Handler.EveryMinute -> System.TimeSpan(0, 0, 1, 0, 0)


type NextExecution =
  { scheduledRunAt : Option<System.DateTime>
    interval : Option<System.TimeSpan> }

let executionCheck (cron : CronScheduleData) : Task<Option<NextExecution>> =
  task {
    let now = System.DateTime.Now in

    match! lastRanAt cron with
    | None ->
      // we should always run if we've never run before
      return Some { scheduledRunAt = Some now; interval = None }
    | Some lrt ->
      // Example:
      //   last_ran_at = 16:00
      //   interval: 1 hour
      //   now: 16:30
      //   therefore:
      //     shouldRunAfter is 17:01
      //     and we should run once now >= shouldRunAfter
      let interval = convertInterval cron.interval
      let shouldRunAfter = lrt + interval

      if now >= shouldRunAfter then
        return
          Some { scheduledRunAt = Some shouldRunAfter; interval = Some interval }
      else
        return None
  }


let recordExecution (cron : CronScheduleData) : Task<unit> =
  Sql.query
    "INSERT INTO cron_records
    (tlid, canvas_id)
    VALUES (@tlid, @canvasID)"
  |> Sql.parameters [ "tlid", Sql.tlid cron.tlid
                      "canvasID", Sql.uuid cron.canvasID ]
  |> Sql.executeStatementAsync


// Check if a given cron spec should execute now, and if so, enqueue it.
//
// Returns true/false whether the cron was enqueued, so we can count it later
let checkAndScheduleWorkForCron (cron : CronScheduleData) : Task<bool> =
  task {
    match! executionCheck cron with
    | Some check ->
      use span = Telemetry.child "cron.enqueue" []

      if Config.triggerCrons then
        do!
          EventQueue.enqueue
            cron.canvasName
            cron.canvasID
            cron.ownerID
            "CRON"
            cron.cronName
            (string cron.interval)
            RT.DNull
        do! recordExecution cron

      // It's a little silly to recalculate now when we just did
      // it in executionCheck, but maybe EventQueue.enqueue was
      // slow or something
      let now = System.DateTime.Now
      let delayMs =
        check.scheduledRunAt
        |> Option.map (fun scheduled ->
          System.TimeSpan(now.Ticks - scheduled.Ticks).TotalMilliseconds)
      let delayLog = delayMs |> Option.map (fun delay -> ("delay", delay :> obj))
      let intervalLog =
        // Floats are IEEE-754, which has a max at 2**31-1;
        // that's 24.85 days worth of milliseconds. Since our
        // longest allowed cron interval is two weeks, this is
        // fine
        check.interval
        |> Option.map (fun interval ->
          ("interval", interval.TotalMilliseconds :> obj))
      let delayRatioLog =
        Option.map2
          (fun delayMs (interval : System.TimeSpan) ->
            delayMs / interval.TotalMilliseconds)
          delayMs
          check.interval
        |> Option.map (fun delayRatio -> ("delay_ratio", delayRatio :> obj))
      let attrs =
        [ ("canvas_name", cron.canvasName :> obj)
          ("tlid", cron.tlid)
          ("handler_name", cron.cronName)
          // method here to use the spec-handler name for
          // consistency with http/worker logs
          ("method", string cron.interval) ]
        @ ([ delayLog; intervalLog; delayRatioLog ] |> List.filterMap (fun a -> a))
      Telemetry.addTags attrs
      return true
    | None -> return false
  }

// Given a list of [cron_schedule_data] records, check which ones are due to
// run, and enqueue them.
//
// Returns a tuple of the number of crons (checked * scheduled) *)
let checkAndScheduleWorkForCrons (crons : CronScheduleData list) : Task<int * int> =
  task {
    use _span = Telemetry.child "check_and_schedule_work_for_crons" []
    let! enqueuedCrons = crons |> Task.mapInParallel checkAndScheduleWorkForCron
    let enqueuedCronCount = List.count Fun.identity enqueuedCrons
    return (List.length crons, enqueuedCronCount)
  }


// checkAndScheduleWorkForAllCrons iterates through every (non-deleted)
// cron toplevel_oplist and checks to see if it should be executed, enqueuing
// work to execute it if necessary.
let checkAndScheduleWorkForAllCrons () : Task<unit> =
  task {
    use _span = Telemetry.child "checkAndScheduleWorkForAllCrons" []
    let! allCrons = Serialize.fetchActiveCrons ()

    // Chunk the crons list so that we don't have to load thousands of
    // canvases into memory/tasks at once
    //
    // 100 was chosen arbitrarily. Please update if data shows this is the wrong number.
    let chunks = allCrons |> List.chunksOf 100
    Telemetry.addTags [ ("crons.count", List.length allCrons)
                        ("chunks.count", List.length chunks) ]

    let! processed = Task.mapSequentially checkAndScheduleWorkForCrons chunks
    let checkedCount, scheduled = sumPairs processed
    Telemetry.addTags [ ("crons.checked", checkedCount)
                        ("crons.scheduled", scheduled) ]
    return ()
  }
