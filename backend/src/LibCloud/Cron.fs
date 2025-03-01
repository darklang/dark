/// Supports "crons" - toplevels that are triggered on a set schedule
module LibCloud.Cron

open System.Threading.Tasks
open FSharp.Control.Tasks

open Db

open Prelude

module Telemetry = LibService.Telemetry
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes


type CronScheduleData = Serialize.CronScheduleData

// TODO: this is a query per cron handler. Might be worth seeing (later) if a we
// could do this better with a join in the initial query that gets all cron
// handlers from `toplevels`.
let lastRanAt (cron : CronScheduleData) : Task<Option<NodaTime.Instant>> =
  Sql.query
    "SELECT ran_at
    FROM cron_records_v0
    WHERE tlid = @tlid
      AND canvas_id = @canvasID
    ORDER BY id DESC
    LIMIT 1"
  |> Sql.parameters
    [ "tlid", Sql.tlid cron.tlid; "canvasID", Sql.uuid cron.canvasID ]
  |> Sql.executeRowOptionAsync (fun read -> read.instantWithoutTimeZone "ran_at")


let convertInterval (interval : PT.Handler.CronInterval) : NodaTime.Period =
  match interval with
  | PT.Handler.EveryDay -> NodaTime.Period.FromDays 1
  | PT.Handler.EveryWeek -> NodaTime.Period.FromWeeks 1
  | PT.Handler.EveryFortnight -> NodaTime.Period.FromWeeks 2
  | PT.Handler.EveryHour -> NodaTime.Period.FromHours 1
  | PT.Handler.Every12Hours -> NodaTime.Period.FromHours 12
  | PT.Handler.EveryMinute -> NodaTime.Period.FromMinutes 1


type NextExecution =
  {
    scheduledRunAt : Option<NodaTime.Instant>
    /// The interval is copied to this record for logging only
    interval : Option<NodaTime.Period>
  }

let executionCheck (cron : CronScheduleData) : Task<Option<NextExecution>> =
  task {
    let now = NodaTime.Instant.now ()

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
      let period = convertInterval cron.interval
      let shouldRunAfter = lrt + period.ToDuration()

      if now >= shouldRunAfter then
        return Some { scheduledRunAt = Some shouldRunAfter; interval = Some period }
      else
        return None
  }


let recordExecution (cron : CronScheduleData) : Task<unit> =
  Sql.query
    "INSERT INTO cron_records_v0
      (id, tlid, canvas_id)
    VALUES
      (@id, @tlid, @canvasID)"
  |> Sql.parameters
    [ "id", Sql.uuid (System.Guid.NewGuid())
      "tlid", Sql.tlid cron.tlid
      "canvasID", Sql.uuid cron.canvasID ]
  |> Sql.executeStatementAsync


/// Check if a given cron spec should execute now, and if so, enqueue it.
///
/// Returns true/false whether the cron was enqueued, so we can count it later
let checkAndScheduleWorkForCron (cron : CronScheduleData) : Task<bool> =
  task {
    match! executionCheck cron with
    | Some check ->
      use _span = Telemetry.child "cron.enqueue" []

      // trigger execution
      if Config.triggerCrons then
        do!
          Queue.enqueueNow
            cron.canvasID
            "CRON"
            cron.cronName
            (PTParser.Handler.CronInterval.toString cron.interval)
            RT.DUnit
        do! recordExecution cron

      // record the execution

      // It's a little silly to recalculate now when we just did
      // it in executionCheck, but maybe EventQueue.enqueue was
      // slow or something
      let now = NodaTime.Instant.now ()
      let delayMs =
        check.scheduledRunAt
        |> Option.map (fun scheduled -> (now - scheduled).TotalMilliseconds)
      let delayLog = delayMs |> Option.map (fun delay -> ("delay", delay :> obj))
      let intervalLog =
        // Floats are IEEE-754, which has a max at 2**31-1;
        // that's 24.85 days worth of milliseconds. Since our
        // longest allowed cron interval is two weeks, this is
        // fine
        // CLEANUP: that^ note assumes too much about our cron setup;
        //   monthly crons would be reasonable
        check.interval
        |> Option.map (fun interval -> ("interval", interval.Milliseconds :> obj))
      let delayRatioLog =
        Option.map2
          (fun delayMs (interval : NodaTime.Period) ->
            delayMs / interval.ToDuration().TotalMilliseconds)
          delayMs
          check.interval
        |> Option.map (fun delayRatio -> ("delay_ratio", delayRatio :> obj))
      let attrs =
        [ ("tlid", cron.tlid :> obj)
          ("handler_name", cron.cronName)
          // method here to use the spec-handler name for
          // consistency with http/worker logs
          ("method", string cron.interval) ]
        @ ([ delayLog; intervalLog; delayRatioLog ] |> List.filterMap (fun a -> a))
      Telemetry.addTags attrs
      return true
    | None -> return false
  }

/// Iterates through every (non-deleted) cron toplevel
/// and checks to see if it should be executed, enqueuing
/// work to execute it if necessary.
let checkAndScheduleWorkForAllCrons () : Task<unit> =
  task {
    use (_span : Telemetry.Span.T) =
      Telemetry.child "checkAndScheduleWorkForAllCrons" []
    let! crons = Serialize.fetchActiveCrons ()

    let concurrencyCount = LibService.Config.pgPoolSize
    let! enqueuedCrons =
      Task.mapWithConcurrency concurrencyCount checkAndScheduleWorkForCron crons
    let enqueuedCronCount = List.count identity enqueuedCrons
    Telemetry.addTags
      [ ("crons.checked", List.length crons)
        ("crons.scheduled", enqueuedCronCount) ]
    return ()
  }
