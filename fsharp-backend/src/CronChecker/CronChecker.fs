module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Execution = LibExecution.Execution

module Telemetry = LibService.Telemetry
module Span = Telemetry.Span

type Activity = System.Diagnostics.Activity

let runCronChecker () : Task<int> =
  task {
    try
      while true do
        do! LibBackend.Cron.checkAndScheduleWorkForAllCrons 0
      return 0
    with
    | e ->
      LibService.Rollbar.lastDitchBlocking
        "Error running CronChecker"
        (Prelude.ExecutionID "cronchecker")
        []
        e
      return (-1)
  }


[<EntryPoint>]
let main args : int =
  print "Starting CronChecker"
  LibBackend.Init.init "CronChecker"
  // CLEANUP rename
  Telemetry.createRoot "Cron.check_and_schedule_work_for_all_crons"
  (runCronChecker ()).Result
