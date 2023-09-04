module Tests.Cron

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Cron = LibCloud.Cron
module Canvas = LibCloud.Canvas
module Serialize = LibCloud.Serialize

let p (code : string) : Task<PT.Expr> =
  LibParser.Parser.parseSimple "cron.tests.fs" code |> Ply.toTask


let testCronFetchActiveCrons =
  testTask "fetch active crons doesn't raise" {
    let! (_cronSchedule : List<Serialize.CronScheduleData>) =
      Serialize.fetchActiveCrons ()

    Expect.equal true true "just checking it didnt raise"
  }


let testCronSanity =
  testTask "cron sanity" {
    let! canvasID = initializeTestCanvas "cron-sanity"

    let! e = p "5 + 3"
    let h = testCron "test" PT.Handler.EveryDay e
    do! Canvas.saveTLIDs canvasID [ (PT.Toplevel.TLHandler h, Serialize.NotDeleted) ]

    let cronScheduleData : Cron.CronScheduleData =
      { canvasID = canvasID
        tlid = h.tlid
        cronName = "test"
        interval = PT.Handler.EveryDay }

    let! executionCheck = Cron.executionCheck cronScheduleData
    Expect.equal (Option.isSome executionCheck) true "should_execute should be true"
  }


let testCronJustRan =
  testTask "test cron just ran" {
    let! canvasID = initializeTestCanvas "cron-just-ran"

    let! e = p "5 + 3"
    let h = testCron "test" PT.Handler.EveryDay e

    do! Canvas.saveTLIDs canvasID [ (PT.Toplevel.TLHandler h, Serialize.NotDeleted) ]

    let cronScheduleData : Cron.CronScheduleData =
      { canvasID = canvasID
        tlid = h.tlid
        cronName = "test"
        interval = PT.Handler.EveryDay }

    do! Cron.recordExecution cronScheduleData

    let! executionCheck = Cron.executionCheck cronScheduleData
    Expect.equal executionCheck None "should_execute should be false"
  }


let tests =
  testList "cron" [ testCronFetchActiveCrons; testCronSanity; testCronJustRan ]
