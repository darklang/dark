module Tests.Cron

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Cron = LibBackend.Cron
module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize

module TI = LibBackend.TraceInputs
module TFR = LibBackend.TraceFunctionResults


let p (code : string) = Parser.parsePTExpr code



let testCronFetchActiveCrons =
  testTask "fetch active crons doesn't raise" {
    let! (_cronSchedule : List<Serialize.CronScheduleData>) =
      Serialize.fetchActiveCrons ()

    Expect.equal true true "just checking it didnt raise"
  }


let testCronSanity =
  testTask "cron sanity" {
    let! meta = initializeTestCanvas (Randomized "cron-sanity")

    let h = testCron "test" PT.Handler.EveryDay (p " 5 + 3")
    let oplists = [ handlerOp h ]

    do!
      Canvas.saveTLIDs
        meta
        [ (h.tlid, oplists, PT.Toplevel.TLHandler h, Canvas.NotDeleted) ]

    let cronScheduleData : Cron.CronScheduleData =
      { canvasID = meta.id
        ownerID = meta.owner
        canvasName = meta.name
        tlid = h.tlid
        cronName = "test"
        interval = PT.Handler.EveryDay }

    let! executionCheck = Cron.executionCheck cronScheduleData
    Expect.equal (Option.isSome executionCheck) true "should_execute should be true"
  }


let testCronJustRan =
  testTask "test cron just ran" {
    let! meta = initializeTestCanvas (Randomized "cron-just-ran")

    let h = testCron "test" PT.Handler.EveryDay (p "5 + 3")

    do!
      Canvas.saveTLIDs
        meta
        [ (h.tlid, [ handlerOp h ], PT.Toplevel.TLHandler h, Canvas.NotDeleted) ]

    let cronScheduleData : Cron.CronScheduleData =
      { canvasID = meta.id
        ownerID = meta.owner
        canvasName = meta.name
        tlid = h.tlid
        cronName = "test"
        interval = PT.Handler.EveryDay }

    do! Cron.recordExecution cronScheduleData

    let! executionCheck = Cron.executionCheck cronScheduleData
    Expect.equal executionCheck None "should_execute should be false"
  }


let tests =
  testList "cron" [ testCronFetchActiveCrons; testCronSanity; testCronJustRan ]
