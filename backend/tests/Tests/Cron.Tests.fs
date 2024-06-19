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
module PackageIDs = LibExecution.PackageIDs
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module C2DT = LibExecution.CommonToDarkTypes
let pm = LibCloud.PackageManager.pt

let p (code : string) : Task<RT.Dval> =
  uply {
    let! (state : RT.ExecutionState) =
      let canvasID = System.Guid.NewGuid()
      executionStateFor pm canvasID false false Map.empty

    let name =
      RT.FQFnName.FQFnName.Package PackageIDs.Fn.LanguageTools.Parser.parsePTExpr

    let args = NEList.singleton (RT.DString code)
    let! exeResult = LibExecution.Execution.executeFunction state name [] args

    return! state |> unwrapExecutionResult exeResult
  }
  |> Ply.toTask


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
    let resultExpr =
      C2DT.Result.fromDT PT2DT.Expr.fromDT e PT2DT.TypeReference.fromDT

    let expr =
      match resultExpr with
      | Ok expr -> expr
      | Error e ->
        Exception.raiseInternal "Error converting to dark types" [ "error", e ]

    let h = testCron "test" PT.Handler.EveryDay expr
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
    let resultExpr =
      C2DT.Result.fromDT PT2DT.Expr.fromDT e PT2DT.TypeReference.fromDT

    let expr =
      match resultExpr with
      | Ok expr -> expr
      | Error e ->
        Exception.raiseInternal "Error converting to dark types" [ "error", e ]

    let h = testCron "test" PT.Handler.EveryDay expr

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
