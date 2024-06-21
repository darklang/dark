module Tests.QueueSchedulingRules

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude

open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module NR = LibParser.NameResolver
module EQ2 = LibCloud.Queue
module Canvas = LibCloud.Canvas
module Serialize = LibCloud.Serialize
module SR = LibCloud.QueueSchedulingRules


let testGetWorkerSchedulesForCanvas =
  testTask "worker schedules for canvas" {
    let! canvasID = initializeTestCanvas "worker-schedules"

    let! e1 = parsePTExpr "1L"
    let! e2 = parsePTExpr "1L"
    let! e3 = parsePTExpr "1L"
    let apple = testWorker "apple" e1
    let banana = testWorker "banana" e2
    let cherry = testWorker "cherry" e3

    do!
      ([ apple; banana; cherry ]
       |> List.map (fun h -> (PT.Toplevel.TLHandler h, Serialize.NotDeleted))
       |> Canvas.saveTLIDs canvasID)

    do! EQ2.pauseWorker canvasID "apple"
    do! EQ2.pauseWorker canvasID "banana"
    do! EQ2.blockWorker canvasID "banana"
    let! result = SR.getWorkerSchedules canvasID

    let check (name : string) (value : SR.WorkerStates.State) =
      let actual =
        Map.get name result
        |> Exception.unwrapOptionInternal "missing workerstate" [ "name", name ]
        |> string
      let expected = string value
      Expect.equal actual expected ($"{name} is {expected}")

    check "apple" SR.WorkerStates.Paused
    check "banana" SR.WorkerStates.Blocked
    check "cherry" SR.WorkerStates.Running
  }

let tests =
  testSequencedGroup
    "eventQueue"
    (testList "eventQueue" [ testGetWorkerSchedulesForCanvas ])
