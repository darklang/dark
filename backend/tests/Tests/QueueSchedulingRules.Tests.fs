module Tests.QueueSchedulingRules

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth
open LibBackend.Db

open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module EQ2 = LibBackend.Queue
module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module SR = LibBackend.QueueSchedulingRules

let p (code : string) : PT.Expr =
  Parser.Parser.parse builtinResolver "queueschedulingrules.fs" code


let testGetWorkerSchedulesForCanvas =
  testTask "worker schedules for canvas" {
    let! canvasID = initializeTestCanvas "worker-schedules"

    let apple = testWorker "apple" (p "1")
    let banana = testWorker "banana" (p "1")
    let cherry = testWorker "cherry" (p "1")

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
