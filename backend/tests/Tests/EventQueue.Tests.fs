module Tests.EventQueue

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
module EQ2 = LibBackend.EventQueueV2
module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module SR = LibBackend.QueueSchedulingRules

module TI = LibBackend.TraceInputs
module TFR = LibBackend.TraceFunctionResults


let p (code : string) = Parser.Parser.parsePTExpr code


let testGetWorkerSchedulesForCanvas =
  testTask "worker schedules for canvas" {
    let! meta = initializeTestCanvas "worker-schedules"

    let apple = testWorker "apple" (p "1")
    let banana = testWorker "banana" (p "1")
    let cherry = testWorker "cherry" (p "1")

    do!
      ([ apple; banana; cherry ]
       |> List.map (fun h ->
         (h.tlid, [ handlerOp h ], PT.Toplevel.TLHandler h, Canvas.NotDeleted))
       |> Canvas.saveTLIDs meta)

    do! EQ2.pauseWorker meta.id "apple"
    do! EQ2.pauseWorker meta.id "banana"
    do! EQ2.blockWorker meta.id "banana"
    let! result = SR.getWorkerSchedules meta.id

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
