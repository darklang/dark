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
module EQ = LibBackend.EventQueue
module EQ2 = LibBackend.EventQueueV2
module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module SR = LibBackend.QueueSchedulingRules

module TI = LibBackend.TraceInputs
module TFR = LibBackend.TraceFunctionResults


let p (code : string) = FSharpToExpr.parsePTExpr code

// This doesn't actually test input, since it's a cron handler and not an actual event handler


let testEventQueueIsFifo =
  testTask "event queue is fifo" {
    let! meta = initializeTestCanvas (Randomized "event-queue-is-fifo")
    let apple = testWorker "apple" (p "event")
    let banana = testWorker "banana" (p "event")

    do!
      ([ apple; banana ]
       |> List.map (fun h ->
         (h.tlid, [ handlerOp h ], PT.Toplevel.TLHandler h, Canvas.NotDeleted))
       |> Canvas.saveTLIDs meta)

    let enqueue (name : string) (i : int64) =
      EQ.enqueue meta.name meta.id meta.owner "WORKER" name "_" (RT.DInt i)

    do! enqueue "apple" 1L
    do! enqueue "apple" 2L
    do! enqueue "banana" 3L
    do! enqueue "apple" 4L
    do! SR.testingScheduleAll ()

    let checkDequeue (i : int64) expectedName : Task<unit> =
      task {
        let! evt = EQ.dequeue ()
        let evt = Exception.unwrapOptionInternal "cannot find event" [] evt

        Expect.equal evt.name expectedName $"dequeue {i} is handler {expectedName}"
        Expect.equal evt.value (RT.DInt i) $"dequeue {i} has value {i}"
        do! EQ.finish evt
        return ()
      }

    do!
      Sql.withTransaction (fun () ->
        task {
          do! checkDequeue 1L "apple"
          do! checkDequeue 2L "apple"
          do! checkDequeue 3L "banana"
          do! checkDequeue 4L "apple"
          return Ok(Some RT.DNull)
        })
  }

let testGetWorkerSchedulesForCanvas =
  testTask "worker schedules for canvas" {
    let! meta = initializeTestCanvas (Randomized "worker-schedules")

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
    (testList "eventQueue" [ testEventQueueIsFifo; testGetWorkerSchedulesForCanvas ])
