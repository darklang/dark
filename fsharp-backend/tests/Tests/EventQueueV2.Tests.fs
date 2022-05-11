module Tests.EventQueueV2

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
module EQ = LibBackend.EventQueueV2
module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize

module TI = LibBackend.TraceInputs
module TFR = LibBackend.TraceFunctionResults


let p (code : string) = FSharpToExpr.parsePTExpr code

// This doesn't actually test input, since it's a cron handler and not an actual event handler

let testEventQueueRoundtrip =
  testTask "event queue roundtrip" {
    // set up handler
    let! meta = initializeTestCanvas (Randomized "event-queue-roundtrip")

    let h = testCron "test" PT.Handler.EveryDay (p "let data = Date.now_v0 in 123")
    let oplists = [ handlerOp h ]

    do!
      Canvas.saveTLIDs
        meta
        [ (h.tlid, oplists, PT.Toplevel.TLHandler h, Canvas.NotDeleted) ]

    // enqueue; schedule
    let input = RT.DNull // I don't believe crons take inputs?
    do! EQ.enqueue meta.id "CRON" "test" "Daily" input

    // verify roundtrip
    let! result = QueueWorker.dequeueAndProcess ()
    Expect.equal result 1 "should have processed"

    // should have at least one trace
    let! eventIDs = TI.loadEventIDs meta.id ("CRON", "test", "Daily")
    let traceID =
      eventIDs
      |> List.head
      |> Exception.unwrapOptionInternal "missing eventID" []
      |> Tuple2.first

    // Saving happens in the background so wait for it
    let mutable functionResults = []
    for i in 1..10 do
      if functionResults = [] then
        let! result = TFR.load meta.id traceID h.tlid
        functionResults <- result
        if functionResults = [] then do! Task.Delay 300

    Expect.equal (List.length functionResults) 1 "should have stored fn result"
  }


let testEventQueueIsFifo =
  testTask "event queue is fifo" {
    ()
  // let! meta = initializeTestCanvas (Randomized "event-queue-is-fifo")
  // let apple = testWorker "apple" (p "event")
  // let banana = testWorker "banana" (p "event")

  // do!
  //   ([ apple; banana ]
  //    |> List.map (fun h ->
  //      (h.tlid, [ handlerOp h ], PT.Toplevel.TLHandler h, Canvas.NotDeleted))
  //    |> Canvas.saveTLIDs meta)

  // let enqueue (name : string) (i : int64) =
  //   EQ.enqueue meta.name meta.id meta.owner "WORKER" name "_" (RT.DInt i)

  // do! enqueue "apple" 1L
  // do! enqueue "apple" 2L
  // do! enqueue "banana" 3L
  // do! enqueue "apple" 4L

  // let checkDequeue (i : int64) expectedName : Task<unit> =
  //   task {
  //     let! evt = EQ.dequeue ()
  //     let evt = Exception.unwrapOptionInternal "cannot find event" [] evt

  //     Expect.equal evt.name expectedName $"dequeue {i} is handler {expectedName}"
  //     Expect.equal evt.value (RT.DInt i) $"dequeue {i} has value {i}"
  //     do! EQ.finish evt
  //     return ()
  //   }

  // do!
  //   Sql.withTransaction (fun () ->
  //     task {
  //       do! checkDequeue 1L "apple"
  //       do! checkDequeue 2L "apple"
  //       do! checkDequeue 3L "banana"
  //       do! checkDequeue 4L "apple"
  //       return Ok(Some RT.DNull)
  //     })
  }

let tests =
  testSequencedGroup
    "eventQueueV2"
    (testList "eventQueueV2" [ testEventQueueRoundtrip; testEventQueueIsFifo ])
