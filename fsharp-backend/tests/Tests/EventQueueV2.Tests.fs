module Tests.EventQueueV2

open System.Threading.Tasks
open FSharp.Control.Tasks

open NodaTime

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

open Npgsql.FSharp
open Npgsql
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

let initializeCanvas (name : string) : Task<Canvas.Meta * tlid> =
  task {
    // set up handler
    let! meta = initializeTestCanvas (Randomized name)

    let h = testCron "test" PT.Handler.EveryDay (p "let data = Date.now_v0 in 123")
    let oplists = [ handlerOp h ]

    do!
      Canvas.saveTLIDs
        meta
        [ (h.tlid, oplists, PT.Toplevel.TLHandler h, Canvas.NotDeleted) ]

    return meta, h.tlid
  }

let enqueue (meta : Canvas.Meta) : Task<unit> =
  let input = RT.DNull // I don't believe crons take inputs?
  EQ.enqueue meta.id "CRON" "test" "Daily" input

let checkSuccess
  (meta : Canvas.Meta)
  (tlid : tlid)
  (result : Result<EQ.T * EQ.Notification, EQ.Notification>)
  =
  task {
    match result with
    | Ok (event, notification) ->
      // TODO: is there a way to count/test the number of messages in the queue?
      let! event = EQ.loadEvent event.canvasID event.id
      Expect.isNone event "should have been deleted"
    | Error _ -> Expect.isOk result "should have processed"

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
        let! result = TFR.load meta.id traceID tlid
        functionResults <- result
        if functionResults = [] then do! Task.Delay 300

    Expect.equal (List.length functionResults) 1 "should have stored fn result"
    let shapeIsAsExpected =
      match functionResults with
      | [ (_, _, _, _, RT.DDate _) ] -> true
      | _ -> false
    Expect.isTrue shapeIsAsExpected "should have a date here"
  }


let testEventQueueSuccess =
  testTask "event queue success" {
    let! (meta : Canvas.Meta, tlid) = initializeCanvas "event-queue-success"
    do! enqueue meta
    let! result = QueueWorker.dequeueAndProcess ()
    do! checkSuccess meta tlid result
  }

let testEventQueueSuccessThree =
  testTask "event queue success three" {
    let! (meta : Canvas.Meta, tlid) = initializeCanvas "event-queue-success-three"
    do! enqueue meta
    do! enqueue meta
    do! enqueue meta
    let! result = QueueWorker.dequeueAndProcess ()
    do! checkSuccess meta tlid result
    let! result = QueueWorker.dequeueAndProcess ()
    do! checkSuccess meta tlid result
    let! result = QueueWorker.dequeueAndProcess ()
    do! checkSuccess meta tlid result
  }

let testEventQueueSuccessLockExpired =
  testTask "event queue success lock expired" {
    let! (meta : Canvas.Meta, tlid) =
      initializeCanvas "event-queue-success-lock-expired"
    do! enqueue meta

    // Lock it
    let earlier = Instant.now () + Duration.FromMinutes -6L
    do!
      Sql.query
        "UPDATE events_v2 SET locked_at = @newValue WHERE canvas_id = @canvasID"
      |> Sql.parameters [ "canvasID", Sql.uuid meta.id
                          "newValue", Sql.instantWithTimeZone earlier ]
      |> Sql.executeStatementAsync

    let! result = QueueWorker.dequeueAndProcess ()
    do! checkSuccess meta tlid result
  }

let testEventFailLocked =
  testTask "event queue fail locked" {
    let! (meta : Canvas.Meta, tlid) = initializeCanvas "event-queue-fail-locked"
    do! enqueue meta

    // Delay it
    do!
      Sql.query
        "UPDATE events_v2 SET locked_at = @newValue WHERE canvas_id = @canvasID"
      |> Sql.parameters [ "canvasID", Sql.uuid meta.id
                          "newValue", Sql.instantWithTimeZone (Instant.now ()) ]
      |> Sql.executeStatementAsync

    let! result = QueueWorker.dequeueAndProcess ()
    Expect.isError result "should fail"

    let! eventIDs = TI.loadEventIDs meta.id ("CRON", "test", "Daily")
    Expect.equal eventIDs [] "no events expected"
  }




// FSTODO: tests to write
// enqueue 3 items, process 3 items
// it's paused (check no retry)
// it's blocked (check no retry)
// it fails execution (check DB contents)
// it fails 3 times (check deleted)


let tests =
  testSequencedGroup
    "eventQueueV2"
    (testList
      "eventQueueV2"
      [ testEventQueueSuccess
        testEventQueueSuccessThree
        testEventQueueSuccessLockExpired
        testEventFailLocked ])
