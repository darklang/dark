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
module SR = LibBackend.QueueSchedulingRules

module TI = LibBackend.TraceInputs
module TFR = LibBackend.TraceFunctionResults


let p (code : string) = Parser.parsePTExpr code

// This doesn't actually test input, since it's a cron handler and not an actual event handler

let initializeCanvas (name : string) : Task<Canvas.Meta * tlid> =
  task {
    // set up handler
    let! meta = initializeTestCanvas (Randomized name)

    let h = testWorker "test" (p "let data = Date.now_v0 in 123")
    let oplists = [ handlerOp h ]

    do!
      Canvas.saveTLIDs
        meta
        [ (h.tlid, oplists, PT.Toplevel.TLHandler h, Canvas.NotDeleted) ]

    return meta, h.tlid
  }

let enqueue (meta : Canvas.Meta) : Task<unit> =
  let input = RT.DNull // crons take inputs, so this could be anything
  EQ.enqueue meta.id "WORKER" "test" "_" input

let checkSuccess
  (meta : Canvas.Meta)
  (tlid : tlid)
  (result : Result<EQ.T * EQ.Notification, string * EQ.Notification>)
  =
  task {
    match result with
    | Ok (event, _notification) ->
      // TODO: is there a way to count/test the number of messages in the queue?
      let! event = EQ.loadEvent event.canvasID event.id
      Expect.isNone event "should have been deleted"
    | Error _ -> Expect.isOk result "should have processed"

    // should have at least one trace
    let! traceIDs = TI.loadEventIDs meta.id ("WORKER", "test", "_")
    let traceID =
      traceIDs
      |> List.head
      |> Exception.unwrapOptionInternal "missing eventID" []
      |> Tuple2.first

    // Saving happens in the background so wait for it
    let mutable functionResults = []
    for _ in 1..10 do
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

let checkExecutedTraces (canvasID : CanvasID) (count : int) =
  task {
    let! traceIDs = TI.loadEventIDs canvasID ("WORKER", "test", "_")
    Expect.hasLength traceIDs count "wrong execution count"
  }

let checkSavedEvents (canvasID : CanvasID) (count : int) =
  task {
    let! queueIDs = EQ.loadEventIDs canvasID ("WORKER", "test", "_")
    Expect.hasLength queueIDs count "wrong stored event count"
  }

let rec dequeueAndProcess () : Task<Result<_, _>> =
  task {
    match! EQ.dequeue 1 with
    | [ notification ] -> return! QueueWorker.processNotification notification
    | [] ->
      do! Task.Delay 1000
      return! dequeueAndProcess ()
    | results ->
      return!
        Exception.raiseInternal "got more than 1" [ "count", List.length results ]
  }

let rec dequeueAndProcessMany (count : int) : Task<List<Result<_, _>>> =
  task {
    let! messages = EQ.dequeue count
    let! theseResults =
      Task.mapSequentially (fun n -> QueueWorker.processNotification n) messages

    // We aren't guaranteed to get `count` of them, so keep going
    let receivedCount = List.length messages
    let! moreResults =
      if receivedCount < count then
        dequeueAndProcessMany (count - receivedCount)
      else
        Task.FromResult []
    return theseResults @ moreResults
  }



let testSuccess =
  testTask "event queue success" {
    let! (meta : Canvas.Meta, tlid) = initializeCanvas "event-queue-success"
    do! enqueue meta
    let! result = dequeueAndProcess ()
    do! checkSuccess meta tlid result
    do! checkExecutedTraces meta.id 1
    do! checkSavedEvents meta.id 0
  }

let testSuccessThree =
  testTask "event queue success three" {
    let! (meta : Canvas.Meta, tlid) = initializeCanvas "event-queue-success-three"
    do! enqueue meta
    do! enqueue meta
    do! enqueue meta
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 3
    let! result = dequeueAndProcess ()
    do! checkSuccess meta tlid result
    do! checkExecutedTraces meta.id 1
    do! checkSavedEvents meta.id 2
    let! result = dequeueAndProcess ()
    do! checkSuccess meta tlid result
    do! checkExecutedTraces meta.id 2
    do! checkSavedEvents meta.id 1
    let! result = dequeueAndProcess ()
    do! checkSuccess meta tlid result
    do! checkExecutedTraces meta.id 3
    do! checkSavedEvents meta.id 0
  }

let testSuccessThreeAtOnce =
  testTask "event queue success three at once" {
    let! (meta : Canvas.Meta, tlid) =
      initializeCanvas "event-queue-success-three-at-once"
    do! enqueue meta
    do! enqueue meta
    do! enqueue meta
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 3
    let! results = dequeueAndProcessMany 3
    let results = List.toArray results
    do! checkExecutedTraces meta.id 3
    do! checkSavedEvents meta.id 0
    do! checkSuccess meta tlid results[0]
    do! checkSuccess meta tlid results[1]
    do! checkSuccess meta tlid results[2]
  }

let testSuccessLockExpired =
  testTask "success lock expired" {
    let! (meta : Canvas.Meta, tlid) = initializeCanvas "success-lock-expired"
    do! enqueue meta

    // Lock it
    let earlier = Instant.now () + Duration.FromMinutes -6L
    do!
      Sql.query
        "UPDATE events_v2 SET locked_at = @newValue WHERE canvas_id = @canvasID"
      |> Sql.parameters [ "canvasID", Sql.uuid meta.id
                          "newValue", Sql.instantWithTimeZone earlier ]
      |> Sql.executeStatementAsync

    let! result = dequeueAndProcess ()
    do! checkSuccess meta tlid result
    do! checkExecutedTraces meta.id 1
    do! checkSavedEvents meta.id 0
  }

let testFailLocked =
  testTask "fail locked" {
    let! (meta : Canvas.Meta, _tlid) = initializeCanvas "fail-locked"
    do! enqueue meta

    // Delay it
    do!
      Sql.query
        "UPDATE events_v2 SET locked_at = @newValue WHERE canvas_id = @canvasID"
      |> Sql.parameters [ "canvasID", Sql.uuid meta.id
                          "newValue", Sql.instantWithTimeZone (Instant.now ()) ]
      |> Sql.executeStatementAsync

    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"

    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1
  }

let testSuccessBlockAndUnblock =
  testTask "block and unblock" {
    let! (meta : Canvas.Meta, _tlid) = initializeCanvas "block-and-unblock"
    let! _id = enqueue meta

    // Block it
    do! EQ.blockWorker meta.id "test"

    // Check blocked
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1

    // Unblock it
    do! EQ.unblockWorker meta.id "test"

    // Check unblocked
    let! result = dequeueAndProcess ()
    Expect.isOk result "should success"
    do! checkExecutedTraces meta.id 1
    do! checkSavedEvents meta.id 0
  }

let testSuccessPauseAndUnpause =
  testTask "pause and unpause" {
    let! (meta : Canvas.Meta, _tlid) = initializeCanvas "pause-and-unpause"
    do! enqueue meta

    // Pause it
    do! EQ.pauseWorker meta.id "test"

    // Check paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1

    // Unpause it
    do! EQ.unpauseWorker meta.id "test"

    // Check unpaused
    let! result = dequeueAndProcess ()
    Expect.isOk result "should success"
    do! checkExecutedTraces meta.id 1
    do! checkSavedEvents meta.id 0
  }

let testFailPauseBlockAndUnpause =
  testTask "pause block and unpause" {
    let! (meta : Canvas.Meta, _tlid) = initializeCanvas "pause-block-and-unpause"
    do! enqueue meta

    // Pause it
    do! EQ.pauseWorker meta.id "test"

    // Check paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1

    // Block and unpause it
    do! EQ.blockWorker meta.id "test"
    do! EQ.unpauseWorker meta.id "test"

    // Check still paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1

  }

let testFailPauseBlockAndUnblock =
  testTask "pause block and unblock" {
    let! (meta : Canvas.Meta, _tlid) = initializeCanvas "pause-block-and-unblock"
    do! enqueue meta

    // Pause it
    do! EQ.pauseWorker meta.id "test"

    // Check paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1

    // Block and unblock it
    do! EQ.blockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"

    // Check still paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1
  }

let testFailBlockPauseAndUnpause =
  testTask "block pause and unpause" {
    let! (meta : Canvas.Meta, _tlid) = initializeCanvas "block-pause-and-unpause"
    do! enqueue meta

    // Block it
    do! EQ.blockWorker meta.id "test"

    // Check blocked
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1

    // Pause and unpause it
    do! EQ.pauseWorker meta.id "test"
    do! EQ.unpauseWorker meta.id "test"

    // Check still blocked
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1
  }

let testFailBlockPauseAndUnblock =
  testTask "block pause and unblock" {
    let! (meta : Canvas.Meta, _tlid) = initializeCanvas "block-pause-and-unblock"
    do! enqueue meta

    // Block it
    do! EQ.blockWorker meta.id "test"

    // Check blocked
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1

    // Pause and unblock  it
    do! EQ.pauseWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"

    // Check still paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 0
    do! checkSavedEvents meta.id 1
  }

let testUnpauseMulitpleTimesInSequence =
  testTask "unpause multiple times in sequence" {
    let! (meta : Canvas.Meta, _tlid) =
      initializeCanvas "unpaise-multiple-times-in-secquence"
    do! enqueue meta

    // Block it
    do! EQ.blockWorker meta.id "test"

    // Pause and unblock  it
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"

    let! result = dequeueAndProcess ()
    Expect.isOk result "should succeed"
    do! checkExecutedTraces meta.id 1
    do! checkSavedEvents meta.id 0
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces meta.id 1
    do! checkSavedEvents meta.id 0
  }

let testUnpauseMultipleTimesInParallel =
  testTask "unpause multiple times in parallel" {
    let! (meta : Canvas.Meta, _tlid) =
      initializeCanvas "unpause-multiple-times-in-parallel"
    do! enqueue meta

    // Block it
    do! EQ.blockWorker meta.id "test"

    // Pause and unblock  it
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"
    do! EQ.unblockWorker meta.id "test"

    let resultTasks =
      [ dequeueAndProcess ()
        dequeueAndProcess ()
        dequeueAndProcess ()
        dequeueAndProcess ()
        dequeueAndProcess ()
        dequeueAndProcess ()
        dequeueAndProcess ()
        dequeueAndProcess ()
        dequeueAndProcess ()
        dequeueAndProcess () ]
    let! results = Task.flatten resultTasks
    let (success, failure) = List.partition Result.isOk results

    Expect.hasLength success 1 "one success only succeed"
    Expect.hasLength failure 9 "nine delayed or deleted"
    do! checkExecutedTraces meta.id 1
    do! checkSavedEvents meta.id 0
  }


let testCount =
  testTask "count is right" {
    let! (meta : Canvas.Meta, tlid) = initializeCanvas "count-is-correct"
    do! enqueue meta
    do! enqueue meta
    do! enqueue meta
    do! enqueue meta
    do! enqueue meta

    let! count = LibBackend.Stats.workerStats meta.id tlid
    Expect.equal count 5 "count should be 5"
    do! checkSavedEvents meta.id 5
  }

let tests =
  testSequencedGroup
    "eventQueueV2"
    (testList
      "eventQueueV2"
      [ testSuccess
        testSuccessThree
        testSuccessThreeAtOnce
        testSuccessLockExpired
        testFailLocked
        testSuccessBlockAndUnblock
        testSuccessPauseAndUnpause
        testFailPauseBlockAndUnpause
        testFailPauseBlockAndUnblock
        testFailBlockPauseAndUnpause
        testFailBlockPauseAndUnblock
        testUnpauseMulitpleTimesInSequence
        testUnpauseMultipleTimesInParallel
        testCount ])
