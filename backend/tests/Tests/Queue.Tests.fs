module Tests.Queue

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
module EQ = LibBackend.Queue
module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module SR = LibBackend.QueueSchedulingRules

module TCS = LibBackend.TraceCloudStorage

let p (code : string) : PT.Expr =
  Parser.Parser.parsePTExpr builtinResolver "Queue.Tests.fs" code

// This doesn't actually test input, since it's a cron handler and not an actual event handler

let initializeCanvas (name : string) : Task<CanvasID * tlid> =
  task {
    // set up handler
    let! canvasID = initializeTestCanvas name

    let h = testWorker "test" (p "let data = DateTime.now_v0 () in 123")

    do! Canvas.saveTLIDs canvasID [ (PT.Toplevel.TLHandler h, Serialize.NotDeleted) ]

    return canvasID, h.tlid
  }

let enqueue (canvasID : CanvasID) : Task<unit> =
  let input = RT.DUnit // crons take inputs, so this could be anything
  EQ.enqueue canvasID "WORKER" "test" "_" input


let checkExecutedTraces (canvasID : CanvasID) (count : int) : Task<unit> =
  task {
    // Saving happens in the background so wait for it
    let mutable traceIDs = []
    for _ in 1..10 do
      if List.length traceIDs <> count then
        let! result = TCS.Test.listAllTraceIDs canvasID
        traceIDs <- result
        if List.length result <> count then do! Task.Delay 500
    Expect.hasLength traceIDs count "wrong execution count"
  }

let checkSuccess
  (canvasID : CanvasID)
  (tlid : tlid)
  (result : Result<EQ.T * EQ.Notification, string * EQ.Notification>)
  =
  task {
    match result with
    | Ok(event, _notification) ->
      // TODO: is there a way to count/test the number of messages in the queue?
      let! event = EQ.loadEvent event.canvasID event.id
      Expect.isNone event "should have been deleted"
    | Error _ -> Expect.isOk result "should have processed"

    let! traceIDs = TCS.Test.listAllTraceIDs canvasID

    let! trace =
      traceIDs
      |> List.head
      |> Exception.unwrapOptionInternal "expectedID" []
      |> TCS.getTraceData canvasID tlid

    let shapeIsAsExpected =
      match (Tuple2.second trace).functionResults with
      | [ (_, _, _, _, RT.DDateTime _) ] -> true
      | _ -> false
    Expect.isTrue shapeIsAsExpected "should have a date here"
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
    let! (canvasID : CanvasID, tlid) = initializeCanvas "event-queue-success"
    do! enqueue canvasID
    let! result = dequeueAndProcess ()
    do! checkSuccess canvasID tlid result
    do! checkExecutedTraces canvasID 1
    do! checkSavedEvents canvasID 0
  }

let testSuccessThree =
  testTask "event queue success three" {
    let! (canvasID : CanvasID, tlid) = initializeCanvas "event-queue-success-three"
    do! enqueue canvasID
    do! enqueue canvasID
    do! enqueue canvasID
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 3
    let! result = dequeueAndProcess ()
    do! checkSuccess canvasID tlid result
    do! checkExecutedTraces canvasID 1
    do! checkSavedEvents canvasID 2
    let! result = dequeueAndProcess ()
    do! checkSuccess canvasID tlid result
    do! checkExecutedTraces canvasID 2
    do! checkSavedEvents canvasID 1
    let! result = dequeueAndProcess ()
    do! checkSuccess canvasID tlid result
    do! checkExecutedTraces canvasID 3
    do! checkSavedEvents canvasID 0
  }

let testSuccessThreeAtOnce =
  testTask "event queue success three at once" {
    let! (canvasID : CanvasID, tlid) =
      initializeCanvas "event-queue-success-three-at-once"
    do! enqueue canvasID
    do! enqueue canvasID
    do! enqueue canvasID
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 3
    let! results = dequeueAndProcessMany 3
    let results = List.toArray results
    do! checkExecutedTraces canvasID 3
    do! checkSavedEvents canvasID 0
    do! checkSuccess canvasID tlid results[0]
    do! checkSuccess canvasID tlid results[1]
    do! checkSuccess canvasID tlid results[2]
  }

let testSuccessLockExpired =
  testTask "success lock expired" {
    let! (canvasID : CanvasID, tlid) = initializeCanvas "success-lock-expired"
    do! enqueue canvasID

    // Lock it
    let earlier = Instant.now () + Duration.FromMinutes -6L
    do!
      Sql.query
        "UPDATE queue_events_v0 SET locked_at = @newValue WHERE canvas_id = @canvasID"
      |> Sql.parameters
        [ "canvasID", Sql.uuid canvasID
          "newValue", Sql.instantWithTimeZone earlier ]
      |> Sql.executeStatementAsync

    let! result = dequeueAndProcess ()
    do! checkSuccess canvasID tlid result
    do! checkExecutedTraces canvasID 1
    do! checkSavedEvents canvasID 0
  }

let testFailLocked =
  testTask "fail locked" {
    let! (canvasID : CanvasID, _tlid) = initializeCanvas "fail-locked"
    do! enqueue canvasID

    // Delay it
    do!
      Sql.query
        "UPDATE queue_events_v0 SET locked_at = @newValue WHERE canvas_id = @canvasID"
      |> Sql.parameters
        [ "canvasID", Sql.uuid canvasID
          "newValue", Sql.instantWithTimeZone (Instant.now ()) ]
      |> Sql.executeStatementAsync

    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"

    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1
  }

let testSuccessBlockAndUnblock =
  testTask "block and unblock" {
    let! (canvasID : CanvasID, _tlid) = initializeCanvas "block-and-unblock"
    let! _id = enqueue canvasID

    // Block it
    do! EQ.blockWorker canvasID "test"

    // Check blocked
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1

    // Unblock it
    do! EQ.unblockWorker canvasID "test"

    // Check unblocked
    let! result = dequeueAndProcess ()
    Expect.isOk result "should success"
    do! checkExecutedTraces canvasID 1
    do! checkSavedEvents canvasID 0
  }

let testSuccessPauseAndUnpause =
  testTask "pause and unpause" {
    let! (canvasID : CanvasID, _tlid) = initializeCanvas "pause-and-unpause"
    do! enqueue canvasID

    // Pause it
    do! EQ.pauseWorker canvasID "test"

    // Check paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1

    // Unpause it
    do! EQ.unpauseWorker canvasID "test"

    // Check unpaused
    let! result = dequeueAndProcess ()
    Expect.isOk result "should success"
    do! checkExecutedTraces canvasID 1
    do! checkSavedEvents canvasID 0
  }

let testFailPauseBlockAndUnpause =
  testTask "pause block and unpause" {
    let! (canvasID : CanvasID, _tlid) = initializeCanvas "pause-block-and-unpause"
    do! enqueue canvasID

    // Pause it
    do! EQ.pauseWorker canvasID "test"

    // Check paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1

    // Block and unpause it
    do! EQ.blockWorker canvasID "test"
    do! EQ.unpauseWorker canvasID "test"

    // Check still paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1

  }

let testFailPauseBlockAndUnblock =
  testTask "pause block and unblock" {
    let! (canvasID : CanvasID, _tlid) = initializeCanvas "pause-block-and-unblock"
    do! enqueue canvasID

    // Pause it
    do! EQ.pauseWorker canvasID "test"

    // Check paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1

    // Block and unblock it
    do! EQ.blockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"

    // Check still paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1
  }

let testFailBlockPauseAndUnpause =
  testTask "block pause and unpause" {
    let! (canvasID : CanvasID, _tlid) = initializeCanvas "block-pause-and-unpause"
    do! enqueue canvasID

    // Block it
    do! EQ.blockWorker canvasID "test"

    // Check blocked
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1

    // Pause and unpause it
    do! EQ.pauseWorker canvasID "test"
    do! EQ.unpauseWorker canvasID "test"

    // Check still blocked
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1
  }

let testFailBlockPauseAndUnblock =
  testTask "block pause and unblock" {
    let! (canvasID : CanvasID, _tlid) = initializeCanvas "block-pause-and-unblock"
    do! enqueue canvasID

    // Block it
    do! EQ.blockWorker canvasID "test"

    // Check blocked
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1

    // Pause and unblock  it
    do! EQ.pauseWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"

    // Check still paused
    let! result = dequeueAndProcess ()
    Expect.isError result "should fail"
    do! checkExecutedTraces canvasID 0
    do! checkSavedEvents canvasID 1
  }

let testUnpauseMulitpleTimesInSequence =
  testTask "unpause multiple times in sequence" {
    let! (canvasID : CanvasID, _tlid) =
      initializeCanvas "unpaise-multiple-times-in-secquence"
    do! enqueue canvasID

    // Block it
    do! EQ.blockWorker canvasID "test"

    // Pause and unblock  it
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"

    let! result = dequeueAndProcess ()
    Expect.isOk result "should succeed"
    do! checkExecutedTraces canvasID 1
    do! checkSavedEvents canvasID 0
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
    do! checkExecutedTraces canvasID 1
    do! checkSavedEvents canvasID 0
  }

let testUnpauseMultipleTimesInParallel =
  testTask "unpause multiple times in parallel" {
    let! (canvasID : CanvasID, _tlid) =
      initializeCanvas "unpause-multiple-times-in-parallel"
    do! enqueue canvasID

    // Block it
    do! EQ.blockWorker canvasID "test"

    // Pause and unblock  it
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"
    do! EQ.unblockWorker canvasID "test"

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
    do! checkExecutedTraces canvasID 1
    do! checkSavedEvents canvasID 0
  }


let testCount =
  testTask "count is right" {
    let! (canvasID : CanvasID, tlid) = initializeCanvas "count-is-correct"
    do! enqueue canvasID
    do! enqueue canvasID
    do! enqueue canvasID
    do! enqueue canvasID
    do! enqueue canvasID

    let! count = LibBackend.Stats.workerStats canvasID tlid
    Expect.equal count 5 "count should be 5"
    do! checkSavedEvents canvasID 5
  }

let tests =
  testSequencedGroup
    "Queue"
    (testList
      "Queue"
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
