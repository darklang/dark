module Tests.Queue

open System.Threading.Tasks
open FSharp.Control.Tasks

open NodaTime

open Expecto

open Prelude

open Npgsql.FSharp
open Npgsql
open LibCloud.Db

open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module EQ = LibCloud.Queue
module Canvas = LibCloud.Canvas
module Serialize = LibCloud.Serialize
module SR = LibCloud.QueueSchedulingRules

module TCS = LibCloud.TraceCloudStorage

let pmPT = LibCloud.PackageManager.pt


// // This doesn't actually test input, since it's a cron handler and not an actual event handler
// let initializeCanvas (name : string) : Task<CanvasID * tlid> =
//   task {
//     // set up handler
//     let! canvasID = initializeTestCanvas name
//     let! e = parsePTExpr "let data = PACKAGE.Darklang.Stdlib.DateTime.now ()\n 123"
//     let h = testWorker "test" e

//     do! Canvas.saveTLIDs canvasID [ (PT.Toplevel.TLHandler h, Serialize.NotDeleted) ]

//     return canvasID, h.tlid
//   }


// let enqueueAtTime (canvasID : CanvasID) (time : Instant) : Task<unit> =
//   // crons take inputs, so this could be anything
//   EQ.enqueueAtTime canvasID "WORKER" "test" "_" time RT.DUnit

// let enqueueNow (canvasID : CanvasID) : Task<unit> =
//   enqueueAtTime canvasID (Instant.now ())


// let checkExecutedTraces (canvasID : CanvasID) (count : int) : Task<unit> =
//   task {
//     // Saving happens in the background so wait for it
//     let mutable traceIDs = []
//     for _ in 1..10 do
//       if List.length traceIDs <> count then
//         let! result = TCS.Test.listAllTraceIDs canvasID
//         traceIDs <- result
//         if List.length result <> count then do! Task.Delay 500
//     Expect.hasLength traceIDs count "wrong execution count"
//   }

// let rec waitForSuccess
//   (canvasID : CanvasID)
//   (tlid : tlid)
//   (count : int)
//   : Task<unit> =
//   let rec getTrace
//     (traceID)
//     (remainingAttempts : int)
//     : Task<LibExecution.AnalysisTypes.Trace> =
//     task {
//       if remainingAttempts <= 0 then
//         return Exception.raiseInternal "no trace found" []
//       else
//         try
//           // This can fail if the background task uploading the trace data hasn't
//           // finished yet
//           return! TCS.getTraceData canvasID tlid traceID
//         with
//         | (:? Exception.InternalException) as e -> return Exception.reraise e
//         | _ ->
//           do! Task.Delay 500
//           return! getTrace traceID (remainingAttempts - 1)
//     }

//   task {
//     let! eventIDs = EQ.loadEventIDs canvasID ("WORKER", "test", "_")
//     let! traceIDs = TCS.Test.listAllTraceIDs canvasID
//     if List.length eventIDs <> 0 || List.length traceIDs <> count then
//       do! Task.Delay 50
//       return! waitForSuccess canvasID tlid count
//     else
//       do!
//         traceIDs
//         |> Task.iterSequentially (fun traceID ->
//           task {
//             let! trace = getTrace traceID 2
//             let shapeIsAsExpected =
//               match (Tuple2.second trace).functionResults with
//               | [ (_, _, _, _, RT.DDateTime _) ] -> true
//               | _ -> false
//             return Expect.isTrue shapeIsAsExpected "should have a date here"
//           })
//   }


// let checkSavedEvents (canvasID : CanvasID) (count : int) =
//   task {
//     let! queueIDs = EQ.loadEventIDs canvasID ("WORKER", "test", "_")
//     Expect.hasLength queueIDs count "wrong stored event count"
//   }

// let mutable queueLastEmptyAt = Instant.MinValue


// let init () : unit =
//   let timeout = System.TimeSpan.FromMilliseconds 10
//   let processContinuouslyInBackground () : unit =
//     task {
//       while true do
//         match! EQ.dequeue timeout 1 with
//         | [ notification ] ->
//           let! _ = QueueWorker.processNotification notification
//           return ()
//         | [] ->
//           queueLastEmptyAt <- Instant.now ()
//           do! Task.Delay 100
//         | results ->
//           return!
//             Exception.raiseInternal
//               "got more than 1"
//               [ "count", List.length results ]

//         return ()
//     }
//     |> fun x -> x.Result
//   let thread = System.Threading.Thread processContinuouslyInBackground
//   thread.IsBackground <- true
//   thread.Name <- "Queue.Tests worker"
//   thread.Start()

// /// Tests that need to check that something isn't going to be run can wait until the
// /// queue is empty (locked/blocked items will be checked and then dropped) to prove
// /// that something isn't going to be run.
// let waitUntilQueueEmpty () : Task<unit> =
//   task {
//     let initialTime = Instant.now ()
//     while initialTime > queueLastEmptyAt do
//       do! Task.Delay 10
//   }


// let testSuccess =
//   testTask "event queue success" {
//     let! (canvasID : CanvasID, tlid) = initializeCanvas "event-queue-success"
//     do! enqueueNow canvasID
//     do! waitForSuccess canvasID tlid 1
//     do! checkExecutedTraces canvasID 1
//     do! checkSavedEvents canvasID 0
//   }

// let testSuccessThree =
//   testTask "event queue success three" {
//     let! (canvasID : CanvasID, tlid) = initializeCanvas "event-queue-success-three"
//     do! enqueueNow canvasID
//     do! enqueueNow canvasID
//     do! enqueueNow canvasID
//     do! waitForSuccess canvasID tlid 3
//     do! checkExecutedTraces canvasID 3
//     do! checkSavedEvents canvasID 0
//   }

// let testSuccessLockExpired =
//   testTask "success lock expired" {
//     let! (canvasID : CanvasID, tlid) = initializeCanvas "success-lock-expired"

//     // Create the event, but don't have it run yet
//     do! enqueueAtTime canvasID (Instant.now () + Duration.FromSeconds 3L)

//     // Lock it
//     let earlier = Instant.now () + Duration.FromMinutes -6L
//     do!
//       Sql.query
//         "UPDATE queue_events_v0 SET locked_at = @newValue WHERE canvas_id = @canvasID"
//       |> Sql.parameters
//         [ "canvasID", Sql.uuid canvasID
//           "newValue", Sql.instantWithTimeZone earlier ]
//       |> Sql.executeStatementAsync

//     // Wait for it to run
//     do! waitForSuccess canvasID tlid 1
//     do! checkExecutedTraces canvasID 1
//     do! checkSavedEvents canvasID 0
//   }

// let testFailLocked =
//   testTask "fail locked" {
//     let! (canvasID : CanvasID, _tlid) = initializeCanvas "fail-locked"

//     // Create the event, but don't have it run yet
//     do! enqueueAtTime canvasID (Instant.now () + Duration.FromSeconds 3L)

//     // Lock it
//     do!
//       Sql.query
//         "UPDATE queue_events_v0 SET locked_at = @newValue WHERE canvas_id = @canvasID"
//       |> Sql.parameters
//         [ "canvasID", Sql.uuid canvasID
//           "newValue", Sql.instantWithTimeZone (Instant.now ()) ]
//       |> Sql.executeStatementAsync

//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1
//   }

// let testSuccessBlockAndUnblock =
//   testTask "block and unblock" {
//     let! (canvasID : CanvasID, tlid) = initializeCanvas "block-and-unblock"

//     // Block it
//     do! EQ.blockWorker canvasID "test"

//     do! enqueueNow canvasID

//     // Check blocked
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1

//     // Unblock it
//     do! EQ.unblockWorker canvasID "test"

//     // Check unblocked
//     do! waitForSuccess canvasID tlid 1
//     do! checkExecutedTraces canvasID 1
//     do! checkSavedEvents canvasID 0
//   }

// let testSuccessPauseAndUnpause =
//   testTask "pause and unpause" {
//     let! (canvasID : CanvasID, tlid) = initializeCanvas "pause-and-unpause"
//     // Pause it
//     do! EQ.pauseWorker canvasID "test"

//     // Enqueue
//     do! enqueueNow canvasID

//     // Check paused
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1

//     // Unpause it
//     do! EQ.unpauseWorker canvasID "test"

//     // Check unpaused
//     do! waitForSuccess canvasID tlid 1
//     do! checkExecutedTraces canvasID 1
//     do! checkSavedEvents canvasID 0
//   }

// let testFailPauseBlockAndUnpause =
//   testTask "pause block and unpause" {
//     let! (canvasID : CanvasID, _tlid) = initializeCanvas "pause-block-and-unpause"

//     // Pause it
//     do! EQ.pauseWorker canvasID "test"

//     // Enqueue
//     do! enqueueNow canvasID

//     // Check paused
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1

//     // Block and unpause it
//     do! EQ.blockWorker canvasID "test"
//     do! EQ.unpauseWorker canvasID "test"

//     // Check still paused
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1
//   }

// let testFailPauseBlockAndUnblock =
//   testTask "pause block and unblock" {
//     let! (canvasID : CanvasID, _tlid) = initializeCanvas "pause-block-and-unblock"

//     // Pause it
//     do! EQ.pauseWorker canvasID "test"

//     // Enqueue
//     do! enqueueNow canvasID

//     // Check paused
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1

//     // Block and unblock it
//     do! EQ.blockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"

//     // Check still paused
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1
//   }

// let testFailBlockPauseAndUnpause =
//   testTask "block pause and unpause" {
//     let! (canvasID : CanvasID, _tlid) = initializeCanvas "block-pause-and-unpause"

//     // Block it
//     do! EQ.blockWorker canvasID "test"

//     do! enqueueNow canvasID

//     // Check blocked
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1

//     // Pause and unpause it
//     do! EQ.pauseWorker canvasID "test"
//     do! EQ.unpauseWorker canvasID "test"

//     // Check still blocked
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1
//   }

// let testFailBlockPauseAndUnblock =
//   testTask "block pause and unblock" {
//     let! (canvasID : CanvasID, _tlid) = initializeCanvas "block-pause-and-unblock"

//     // Block it
//     do! EQ.blockWorker canvasID "test"

//     // Enqueue
//     do! enqueueNow canvasID

//     // Check blocked
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1

//     // Pause and unblock  it
//     do! EQ.pauseWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"

//     // Check still paused
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 0
//     do! checkSavedEvents canvasID 1
//   }

// let testUnpauseMulitpleTimesInSequence =
//   testTask "unpause multiple times in sequence" {
//     let! (canvasID : CanvasID, tlid) =
//       initializeCanvas "unpause-multiple-times-in-secquence"

//     // Block it
//     do! EQ.blockWorker canvasID "test"

//     // Enqueue
//     do! enqueueNow canvasID

//     // Pause and unblock  it
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"

//     do! waitForSuccess canvasID tlid 1
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 1
//     do! checkSavedEvents canvasID 0
//   }

// let testUnpauseMultipleTimesInParallel =
//   testTask "unpause multiple times in parallel" {
//     let! (canvasID : CanvasID, tlid) =
//       initializeCanvas "unpause-multiple-times-in-parallel"

//     // Block it
//     do! EQ.blockWorker canvasID "test"

//     // Enqueue
//     do! enqueueNow canvasID

//     // Pause and unblock  it
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"
//     do! EQ.unblockWorker canvasID "test"

//     do! waitForSuccess canvasID tlid 1
//     do! waitUntilQueueEmpty ()
//     do! checkExecutedTraces canvasID 1
//     do! checkSavedEvents canvasID 0
//   }


// let testCount =
//   testTask "count is right" {
//     let! (canvasID : CanvasID, tlid) = initializeCanvas "count-is-correct"
//     do! EQ.blockWorker canvasID "test"
//     do! enqueueNow canvasID
//     do! enqueueNow canvasID
//     do! enqueueNow canvasID
//     do! enqueueNow canvasID
//     do! enqueueNow canvasID

//     let! count = LibCloud.Stats.workerStats canvasID tlid
//     Expect.equal count 5 "count should be 5"

//     do! EQ.unblockWorker canvasID "test"
//     do! checkSavedEvents canvasID 5
//   }

let tests =
  //init ()
  testSequencedGroup
    "Queue"
    (testList
      "Queue"
      [
      // testSuccess
      // testSuccessThree
      // testSuccessLockExpired
      // testFailLocked
      // testSuccessBlockAndUnblock
      // testSuccessPauseAndUnpause
      // testFailPauseBlockAndUnpause
      // testFailPauseBlockAndUnblock
      // testFailBlockPauseAndUnpause
      // testFailBlockPauseAndUnblock
      // testUnpauseMulitpleTimesInSequence
      // testUnpauseMultipleTimesInParallel
      // testCount
      ])
