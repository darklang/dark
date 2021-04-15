module Tests.EventQueue

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes
module EQ = LibBackend.EventQueue
module Cron = LibBackend.Cron
module Canvas = LibBackend.Canvas
module QW = LibBackend.QueueWorker

module TI = LibBackend.TraceInputs
module TFR = LibBackend.TraceFunctionResults
let p (code : string) = FSharpToExpr.parsePTExpr code

// This doesn't actually test input, since it's a cron handler and not an actual event handler

let testEventQueueRoundtrip =
  testTask "event queue roundtrip" {
    let name = "test-event_queue"
    do! clearCanvasData (CanvasName.create name)
    let! (meta : Canvas.Meta) = testCanvasInfo name
    let executionID = gid ()

    let h = testCron "test" "Daily" (p "let data = Date.now_v0 in 123")
    let oplists = [ hop h ]

    do!
      Canvas.saveTLIDs meta [ (h.tlid, oplists, PT.TLHandler h, Canvas.NotDeleted) ]

    do! EQ.enqueue meta.id meta.owner "CRON" "test" "Daily" RT.DNull // I don't believe crons take inputs?

    do! EQ.testingScheduleAll ()
    let! result = QW.run executionID

    match result with
    | Ok (Some resultDval) ->
        // should have at least one trace
        let! eventIDs = TI.loadEventIDs meta.id ("CRON", "test", "Daily")
        let traceID = eventIDs |> List.head |> Option.unwrapUnsafe |> Tuple2.first

        let! functionResults = TFR.load meta.id traceID h.tlid

        Expect.equal (List.length functionResults) 1 "should have stored fn result"
        Expect.equal (RT.DInt 123I) resultDval "Round tripped value"
    | Ok None -> failwith "Failed: expected Some, got None"
    | Error e -> failwith $"Failed: got error: {e}"
  }


let testEventQueueIsFifo =
  testTask "event queue is fifo" {
    let name = "fifo"
    do! clearCanvasData (CanvasName.create name)
    let! meta = testCanvasInfo name
    let apple = testWorker "apple" (p "event")
    let banana = testWorker "banana" (p "event")

    do!
      ([ apple; banana ]
       |> List.map (fun h -> (h.tlid, [ hop h ], PT.TLHandler h, Canvas.NotDeleted))
       |> Canvas.saveTLIDs meta)

    let enqueue (name : string) (i : bigint) =
      EQ.enqueue meta.id meta.owner "WORKER" name "_" (RT.DInt i)

    do! enqueue "apple" 1I
    do! enqueue "apple" 2I
    do! enqueue "banana" 3I
    do! enqueue "apple" 4I
    do! EQ.testingScheduleAll ()

    let checkDequeue span (i : bigint) exname : Task<unit> =
      task {
        let! evt = EQ.dequeue span
        let evt = Option.unwrapUnsafe evt

        Expect.equal exname evt.name $"dequeue {i} is handler {exname}"
        Expect.equal evt.value (RT.DInt i) $"dequeue {i} has value {i}"
        do! EQ.finish span evt
        return ()
      }

    use span = LibService.Telemetry.Span.root "test"

    do!
      EQ.withTransaction
        (fun () ->
          task {
            do! checkDequeue span 1I "apple"
            do! checkDequeue span 2I "apple"
            do! checkDequeue span 3I "banana"
            do! checkDequeue span 4I "apple"
            return Ok(Some RT.DNull)
          })
  }



// let testCronFetchActiveCrons =
//   test "fetch active crons doesn't raise" {
//     Telemetry.with_root
//       "test"
//       (fun span ->
//         Serialize.fetch_active_crons span
//         (* Just checking that this doesn't raise *)
//         |> ignore)
//   }
//
//
// let testCronSanity =
//   testTask "cron sanity" {
//     let! c = testCanvasInfo "cronSanity"
//
//     do! clearCanvasData c.name
//
//     let h = daily_cron (binop "+" (int 5) (int 3))
//     let c = ops2c_exn "test-cron_works" [ hop h ]
//
//     let cron_schedule_data : LibBackend.Cron.cron_schedule_data =
//       { canvas_id = c.id
//         owner = Uuidm.nil
//         host = !c.host
//         tlid = h.tlid |> Int63.to_string
//         name =
//           (match h.spec.name with
//            | Filled (_, s) -> s
//            | _ -> "CAN'T HAPPEN")
//         modifier =
//           (match h.spec.modifier with
//            | Filled (_, s) -> s
//            | _ -> "CAN'T HAPPEN") }
//
//     let ({ should_execute = should_execute
//            scheduled_run_at = scheduled_run_at
//            interval = interval } : LibBackend.Cron.execution_check_type) =
//       Telemetry.with_root
//         "test"
//         (fun span -> Cron.execution_check span cron_schedule_data)
//
//     AT.check AT.bool "should_execute should be true" should_execute true
//     ()
//   }
//
//
// let testCronJustRan =
//   testTask "test cron just ran" {
//     let! c = testCanvasInfo "cronJustRan"
//     clearCanvasData ()
//
//     let h = daily_cron (binop "+" (int 5) (int 3))
//     let c = ops2c_exn "test-cron_works" [ hop h ]
//
//     let cronScheduleData : LibBackend.Cron.cron_schedule_data =
//       { canvas_id = c.id
//         owner = Uuidm.nil
//         host = c.host
//         tlid = h.tlid |> Int63.to_string
//         name =
//           (match h.spec.name with
//            | Filled (_, s) -> s
//            | _ -> "CAN'T HAPPEN")
//         modifier =
//           (match h.spec.modifier with
//            | Filled (_, s) -> s
//            | _ -> "CAN'T HAPPEN") }
//
//     Cron.record_execution cron_schedule_data
//
//     let ({ should_execute = should_execute
//            scheduled_run_at = scheduled_run_at
//            interval = interval } : LibBackend.Cron.execution_check_type) =
//       Telemetry.with_root
//         "test"
//         (fun span -> Cron.execution_check span cron_schedule_data)
//
//     AT.check AT.bool "should_execute should be false" should_execute false
//     ()
//   }
//
//
//
// let testGetWorkerSchedulesForCanvas =
//   test "worker schedules for canvas" {
//     clear_test_data ()
//     let apple = worker "apple" (int 1)
//     let banana = worker "banana" (int 1)
//     let cherry = worker "cherry" (int 1)
//
//     let c =
//       ops2c_exn "test-worker-scheduling-rules" [ hop apple; hop banana; hop cherry ]
//
//     Canvas.save_all !c
//     E.pauseWorker c.id "apple"
//     E.pauseWorker c.id "banana"
//     E.blockWorker c.id "banana"
//     let res = get_worker_schedules_for_canvas !c.id
//
//     let check name value =
//       let actual =
//         Core_kernel.Map.find res name |> Option.value_exn |> state_to_string
//
//       let expected = state_to_string value in
//       Expect.equal actual expected ($"{name} is {expected}")
//
//     check "apple" Paused
//     check "banana" Blocked
//     check "cherry" Running
//   }

let tests = testList "eventQueue" [ testEventQueueRoundtrip; testEventQueueIsFifo ]

// testCronJustRan
// testCronFetchActiveCrons
// testCronSanity
// testGetWorkerSchedulesForCanvas
