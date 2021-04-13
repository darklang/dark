module Tests.EventQueue

open Expecto
open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes
module E = LibBackend.EventQueue
module Cron = LibBackend.Cron
module Canvas = LibBackend.Canvas
// module QW = LibBackend.QueueWorker

module TI = LibBackend.TraceInputs
module TFR = LibBackend.TraceFunctionResults

// This doesn't actually test input, since it's a cron handler and not an actual event handler

// let testEventQueueRoundtrip =
//   testTask "event queue roundtrip" {
//     let! (c : Canvas.Meta) = testCanvasInfo "test-queueRoundTrip"
//     do! clearCanvasData c.name
//
//     let h = daily_cron (let' "date" (fn "Date::now" []) (int 123))
//     let c = ops2c_exn "test-event_queue" [ hop h ]
//     Canvas.saveAll c
//
//     E.enqueue
//       "CRON"
//       "test"
//       "Daily"
//       RT.DNull // I don't believe crons take inputs?
//       c.owner
//       c.id
//
//     E.scheduleAll ()
//     let result = QW.run execution_id
//
//     match result with
//     | Ok (Some resultDval) ->
//         (* should have at least one trace *)
//         let traceID =
//           TI.loadEventIDs c.id ("CRON", "test", "Daily")
//           |> List.hd_exn
//           |> Tuple2.first
//
//         Expect.equal
//           (TFR.load c.id traceID h.tlid |> List.length)
//           1
//           "should have stored fn result"
//
//         Expect.equal (RT.DInt 123I) resultDval "Round tripped value"
//     | Ok None -> failwith "Failed: expected Some, got None"
//     | Error e -> failwith $"Failed: got error: {e}"
//   }
//
//
// let testEventQueueIsFifo =
//   testTask "event queue is fifo" {
//     let c = testCanvasInfo "fifo"
//     do! clearCanvasData c.name
//     let apple = worker "apple" (var "event")
//     let banana = worker "banana" (var "event")
//     let c = ops2c_exn "test-worker-fifo" [ hop apple; hop banana ]
//     Canvas.saveAll !c
//
//     let enqueue name (i : bigint) =
//       E.enqueue "WORKER" name "_" (RT.DInt i) c.ownerID c.id
//
//     enqueue "apple" 1I
//     enqueue "apple" 2I
//     enqueue "banana" 3I
//     enqueue "apple" 4I
//     E.scheduleAll ()
//
//     let checkDequeue span tx (i : bigint) exname =
//       let evt = E.dequeue span tx |> Option.unwrapUnsafe
//
//       Expect.equal exname evt.name $"dequeue %d{i} is handler %s{exname}"
//
//
//       let actual =
//         match evt.value with
//         | RT.DInt i -> i
//         | _ -> 0I
//
//       Expect.equal actual i $"dequeue {i} has value {i}"
//       E.finish tx evt
//
//     do!
//       Telemetry.with_root
//         "test"
//         (fun span ->
//           E.withTransaction
//             span
//             (fun span tx ->
//               checkDequeue span tx 1 "apple"
//               checkDequeue span tx 2 "apple"
//               checkDequeue span tx 3 "banana"
//               checkDequeue span tx 4 "apple"
//               Ok(Some DNull)))
//   }
//
//
//
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
//
// let tests =
//   testList
//     "eventQueue"
//     [ testEventQueueRoundtrip
//       testEventQueueIsFifo
//       testCronJustRan
//       testCronFetchActiveCrons
//       testCronSanity
//       testGetWorkerSchedulesForCanvas ]
let tests = testList "eventQueue" []
