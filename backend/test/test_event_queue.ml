open Core_kernel
open Libexecution
open Libbackend
open Utils
open Libcommon

(* ------------------- *)
(*      event queue    *)
(* ------------------- *)

(* This doesn't actually test input, since it's a cron handler and not an actual
 * event handler *)
let t_event_queue_roundtrip () =
  clear_test_data () ;
  let h = daily_cron (ast_for "(let date (Date::now) 123)") in
  let c = ops2c_exn "test-event_queue" [hop h] in
  Canvas.save_all !c ;
  Event_queue.enqueue
    "CRON"
    "test"
    "Daily"
    DNull (* I don't believe crons take inputs? *)
    ~account_id:!c.owner
    ~canvas_id:!c.id ;
  Event_queue.schedule_all () ;
  let result = Queue_worker.run execution_id in
  ( match result with
  | Ok (Some result_dval) ->
      (* should have at least one trace *)
      let trace_id =
        Stored_event.load_event_ids ~canvas_id:!c.id ("CRON", "test", "Daily")
        |> List.hd_exn
        |> Tuple.T2.get1
      in
      AT.check
        AT.int
        "should have stored fn result"
        ( Stored_function_result.load ~canvas_id:!c.id ~trace_id h.tlid
        |> List.length )
        1 ;
      check_dval "Round tripped value" (Dval.dint 123) result_dval
  | Ok None ->
      AT.fail "Failed: expected Some, got None"
  | Error e ->
      AT.fail ("Failed: got error: " ^ Log.dump e) ) ;
  ()


(* ------------------- *)
(*        cron         *)
(* ------------------- *)

let t_cron_sanity () =
  clear_test_data () ;
  let h = daily_cron (ast_for "(+ 5 3)") in
  let c = ops2c_exn "test-cron_works" [hop h] in
  let handler = !c.handlers |> TL.handlers |> List.hd_exn in
  let should_run = Cron.should_execute !c.id handler execution_id in
  AT.check AT.bool "should_run should be true" should_run true ;
  ()


let t_cron_just_ran () =
  clear_test_data () ;
  let h = daily_cron (ast_for "(+ 5 3)") in
  let c = ops2c_exn "test-cron_works" [hop h] in
  let handler = !c.handlers |> TL.handlers |> List.hd_exn in
  Cron.record_execution !c.id handler ;
  let should_run = Cron.should_execute !c.id handler execution_id in
  AT.check AT.bool "should_run should be false" should_run false ;
  ()


(* ------------------- *)
(*      scheduling     *)
(* ------------------- *)

let t_get_worker_schedules_for_canvas () =
  clear_test_data () ;
  let t1, t2, t3 = (Int63.of_int 7, Int63.of_int 7, Int63.of_int 7) in
  let apple = {(worker "apple" (ast_for "1")) with tlid = t1} in
  let banana = {(worker "banana" (ast_for "1")) with tlid = t2} in
  let cherry = {(worker "cherry" (ast_for "1")) with tlid = t3} in
  let c =
    ops2c_exn
      "test-worker-scheduling-rules"
      [ Op.SetHandler (t1, pos, apple)
      ; Op.SetHandler (t2, pos, banana)
      ; Op.SetHandler (t3, pos, cherry) ]
  in
  Canvas.save_all !c ;
  let open Event_queue in
  let open Event_queue.Worker_states in
  pause_worker !c.id "apple" ;
  pause_worker !c.id "banana" ;
  block_worker !c.id "banana" ;
  let res = get_worker_schedules_for_canvas !c.id in
  let check name value =
    let actual =
      Core_kernel.Map.find res name |> Option.value_exn |> state_to_string
    in
    let expected = state_to_string value in
    AT.check AT.string (name ^ " is " ^ expected) expected actual
  in
  check "apple" Paused ;
  check "banana" Blocked ;
  check "cherry" Running ;
  ()


let suite =
  [ ("event_queue roundtrip", `Quick, t_event_queue_roundtrip)
  ; ("Cron should run sanity", `Quick, t_cron_sanity)
  ; ("Cron just ran", `Quick, t_cron_just_ran)
  ; ( "get_worker_schedules_for_canvas"
    , `Quick
    , t_get_worker_schedules_for_canvas ) ]
