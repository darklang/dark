open Core_kernel
open Libexecution
open Libbackend
open Libshared.FluidShortcuts
open Utils
open Libcommon

(* ------------------- *)
(*      event queue    *)
(* ------------------- *)

(* This doesn't actually test input, since it's a cron handler and not an actual
 * event handler *)
let t_event_queue_roundtrip () =
  clear_test_data () ;
  let h = daily_cron (let' "date" (fn "Date::now" []) (int 123)) in
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


let t_event_queue_is_fifo () =
  let module E = Event_queue in
  clear_test_data () ;
  let apple = worker "apple" (var "event") in
  let banana = worker "banana" (var "event") in
  let c = ops2c_exn "test-worker-fifo" [hop apple; hop banana] in
  Canvas.save_all !c ;
  let enqueue name i =
    E.enqueue
      "WORKER"
      name
      "_"
      (DInt (Dint.of_int i))
      ~account_id:!c.owner
      ~canvas_id:!c.id
  in
  enqueue "apple" 1 ;
  enqueue "apple" 2 ;
  enqueue "banana" 3 ;
  enqueue "apple" 4 ;
  E.schedule_all () ;
  let check_dequeue span tx i exname =
    let evt = E.dequeue span tx |> Option.value_exn in
    AT.check
      AT.string
      (Printf.sprintf "dequeue %d is handler %s" i exname)
      exname
      evt.name ;
    let actual = match evt.value with DInt i -> Dint.to_int_exn i | _ -> 0 in
    AT.check AT.int (Printf.sprintf "dequeue %d has value %d" i i) i actual ;
    E.finish tx evt
  in
  let _ =
    Telemetry.with_root "test" (fun span ->
        E.with_transaction span (fun span tx ->
            check_dequeue span tx 1 "apple" ;
            check_dequeue span tx 2 "apple" ;
            check_dequeue span tx 3 "banana" ;
            check_dequeue span tx 4 "apple" ;
            Ok (Some DNull)))
  in
  ()


(* ------------------- *)
(*        cron         *)
(* ------------------- *)

let t_cron_fetch_active_crons () =
  Telemetry.with_root "test" (fun span ->
      Serialize.fetch_active_crons span
      (* Just checking that this doesn't raise *)
      |> ignore)


let t_cron_sanity () =
  clear_test_data () ;
  let h = daily_cron (binop "+" (int 5) (int 3)) in
  let c = ops2c_exn "test-cron_works" [hop h] in
  let cron_schedule_data : Libbackend.Cron.cron_schedule_data =
    { canvas_id = !c.id
    ; owner = Uuidm.nil
    ; host = !c.host
    ; tlid = h.tlid |> Int63.to_string
    ; name = (match h.spec.name with Filled (_, s) -> s | _ -> "CAN'T HAPPEN")
    ; modifier =
        (match h.spec.modifier with Filled (_, s) -> s | _ -> "CAN'T HAPPEN") }
  in
  let ({should_execute; scheduled_run_at; interval}
        : Libbackend.Cron.execution_check_type) =
    Telemetry.with_root "test" (fun span ->
        Cron.execution_check span cron_schedule_data)
  in
  AT.check AT.bool "should_execute should be true" should_execute true ;
  ()


let t_cron_just_ran () =
  clear_test_data () ;
  let h = daily_cron (binop "+" (int 5) (int 3)) in
  let c = ops2c_exn "test-cron_works" [hop h] in
  let cron_schedule_data : Libbackend.Cron.cron_schedule_data =
    { canvas_id = !c.id
    ; owner = Uuidm.nil
    ; host = !c.host
    ; tlid = h.tlid |> Int63.to_string
    ; name = (match h.spec.name with Filled (_, s) -> s | _ -> "CAN'T HAPPEN")
    ; modifier =
        (match h.spec.modifier with Filled (_, s) -> s | _ -> "CAN'T HAPPEN") }
  in
  Cron.record_execution cron_schedule_data ;
  let ({should_execute; scheduled_run_at; interval}
        : Libbackend.Cron.execution_check_type) =
    Telemetry.with_root "test" (fun span ->
        Cron.execution_check span cron_schedule_data)
  in
  AT.check AT.bool "should_execute should be false" should_execute false ;
  ()


(* ------------------- *)
(*      scheduling     *)
(* ------------------- *)

let t_get_worker_schedules_for_canvas () =
  clear_test_data () ;
  let apple = worker "apple" (int 1) in
  let banana = worker "banana" (int 1) in
  let cherry = worker "cherry" (int 1) in
  let c =
    ops2c_exn "test-worker-scheduling-rules" [hop apple; hop banana; hop cherry]
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
  ; ( "Cron should be able to fetch active crons"
    , `Quick
    , t_cron_fetch_active_crons )
  ; ("Cron should run sanity", `Quick, t_cron_sanity)
  ; ("Cron just ran", `Quick, t_cron_just_ran)
  ; ("Event queue is FIFO per worker", `Quick, t_event_queue_is_fifo)
  ; ( "get_worker_schedules_for_canvas"
    , `Quick
    , t_get_worker_schedules_for_canvas ) ]
