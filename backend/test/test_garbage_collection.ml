open Core_kernel
open Libexecution
open Libbackend
open Libshared.FluidShortcuts
open Utils
open Libcommon
module SE = Libbackend.Stored_event

let t_unmatched_garbage () =
  let span : Telemetry.Span.t =
    { name = "test"
    ; service_name = "test"
    ; span_id = 0
    ; trace_id = 0
    ; parent_id = 0
    ; start_time = Time.now ()
    ; attributes = String.Table.create () }
  in
  (* old garbage which matches no handler is completely gone. *)
  clear_test_data () ;

  (* Setup canvas / handler with "Good" data *)
  let c =
    ops2c_exn
      "test-host"
      [Types.SetHandler (tlid, pos, http_route_handler ~route:"/path" ())]
  in
  Libbackend.Canvas.save_all !c ;
  let canvas_id = !c.id in
  let good_handler = ("HTTP", "/path", "GET") in

  (* Add 404 data *)
  let ten_days_ago = Time.sub (Time.now ()) (Time.Span.create ~day:10 ()) in
  SE.clear_all_events ~canvas_id () ;
  let f404_handler = ("HTTP", "/path", "POST") in
  let store_data handler =
    let trace_id = Util.create_uuid () in
    ignore
      (SE.store_event
         ~canvas_id
         ~trace_id
         ~timestamp:ten_days_ago
         handler
         (Dval.dstr_of_string_exn "1")) ;
    ()
  in
  store_data good_handler ;
  store_data good_handler ;
  store_data good_handler ;
  store_data f404_handler ;
  store_data f404_handler ;
  store_data f404_handler ;
  (* We expect that we keep 4 traces (good + latest 404) and delete 2
   * (non-latest 404s)) *)
  let expected =
    Stored_event.trim_events_for_canvas
      ~span
      ~action:Count
      canvas_id
      "test-host"
      10000
  in
  AT.check AT.int "expected is right" 2 expected ;
  let deleted =
    Stored_event.trim_events_for_canvas
      ~span
      ~action:Delete
      canvas_id
      "test-host"
      10000
  in
  AT.check AT.int "deleted row count is right" 2 deleted ;
  let f404 = SE.load_events ~canvas_id f404_handler in
  AT.check AT.int "1 404 remains" 1 (List.length f404) ;
  let good = SE.load_events ~canvas_id good_handler in
  AT.check AT.int "all good events remain" 3 (List.length good) ;
  ()


let suite = [("unmatched_garbage", `Quick, t_unmatched_garbage)]
