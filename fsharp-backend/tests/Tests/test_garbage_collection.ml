open Core_kernel
open Libexecution
open Libbackend
open Libshared.FluidShortcuts
open Utils
open Libcommon
module SE = Libbackend.Stored_event

let span : Telemetry.Span.t =
  { name = "test"
  ; service_name = "test"
  ; span_id = 0
  ; trace_id = 0
  ; parent_id = 0
  ; start_time = Time.now ()
  ; attributes = String.Table.create () }


let ten_days_ago = Time.sub (Time.now ()) (Time.Span.create ~day:10 ())

(* old garbage which matches no handler is completely gone. *)
let t_unmatched_garbage () =
  clear_test_data () ;

  (* Setup canvas *)
  let c =
    ops2c_exn
      "test-host"
      [Types.SetHandler (tlid, pos, http_route_handler ~route:"/path" ())]
  in
  Libbackend.Canvas.save_all !c ;
  let canvas_id = !c.id in

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
  let good_handler = ("HTTP", "/path", "GET") in
  let f404_handler = ("HTTP", "/path", "POST") in
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


let t_wildcard_cleanup () =
  (* old garbage which matches no handler is completely gone. *)
  clear_test_data () ;

  (* Setup canvas *)
  let path = "/:part1/other/:part2" in
  let c =
    ops2c_exn
      "test-host"
      [Types.SetHandler (tlid, pos, http_route_handler ~route:path ())]
  in
  Libbackend.Canvas.save_all !c ;
  let canvas_id = !c.id in

  let store_data segment1 segment2 =
    let trace_id = Util.create_uuid () in
    ignore
      (SE.store_event
         ~canvas_id
         ~trace_id
         ~timestamp:ten_days_ago
         ("HTTP", "/" ^ segment1 ^ "/other/" ^ segment2, "GET")
         (Dval.dstr_of_string_exn "1")) ;
    ()
  in
  store_data "a1" "b1" ;
  store_data "a2" "b2" ;
  store_data "a3" "b3" ;
  store_data "a4" "b4" ;
  store_data "a5" "b5" ;
  store_data "a6" "b6" ;
  store_data "a7" "b7" ;
  store_data "a8" "b8" ;
  store_data "a9" "b9" ;
  store_data "a10" "b10" ;
  store_data "a11" "b11" ;
  store_data "a12" "b12" ;
  store_data "a13" "b13" ;
  store_data "a14" "b14" ;
  store_data "a15" "b15" ;
  store_data "a16" "b16" ;
  store_data "a17" "b17" ;
  store_data "a18" "b18" ;
  (* We expect that we keep 10 traces (good + latest 404) and delete 8 *)
  let expected =
    Stored_event.trim_events_for_canvas
      ~span
      ~action:Count
      canvas_id
      "test-host"
      10000
  in
  AT.check AT.int "expected is right" 8 expected ;
  let deleted =
    Stored_event.trim_events_for_canvas
      ~span
      ~action:Delete
      canvas_id
      "test-host"
      10000
  in
  AT.check AT.int "deleted row count is right" 8 deleted ;
  let good = SE.load_events ~canvas_id ("HTTP", path, "GET") in
  AT.check AT.int "all good events remain" 10 (List.length good) ;
  ()


let suite =
  [ ("unmatched_garbage", `Quick, t_unmatched_garbage)
  ; ("wildcards are treated properly", `Quick, t_wildcard_cleanup) ]
