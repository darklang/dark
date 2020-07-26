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
  let owner : Uuidm.t =
    Account.owner ~auth_domain:"test" |> fun x -> Option.value_exn x
  in
  let canvas_id = Serialize.fetch_canvas_id owner "host" in
  let c =
    ops2c_exn
      "test-host"
      [Types.SetHandler (tlid, pos, http_route_handler ~route:"/path" ())]
  in
  Libbackend.Canvas.save_all !c ;
  let good_handler = ("HTTP", "/path", "GET") in

  (* Add Bad data *)
  let ten_days_ago = Time.sub (Time.now ()) (Time.Span.create ~day:10 ()) in
  SE.clear_all_events ~canvas_id () ;
  let bad_handler = ("HTTP", "/path", "POST") in
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
  store_data bad_handler ;
  store_data bad_handler ;
  store_data bad_handler ;
  let expected =
    Stored_event.trim_events_for_canvas
      ~span
      ~action:Count
      canvas_id
      "host"
      10000
  in
  AT.check AT.int "expected is right" 3 expected ;
  let deleted =
    Stored_event.trim_events_for_canvas
      ~span
      ~action:Delete
      canvas_id
      "host"
      10000
  in
  AT.check AT.int "deleted row count is right" 3 deleted ;
  let all = SE.load_events ~canvas_id bad_handler in
  AT.check AT.int "no events exist" 0 (List.length all) ;
  ()


let suite = [("unmatched_garbage", `Quick, t_unmatched_garbage)]
