open Core_kernel
open Libcommon
open Libexecution
open Libbackend
open Types
open Types.RuntimeT
open Ast
open Lwt
open Utils
module Resp = Cohttp_lwt_unix.Response
module Req = Cohttp_lwt_unix.Request
module Header = Cohttp.Header
module Code = Cohttp.Code
module C = Canvas
module RT = Runtime
module TL = Toplevel
module AT = Alcotest
module SE = Stored_event

(* ------------------- *)
(* Stored events *)
(* ------------------- *)

let t_stored_event_roundtrip () =
  clear_test_data () ;
  let owner : Uuidm.t =
    Account.owner ~auth_domain:"test" |> fun x -> Option.value_exn x
  in
  let id1 = Serialize.fetch_canvas_id owner "host" in
  let id2 = Serialize.fetch_canvas_id owner "host2" in
  let t1 = Util.create_uuid () in
  let t2 = Util.create_uuid () in
  let t3 = Util.create_uuid () in
  let t4 = Util.create_uuid () in
  let t5 = Util.create_uuid () in
  let t6 = Util.create_uuid () in
  SE.clear_all_events ~canvas_id:id1 () ;
  SE.clear_all_events ~canvas_id:id2 () ;
  let desc1 = ("HTTP", "/path", "GET") in
  let desc2 = ("HTTP", "/path2", "GET") in
  let desc3 = ("HTTP", "/path", "POST") in
  let desc4 = ("BG", "lol", "_") in
  ignore
    (SE.store_event
       ~canvas_id:id1
       ~trace_id:t1
       desc1
       (Dval.dstr_of_string_exn "1")) ;
  ignore
    (SE.store_event
       ~canvas_id:id1
       ~trace_id:t2
       desc1
       (Dval.dstr_of_string_exn "2")) ;
  ignore
    (SE.store_event
       ~canvas_id:id1
       ~trace_id:t3
       desc3
       (Dval.dstr_of_string_exn "3")) ;
  ignore
    (SE.store_event
       ~canvas_id:id1
       ~trace_id:t4
       desc2
       (Dval.dstr_of_string_exn "3")) ;
  ignore
    (SE.store_event
       ~canvas_id:id2
       ~trace_id:t5
       desc2
       (Dval.dstr_of_string_exn "3")) ;
  ignore
    (SE.store_event
       ~canvas_id:id2
       ~trace_id:t6
       desc4
       (Dval.dstr_of_string_exn "3")) ;
  let at_trace_id = AT.of_pp Uuidm.pp_string in
  let to_trace_id (t1, t2, t3, t4, t5) = t5 in
  let listed = SE.list_events ~limit:`All ~canvas_id:id1 () in
  AT.check
    (AT.list at_trace_id)
    "list host events"
    (List.sort ~compare [t1; t3; t4])
    (List.sort ~compare (List.map ~f:to_trace_id listed)) ;
  let loaded =
    SE.load_event_ids ~canvas_id:id2 desc4 |> List.map ~f:Tuple.T2.get1
  in
  AT.check
    (AT.list at_trace_id)
    "list desc events"
    (List.sort ~compare [t6])
    (List.sort ~compare loaded) ;
  let loaded1 = SE.load_events ~canvas_id:id1 desc1 |> List.map ~f:t4_get4th in
  check_dval_list
    "load GET events"
    [Dval.dstr_of_string_exn "2"; Dval.dstr_of_string_exn "1"]
    loaded1 ;
  let loaded2 = SE.load_events ~canvas_id:id1 desc3 |> List.map ~f:t4_get4th in
  check_dval_list "load POST events" [Dval.dstr_of_string_exn "3"] loaded2 ;
  let loaded3 = SE.load_events ~canvas_id:id2 desc3 |> List.map ~f:t4_get4th in
  check_dval_list "load no host2 events" [] loaded3 ;
  let loaded4 = SE.load_events ~canvas_id:id2 desc2 |> List.map ~f:t4_get4th in
  check_dval_list "load host2 events" [Dval.dstr_of_string_exn "3"] loaded4 ;
  ()


let t_trace_data_json_format_redacts_passwords () =
  let id = fid () in
  let trace_data : Analysis_types.trace_data =
    { input = [("event", DPassword (PasswordBytes.of_string "redactme1"))]
    ; timestamp = Time.epoch
    ; function_results =
        [ ( "Password::hash"
          , id
          , "foobar"
          , DPassword (PasswordBytes.of_string "redactme2") ) ] }
  in
  let expected : Analysis_types.trace_data =
    { input = [("event", DPassword (PasswordBytes.of_string "Redacted"))]
    ; timestamp = Time.epoch
    ; function_results =
        [ ( "Password::hash"
          , id
          , "foobar"
          , DPassword (PasswordBytes.of_string "Redacted") ) ] }
  in
  trace_data
  |> Analysis_types.trace_data_to_yojson
  |> Analysis_types.trace_data_of_yojson
  |> Result.ok_or_failwith
  |> AT.check
       (AT.testable
          Analysis_types.pp_trace_data
          Analysis_types.equal_trace_data)
       "trace_data round trip"
       expected


let t_route_variables_work_with_stored_events () =
  (* set up test *)
  clear_test_data () ;
  let host = "test-route_variables_works" in
  let oplist = [Op.SetHandler (tlid, pos, http_route_handler ())] in
  let c = ops2c_exn host oplist in
  Canvas.serialize_only [tlid] !c ;
  let t1 = Util.create_uuid () in
  let desc = ("HTTP", http_request_path, "GET") in
  let route = ("HTTP", http_route, "GET") in
  (* store an event and check it comes out *)
  ignore
    (SE.store_event
       ~canvas_id:!c.id
       ~trace_id:t1
       desc
       (Dval.dstr_of_string_exn "1")) ;
  (* check we get back the path for a route with a variable in it *)
  let loaded1 = SE.load_events ~canvas_id:!c.id route in
  check_dval_list
    "load GET events"
    [Dval.dstr_of_string_exn "1"]
    (loaded1 |> List.map ~f:t4_get4th) ;
  AT.check
    (AT.list AT.string)
    "path returned correctly"
    (loaded1 |> List.map ~f:t4_get1st)
    [http_request_path] ;
  (* check that the event is not in the 404s *)
  let f404s = Analysis.get_404s ~since:Time.epoch !c in
  AT.check (AT.list (AT.of_pp Stored_event.pp_four_oh_four)) "no 404s" [] f404s ;
  ()


let t_route_variables_work_with_stored_events_and_wildcards () =
  (* set up test *)
  clear_test_data () ;
  let host = "test-route_variables_works_with_wildcards" in
  let route = "/api/create_token" in
  let request_path = "/api/create-token" in
  (* note hyphen vs undeerscore *)
  let oplist = [Op.SetHandler (tlid, pos, http_route_handler ~route ())] in
  let c = ops2c_exn host oplist in
  Canvas.serialize_only [tlid] !c ;
  let t1 = Util.create_uuid () in
  let desc = ("HTTP", request_path, "GET") in
  let route = ("HTTP", route, "GET") in
  (* store an event and check it comes out *)
  ignore
    (SE.store_event
       ~canvas_id:!c.id
       ~trace_id:t1
       desc
       (Dval.dstr_of_string_exn "1")) ;
  (* check we get back the path for a route with a variable in it *)
  let loaded1 = SE.load_events ~canvas_id:!c.id route in
  check_dval_list "load GET events" [] (loaded1 |> List.map ~f:t4_get4th) ;
  ()


(* ------------------- *)
(* event queue *)
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
(* cron *)
(* ------------------- *)

let t_cron_sanity () =
  clear_test_data () ;
  let h = daily_cron (ast_for "(+ 5 3)") in
  let c = ops2c_exn "test-cron_works" [hop h] in
  let handler = !c.handlers |> TL.handlers |> List.hd_exn in
  let should_run = Cron.should_execute !c.id handler in
  AT.check AT.bool "should_run should be true" should_run true ;
  ()


let t_cron_just_ran () =
  clear_test_data () ;
  let h = daily_cron (ast_for "(+ 5 3)") in
  let c = ops2c_exn "test-cron_works" [hop h] in
  let handler = !c.handlers |> TL.handlers |> List.hd_exn in
  Cron.record_execution !c.id handler ;
  let should_run = Cron.should_execute !c.id handler in
  AT.check AT.bool "should_run should be false" should_run false ;
  ()


(* ------------------- *)
(* httpclient *)
(* ------------------- *)
let t_curl_file_urls () =
  AT.check
    (AT.option AT.string)
    "aaa"
    (* Before we limited the protocols for curl, .info.error was "",
       since Httpclient.http_call checked for a 2xx HTTP code. But the file
       contents ended up in the error message. Now we've restricted the URL
       protocols, so we get CURLE_UNSUPPORTED_PROTOCOL before a request
       is even sent. *)
    (Some "Unsupported protocol")
    ( try
        ignore
          (Httpclient.http_call
             "file://localhost/etc/passwd"
             []
             Httpclient.GET
             []
             "") ;
        None
      with
    | Exception.DarkException i ->
        List.Assoc.find i.info ~equal:( = ) "error"
    | _ ->
        None )


let suite =
  [ ("stored_events", `Quick, t_stored_event_roundtrip)
  ; ( "Trace data redacts passwords"
    , `Quick
    , t_trace_data_json_format_redacts_passwords )
  ; ( "Route variables work with stored events"
    , `Quick
    , t_route_variables_work_with_stored_events )
  ; ( "Route variables work with stored events and wildcards"
    , `Quick
    , t_route_variables_work_with_stored_events_and_wildcards )
  ; ("event_queue roundtrip", `Quick, t_event_queue_roundtrip)
  ; ("Cron should run sanity", `Quick, t_cron_sanity)
  ; ("Cron just ran", `Quick, t_cron_just_ran)
  ; ("Dark code can't curl file:// urls", `Quick, t_curl_file_urls) ]
