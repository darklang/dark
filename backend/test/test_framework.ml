open Core_kernel
open Libcommon
open Libexecution
open Libbackend
open Libshared.FluidShortcuts
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
          , 0
          , DPassword (PasswordBytes.of_string "redactme2") ) ] }
  in
  let expected : Analysis_types.trace_data =
    { input = [("event", DPassword (PasswordBytes.of_string "Redacted"))]
    ; timestamp = Time.epoch
    ; function_results =
        [ ( "Password::hash"
          , id
          , "foobar"
          , 0
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
  let oplist = [SetHandler (tlid, pos, http_route_handler ())] in
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
  let oplist = [SetHandler (tlid, pos, http_route_handler ~route ())] in
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


(* ------------------- *)
(* cron *)
(* ------------------- *)

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
    (* TODO: use modern http_call *)
    ( try
        ignore
          (Legacy.HttpclientV0.http_call
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


let t_encode_request_body () =
  clear_test_data () ;
  let () =
    let headers = [] in
    let body =
      Some
        (Dval.to_dobj_exn
           [("hello", DStr (Unicode_string.of_string_exn "world"))])
    in
    let encoded_body, munged_headers =
      Libhttpclient.encode_request_body headers body
    in
    AT.check
      (AT.option AT.string)
      "jsonifies by default"
      encoded_body
      (Some "{ \"hello\": \"world\" }") ;
    AT.check
      (AT.list (AT.pair AT.string AT.string))
      "Adds default application/json header"
      munged_headers
      [("Content-Type", "application/json; charset=utf-8")]
  in
  let () =
    let headers = [("Content-Type", "text/plain; charset=utf-8")] in
    let body = Some (DOption OptNothing) in
    let encoded_body, munged_headers =
      Libhttpclient.encode_request_body headers body
    in
    AT.check
      (AT.option AT.string)
      "Uses our plaintext format if passed text/plain"
      encoded_body
      (Some "Nothing") ;
    AT.check
      (AT.list (AT.pair AT.string AT.string))
      "Passes text/plain thru transparently"
      munged_headers
      [("Content-Type", "text/plain; charset=utf-8")]
  in
  let () =
    let headers = [("Content-Type", "application/x-www-form-urlencoded")] in
    let body =
      Some
        (Dval.to_dobj_exn
           [("hello", DStr (Unicode_string.of_string_exn "world"))])
    in
    let encoded_body, munged_headers =
      Libhttpclient.encode_request_body headers body
    in
    AT.check
      (AT.option AT.string)
      "Uses form encoding if passed application/x-www-form-urlencoded"
      encoded_body
      (Some "hello=world") ;
    AT.check
      (AT.list (AT.pair AT.string AT.string))
      "Passes application/x-www-form-urlencoded through transparently"
      munged_headers
      [("Content-Type", "application/x-www-form-urlencoded")]
  in
  let () =
    let headers = [] in
    let body = Some (Dval.dstr_of_string_exn "") in
    let encoded_body, munged_headers =
      Libhttpclient.encode_request_body headers body
    in
    AT.check
      (AT.option AT.string)
      "Empty string is morphed to no body"
      encoded_body
      None ;
    AT.check
      (AT.list (AT.pair AT.string AT.string))
      "Adds text/plain to empty String bodies #1"
      munged_headers
      [("Content-Type", "text/plain; charset=utf-8")]
  in
  let () =
    let headers = [] in
    let body = None in
    let encoded_body, munged_headers =
      Libhttpclient.encode_request_body headers body
    in
    AT.check
      (AT.option AT.string)
      "No body is transparently passed thru"
      encoded_body
      None ;
    AT.check
      (AT.list (AT.pair AT.string AT.string))
      "Adds text/plain to empty bodies #2"
      munged_headers
      [("Content-Type", "text/plain; charset=utf-8")]
  in
  let () =
    let headers = [] in
    let body = Some (Dval.dstr_of_string_exn "hello, world!") in
    let encoded_body, munged_headers =
      Libhttpclient.encode_request_body headers body
    in
    AT.check
      (AT.option AT.string)
      "Strings are transparently passed through with no extra quotations, in utf-8"
      encoded_body
      (Some "hello, world!") ;
    AT.check
      (AT.list (AT.pair AT.string AT.string))
      "Strings are considered text/plain"
      munged_headers
      [("Content-Type", "text/plain; charset=utf-8")]
  in
  ()


(* ------------------- *)
(* functions *)
(* ------------------- *)

let t_function_traces_are_stored () =
  clear_test_data () ;
  let fntlid : tlid = id_of_int 12312345234 in
  let f = user_fn "test_fn" [] (fn "DB::generateKey" []) in
  let f = {f with tlid = fntlid} in
  let h = handler (fn "test_fn" []) in
  let host = "test" in
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let trace_id = Util.create_uuid () in
  let _ = execute_ops ~trace_id [fop f; hop h] in
  (* get the trace for the execution *)
  AT.check
    AT.int
    "handler should only have fn result for test_fn"
    1
    (Stored_function_result.load ~canvas_id ~trace_id h.tlid |> List.length) ;
  AT.check
    AT.int
    "functions should only have fn result for DB::generateKey"
    1
    (Stored_function_result.load ~canvas_id ~trace_id fntlid |> List.length) ;
  ()


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
  ; ("Dark code can't curl file:// urls", `Quick, t_curl_file_urls)
  ; ("Function traces are stored", `Quick, t_function_traces_are_stored)
  ; ("Httpclient encodes request bodies per spec", `Quick, t_encode_request_body)
  ]
