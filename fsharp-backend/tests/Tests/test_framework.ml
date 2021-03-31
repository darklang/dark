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
