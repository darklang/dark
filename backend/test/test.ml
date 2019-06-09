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

(* ----------------------- *)
(* The tests *)
(* ----------------------- *)

let t_stdlib_works () =
  check_dval
    "uniqueBy1"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> (Int::divide x 2)))")
    (DList [DInt 1; DInt 3; DInt 4]) ;
  check_dval
    "uniqueBy2"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> x))")
    (DList [DInt 1; DInt 2; DInt 3; DInt 4]) ;
  check_error_contains
    "base64decode"
    (exec_ast "(String::base64Decode 'random string')")
    "Not a valid base64 string" ;
  check_dval
    "getAt1"
    (exec_ast "(List::getAt (1 2 3 4) 0)")
    (DOption (OptJust (DInt 1))) ;
  check_dval
    "getAt2"
    (exec_ast "(List::getAt (1 2 3 4) 4)")
    (DOption OptNothing) ;
  ()


let t_option_stdlibs_work () =
  check_dval
    "map just"
    (exec_ast "(Option::map (Just 4) (\\x -> (Int::divide x 2)))")
    (DOption (OptJust (DInt 2))) ;
  check_dval
    "map nothing"
    (exec_ast "(Option::map (Nothing) (\\x -> (Int::divide x 2)))")
    (DOption OptNothing) ;
  check_dval
    "withDefault just"
    (exec_ast "(Option::withDefault (Just 6) 5)")
    (DInt 6) ;
  check_dval
    "withDefault nothing"
    (exec_ast "(Option::withDefault (Nothing) 5)")
    (DInt 5) ;
  check_dval
    "andThen just,nothing"
    (exec_ast "(Option::andThen (Just 5) (\\x -> (Nothing)))")
    (DOption OptNothing) ;
  check_dval
    "andThen just,just"
    (exec_ast "(Option::andThen (Just 5) (\\x -> (Just (+ 1 x))))")
    (DOption (OptJust (DInt 6))) ;
  check_dval
    "andThen nothing,just"
    (exec_ast "(Option::andThen (Nothing) (\\x -> (Just 5)))")
    (DOption OptNothing) ;
  check_dval
    "andThen nothing,nothing"
    (exec_ast "(Option::andThen (Nothing) (\\x -> (Nothing)))")
    (DOption OptNothing) ;
  AT.check
    AT.bool
    "andThen wrong type"
    ( match
        exec_ast "(Option::andThen (Just 8) (\\x -> (Int::divide x 2)))"
      with
    | DError msg ->
        Prelude.String.contains
          ~substring:"Expected `f` to return an option"
          msg
    | _ ->
        false )
    true ;
  ()


let t_result_stdlibs_work () =
  let test_string = Dval.dstr_of_string_exn "test" in
  check_dval
    "map ok"
    (exec_ast "(Result::map (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (DInt 2))) ;
  check_dval
    "map error"
    (exec_ast "(Result::map (Error 'test') (\\x -> (Int::divide x 2)))")
    (DResult (ResError test_string)) ;
  check_dval
    "maperror ok"
    (exec_ast "(Result::mapError (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (DInt 4))) ;
  check_dval
    "maperror error"
    (exec_ast
       "(Result::mapError (Error 'test') (\\x -> (String::append x '-appended')))")
    (DResult (ResError (Dval.dstr_of_string_exn "test-appended"))) ;
  check_dval
    "withDefault ok"
    (exec_ast "(Result::withDefault (Ok 6) 5)")
    (DInt 6) ;
  check_dval
    "withDefault error"
    (exec_ast "(Result::withDefault (Error 'test') 5)")
    (DInt 5) ;
  check_dval
    "fromOption just"
    (exec_ast "(Result::fromOption (Just 6) 'test')")
    (DResult (ResOk (DInt 6))) ;
  check_dval
    "fromOption nothing"
    (exec_ast "(Result::fromOption (Nothing) 'test')")
    (DResult (ResError test_string)) ;
  check_dval
    "toOption ok"
    (exec_ast "(Result::toOption (Ok 6))")
    (DOption (OptJust (DInt 6))) ;
  check_dval
    "toOption error"
    (exec_ast "(Result::toOption (Error 'test'))")
    (DOption OptNothing) ;
  check_dval
    "andThen ok,error"
    (exec_ast "(Result::andThen (Ok 5) (\\x -> (Error 'test')))")
    (DResult (ResError test_string)) ;
  check_dval
    "andThen ok,ok"
    (exec_ast "(Result::andThen (Ok 5) (\\x -> (Ok (+ 1 x))))")
    (DResult (ResOk (DInt 6))) ;
  check_dval
    "andThen error,ok"
    (exec_ast "(Result::andThen (Error 'test') (\\x -> (Ok 5)))")
    (DResult (ResError test_string)) ;
  check_dval
    "andThen error,error"
    (exec_ast "(Result::andThen (Error 'test') (\\x -> (Error 'test')))")
    (DResult (ResError test_string)) ;
  AT.check
    AT.bool
    "andThen wrong type"
    ( match exec_ast "(Result::andThen (Ok 8) (\\x -> (Int::divide x 2)))" with
    | DError msg ->
        Prelude.String.contains
          ~substring:"Expected `f` to return a result"
          msg
    | _ ->
        false )
    true ;
  ()


module SE = Stored_event

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
  SE.clear_all_events ~canvas_id:id1 () ;
  SE.clear_all_events ~canvas_id:id2 () ;
  let desc1 = ("HTTP", "/path", "GET") in
  let desc2 = ("HTTP", "/path2", "GET") in
  let desc3 = ("HTTP", "/path", "POST") in
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
  let at_trace_id = AT.of_pp Uuidm.pp_string in
  let to_trace_id (t1, t2, t3, t4, t5) = t5 in
  let listed = SE.list_events ~limit:`All ~canvas_id:id1 () in
  AT.check
    (AT.list at_trace_id)
    "list host events"
    (List.sort ~compare [t1; t3; t4])
    (List.sort ~compare (List.map ~f:to_trace_id listed)) ;
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


(* This doesn't actually test input, since it's a cron handler and not an actual
 * event handler *)
let t_event_queue_roundtrip () =
  clear_test_data () ;
  let h = daily_cron (ast_for "123") in
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
      check_dval "Round tripped value" (DInt 123) result_dval
  | Ok None ->
      AT.fail "Failed: expected Some, got None"
  | Error e ->
      AT.fail ("Failed: got error: " ^ Log.dump e) ) ;
  ()


let t_hmac_signing _ =
  let url = "https://api.twitter.com/1.1/statuses/update.json" in
  let ts = "1318622958" in
  let nonce = "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg" in
  let secret : Secret.twitter_secret =
    { consumer_key = "xvz1evFS4wEEPTGEFPHBog"
    ; consumer_secret = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw"
    ; access_token = "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"
    ; access_token_secret = "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE" }
  in
  let k1 = "status" in
  let v1 = "Hello Ladies + Gentlemen, a signed OAuth request!" in
  let k2 = "include_entities" in
  let v2 = "true" in
  (* Test 1 - just the sig *)
  AT.check
    AT.string
    "hmac_signing_1"
    "hCtSmYh+iHYCEqBWrE7C7hYmtUk="
    (Twitter.sign
       secret.consumer_secret
       secret.access_token_secret
       url
       "POST"
       [ (k1, v1)
       ; (k2, v2)
       ; ("oauth_consumer_key", secret.consumer_key)
       ; ("oauth_nonce", nonce)
       ; ("oauth_signature_method", "HMAC-SHA1")
       ; ("oauth_timestamp", ts)
       ; ("oauth_token", secret.access_token)
       ; ("oauth_version", "1.0") ]) ;
  (* Test 2 - full header *)
  let url = "https://api.twitter.com/1.1/statuses/update.json" in
  Mock.set_string "ts" ts ;
  Mock.set_string "nonce" nonce ;
  let args =
    DvalMap.from_list
      [(k1, Dval.dstr_of_string_exn v1); (k2, Dval.dstr_of_string_exn v2)]
  in
  let expected_header =
    "OAuth oauth_consumer_key=\"xvz1evFS4wEEPTGEFPHBog\", oauth_nonce=\"kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg\", oauth_signature=\"hCtSmYh%2BiHYCEqBWrE7C7hYmtUk%3D\", oauth_signature_method=\"HMAC-SHA1\", oauth_timestamp=\"1318622958\", oauth_token=\"370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb\", oauth_version=\"1.0\""
  in
  let actual = Twitter.oauth_header secret url "POST" args in
  AT.check AT.string "hmac_signing_2" expected_header actual


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


let t_escape_pg_escaping () =
  AT.check AT.string "no quotes" "asdd" (Db.escape_single "asdd") ;
  AT.check AT.string "single" "as''dd" (Db.escape_single "as'dd") ;
  AT.check AT.string "double" "as\"dd" (Db.escape_single "as\"dd") ;
  ()


let t_password_hashing_and_checking_works () =
  let ast =
    "(let password 'password'
               (Password::check (Password::hash password)
               password))"
  in
  check_dval
    "A `Password::hash'd string `Password::check's against itself."
    (exec_ast ast)
    (DBool true)


let t_password_serialization () =
  let does_serialize name expected f =
    let bytes = Bytes.of_string "encryptedbytes" in
    let password = DPassword bytes in
    AT.check
      AT.bool
      ("Passwords serialize in non-redaction function: " ^ name)
      expected
      (String.is_substring
         ~substring:(B64.encode "encryptedbytes")
         (f password))
  in
  let roundtrips name serialize deserialize =
    let bytes = Bytes.of_string "encryptedbytes" in
    let password = DPassword bytes in
    AT.check
      at_dval
      ("Passwords serialize in non-redaction function: " ^ name)
      password
      (password |> serialize |> deserialize |> serialize |> deserialize)
  in
  (* doesn't redact *)
  does_serialize
    "to_internal_roundtrippable_v0"
    true
    Dval.to_internal_roundtrippable_v0 ;
  does_serialize "to_internal_queryable_v0" true Dval.to_internal_queryable_v0 ;
  (* roundtrips *)
  roundtrips
    "to_internal_roundtrippable_v0"
    Dval.to_internal_roundtrippable_v0
    Dval.of_internal_roundtrippable_v0 ;
  roundtrips
    "to_internal_queryable_v0"
    Dval.to_internal_queryable_v0
    Dval.of_internal_roundtrippable_v0 ;
  (* redacting *)
  does_serialize
    "to_enduser_readable_text_v0"
    false
    Dval.to_enduser_readable_text_v0 ;
  does_serialize
    "to_enduser_readable_html_v0"
    false
    Dval.to_enduser_readable_html_v0 ;
  does_serialize "to_developer_repr_v0" false Dval.to_developer_repr_v0 ;
  does_serialize
    "to_pretty_machine_json_v1"
    false
    Dval.to_pretty_machine_json_v1 ;
  does_serialize
    "to_pretty_request_json_v0"
    false
    Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0 ;
  does_serialize
    "to_pretty_response_json_v1"
    false
    Legacy.PrettyResponseJsonV0.to_pretty_response_json_v0 ;
  ()


let t_password_json_round_trip_forwards () =
  let password = DPassword (Bytes.of_string "x") in
  check_dval
    "Passwords serialize and deserialize if there's no redaction."
    password
    ( password
    |> Dval.to_internal_roundtrippable_v0
    |> Dval.of_internal_roundtrippable_v0
    |> Dval.to_internal_roundtrippable_v0
    |> Dval.of_internal_roundtrippable_v0 )


let t_html_escaping () =
  check_dval
    "html escaping works"
    (* TODO: add back in check that `'` is correctly escaped. It didn't
     * play nice with our hacky `'` removal in the DSL parser *)
    (Dval.dstr_of_string_exn "test&lt;&gt;&amp;&quot;")
    (exec_ast "(String::htmlEscape 'test<>&\\\"')")


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


let t_uuid_string_roundtrip () =
  let ast =
    "(let i (Uuid::generate)
               (let s (toString i)
                 (let parsed (String::toUUID s)
                   (i parsed))))"
  in
  AT.check
    AT.int
    "A generated id can round-trip"
    0
    (match exec_ast ast with DList [p1; p2] -> compare_dval p1 p2 | _ -> 1)


let t_parsed_request_cookies () =
  let with_headers h =
    Parsed_request.from_request h [] ""
    |> Parsed_request.to_dval
    |> fun v ->
    match v with
    | DObj o ->
        Base.Map.find_exn o "cookies"
    | _ ->
        failwith "didn't end up with 'cookies' in the DObj"
  in
  let with_cookies c = with_headers [("cookie", c)] in
  AT.check
    (AT.list at_dval)
    "Parsed_request.from_request parses cookies correctly."
    [ with_headers []
    ; with_cookies ""
    ; with_cookies "a"
    ; with_cookies "a="
    ; with_cookies "a=b"
    ; with_cookies "a=b;"
    ; with_cookies "a=b; c=d"
    ; with_cookies "a=b; c=d;" ]
    [ Dval.to_dobj_exn []
    ; Dval.to_dobj_exn []
    ; Dval.to_dobj_exn []
    ; Dval.to_dobj_exn [("a", Dval.dstr_of_string_exn "")]
    ; Dval.to_dobj_exn [("a", Dval.dstr_of_string_exn "b")]
    ; Dval.to_dobj_exn [("a", Dval.dstr_of_string_exn "b")]
    ; Dval.to_dobj_exn
        [("a", Dval.dstr_of_string_exn "b"); ("c", Dval.dstr_of_string_exn "d")]
    ; Dval.to_dobj_exn
        [("a", Dval.dstr_of_string_exn "b"); ("c", Dval.dstr_of_string_exn "d")]
    ]


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


(* ------------------- *)
(* Test setup *)
(* ------------------- *)

let suite =
  (* ------------------- *)
  (* stdlib: Twitter *)
  (* ------------------- *)
  [ ("hmac signing works", `Quick, t_hmac_signing) (* stdlib: uuids *)
  ; ("UUIDs round-trip to/from strings", `Quick, t_uuid_string_roundtrip)
    (* ------------------- *)
    (* Analysis *)
    (* ------------------- *)
  ; ("stored_events", `Quick, t_stored_event_roundtrip)
  ; ( "Trace data redacts passwords"
    , `Quick
    , t_trace_data_json_format_redacts_passwords )
  ; ( "Route variables work with stored events"
    , `Quick
    , t_route_variables_work_with_stored_events )
  ; ( "Route variables work with stored events and wildcards"
    , `Quick
    , t_route_variables_work_with_stored_events_and_wildcards )
    (* ------------------- *)
    (* event queue *)
    (* ------------------- *)
  ; ("event_queue roundtrip", `Quick, t_event_queue_roundtrip)
    (* ------------------- *)
    (* stdlib *)
    (* ------------------- *)
  ; ("Stdlib fns work", `Quick, t_stdlib_works)
    (* ------------------- *)
    (* cron *)
    (* ------------------- *)
  ; ("Cron should run sanity", `Quick, t_cron_sanity)
  ; ("Cron just ran", `Quick, t_cron_just_ran)
    (* ------------------- *)
    (* general DB stuff *)
    (* ------------------- *)
  ; ("Test postgres escaping", `Quick, t_escape_pg_escaping)
    (* ------------------- *)
    (* passwords *)
    (* ------------------- *)
  ; ( "Passwords serialize correctly and redact (or not) correctly"
    , `Quick
    , t_password_serialization )
  ; ( "End-user password hashing and checking works"
    , `Quick
    , t_password_hashing_and_checking_works )
    (* ------------------- *)
    (* stdlib: string *)
    (* ------------------- *)
  ; ("HTML escaping works reasonably", `Quick, t_html_escaping)
    (* ------------------- *)
    (* stdlib: http client *)
    (* ------------------- *)
  ; ("Dark code can't curl file:// urls", `Quick, t_curl_file_urls)
    (* ------------------- *)
    (* stdlib: option / result *)
    (* ------------------- *)
  ; ("Option stdlibs work", `Quick, t_option_stdlibs_work)
  ; ("Result stdlibs work", `Quick, t_result_stdlibs_work) ]


let () =
  Libbackend.Init.init ~run_side_effects:true ;
  Log.set_level `All ;
  Account.init_testing () ;
  let wrap f () =
    try f () with e ->
      Exception.reraise_after e (fun bt ->
          print_endline (Exception.to_string e) ;
          print_endline (Exception.backtrace_to_string bt) )
  in
  let suites =
    [ ("http", Test_http.suite)
    ; ("accounts", Test_account.suite)
    ; ("webserver", Test_webserver.suite)
    ; ("language", Test_language.suite)
    ; ("tests", suite)
    ; ("canvas+ops", Test_canvas_ops.suite)
    ; ("json", Test_json.suite)
    ; ("user_db", Test_user_db.suite)
    ; ("string-libs", Test_string_libs.suite)
    ; ("db-libs", Test_db_libs.suite) ]
  in
  let wrapped_suites =
    List.map suites ~f:(fun (n, ts) ->
        (n, List.map ts ~f:(fun (n, m, t) -> (n, m, wrap t))) )
  in
  let suite, exit = Junit_alcotest.run_and_report "all" wrapped_suites in
  let report = Junit.make [suite] in
  File.mkdir ~root:Testresults "" ;
  let file =
    File.check_filename ~mode:`Write ~root:Testresults "backend.xml"
  in
  Junit.to_file report file ;
  exit ()
