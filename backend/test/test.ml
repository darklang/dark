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


(* ------------------- *)
(* Test setup *)
(* ------------------- *)

let suite =
  [ ("Dark code can't curl file:// urls", `Quick, t_curl_file_urls)
  ; ("Test postgres escaping", `Quick, t_escape_pg_escaping)
  ; ( "Passwords serialize correctly and redact (or not) correctly"
    , `Quick
    , t_password_serialization )
  ; ( "End-user password hashing and checking works"
    , `Quick
    , t_password_hashing_and_checking_works ) ]


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
    ; ("db-libs", Test_db_libs.suite)
    ; ("api-libs", Test_api_libs.suite)
    ; ("framework", Test_framework.suite)
    ; ("other-libs", Test_other_libs.suite) ]
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
