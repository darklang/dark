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


(* ------------------- *)
(* Test setup *)
(* ------------------- *)

let suite =
  [ ("Dark code can't curl file:// urls", `Quick, t_curl_file_urls)
  ; ("Test postgres escaping", `Quick, t_escape_pg_escaping) ]


let () =
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
  Libbackend.Init.init ~run_side_effects:true ;
  Log.set_level `All ;
  Account.init_testing () ;
  let wrapped_suites =
    let wrap f () =
      try f () with e ->
        Exception.reraise_after e (fun bt ->
            print_endline (Exception.to_string e) ;
            print_endline (Exception.backtrace_to_string bt) )
    in
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
