open Core_kernel
open Libcommon
open Libexecution
open Libbackend

let () =
  let suites =
    [ ("http", Test_http.suite)
    ; ("accounts", Test_account.suite)
    ; ("webserver", Test_webserver.suite)
    ; ("language", Test_language.suite)
    ; ("canvas+ops", Test_canvas_ops.suite)
    ; ("json", Test_json.suite)
    ; ("user_db", Test_user_db.suite)
    ; ("string-libs", Test_string_libs.suite)
    ; ("db-libs", Test_db_libs.suite)
    ; ("api-libs", Test_api_libs.suite)
    ; ("framework", Test_framework.suite)
    ; ("other-libs", Test_other_libs.suite) ]
  in
  Init.init ~run_side_effects:true ;
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
