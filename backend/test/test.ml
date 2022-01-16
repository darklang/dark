open Core_kernel
open Libcommon
open Libexecution
open Libbackend
module Account = Libbackend.Account
module File = Libbackend_basics.File

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
    ; ("framework", Test_framework.suite)
    ; ("other-libs", Test_other_libs.suite)
    ; ("analysis", Test_analysis.suite)
    ; ("package_manager", Test_package_manager.suite)
    ; ("garbage_collection", Test_garbage_collection.suite)
    ; ("event-queue", Test_event_queue.suite) ]
  in
  Init.init ~run_migrations:true ~run_side_effects:true ;
  Log.set_level `All ;
  Account.init_testing () ;
  let wrapped_suites =
    let wrap testname f () =
      try
        Utils.comment := testname ;
        Utils.code := None ;
        f ()
      with e ->
        Exception.reraise_after e (fun bt ->
            print_endline (Exception.to_string e) ;
            print_endline (Exception.backtrace_to_string bt))
    in
    List.map suites ~f:(fun (suitename, ts) ->
        let pre () = Utils.pre_suite suitename in
        let post () = Utils.post_suite suitename in
        let pre_test = ("pre-" ^ suitename, `Quick, pre) in
        let post_test = ("post-" ^ suitename, `Quick, post) in
        let tests =
          List.map ts ~f:(fun (testname, speed, t) ->
              (testname, speed, wrap testname t))
        in
        (suitename, (pre_test :: tests) @ [post_test]))
  in
  let suite, exit = Junit_alcotest.run_and_report "all" wrapped_suites in
  let report = Junit.make [suite] in
  File.mkdir ~root:Testresults "" ;
  let file = File.check_filename ~mode:`Write ~root:Testresults "backend.xml" in
  Junit.to_file report file ;
  exit ()
