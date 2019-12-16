let domTests = ref false

let process_cmdline_args () =
  let command = ref None in
  Tc.Array.iter Sys.argv ~f:(fun str ->
      match (!command, str) with
      | None, "--pattern" ->
          command := Some str
      | None, "--dom" ->
          domTests := true
      | None, "--verbose" ->
          Tester.verbose := true
      | None, "--help" ->
          Js.log
            "Run Dark's client-side unit tests. Supported arguments:\n  --dom: run the DOM tests (slow)\n  --verbose: print test names\n  --help: Print this message\n  --pattern 'some-regex': Run any test that contains this regex"
      | Some "--pattern", str ->
          Tester.pattern := Some (Js.Re.fromString str) ;
          command := None
      | None, _
        when Tc.String.contains
               str
               ~substring:"lib/js/__tests__/unittests.bs.js" ->
          (* ignore the filename (can't use the whole name as
           * assert-in-container rewrites it *)
          ()
      | None, "/usr/bin/node" ->
          (* ignore *)
          ()
      | _ ->
          Js.log ("Unsupported command line argument: " ^ str) )


let () =
  process_cmdline_args () ;
  Analysis_test.run () ;
  Ast_test.run () ;
  Autocomplete_test.run () ;
  Curl_test.run () ;
  Fluid_clipboard_test.run () ;
  Darkstorage_test.run () ;
  Fluid_ac_test.run () ;
  Fluid_pattern_test.run () ;
  Fluid_test.run () ;
  Fluid_token_test.run () ;
  Introspect_test.run () ;
  Porting_test.run () ;
  Refactor_test.run () ;
  Rpc_test.run () ;
  Runtime_test.run () ;
  View_blankor.run () ;
  if !domTests
  then (
    DOMSupport.init () ;
    Page_test.run () ) ;
  Tester.finish ()
