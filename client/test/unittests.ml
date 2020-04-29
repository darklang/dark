(* See docs/unittests.md for documentation on how to use this. *)

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
            "Run Dark's client-side unit tests. Supported arguments:\n  --dom: run the DOM tests (slow)\n  --verbose: print test names\n  --help: Print this message\n  --pattern 'some-regex': Run any test that contains this regex" ;
          exit 0
      | Some "--pattern", str ->
          Tester.pattern := Some (Js.Re.fromString str) ;
          command := None
      | None, _ when Tc.String.endsWith str ~suffix:"unittests.bs.js" ->
          (* ignore the filename *)
          ()
      | None, "/usr/bin/node" ->
          (* ignore *)
          ()
      | _ ->
          Js.log ("Unsupported command line argument: " ^ str))


(* See docs/unittests.md for documentation on how to use this. *)
let () =
  let open Tester in
  process_cmdline_args () ;
  describe "Analysis_test" Analysis_test.run ;
  describe "Ast_test" Ast_test.run ;
  describe "Autocomplete_test" Autocomplete_test.run ;
  describe "Curl_test" Curl_test.run ;
  describe "Darkstorage_test" Darkstorage_test.run ;
  describe "Encoder test" Encoder_test.run ;
  describe "Feature Flag test" Feature_flag_test.run ;
  describe "Fluid_ac_test" Fluid_ac_test.run ;
  describe "Fluid_clipboard_test" Fluid_clipboard_test.run ;
  describe "Fluid_command_test" Fluid_commands_test.run ;
  describe "Fluid_expression_test" Fluid_expression_test.run ;
  describe "Fluid_pattern_test" Fluid_pattern_test.run ;
  describe "Fluid_selection_test" Fluid_selection_test.run ;
  describe "Fluid_test" Fluid_test.run ;
  describe "Fluid_token_test" Fluid_token_test.run ;
  describe "Introspect_test" Introspect_test.run ;
  describe "Porting_test" Porting_test.run ;
  describe "Refactor_test" Refactor_test.run ;
  describe "Rpc_test" Rpc_test.run ;
  describe "Runtime_test" Runtime_test.run ;
  describe "User_functions_test" Userfn_test.run ;
  describe "View_blankor" View_blankor.run ;
  describe "Prettydocs_test" Prettydocs_test.run ;
  describe "Fluid_printer_test" Fluid_printer_test.run ;
  if !domTests
  then (
    DOMSupport.init () ;
    describe "Page_test" Page_test.run ) ;
  Tester.finish ()
