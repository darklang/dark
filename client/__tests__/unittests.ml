let domTests = ref false

let process_cmdline_args () =
  let command = ref None in
  Tc.Array.iter Sys.argv ~f:(fun str ->
      match !command with
      | None ->
          if str = "--pattern"
          then command := Some "--pattern"
          else if str = "--dom"
          then domTests := true
          else if str = "--verbose"
          then Tester.verbose := true
      | Some "--pattern" ->
          Tester.pattern := Some (Js.Re.fromString str) ;
          command := None
      | _ ->
          failwith ("Unsupported command line argument: " ^ str) )


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
