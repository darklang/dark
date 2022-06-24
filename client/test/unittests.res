// See docs/unittests.md for documentation on how to use this.

let domTests = ref(false)

let process_cmdline_args = () => {
  let command = ref(None)
  Tc.Array.forEach(Sys.argv, ~f=str =>
    switch (command.contents, str) {
    | (None, "--pattern") => command := Some(str)
    | (None, "--dom") => domTests := true
    | (None, "--verbose") => Tester.verbose := true
    | (None, "--help") =>
      Js.log(
        "Run Dark's client-side unit tests. Supported arguments:\n  --dom: run the DOM tests (slow)\n  --verbose: print test names\n  --help: Print this message\n  --pattern 'some-regex': Run any test that contains this regex",
      )
      exit(0)
    | (Some("--pattern"), str) =>
      Tester.pattern := Some(Js.Re.fromString(str))
      command := None
    | (None, _) if Tc.String.endsWith(str, ~suffix="unittests.bs.js") => // ignore the filename
      ()
    | (None, "/usr/bin/node") => // ignore
      ()
    | _ => Js.log("Unsupported command line argument: " ++ str)
    }
  )
}

// See docs/unittests.md for documentation on how to use this.
let () = {
  open Tester
  process_cmdline_args()
  describe("Analysis_test", TestAnalysis.run)
  describe("APIError test", TestApiError.run)
  describe("Ast_test", TestAst.run)
  describe("Autocomplete_test", TestAutocomplete.run)
  describe("Curl_test", TestCurl.run)
  describe("Darkstorage_test", TestDarkstorage.run)
  describe("Decoder test", TestDecoder.run)
  describe("Encoder test", TestEncoder.run)
  describe("Feature Flag test", TestFeatureFlag.run)
  describe("Fluid_ac_test", TestFluidAutocomplete.run)
  describe("Fluid_clipboard_test", TestFluidClipboard.run)
  describe("Fluid_command_test", TestFluidCommands.run)
  describe("Fluid_expression_test", TestFluidExpressions.run)
  describe("Fluid_pattern_test", TestFluidPattern.run)
  describe("Fluid_selection_test", TestFluidSelection.run)
  describe("Fluid_test", TestFluid.run)
  describe("Fluid_token_test", TestFluidToken.run)
  describe("Introspect_test", TestIntrospect.run)
  describe("Porting_test", TestPorting.run)
  describe("Refactor_test", TestRefactor.run)
  describe("Rpc_test", TestRpcs.run)
  describe("Runtime_test", TestRuntime.run)
  describe("User_functions_test", TestUserfn.run)
  describe("View_blankor", TestViewBlankor.run)
  describe("Prettydocs_test", TestPrettyDocs.run)
  describe("Fluid_printer_test", TestFluidPrinter.run)
  describe("Tablecloth_test", TestTablecloth.run)
  describe("Util_test", TestUtils.run)
  if domTests.contents {
    DOMSupport.init()
    describe("Page_test", TestPage.run)
  }
  Tester.finish()
}
