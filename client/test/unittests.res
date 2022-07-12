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
  describe("TestAnalysis", TestAnalysis.run)
  describe("TestApiError", TestApiError.run)
  describe("TestAst", TestAst.run)
  describe("TestAutocomplete", TestAutocomplete.run)
  describe("TestCurl", TestCurl.run)
  describe("TestDarkstorage", TestDarkstorage.run)
  describe("TestDecoder test", TestDecoder.run)
  describe("TestEncoder", TestEncoder.run)
  describe("TestFeatureFlag", TestFeatureFlag.run)
  describe("TestFluidAutocomplete", TestFluidAutocomplete.run)
  describe("TestFluidClipboard", TestFluidClipboard.run)
  describe("TestFluidCommands", TestFluidCommands.run)
  describe("TestFluidExpressions", TestFluidExpressions.run)
  describe("TestFluidPattern", TestFluidPattern.run)
  describe("TestFluidselection", TestFluidSelection.run)
  describe("TestFluid", TestFluid.run)
  describe("TestFluidToken", TestFluidToken.run)
  describe("TestIntrospect", TestIntrospect.run)
  describe("TestPorting", TestPorting.run)
  describe("TestRefactor", TestRefactor.run)
  describe("TestRuntime", TestRuntime.run)
  describe("TestUserFn", TestUserfn.run)
  describe("TestViewBlankOr", TestViewBlankor.run)
  describe("TestPrettydocs", TestPrettyDocs.run)
  describe("TestFluidPrinter", TestFluidPrinter.run)
  describe("TestTablecloth", TestTablecloth.run)
  describe("TestUtils", TestUtils.run)
  if domTests.contents {
    DOMSupport.init()
    describe("TestPage", TestPage.run)
  }
  Tester.finish()
}
