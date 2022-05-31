open Prelude
open Fluid
open FluidTestData
open FluidFuzzer
module K = FluidKeyboard

/* See docs/fuzzer.md for documentation on how to use this. */

/* ------------------ */
/* Cmd-line args */
/* ------------------ */
let process_cmdline_args = () => {
  let command = ref(None)
  Tc.Array.forEach(Sys.argv, ~f=str =>
    switch (command.contents, str) {
    | (None, "--pattern")
    | (None, "--size")
    | (None, "--maxTestSize")
    | (None, "--seed")
    | (None, "--verbosityThreshold") =>
      command := Some(str)
    | (None, "--stopOnFail") => FluidFuzzer.stopOnFail := true
    | (None, "--continue") => FluidFuzzer.continue := true
    | (None, "--help") =>
      Js.log(
        "Run Dark's client-side fuzzer. Supported arguments:\n  --seed: set the seed (otherwise uses timestamp)\n  --continue: continue running after first test case\n  --stopOnFail: stop on the first failed test case\n  --size: the size of the test cases\n  --maxTestSize: the maximum number of elements (exprs or patterns) in a test case\n  --verbosityThreshold: once the number of expressions drops below this number, start printing more verbosity\n  --help: Print this message\n  --pattern 'some-regex': Only run tests that contains this regex",
      )
      exit(0)
    | (Some("--pattern"), str) =>
      Tester.pattern := Some(Js.Re.fromString(str))
      command := None
    | (Some("--seed"), str) =>
      FluidFuzzer.initialSeed := int_of_string(str)
      command := None
    | (Some("--maxTestSize"), str) =>
      FluidFuzzer.maxTestSize := int_of_string(str)
      command := None
    | (Some("--size"), str) =>
      FluidFuzzer.itemSize := int_of_string(str)
      command := None
    | (Some("--verbosityThreshold"), str) =>
      FluidFuzzer.verbosityThreshold := int_of_string(str)
      command := None
    | (None, _) if Tc.String.endsWith(str, ~suffix="fuzz_tests.bs.js") => /* ignore the filename */
      ()
    | (None, "/usr/bin/node") => /* ignore */
      ()
    | _ => Js.log("Unsupported command line argument: " ++ str)
    }
  )
}

/* ------------------ */
/* Keyboard-based fuzzing */
/* ------------------ */
let keypress = (key: K.key): fluidInputEvent => Keypress({
  key: key,
  shiftKey: false,
  altKey: false,
  metaKey: false,
  ctrlKey: false,
})

let processMsg = (inputs: list<fluidInputEvent>, s: fluidState, ast: E.t): (E.t, fluidState) => {
  let h = FluidUtils.h(ast)
  let m = {...defaultTestModel, handlers: Handlers.fromList(list{h})}
  List.fold(inputs, ~initial=(h.ast, s, list{}), ~f=((ast, s, _), input) =>
    updateMsg(m, h.hTLID, ast, s, FluidInputEvent(input))
  ) |> (((ast, s, _)) => (FluidAST.toExpr(ast), s))
}

/* ------------------ */
/* The actual tests */
/* ------------------ */
let deleteAllTest: FuzzTest.t = {
  name: "delete-all deletes all",
  check: (~testcase as _, ~newAST, newState) => toText(newAST) == "   " && newState.newPos == 0,
  ignore: _ => false,
  fn: testcase =>
    processMsg(list{keypress(K.SelectAll), DeleteContentBackward}, defaultTestState, testcase),
}

let copyPasteTest: FuzzTest.t = {
  module E = FluidExpression
  {
    name: "copy paste roundtrips successfully",
    check: (~testcase, ~newAST, _) => toText(testcase) == toText(newAST),
    ignore: ast =>
      /* the copy/paste algorithm doesn't work properly for partials */
      E.filter(ast, ~f=x =>
        switch x {
        | EPartial(_) | ERightPartial(_) => true
        | _ => false
        }
      ) |> \"<>"(list{}),
    fn: testcase => (TestFluidClipboard.execute_roundtrip(testcase), defaultTestState),
  }
}

let encodingRoundtrip: FuzzTest.t = {
  name: "encoder/decoder roundtrips successfully",
  check: (~testcase, ~newAST, _) => testcase == newAST,
  ignore: _ => false,
  fn: testcase => (testcase |> Encoders.fluidExpr |> Decoders.fluidExpr, defaultTestState),
}

let longLines: FuzzTest.t = {
  name: "no lines above 120 chars",
  check: (~testcase as _, ~newAST, _) => {
    let allTokens = FluidTokenizer.tokenize(newAST)
    List.all(allTokens, ~f=ti => ti.startCol + ti.length <= 120)
  },
  ignore: _ => false,
  fn: testcase => (testcase, defaultTestState),
}

/* ------------------ */
/* Run the tests */
/* ------------------ */

/* See docs/fuzzer.md for documentation on how to use this. */
let () = {
  Tester.verbose := true
  process_cmdline_args()
  runTest(longLines)
  runTest(deleteAllTest)
  runTest(encodingRoundtrip)
  runTest(copyPasteTest)
  ()
}
