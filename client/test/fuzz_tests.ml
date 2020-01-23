open Prelude
open Fluid
open Fluid_test_data
open Fluid_fuzzer
module K = FluidKeyboard

(* See docs/fuzzer.md for documentation on how to use this. *)

(* ------------------ *)
(* Cmd-line args *)
(* ------------------ *)
let process_cmdline_args () =
  let command = ref None in
  Tc.Array.iter Sys.argv ~f:(fun str ->
      match (!command, str) with
      | None, "--pattern"
      | None, "--size"
      | None, "--seed"
      | None, "--verbosityThreshold" ->
          command := Some str
      | None, "--stopOnFail" ->
          Fluid_fuzzer.stopOnFail := true
      | None, "--continue" ->
          Fluid_fuzzer.continue := true
      | None, "--help" ->
          Js.log
            "Run Dark's client-side fuzzer. Supported arguments:\n  --seed: set the seed (otherwise uses timestamp)\n  --continue: continue running after first test case\n  --stopOnFail: stop on the first failed test case\n  --size: the size of the test cases\n  --verbosityThreshold: once the number of expressions drops below this number, start printing more verbosity\n  --help: Print this message\n  --pattern 'some-regex': Only run tests that contains this regex" ;
          exit 0
      | Some "--pattern", str ->
          Tester.pattern := Some (Js.Re.fromString str) ;
          command := None
      | Some "--seed", str ->
          Fluid_fuzzer.initialSeed := int_of_string str ;
          command := None
      | Some "--size", str ->
          Fluid_fuzzer.size := int_of_string str ;
          command := None
      | Some "--verbosityThreshold", str ->
          Fluid_fuzzer.verbosityThreshold := int_of_string str ;
          command := None
      | None, _ when Tc.String.endsWith str ~suffix:"fuzz_tests.bs.js" ->
          (* ignore the filename *)
          ()
      | None, "/usr/bin/node" ->
          (* ignore *)
          ()
      | _ ->
          Js.log ("Unsupported command line argument: " ^ str))


(* ------------------ *)
(* Keyboard-based fuzzing *)
(* ------------------ *)
let keypress (key : K.key) : fluidInputEvent =
  Keypress
    {key; shiftKey = false; altKey = false; metaKey = false; ctrlKey = false}


let processMsg (inputs : fluidInputEvent list) (s : fluidState) (ast : E.t) :
    E.t * fluidState =
  let h = Fluid_utils.h ast in
  let m = {defaultTestModel with handlers = Handlers.fromList [h]} in
  List.foldl inputs ~init:(ast, s) ~f:(fun input (ast, s) ->
      updateMsg m h.hTLID ast (FluidInputEvent input) s)


(* ------------------ *)
(* The actual tests *)
(* ------------------ *)
let deleteAllTest : FuzzTest.t =
  { name = "delete-all deletes all"
  ; check =
      (fun ~testcase:_ ~newAST newState ->
        toText newAST = "   " && newState.newPos = 0)
  ; ignore = (fun _ -> false)
  ; fn =
      (fun testcase ->
        let selectAll = keypress K.SelectAll in
        processMsg [selectAll; keypress K.Backspace] defaultTestState testcase)
  }


let copyPasteTest : FuzzTest.t =
  { name = "copy paste roundtrips successfully"
  ; check = (fun ~testcase ~newAST _ -> toText testcase = toText newAST)
  ; ignore = (function EString _ -> true | _ -> false)
  ; fn =
      (fun testcase ->
        (Fluid_clipboard_test.execute_roundtrip testcase, defaultTestState)) }


let encodingRoundtrip : FuzzTest.t =
  { name = "encoder/decoder roundtrips successfully"
  ; check = (fun ~testcase ~newAST _ -> testcase = newAST)
  ; ignore = (fun _ -> false)
  ; fn =
      (fun testcase ->
        (testcase |> Encoders.fluidExpr |> Decoders.fluidExpr, defaultTestState))
  }


(* ------------------ *)
(* Run the tests *)
(* ------------------ *)

(* See docs/fuzzer.md for documentation on how to use this. *)
let () =
  Tester.verbose := true ;
  process_cmdline_args () ;
  runTest deleteAllTest ;
  runTest encodingRoundtrip ;
  runTest copyPasteTest ;
  ()
