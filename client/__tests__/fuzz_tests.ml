open Tc
open Types
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
      | None, "--count"
      | None, "--initialSeed"
      | None, "--verbosityThreshold" ->
          command := Some str
      | None, "--help" ->
          Js.log
            "Run Dark's client-side fuzzer. Supported arguments:\n  --initialSeed: change the seed\n  --count: run count number of tests\n  --verbosityThreshold: once the number of expressions drops below this number, start printing more verbosity\n  --help: Print this message\n  --pattern 'some-regex': Only run tests that contains this regex" ;
          exit 0
      | Some "--pattern", str ->
          Tester.pattern := Some (Js.Re.fromString str) ;
          command := None
      | Some "--count", str ->
          Fluid_fuzzer.count := int_of_string str ;
          command := None
      | Some "--initialSeed", str ->
          Fluid_fuzzer.initialSeed := int_of_string str ;
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
type modifierKeys =
  { shiftKey : bool
  ; altKey : bool
  ; metaKey : bool
  ; ctrlKey : bool }

let noModifiers =
  {shiftKey = false; altKey = false; metaKey = false; ctrlKey = false}


let processMsg (keys : (K.key * modifierKeys) list) (s : fluidState) (ast : E.t)
    : E.t * fluidState =
  let h = Fluid_utils.h ast in
  let m = {defaultTestModel with handlers = Handlers.fromList [h]} in
  List.foldl keys ~init:(ast, s) ~f:(fun (key, modifierKeys) (ast, s) ->
      updateMsg
        m
        h.hTLID
        ast
        (FluidKeyPress
           { key
           ; shiftKey = modifierKeys.shiftKey
           ; altKey = modifierKeys.altKey
           ; metaKey = modifierKeys.metaKey
           ; ctrlKey = modifierKeys.ctrlKey })
        s)


(* ------------------ *)
(* The actual tests *)
(* ------------------ *)
let deleteAllTest : FuzzTest.t =
  { name = "delete-all deletes all"
  ; check =
      (fun ~testcase:_ ~newAST newState ->
        toText newAST = "___" && newState.newPos = 0)
  ; fn =
      (fun testcase ->
        processMsg
          [ (K.SelectAll, {noModifiers with shiftKey = true})
          ; (K.Backspace, noModifiers) ]
          defaultTestState
          testcase) }


let copyPasteTest : FuzzTest.t =
  { name = "copy paste roundtrips successfully"
  ; check = (fun ~testcase ~newAST _ -> toText testcase = toText newAST)
  ; fn =
      (fun testcase ->
        (Fluid_clipboard_test.execute_roundtrip testcase, defaultTestState)) }


(* ------------------ *)
(* Run the tests *)
(* ------------------ *)

(* See docs/fuzzer.md for documentation on how to use this. *)
let () =
  Tester.verbose := true ;
  process_cmdline_args () ;
  runTest deleteAllTest ;
  runTest copyPasteTest ;
  ()
