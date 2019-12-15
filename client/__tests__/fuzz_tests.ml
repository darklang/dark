open Tester
open Tc
open Types
open Fluid
open Fluid_test_data
module K = FluidKeyboard

(* See docs/fuzzer.md for documentation on how to use this. *)
type modifierKeys =
  { shiftKey : bool
  ; altKey : bool
  ; metaKey : bool
  ; ctrlKey : bool }

let noModifiers =
  {shiftKey = false; altKey = false; metaKey = false; ctrlKey = false}


let processMsg
    (keys : (K.key * modifierKeys) list) (s : fluidState) (ast : ast) :
    ast * fluidState =
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
        s )


let insertCaret (index : int) (str : string) : string =
  let caretString = "~" in
  let a, b = String.splitAt ~index str in
  String.join ~sep:caretString [a; b]


(* See docs/fuzzer.md for documentation on how to use this. *)

let toText ast = eToString defaultTestState ast

let testFn testcase =
  processMsg
    [ (K.SelectAll, {noModifiers with shiftKey = true})
    ; (K.Backspace, noModifiers) ]
    defaultTestState
    testcase


let testChecker (newAST : fluidExpr) (newState : fluidState) =
  toText newAST = "___" && newState.newPos = 0


let testName = "delete-all deletes all"

let () =
  describe "Fixing delete-all" (fun () ->
      let testsToRun = 3 in
      try
        for i = 1 to testsToRun do
          let name = testName ^ " #" ^ string_of_int i in
          test name (fun () ->
              Fluid_fuzzer.setSeed i ;
              let testcase =
                Fluid_fuzzer.generateExpr ()
                |> Fluid.clone ~state:defaultTestState
              in
              Js.log2 "testing: " name ;
              let newAST, newState = testFn testcase in
              Js.log2 "checking: " name ;
              let passed = testChecker newAST newState in
              if passed = false
              then (
                Js.log2 "failed: " name ;
                let reduced =
                  Fluid_fuzzer.reduce testcase testFn testChecker
                in
                let text = toText reduced in
                Js.log2 "finished program:\n" text ;
                Js.log2 "structure" (show_fluidExpr reduced) ;
                expect false |> toEqual true )
              else expect true |> toEqual true )
        done
      with _ -> () )
