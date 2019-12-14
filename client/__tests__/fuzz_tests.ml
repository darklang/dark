open Jest
open Tc
open Fluid
module Regex = Util.Regex
module B = Blank
module K = FluidKeyboard

(* See docs/fuzzer.md for documentation on how to use this. *)

let () =
  describe "Fixing delete-all" (fun () ->
      let testsToRun = 0 in
      let state = Fluid_test.defaultTestState in
      (* See docs/fuzzer.md for documentation on how to use this. *)
      try
        for i = 1 to testsToRun do
          Fluid_fuzzer.setSeed i ;
          let testcase = Fluid_fuzzer.generateExpr () in
          let text = eToString state testcase in
          Js.log2 "index" i ;
          Js.log2 "text" text ;
          Js.log2 "structure" (eToStructure state testcase) ;
          let length = String.length text in
          Fluid_test.t
            ("delete-all deletes all #" ^ string_of_int i)
            testcase
            (Fluid_test.keys ~wrap:false [K.SelectAll; K.Backspace] (length - 1))
            "~___"
        done
      with _ -> () )
