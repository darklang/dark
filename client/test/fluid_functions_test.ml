(* Tests for functions in Fluid.ml that does not conform well with the tests in fluid-test.ml  *)

open Tester
open Prelude
open Fluid
module B = BlankOr
module K = FluidKeyboard
module E = FluidExpression
open Fluid_test_data

let run () =
  describe "getSelectedExprID" (fun () ->
      test "nothing selected" (fun () ->
          let s = {defaultTestState with newPos = 2} in
          let ast = plainIf in
          expect (getSelectedExprID s ast) |> toEqual None) ;
      test "select atomic expression" (fun () ->
          let ast = let' "a" (EInteger (ID "letVal", "1999")) b in
          let s =
            { defaultTestState with
              oldPos = 8
            ; newPos = 12
            ; selectionStart = Some 8 }
          in
          expect (getSelectedExprID s ast) |> toEqual (Some (ID "letVal"))) ;
      test "select larger expressions" (fun () ->
          let ast = EFnCall (ID "fn", "+", [int "1"; int "2"], NoRail) in
          let s =
            { defaultTestState with
              oldPos = 0
            ; newPos = 4
            ; selectionStart = Some 0 }
          in
          expect (getSelectedExprID s ast) |> toEqual (Some (ID "fn"))) ;
      test "selects part of AST" (fun () ->
          let ast =
            let' "a" (EFnCall (ID "fn", "+", [int "1"; int "2"], NoRail)) b
          in
          let s =
            { defaultTestState with
              oldPos = 8
            ; newPos = 12
            ; selectionStart = Some 8 }
          in
          expect (getSelectedExprID s ast) |> toEqual (Some (ID "fn"))) ;
      ()) ;
  ()
