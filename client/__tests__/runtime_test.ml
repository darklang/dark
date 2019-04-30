open! Tc

(* open Types *)
open Jest
open Expect
open Runtime

type ('a, 'b) transformation_test_result =
  | Pass
  | Fail of 'a * 'b

let () =
  describe "runtime" (fun () ->
      test "string with newline is valid" (fun () ->
          expect (isValidStringLiteral "\\n") |> toEqual true ) ;
      test "string with carriage return is valid" (fun () ->
          expect (isValidStringLiteral "\\t") |> toEqual true ) ;
      test "string with tab is valid" (fun () ->
          expect (isValidStringLiteral "\\r") |> toEqual true ) ;
      test "string with escaped backslash is valid" (fun () ->
          expect (isValidStringLiteral "\\\\") |> toEqual true ) ;
      test "string with naked backslash is invalid" (fun () ->
          expect (isValidStringLiteral "\\") |> toEqual false ) ;
      test "string with invalid backslashed char is invalid" (fun () ->
          expect (isValidStringLiteral "\\6") |> toEqual false ) ;
      () ) ;
  ()
