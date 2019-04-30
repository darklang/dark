open! Tc

(* open Types *)
open Jest
open Expect
open Runtime

type ('a, 'b) transformation_test_result =
  | Pass
  | Fail of 'a * 'b

let () =
  describe "validStringLiteral" (fun () ->
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
  describe "convertLiteralStringToDisplay " (fun () ->
      test "string with newline is valid" (fun () ->
          expect (convertLiteralStringToDisplay "\\n") |> toEqual "\\\\n" ) ;
      test "string with newline is valid2" (fun () ->
          expect (convertLiteralStringToDisplay "asd\\nqwe")
          |> toEqual "asd\\\\nqwe" ) ;
      test "string with carriage return is valid" (fun () ->
          expect (convertLiteralStringToDisplay "\\t") |> toEqual "\\\\t" ) ;
      test "string with carriage return is valid2" (fun () ->
          expect (convertLiteralStringToDisplay "asd\\tqwe")
          |> toEqual "asd\\\\tqwe" ) ;
      test "string with tab is valid" (fun () ->
          expect (convertLiteralStringToDisplay "\\r") |> toEqual "\\\\r" ) ;
      test "string with tab is valid2" (fun () ->
          expect (convertLiteralStringToDisplay "asd\\rqwe")
          |> toEqual "asd\\\\rqwe" ) ;
      test "string with escaped backslash is valid" (fun () ->
          expect (convertLiteralStringToDisplay "\\") |> toEqual "\\\\" ) ;
      test "string with escaped backslash is valid2" (fun () ->
          expect (convertLiteralStringToDisplay "asd\\qwe")
          |> toEqual "asd\\\\qwe" ) ;
      () ) ;
  describe "convertDisplayStringToLiteral " (fun () ->
      test "string with newline is valid" (fun () ->
          expect (convertDisplayStringToLiteral "\\n") |> toEqual "\n" ) ;
      test "string with newline is valid2" (fun () ->
          expect (convertDisplayStringToLiteral "asd\\nqwe")
          |> toEqual "asd\nqwe" ) ;
      test "string with carriage return is valid" (fun () ->
          expect (convertDisplayStringToLiteral "\\t") |> toEqual "\t" ) ;
      test "string with carriage return is valid2" (fun () ->
          expect (convertDisplayStringToLiteral "asd\\tqwe")
          |> toEqual "asd\tqwe" ) ;
      test "string with tab is valid" (fun () ->
          expect (convertDisplayStringToLiteral "\\r") |> toEqual "\r" ) ;
      test "string with tab is valid2" (fun () ->
          expect (convertDisplayStringToLiteral "asd\\rqwe")
          |> toEqual "asd\rqwe" ) ;
      test "string with escaped backslash is valid" (fun () ->
          expect (convertDisplayStringToLiteral "\\\\") |> toEqual "\\" ) ;
      test "string with escaped backslash is valid2" (fun () ->
          expect (convertDisplayStringToLiteral "asd\\\\qwe")
          |> toEqual "asd\\qwe" ) ;
      () ) ;
  ()
