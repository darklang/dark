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
      let t name subject expected =
        test name (fun () ->
            expect (isValidStringLiteral subject) |> toEqual expected )
      in
      t "newline" "\\n" true ;
      t "carriage return" "\\t" true ;
      t "tab" "\\r" true ;
      t "escaped backslash" "\\\\" true ;
      t "escaped quote" "\\\"" true ;
      t "naked backslash" "\\" false ;
      t "invalid backslashed char" "\\6" false ;
      t "many" "\\n\\t\\r\\\\\\\"" true ;
      () ) ;
  describe "convertLiteralStringToDisplay" (fun () ->
      let t name subject expected =
        test name (fun () ->
            expect (convertLiteralToDisplayString subject) |> toEqual expected
        )
      in
      t "newline" "\n" "\\n" ;
      t "newline2" "asd\nqwe" "asd\\nqwe" ;
      t "carriage return" "\t" "\\t" ;
      t "carriage return2" "asd\tqwe" "asd\\tqwe" ;
      t "tab" "\r" "\\r" ;
      t "tab2" "asd\rqwe" "asd\\rqwe" ;
      t "escaped backslash" "\\" "\\\\" ;
      t "escaped backslash2" "asd\\qwe" "asd\\\\qwe" ;
      t "escaped quote" "\"" "\\\"" ;
      t "escaped quote2" "asd\"qwe" "asd\\\"qwe" ;
      t
        "many"
        "asd\n\t\r\n\t\r\n\t\r\"\"\"qwe"
        "asd\\n\\t\\r\\n\\t\\r\\n\\t\\r\\\"\\\"\\\"qwe" ;
      () ) ;
  describe "convertDisplayStringToLiteral " (fun () ->
      let t name subject expected =
        test name (fun () ->
            expect (convertDisplayStringToLiteral subject) |> toEqual expected
        )
      in
      t "newline" "\\n" "\n" ;
      t "newline2" "asd\\nqwe" "asd\nqwe" ;
      t "carriage return" "\\t" "\t" ;
      t "carriage return2" "asd\\tqwe" "asd\tqwe" ;
      t "tab2" "\\r" "\r" ;
      t "tab2 " "asd\\rqwe" "asd\rqwe" ;
      t "escaped backslash" "\\\\" "\\" ;
      t "escaped backslash2" "asd\\\\qwe" "asd\\qwe" ;
      t "escaped quote" "\\\"" "\"" ;
      t "escaped quote2" "asd\\\"qwe" "asd\"qwe" ;
      () ) ;
  ()
