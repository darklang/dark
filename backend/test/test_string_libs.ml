open Core_kernel
open Libexecution
open Types.RuntimeT
open Utils

let t_string_length_v1_works_on_emoji () =
  check_dval
    "stringLength"
    (exec_ast "(String::length_v1 '\xef\xbf\xbd')")
    (DInt 1)


let t_string_uppercase_works_for_ascii_range () =
  check_dval
    "stringUppercaseASCII"
    (exec_ast "(String::toUppercase_v1 'abcdef')")
    (Dval.dstr_of_string_exn "ABCDEF")


let t_string_lowercase_works_for_ascii_range () =
  check_dval
    "stringLowercaseASCII"
    (exec_ast "(String::toLowercase_v1 'ABCDEF')")
    (Dval.dstr_of_string_exn "abcdef")


let t_string_uppercase_v1_works_on_mixed_strings () =
  check_dval
    "stringUpppercaseMixed"
    (exec_ast "(String::toUppercase_v1 'hello\xf0\x9f\x98\x84world')")
    (Dval.dstr_of_string_exn "HELLO\xf0\x9f\x98\x84WORLD")


let t_string_split_works_for_emoji () =
  check_dval
    "stringSplit"
    (exec_ast "(String::split 'hello\xf0\x9f\x98\x84world' '\xf0\x9f\x98\x84')")
    (DList [Dval.dstr_of_string_exn "hello"; Dval.dstr_of_string_exn "world"])


let t_string_uppercase_v1_works_on_non_ascii_strings () =
  check_dval
    "stringUpppercaseMixed"
    (exec_ast "(String::toUppercase_v1 'żółw')")
    (Dval.dstr_of_string_exn "ŻÓŁW")


let suite =
  [ ( "String::length_v2 returns the correct length for a string containing an emoji"
    , `Quick
    , t_string_length_v1_works_on_emoji )
  ; ( "String::toUppercase_v1 works for ASCII range"
    , `Quick
    , t_string_uppercase_works_for_ascii_range )
  ; ( "String::toLowercase_v1 works for ASCII range"
    , `Quick
    , t_string_lowercase_works_for_ascii_range )
  ; ( "String::toUppercase_v1 works on mixed strings"
    , `Quick
    , t_string_uppercase_v1_works_on_mixed_strings )
  ; ( "String::toUppercase_v1 works on non-ascii strings"
    , `Quick
    , t_string_uppercase_v1_works_on_non_ascii_strings )
  ; ( "String split works on strings with emoji + ascii"
    , `Quick
    , t_string_split_works_for_emoji ) ]
