open Core_kernel
open Libexecution
open Types.RuntimeT
open Utils
open Libshared.FluidShortcuts

let t_string_length_v1_works_on_emoji () =
  check_dval
    "stringLength"
    (exec_ast (fn "String::length_v1" [str "\xef\xbf\xbd"]))
    (DInt Dint.one)


let t_string_uppercase_works_for_ascii_range () =
  check_dval
    "stringUppercaseASCII"
    (exec_ast (fn "String::toUppercase_v1" [str "abcdef"]))
    (Dval.dstr_of_string_exn "ABCDEF")


let t_string_lowercase_works_for_ascii_range () =
  check_dval
    "stringLowercaseASCII"
    (exec_ast (fn "String::toLowercase_v1" [str "ABCDEF"]))
    (Dval.dstr_of_string_exn "abcdef")


let t_string_uppercase_v1_works_on_mixed_strings () =
  check_dval
    "stringUpppercaseMixed"
    (exec_ast (fn "String::toUppercase_v1" [str "hello\xf0\x9f\x98\x84world"]))
    (Dval.dstr_of_string_exn "HELLO\xf0\x9f\x98\x84WORLD")


let t_string_split_works_for_emoji () =
  check_dval
    "stringSplit"
    (exec_ast
       (fn
          "String::split"
          [str "hello\xf0\x9f\x98\x84world"; str "\xf0\x9f\x98\x84"]))
    (DList [Dval.dstr_of_string_exn "hello"; Dval.dstr_of_string_exn "world"])


let t_string_uppercase_v1_works_on_non_ascii_strings () =
  check_dval
    "stringUpppercaseMixed"
    (exec_ast (fn "String::toUppercase_v1" [str "żółw"]))
    (Dval.dstr_of_string_exn "ŻÓŁW")


let t_string_trim_noop () =
  check_dval
    "stringTrimNoop"
    (exec_ast (fn "String::trim" [str "foo"]))
    (Dval.dstr_of_string_exn "foo")


let t_string_trim_left_trivial () =
  check_dval
    "stringTrimLeftTrivial"
    (exec_ast (fn "String::trim" [str "  foo"]))
    (Dval.dstr_of_string_exn "foo")


let t_string_trim_right_trivial () =
  check_dval
    "stringTrimRightTrivial"
    (exec_ast (fn "String::trim" [str "foo  "]))
    (Dval.dstr_of_string_exn "foo")


let t_string_trim_both_trivial () =
  check_dval
    "stringTrimBothTrivial"
    (exec_ast (fn "String::trim" [str "  foo  "]))
    (Dval.dstr_of_string_exn "foo")


let t_string_trim_both_not_inner_trivial () =
  check_dval
    "stringTrimBothNotInnerTrivial"
    (exec_ast (fn "String::trim" [str "  foo bar  "]))
    (Dval.dstr_of_string_exn "foo bar")


let t_string_trim_both_not_inner_unicode () =
  check_dval
    "stringTrimBothUnicode"
    (* Leading em-space, inner thin space, trailing space *)
    (exec_ast (fn "String::trim" [str " \xe2\x80\x83foo\xe2\x80\x83bar "]))
    (Dval.dstr_of_string_exn "foo\xe2\x80\x83bar")


let t_string_trim_all () =
  check_dval
    "stringTrimAll"
    (exec_ast (fn "String::trim" [str "      "]))
    (Dval.dstr_of_string_exn "")


let t_string_trim_preserves_emoji () =
  check_dval
    "stringTrimPreservesEmoji"
    (exec_ast
       (fn "String::trim" [str " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 "]))
    (Dval.dstr_of_string_exn "\xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84")


let t_html_escaping () =
  check_dval
    "html escaping works"
    (* TODO: add back in check that `'` is correctly escaped. It didn't
     * play nice with our hacky `'` removal in the DSL parser *)
    (Dval.dstr_of_string_exn "test&lt;&gt;&amp;&quot;")
    (exec_ast (fn "String::htmlEscape" [str "test<>&\""]))


let t_slugify_works () =
  check_dval
    "slugify_v1 escaping works"
    (Dval.dstr_of_string_exn
       "my-super-really-excellent-uber-amazing-very-clever-thing-coffee")
    (exec_ast
       (fn
          "String::slugify_v1"
          [ str
              "  m@y  'super'  really- excellent *uber_ amazing* ~very  ~ \"clever\" thing: coffee😭!"
          ])) ;
  check_dval
    "slugify_v2 escaping works"
    (Dval.dstr_of_string_exn
       "my-super-really-excellent-uber-amazing-very-5x5-clever-thing-coffee")
    (exec_ast
       (fn
          "String::slugify_v2"
          [ str
              "  M@y  'super'  Really- exce+llent *Uber_ ama\"zing* ~very   5x5 ~ \"clever\" thing: coffee😭!"
          ])) ;
  ()


let t_string_append_works_for_ascii_range () =
  check_dval
    "stringAppendASCII"
    (exec_ast (fn "String::append_v1" [str "hello"; str "world"]))
    (Dval.dstr_of_string_exn "helloworld")


let t_string_append_works_on_non_ascii_strings () =
  check_dval
    "stringAppendMixed"
    (exec_ast (fn "String::append_v1" [str "żółw"; str "\xf0\x9f\x98\x84"]))
    (Dval.dstr_of_string_exn "żółw\xf0\x9f\x98\x84")


let t_string_prepend_works_for_ascii_range () =
  check_dval
    "stringPrependASCII"
    (exec_ast (fn "String::prepend" [str "hello"; str "world"]))
    (Dval.dstr_of_string_exn "worldhello")


let t_string_prepend_works_on_non_ascii_strings () =
  check_dval
    "stringPrependMixed"
    (exec_ast (fn "String::prepend" [str "żółw"; str "\xf0\x9f\x98\x84"]))
    (Dval.dstr_of_string_exn "\xf0\x9f\x98\x84żółw")


let t_uuid_string_roundtrip () =
  let ast =
    let'
      "i"
      (fn "Uuid::generate" [])
      (let'
         "s"
         (fn "toString" [var "i"])
         (let'
            "parsed"
            (fn "String::toUUID" [var "s"])
            (list [var "i"; var "parsed"])))
  in
  AT.check
    AT.int
    "A generated id can round-trip"
    0
    (match exec_ast ast with DList [p1; p2] -> compare_dval p1 p2 | _ -> 1)


let t_substring_works () =
  check_dval
    "substring"
    (exec_ast (fn "String::isSubstring_v1" [str "a string"; str "in"]))
    (DBool true) ;
  check_dval
    "not substring"
    (exec_ast (fn "String::isSubstring_v1" [str "a string"; str "x"]))
    (DBool false)


let t_startsWith_works () =
  check_dval
    "prefix"
    (exec_ast (fn "String::startsWith" [str "a string"; str "a s"]))
    (DBool true) ;
  check_dval
    "not prefix"
    (exec_ast (fn "String::startsWith" [str "a string"; str " s"]))
    (DBool false)


let t_endsWith_works () =
  check_dval
    "suffix"
    (exec_ast (fn "String::endsWith" [str "a string"; str "ing"]))
    (DBool true) ;
  check_dval
    "not suffix"
    (exec_ast (fn "String::endsWith" [str "a string"; str "in"]))
    (DBool false)


let t_toint_works () =
  check_dval
    "String::toInt works"
    (exec_ast (fn "String::toInt" [str "1"]))
    (DInt Dint.one) ;
  check_dval
    "String::toInt works"
    (exec_ast (fn "String::toInt_v1" [str "1"]))
    (DResult (ResOk (Dval.dint 1)))


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
    , t_string_split_works_for_emoji )
  ; ("String trim noops", `Quick, t_string_trim_noop)
  ; ("String trim empties a whitespace only string", `Quick, t_string_trim_all)
  ; ("String trims leading spaces", `Quick, t_string_trim_left_trivial)
  ; ("String trims trailing spaces", `Quick, t_string_trim_right_trivial)
  ; ( "String trims both leading + trailing spaces"
    , `Quick
    , t_string_trim_both_trivial )
  ; ( "String trims both leading + trailing spaces, leaving inner untouched"
    , `Quick
    , t_string_trim_both_not_inner_trivial )
  ; ( "String trims both leading + trailing spaces, leaving inner untouched w/ unicode spaces"
    , `Quick
    , t_string_trim_both_not_inner_unicode )
  ; ( "String trims both leading + trailing spaces, preserving emoji"
    , `Quick
    , t_string_trim_preserves_emoji )
  ; ("HTML escaping works reasonably", `Quick, t_html_escaping)
  ; ("UUIDs round-trip to/from strings", `Quick, t_uuid_string_roundtrip)
  ; ("Slugify works", `Quick, t_slugify_works)
  ; ("substring works", `Quick, t_substring_works)
  ; ("startsWith works", `Quick, t_startsWith_works)
  ; ("endsWith works", `Quick, t_endsWith_works)
  ; ("string_toint_works", `Quick, t_toint_works)
  ; ( "String::append_v1 works for ASCII range"
    , `Quick
    , t_string_append_works_for_ascii_range )
  ; ( "String::append_v1 works on non-ascii strings"
    , `Quick
    , t_string_append_works_on_non_ascii_strings )
  ; ( "String::prepend works for ASCII range"
    , `Quick
    , t_string_prepend_works_for_ascii_range )
  ; ( "String::prepend works on non-ascii strings"
    , `Quick
    , t_string_prepend_works_on_non_ascii_strings ) ]
