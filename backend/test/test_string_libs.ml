open Core_kernel
open Libexecution
open Types.RuntimeT
open Utils
open Libshared.FluidShortcuts

let t_string_length_v1_works_on_emoji () =
  check_dval
    "stringLength"
    (exec_ast "(String::length_v1 '\xef\xbf\xbd')")
    (DInt Dint.one)


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


let t_string_trim_noop () =
  check_dval
    "stringTrimNoop"
    (exec_ast "(String::trim 'foo')")
    (Dval.dstr_of_string_exn "foo")


let t_string_trim_left_trivial () =
  check_dval
    "stringTrimLeftTrivial"
    (exec_ast "(String::trim '  foo')")
    (Dval.dstr_of_string_exn "foo")


let t_string_trim_right_trivial () =
  check_dval
    "stringTrimRightTrivial"
    (exec_ast "(String::trim 'foo  ')")
    (Dval.dstr_of_string_exn "foo")


let t_string_trim_both_trivial () =
  check_dval
    "stringTrimBothTrivial"
    (exec_ast "(String::trim '  foo  ')")
    (Dval.dstr_of_string_exn "foo")


let t_string_trim_both_not_inner_trivial () =
  check_dval
    "stringTrimBothNotInnerTrivial"
    (exec_ast "(String::trim '  foo bar  ')")
    (Dval.dstr_of_string_exn "foo bar")


let t_string_trim_both_not_inner_unicode () =
  check_dval
    "stringTrimBothUnicode"
    (* Leading em-space, inner thin space, trailing space *)
    (exec_ast "(String::trim ' \xe2\x80\x83foo\xe2\x80\x83bar ')")
    (Dval.dstr_of_string_exn "foo\xe2\x80\x83bar")


let t_string_trim_all () =
  check_dval
    "stringTrimAll"
    (exec_ast' (fn "String::trim" [str "      "]))
    (Dval.dstr_of_string_exn "")


let t_string_trim_preserves_emoji () =
  check_dval
    "stringTrimPreservesEmoji"
    (exec_ast "(String::trim ' \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 ')")
    (Dval.dstr_of_string_exn "\xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84")


let t_html_escaping () =
  check_dval
    "html escaping works"
    (* TODO: add back in check that `'` is correctly escaped. It didn't
     * play nice with our hacky `'` removal in the DSL parser *)
    (Dval.dstr_of_string_exn "test&lt;&gt;&amp;&quot;")
    (exec_ast "(String::htmlEscape 'test<>&\\\"')")


let t_slugify_works () =
  check_dval
    "slugify escaping works"
    (Dval.dstr_of_string_exn
       "my-super-really-excellent-uber-amazing-very-clever-thing-coffee")
    (exec_ast'
       (fn
          "String::slugify_v1"
          [ str
              "  m@y  'super'  really- excellent *uber_ amazing* ~very  ~ \"clever\" thing: coffee😭!"
          ]))


let t_uuid_string_roundtrip () =
  let ast =
    "(let i (Uuid::generate)
               (let s (toString i)
                 (let parsed (String::toUUID s)
                   (i parsed))))"
  in
  AT.check
    AT.int
    "A generated id can round-trip"
    0
    ( match exec_ast ast with
    | DList [p1; p2] ->
        compare_dval compare_expr p1 p2
    | _ ->
        1 )


let t_substring_works () =
  check_dval
    "substring"
    (exec_ast' (fn "String::isSubstring_v1" [str "a string"; str "in"]))
    (DBool true) ;
  check_dval
    "not substring"
    (exec_ast' (fn "String::isSubstring_v1" [str "a string"; str "x"]))
    (DBool false)


let t_startsWith_works () =
  check_dval
    "prefix"
    (exec_ast' (fn "String::startsWith" [str "a string"; str "a s"]))
    (DBool true) ;
  check_dval
    "not prefix"
    (exec_ast' (fn "String::startsWith" [str "a string"; str " s"]))
    (DBool false)


let t_endsWith_works () =
  check_dval
    "suffix"
    (exec_ast' (fn "String::endsWith" [str "a string"; str "ing"]))
    (DBool true) ;
  check_dval
    "not suffix"
    (exec_ast' (fn "String::endsWith" [str "a string"; str "in"]))
    (DBool false)


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
  ; ("endsWith works", `Quick, t_endsWith_works) ]
