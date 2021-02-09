open Core_kernel
open Libexecution
open Libshared.FluidShortcuts
open Types.RuntimeT
open Utils

let t_multiple_copies_of_same_name () =
  check_error
    "record field names"
    (exec_ast (record [("col1", int 1); ("col1", int 2)]))
    "Duplicate key: col1" ;
  ()


let t_feature_flags_work () =
  check_dval
    "flag shows new for true"
    (Dval.dint 1)
    (exec_ast (flag (bool true) (int 2) (int 1))) ;
  check_dval
    "flag shows old for false"
    (Dval.dint 2)
    (exec_ast (flag (bool false) (int 2) (int 1))) ;
  check_dval
    "flag shows old for incomplete cond"
    (Dval.dint 2)
    (exec_ast (flag (blank ()) (int 2) (int 1))) ;
  check_dval
    "flag shows old for null"
    (Dval.dint 2)
    (exec_ast (flag null (int 2) (int 1))) ;
  check_dval
    "flag shows old for error"
    (Dval.dint 2)
    (exec_ast (flag (fn "List::head" []) (int 2) (int 1))) ;
  check_dval
    "flag shows old for errorrail"
    (Dval.dint 2)
    (exec_ast (flag (fn "List::head" ~ster:Rail []) (int 2) (int 1))) ;
  check_dval
    "flag shows old for object"
    (Dval.dint 2)
    (exec_ast (flag (record [("x", bool true)]) (int 2) (int 1))) ;
  check_dval
    "flag shows old for list"
    (Dval.dint 2)
    (exec_ast (flag (list []) (int 2) (int 1))) ;
  ()


let t_incomplete_propagation () =
  check_incomplete
    "Fn with incomplete return incomplete"
    (exec_ast (fn "List::head" [blank ()])) ;
  check_incomplete
    "incomplete if conds are incomplete"
    (exec_ast (if' (blank ()) (int 5) (int 6))) ;
  check_incomplete "empty thread is incomplete" (exec_ast (pipe (blank ()) [])) ;
  check_incomplete
    "incomplete obj in field access is incomplete"
    (exec_ast (fieldAccess (fn "List::head" [blank ()]) "field")) ;
  check_incomplete
    "incomplete name in field access is incomplete"
    (exec_ast (fieldAccess (record [("i", int 5)]) "")) ;
  ()


let t_derror_propagation () =
  check_error
    "Mapping error results in error"
    (exec_ast
       (fn
          "List::map"
          [list [int 1; int 2; int 3; int 4; int 5]; lambda ["x"; "y"] (var "x")]))
    "Expected 2 arguments, got 1" ;
  check_dval
    "ErrorRail in Error results in ErrorRail"
    (DErrorRail (DOption OptNothing))
    (exec_ast (error (fn "List::last_v1" ~ster:Rail [list []]))) ;
  ()


(* ---------------- *)
(* Errorrail *)
(* ---------------- *)

let t_errorrail_toplevel () =
  check_dval
    "Errorrail goes to 404"
    (DResp (Response (404, []), Dval.dstr_of_string_exn "Not found"))
    (exec_handler
       (pipe
          (list [])
          [ fn "List::head_v1" ~ster:Rail [pipeTarget]
          ; binop "+" pipeTarget (int 3)
          ; lambda
              ["x"]
              (if'
                 (binop ">" (binop "+" (var "x") (int 4)) (int 1))
                 (var "x")
                 (binop "+" (int 1) (var "x"))) ])) ;
  check_dval
    "No errorrail goes to option"
    (DOption OptNothing)
    (exec_handler (fn "List::head_v1" [list []])) ;
  ()


let t_errorrail_userfn () =
  check_dval
    "userfn unwraps"
    (DOption OptNothing)
    (exec_userfn
       (pipe
          (list [])
          [ fn "List::head_v1" ~ster:Rail [pipeTarget]
          ; binop "+" pipeTarget (int 3)
          ; lambda
              ["x"]
              (if'
                 (binop ">" (binop "+" (var "x") (int 4)) (int 1))
                 (var "x")
                 (binop "+" (int 1) (var "x"))) ])) ;
  ()


(* ---------------- *)
(* Type checking *)
(* ---------------- *)
let t_basic_typecheck_works_happy () =
  let args = DvalMap.from_list [("a", Dval.dint 5); ("b", Dval.dint 4)] in
  let fn = Libs.get_fn_exn ~user_fns:[] "Int::add" in
  let user_tipes = [] in
  AT.check
    AT.bool
    "Basic typecheck succeeds"
    true
    (Type_checker.check_function_call ~user_tipes fn args |> Result.is_ok)


let t_basic_typecheck_works_unhappy () =
  let args = DvalMap.from_list [("a", Dval.dint 5); ("b", DBool true)] in
  let fn = Libs.get_fn_exn ~user_fns:[] "Int::add" in
  let user_tipes = [] in
  AT.check
    AT.bool
    "Basic typecheck succeeds"
    true
    (Type_checker.check_function_call ~user_tipes fn args |> Result.is_error)


let t_typecheck_any () =
  let args = DvalMap.from_list [("v", Dval.dint 5)] in
  let fn = Libs.get_fn_exn ~user_fns:[] "toString" in
  let user_tipes = [] in
  AT.check
    AT.bool
    "Typechecking 'Any' succeeds"
    true
    (Type_checker.check_function_call ~user_tipes fn args |> Result.is_ok)


let t_typechecker_error_isnt_wrapped_by_errorail () =
  check_condition
    "typechecker_error_dict_get"
    (exec_ast (fn "Dict::get_v1" [fn "List::empty" []; str "hello"]))
    ~f:(function DError _ -> true | _ -> false)


let t_typechecker_return_types () =
  let myBadFn = user_fn "myBadFn" ~return_type:TStr [] (int 5) in
  check_condition
    "typecheck userfn with bad return type"
    (exec_ast ~ops:[fop myBadFn] (fn "myBadFn" []))
    ~f:(function
      | DError
          ( SourceId _
          , "Type error(s) in return type: Expected to see a value of type Str but found a Int"
          ) ->
          true
      | _ ->
          false) ;
  let myGoodFn = user_fn "myGoodFn" ~return_type:TStr [] (str "str") in
  check_dval
    "typecheck userfn with good return type"
    (exec_ast ~ops:[fop myGoodFn] (fn "myGoodFn" []))
    (Dval.dstr_of_string_exn "str") ;
  let myAnyFn = user_fn "myAnyFn" ~return_type:TAny [] (int 5) in
  check_dval
    "typecheck userfn with any return type"
    (exec_ast ~ops:[fop myAnyFn] (fn "myAnyFn" []))
    (Dval.dint 5) ;
  ()


let t_dark_internal_fns_are_internal () =
  let ast = fn "DarkInternal::checkAccess" [] in
  let check_access canvas_name =
    match exec_ast ~canvas_name ast with DError _ -> None | dval -> Some dval
  in
  AT.check
    (AT.list (AT.option at_dval))
    "DarkInternal:: functions are internal."
    [check_access "test"; check_access "test_admin"]
    [None; Some DNull]


(* ---------------- *)
(* Unicode *)
(* ---------------- *)

let t_ascii_string_literal_validates_as_utf8 () =
  AT.check
    AT.int
    "ASCII string validates as UTF-8"
    0
    (match Dval.dstr_of_string "foobar" with Some _ -> 0 | _ -> 1)


let t_unicode_replacement_character_utf8_byte_seq_validates_as_utf8 () =
  AT.check
    AT.int
    "Replacement character utf8 multi-byte sequence validates"
    0
    (match Dval.dstr_of_string "\xef\xbf\xbd" with Some _ -> 0 | _ -> 1)


let t_family_emoji_utf8_byte_seq_validates_as_utf8 () =
  AT.check
    AT.int
    "Emoji utf8 multi-byte sequence validates"
    0
    (match Dval.dstr_of_string "\xf0\x9f\x91\xaa" with Some _ -> 0 | _ -> 1)


let t_family_emoji_utf16_byte_seq_fails_validation () =
  AT.check
    AT.int
    "UTF16 representation of family emoji does not validate"
    0
    (match Dval.dstr_of_string "\xd8\x3d\xdc\x6A" with Some _ -> 1 | _ -> 0)


let t_mix_of_ascii_and_utf16_fails_validation () =
  AT.check
    AT.int
    "Mix of valid ASCII followed by a UTF16 byte sequence fails validation"
    0
    ( match Dval.dstr_of_string "hello, \xd8\x3d\xdc\x6A" with
    | Some _ ->
        1
    | _ ->
        0 )


let t_u0000_fails_validation () =
  AT.check
    AT.int
    "String containing U+0000/0x00 fails to validate (due to Postgres quirks)"
    0
    (match Dval.dstr_of_string "hello, \x00" with Some _ -> 1 | _ -> 0)


let unicode_string_tester = AT.testable Unicode_string.pp Unicode_string.equal

let t_unicode_string_reverse_works_with_emojis () =
  let s1 = Unicode_string.of_string_exn "hello\xf0\x9f\x98\x84world" in
  let expected = Unicode_string.of_string_exn "dlrow\xf0\x9f\x98\x84olleh" in
  AT.check
    unicode_string_tester
    "emoji_reverse"
    expected
    (Unicode_string.rev s1)


let t_unicode_string_length_works_with_emojis () =
  let s1 = Unicode_string.of_string_exn "hello\xf0\x9f\x98\x84world" in
  let expected = 11 in
  AT.check AT.int "emoji_length" expected (Unicode_string.length s1)


let t_unicode_string_regex_replace_works_with_emojis () =
  let s1 = Unicode_string.of_string_exn "hello\xf0\x9f\x98\x84world" in
  let pattern = "\xf0\x9f\x98\x84" in
  let replacement = Unicode_string.of_string_exn "FOO" in
  let expected = Unicode_string.of_string_exn "helloFOOworld" in
  AT.check
    unicode_string_tester
    "emoji_regex_replace"
    expected
    (Unicode_string.regexp_replace ~pattern ~replacement s1)


(* ---------------- *)
(* Dval hashing *)
(* ---------------- *)
let t_dval_hash_differs_for_version_0_and_1 () =
  let arglist =
    [ DBytes ("ab" |> Libtarget.bytes_from_base64url)
    ; DBytes ("c" |> Libtarget.bytes_from_base64url) ]
  in
  AT.check
    AT.bool
    "DVal.hash differs for version 0 and 1"
    false
    (Dval.hash 0 arglist = Dval.hash 1 arglist)


let suite =
  [ ("int_add_works", `Quick, t_int_add_works)
  ; ("lambda_with_foreach", `Quick, t_lambda_with_foreach)
  ; ("match_works", `Quick, t_match_works)
  ; ("pipe works", `Quick, t_pipe_works)
  ; ( "Multiple copies of same name don't crash"
    , `Quick
    , t_multiple_copies_of_same_name )
  ; ("Feature flags work", `Quick, t_feature_flags_work)
  ; ("Handling nothing in code works", `Quick, t_nothing)
  ; ("Incompletes propagate correctly", `Quick, t_incomplete_propagation)
  ; ("Errorrail simple", `Quick, t_errorrail_simple)
  ; ("Errorrail works in toplevel", `Quick, t_errorrail_toplevel)
  ; ("Errorrail works in user_function", `Quick, t_errorrail_userfn)
  ; ( "Basic typechecking works in happy case"
    , `Quick
    , t_basic_typecheck_works_happy )
  ; ( "Basic typechecking works in unhappy case"
    , `Quick
    , t_basic_typecheck_works_unhappy )
  ; ("Type checking supports `Any` in user functions", `Quick, t_typecheck_any)
  ; ( "Type checking error isn't wrapped by error rail"
    , `Quick
    , t_typechecker_error_isnt_wrapped_by_errorail )
  ; ("Type checkingfor return types", `Quick, t_typechecker_return_types)
  ; ( "Error rail is propagated by functions"
    , `Quick
    , t_error_rail_is_propagated_by_functions )
  ; ( "DarkInternal:: functions are internal"
    , `Quick
    , t_dark_internal_fns_are_internal )
  ; ( "Dval.dstr_of_string validates ASCII as UTF8"
    , `Quick
    , t_ascii_string_literal_validates_as_utf8 )
  ; ( "Dval.dstr_of_string validates replacement character utf8 repr as UTF8"
    , `Quick
    , t_unicode_replacement_character_utf8_byte_seq_validates_as_utf8 )
  ; ( "Dval.dstr_of_string validates utf8 emoji repr as UTF8"
    , `Quick
    , t_family_emoji_utf8_byte_seq_validates_as_utf8 )
  ; ( "Dval.dstr_of_string rejects UTF16 repr of emoji"
    , `Quick
    , t_family_emoji_utf16_byte_seq_fails_validation )
  ; ( "Dval.dstr_of_string rejects mix of ASCII and UTF16"
    , `Quick
    , t_mix_of_ascii_and_utf16_fails_validation )
  ; ("Dval.dstr_of_string rejects 0x00", `Quick, t_u0000_fails_validation)
  ; ( "Unicode_string.reverse works on strings with emoji + ascii"
    , `Quick
    , t_unicode_string_reverse_works_with_emojis )
  ; ( "Unicode_string.length works for strings with emoji + ascii"
    , `Quick
    , t_unicode_string_length_works_with_emojis )
  ; ( "Unicode_string.regex_replace_works_with_emojis"
    , `Quick
    , t_unicode_string_regex_replace_works_with_emojis )
  ; ("DError propagation", `Quick, t_derror_propagation)
  ; ("Dval.hash", `Quick, t_dval_hash_differs_for_version_0_and_1)
  ; ("t_int_functions_works", `Quick, t_int_functions_works)
  ; ("lambda scopes correctly", `Quick, t_lambda_scopes_correctly)
  ; ("pattern matches work", `Quick, t_pattern_matches_work)
  ; ("shadowing all the way down", `Quick, t_shadowing_all_the_way_down) ]
