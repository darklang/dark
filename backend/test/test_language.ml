open Core_kernel
open Libexecution
open Types.RuntimeT
open Utils
module AT = Alcotest

(* ---------------- *)
(* Language features *)
(* ---------------- *)
let t_int_add_works () =
  (* Couldn't call Int::add *)
  check_dval "int_add" (Dval.dint 8) (exec_ast "(+ 5 3)")


let t_lambda_with_foreach () =
  check_dval
    "lambda_with_foreach"
    (Dval.dstr_of_string_exn "SOME STRING")
    (exec_ast
       "(String::join
       (List::foreach (String::toList_v1 'some string') (\\var ->
(String::toUppercase (String::fromChar_v1 var)))) '')")


let t_match_works () =
  let open Libshared.FluidShortcuts in
  let check_match arg expected =
    check_dval
      ("match " ^ Libshared.FluidExpression.show arg)
      (Dval.dstr_of_string_exn expected)
      (exec_ast'
         (match'
            arg
            [ (pInt 5, str "int")
            ; (pFloat "5" "6", str "float")
            ; (pBool false, str "bool")
            ; (pNull, str "null")
            ; (pBlank, str "blank")
            ; (pConstructor "Ok" [pVar "x"], fn "++" [str "ok: "; var "x"])
            ; (pConstructor "Nothing" [], str "constructor nothing")
            ; (pVar "name", fn "++" [var "name"; str "var"]) ]))
  in
  check_match (int 5) "int" ;
  check_match (float' "5" "6") "float" ;
  check_match (bool false) "bool" ;
  check_match null "null" ;
  check_match (constructor "Ok" [str "x"]) "ok: x" ;
  check_match (constructor "Nothing" []) "constructor nothing" ;
  check_match (str "not matched: ") "not matched: var" ;
  ()


let t_lambda_scopes_correctly () =
  let open Fluid in
  check_dval
    "lambda uses scope at create time, not call time"
    (DList [Dval.dint 6; Dval.dint 7; Dval.dint 8; Dval.dint 9])
    (exec_ast
       "(let x 5
         (let y (\\c -> (+ x c))
          (let x 6
           (|
              (1 2 3 4)
    (List::map y)))))") ;
  ()


let t_shadowing_all_the_way_down () =
  let open Libshared.FluidShortcuts in
  check_dval
    "simple let shadowing"
    (Dval.dint 6)
    (exec_ast' (let' "x" (int 5) (let' "x" (int 6) (var "x")))) ;
  check_dval
    "match within let"
    (Dval.dint 6)
    (exec_ast' (let' "x" (int 35) (match' (int 6) [(pVar "x", var "x")]))) ;
  check_dval
    "var pattern within let"
    (Dval.dint 6)
    (exec_ast' (let' "x" (int 35) (match' (int 6) [(pVar "x", var "x")]))) ;
  check_dval
    "var pattern within var constructor within let"
    (Dval.dint 6)
    (exec_ast'
       (let'
          "x"
          (int 35)
          (match'
             (constructor "Ok" [int 6])
             [(pConstructor "Ok" [pVar "x"], var "x")]))) ;
  check_dval
    "lambda within let"
    (DList [Dval.dint 3; Dval.dint 4; Dval.dint 5; Dval.dint 6])
    (exec_ast'
       (let'
          "x"
          (int 35)
          (fn
             "List::map"
             [ list [int 1; int 2; int 3; int 4]
             ; lambda ["x"] (fn "+" [var "x"; int 2]) ]))) ;
  check_dval
    "lambda within match within let"
    (DList [Dval.dint 3; Dval.dint 4; Dval.dint 5; Dval.dint 6])
    (exec_ast'
       (let'
          "x"
          (int 35)
          (match'
             (constructor "Ok" [int 6])
             [ ( pConstructor "Ok" [pVar "x"]
               , fn
                   "List::map"
                   [ list [int 1; int 2; int 3; int 4]
                   ; lambda ["x"] (fn "+" [var "x"; int 2]) ] ) ]))) ;
  check_dval
    "match within let within lambda"
    (DList [Dval.dint 8; Dval.dint 8; Dval.dint 8; Dval.dint 8])
    (exec_ast'
       (fn
          "List::map"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda
              ["x"]
              (let'
                 "x"
                 (int 35)
                 (match'
                    (constructor "Ok" [int 6])
                    [(pConstructor "Ok" [pVar "x"], fn "+" [var "x"; int 2])]))
          ])) ;
  check_dval
    "let within match within lambda"
    (DList [Dval.dint 11; Dval.dint 11; Dval.dint 11; Dval.dint 11])
    (exec_ast'
       (fn
          "List::map"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda
              ["x"]
              (match'
                 (constructor "Ok" [int 6])
                 [ ( pConstructor "Ok" [pVar "x"]
                   , let' "x" (int 9) (fn "+" [var "x"; int 2]) ) ]) ])) ;
  ()


let t_multiple_copies_of_same_name () =
  check_error
    "record field names"
    (exec_ast "(obj (col1 1) (col1 2))")
    "Duplicate key: col1" ;
  ()


let t_feature_flags_work () =
  check_dval
    "flag shows new for true"
    (Dval.dint 1)
    (exec_ast "(flag _ true 2 1)") ;
  check_dval
    "flag shows old for false"
    (Dval.dint 2)
    (exec_ast "(flag _ false 2 1)") ;
  check_dval
    "flag shows old for incomplete cond"
    (Dval.dint 2)
    (exec_ast "(flag _ _ 2 1)") ;
  check_dval
    "flag shows old for null"
    (Dval.dint 2)
    (exec_ast "(flag _ null 2 1)") ;
  check_dval
    "flag shows old for error"
    (Dval.dint 2)
    (exec_ast "(flag _ (List::head) 2 1)") ;
  check_dval
    "flag shows old for errorrail"
    (Dval.dint 2)
    (exec_ast "(flag _ (`List::head []) 2 1)") ;
  check_dval
    "flag shows old for object"
    (Dval.dint 2)
    (exec_ast "(flag _ (obj (x true)) 2 1)") ;
  check_dval
    "flag shows old for list"
    (Dval.dint 2)
    (exec_ast "(flag _ [] 2 1)") ;
  ()


let t_nothing () =
  check_dval "can specifiy nothing" (DOption OptNothing) (exec_ast "(Nothing)") ;
  check_dval
    "nothing works as expected"
    (DBool true)
    (exec_ast "(== (List::head_v1 []) (Nothing))") ;
  ()


let t_incomplete_propagation () =
  check_incomplete
    "Fn with incomplete return incomplete"
    (exec_ast "(List::head _)") ;
  check_dval
    "Incompletes stripped from lists"
    (DList [Dval.dint 5; Dval.dint 6])
    (exec_ast "(5 6 (List::head _))") ;
  check_dval
    "Blanks stripped from lists"
    (DList [Dval.dint 5; Dval.dint 6])
    (exec_ast "(5 6 _)") ;
  check_dval
    "Blanks stripped from objects"
    (DObj (DvalMap.from_list [("m", Dval.dint 5); ("n", Dval.dint 6)]))
    (exec_ast "(obj (i _) (m 5) (j (List::head _)) (n 6))") ;
  check_incomplete "incomplete if conds are incomplete" (exec_ast "(if _ 5 6)") ;
  check_dval
    "blanks in threads are ignored"
    (Dval.dint 8)
    (exec_ast "(| 5 _ (+ 3))") ;
  check_dval
    "incomplete in the middle of a thread is skipped"
    (Dval.dint 8)
    (exec_ast "(| 5 (+ _) (+ 3))") ;
  check_dval
    "incomplete at the end of a thread is skipped"
    (Dval.dint 5)
    (exec_ast "(| 5 (+ _))") ;
  check_incomplete "empty thread is incomplete" (exec_ast "(|)") ;
  check_incomplete
    "incomplete obj in field access is incomplete"
    (exec_ast "(. (List::head _) field)") ;
  check_incomplete
    "incomplete name in field access is incomplete"
    (exec_ast "(. (obj (i 5)) _)") ;
  ()


let t_derror_propagation () =
  check_error
    "Mapping error results in error"
    (exec_ast "(List::map (1 2 3 4 5) (\\x y -> x))")
    "Expected 2 arguments, got 1" ;
  check_incomplete
    "Incomplete in Just results in Incomplete"
    (exec_ast "(Just _)") ;
  check_incomplete "Incomplete in Ok results in Incomplete" (exec_ast "(Ok _)") ;
  check_incomplete
    "Incomplete in Error results in Incomplete"
    (exec_ast "(Error _)") ;
  check_dval
    "ErrorRail in Error results in ErrorRail"
    (DErrorRail (DOption OptNothing))
    (exec_ast "(Error (`List::last_v1 []))") ;
  ()


(* ---------------- *)
(* Errorrail *)
(* ---------------- *)

let t_errorrail_simple () =
  check_dval
    "rail"
    (DErrorRail (DOption OptNothing))
    (exec_ast "(`List::last_v1 [])") ;
  check_dval "no rail" (DOption OptNothing) (exec_ast "(Dict::get_v1 {} 'i')") ;
  check_dval
    "no rail deeply nested"
    (Dval.dint 8)
    (exec_ast
       "(| (5)
                  (`List::head_v1)
                  (+ 3)
                  (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
               )") ;
  check_dval
    "to rail deeply nested"
    (DErrorRail (DOption OptNothing))
    (exec_ast
       "(| ()
                  (`List::head_v1)
                  (+ 3)
                  (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
               )") ;
  ()


let t_errorrail_toplevel () =
  check_dval
    "Errorrail goes to 404"
    (DResp (Response (404, []), Dval.dstr_of_string_exn "Not found"))
    (exec_handler
       "(| ()
                      (`List::head_v1)
                      (+ 3)
                      (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
                    )") ;
  check_dval
    "No errorrail goes to option"
    (DOption OptNothing)
    (exec_handler "(List::head_v1 [])") ;
  ()


let t_error_rail_is_propagated_by_functions () =
  check_dval
    "push"
    (DErrorRail (DOption OptNothing))
    (exec_ast "(List::push (1 2 3 4) (`List::head_v1 []))") ;
  check_incomplete
    "filter with incomplete"
    (exec_ast "(List::filter_v1 (1 2 3 4) (\\x -> _))") ;
  check_incomplete
    "map with incomplete"
    (exec_ast "(List::map (1 2 3 4) (\\x -> _))") ;
  check_incomplete
    "fold with incomplete"
    (exec_ast "(List::fold (1 2 3 4) 1 (\\x y -> (+ x _)))") ;
  check_dval
    "filter with error rail"
    (DErrorRail (DOption OptNothing))
    (exec_ast "(List::filter_v1 (1 2 3 4) (\\x -> (`List::head_v1 [])))") ;
  check_dval
    "map with error rail"
    (DErrorRail (DOption OptNothing))
    (exec_ast "(List::map (1 2 3 4) (\\x -> (`List::head_v1 [])))") ;
  check_dval
    "fold with error rail"
    (DErrorRail (DOption OptNothing))
    (exec_ast "(List::fold (1 2 3 4) 1 (\\x y -> (`List::head_v1 [])))")


let t_errorrail_userfn () =
  check_dval
    "userfn unwraps"
    (DOption OptNothing)
    (exec_userfn
       "(| ()
                     (`List::head_v1)
                     (+ 3)
                     (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
                   )") ;
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
    (exec_ast "(Dict::get_v1 (List::empty) 'hello')")
    ~f:(function
      | DError _ ->
          true
      | _ ->
          false)


let t_int_functions_works () =
  check_condition
    "Int::random_v1 0 3 returns a number between [0,3]"
    (exec_ast "(Int::random_v1 0 3)")
    ~f:(fun dv ->
      match dv with
      | DInt i ->
        (match Dint.to_int i with Some r -> 0 <= r && r <= 3 | None -> false)
      | _ ->
          false) ;
  check_condition
    "Int::random_v1 3 0, will swap 3 0 and returns a number between [0,3]"
    (exec_ast "(Int::random_v1 3 0)")
    ~f:(fun dv ->
      match dv with
      | DInt i ->
        (match Dint.to_int i with Some r -> 0 <= r && r <= 3 | None -> false)
      | _ ->
          false)


(* ---------------- *)
(* Dark internal *)
(* ---------------- *)

let t_dark_internal_fns_are_internal () =
  let ast = "(DarkInternal::checkAccess)" in
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
  ; ("shadowing all the way down", `Quick, t_shadowing_all_the_way_down) ]
