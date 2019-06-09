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
  check_dval "int_add" (DInt 8) (exec_ast "(+ 5 3)")


let t_lambda_with_foreach () =
  check_dval
    "lambda_with_foreach"
    (Dval.dstr_of_string_exn "SOME STRING")
    (exec_ast
       "(String::join
       (List::foreach (String::toList_v1 'some string') (\\var ->
(String::toUppercase (String::fromChar_v1 var)))) '')")


let t_multiple_copies_of_same_name () =
  check_dval
    "record field names"
    (exec_ast "(obj (col1 1) (col1 2))")
    (DError "Duplicate key: col1") ;
  ()


let t_feature_flags_work () =
  check_dval "flag shows new for true" (DInt 1) (exec_ast "(flag _ true 2 1)") ;
  check_dval
    "flag shows old for false"
    (DInt 2)
    (exec_ast "(flag _ false 2 1)") ;
  check_dval
    "flag shows old for incomplete cond"
    (DInt 2)
    (exec_ast "(flag _ _ 2 1)") ;
  check_dval "flag shows old for null" (DInt 2) (exec_ast "(flag _ null 2 1)") ;
  check_dval
    "flag shows old for error"
    (DInt 2)
    (exec_ast "(flag _ (List::head) 2 1)") ;
  check_dval
    "flag shows old for errorrail"
    (DInt 2)
    (exec_ast "(flag _ (`List::head []) 2 1)") ;
  check_dval
    "flag shows old for object"
    (DInt 2)
    (exec_ast "(flag _ (obj (x true)) 2 1)") ;
  check_dval "flag shows old for list" (DInt 2) (exec_ast "(flag _ [] 2 1)") ;
  ()


let t_nothing () =
  check_dval "can specifiy nothing" (DOption OptNothing) (exec_ast "(Nothing)") ;
  check_dval
    "nothing works as expected"
    (DBool true)
    (exec_ast "(== (List::head_v1 []) (Nothing))") ;
  ()


let t_incomplete_propagation () =
  check_dval
    "Fn with incomplete return incomplete"
    DIncomplete
    (exec_ast "(List::head _)") ;
  check_dval
    "Incompletes stripped from lists"
    (DList [DInt 5; DInt 6])
    (exec_ast "(5 6 (List::head _))") ;
  check_dval
    "Blanks stripped from lists"
    (DList [DInt 5; DInt 6])
    (exec_ast "(5 6 _)") ;
  check_dval
    "Blanks stripped from objects"
    (DObj (DvalMap.from_list [("m", DInt 5); ("n", DInt 6)]))
    (exec_ast "(obj (i _) (m 5) (j (List::head _)) (n 6))") ;
  check_dval
    "incomplete if conds are incomplete"
    DIncomplete
    (exec_ast "(if _ 5 6)") ;
  check_dval
    "blanks in threads are ignored"
    (DInt 8)
    (exec_ast "(| 5 _ (+ 3))") ;
  check_dval
    "incomplete in the middle of a thread is skipped"
    (DInt 8)
    (exec_ast "(| 5 (+ _) (+ 3))") ;
  check_dval
    "incomplete at the end of a thread is skipped"
    (DInt 5)
    (exec_ast "(| 5 (+ _))") ;
  check_dval "empty thread is incomplete" DIncomplete (exec_ast "(|)") ;
  check_dval
    "incomplete obj in field access is incomplete"
    DIncomplete
    (exec_ast "(. (List::head _) field)") ;
  check_dval
    "incomplete name in field access is incomplete"
    DIncomplete
    (exec_ast "(. (obj (i 5)) _)") ;
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
    (DInt 8)
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
  check_dval
    "filter with incomplete"
    DIncomplete
    (exec_ast "(List::filter_v1 (1 2 3 4) (\\x -> _))") ;
  check_dval
    "map with incomplete"
    DIncomplete
    (exec_ast "(List::map (1 2 3 4) (\\x -> _))") ;
  check_dval
    "fold with incomplete"
    DIncomplete
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
  let args = DvalMap.from_list [("a", DInt 5); ("b", DInt 4)] in
  let fn = Libs.get_fn_exn ~user_fns:[] "Int::add" in
  let user_tipes = [] in
  AT.check
    AT.bool
    "Basic typecheck succeeds"
    true
    (Type_checker.check_function_call ~user_tipes fn args |> Result.is_ok)


let t_basic_typecheck_works_unhappy () =
  let args = DvalMap.from_list [("a", DInt 5); ("b", DBool true)] in
  let fn = Libs.get_fn_exn ~user_fns:[] "Int::add" in
  let user_tipes = [] in
  AT.check
    AT.bool
    "Basic typecheck succeeds"
    true
    (Type_checker.check_function_call ~user_tipes fn args |> Result.is_error)


let t_typecheck_any () =
  let args = DvalMap.from_list [("v", DInt 5)] in
  let fn = Libs.get_fn_exn ~user_fns:[] "toString" in
  let user_tipes = [] in
  AT.check
    AT.bool
    "Typechecking 'Any' succeeds"
    true
    (Type_checker.check_function_call ~user_tipes fn args |> Result.is_ok)


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


let suite =
  [ ("int_add_works", `Quick, t_int_add_works)
  ; ("lambda_with_foreach", `Quick, t_lambda_with_foreach)
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
  ; ( "Error rail is propagated by functions"
    , `Quick
    , t_error_rail_is_propagated_by_functions )
  ; ( "DarkInternal:: functions are internal"
    , `Quick
    , t_dark_internal_fns_are_internal ) ]
