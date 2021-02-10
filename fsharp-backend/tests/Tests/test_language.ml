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
