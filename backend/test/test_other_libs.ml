open Core_kernel
open Libexecution
open Types.RuntimeT
open Utils

let t_stdlib_works () =
  check_dval
    "uniqueBy1"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> (Int::divide x 2)))")
    (DList [Dval.dint 1; Dval.dint 3; Dval.dint 4]) ;
  check_dval
    "uniqueBy2"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> x))")
    (DList [Dval.dint 1; Dval.dint 2; Dval.dint 3; Dval.dint 4]) ;
  check_error_contains
    "base64decode"
    (exec_ast "(String::base64Decode 'random string')")
    "Not a valid base64 string" ;
  check_dval
    "getAt1"
    (exec_ast "(List::getAt (1 2 3 4) 0)")
    (DOption (OptJust (Dval.dint 1))) ;
  check_dval
    "getAt2"
    (exec_ast "(List::getAt (1 2 3 4) 4)")
    (DOption OptNothing) ;
  ()


let t_option_stdlibs_work () =
  check_dval
    "map just"
    (exec_ast "(Option::map (Just 4) (\\x -> (Int::divide x 2)))")
    (DOption (OptJust (Dval.dint 2))) ;
  check_dval
    "map nothing"
    (exec_ast "(Option::map (Nothing) (\\x -> (Int::divide x 2)))")
    (DOption OptNothing) ;
  check_dval
    "withDefault just"
    (exec_ast "(Option::withDefault (Just 6) 5)")
    (Dval.dint 6) ;
  check_dval
    "withDefault nothing"
    (exec_ast "(Option::withDefault (Nothing) 5)")
    (Dval.dint 5) ;
  check_dval
    "andThen just,nothing"
    (exec_ast "(Option::andThen (Just 5) (\\x -> (Nothing)))")
    (DOption OptNothing) ;
  check_dval
    "andThen just,just"
    (exec_ast "(Option::andThen (Just 5) (\\x -> (Just (+ 1 x))))")
    (DOption (OptJust (Dval.dint 6))) ;
  check_dval
    "andThen nothing,just"
    (exec_ast "(Option::andThen (Nothing) (\\x -> (Just 5)))")
    (DOption OptNothing) ;
  check_dval
    "andThen nothing,nothing"
    (exec_ast "(Option::andThen (Nothing) (\\x -> (Nothing)))")
    (DOption OptNothing) ;
  AT.check
    AT.bool
    "andThen wrong type"
    ( match
        exec_ast "(Option::andThen (Just 8) (\\x -> (Int::divide x 2)))"
      with
    | DError msg ->
        Prelude.String.contains
          ~substring:"Expected `f` to return an option"
          msg
    | _ ->
        false )
    true ;
  ()


let t_result_stdlibs_work () =
  let test_string = Dval.dstr_of_string_exn "test" in
  check_dval
    "map ok"
    (exec_ast "(Result::map (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (Dval.dint 2))) ;
  check_dval
    "map error"
    (exec_ast "(Result::map (Error 'test') (\\x -> (Int::divide x 2)))")
    (DResult (ResError test_string)) ;
  check_dval
    "maperror ok"
    (exec_ast "(Result::mapError (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (Dval.dint 4))) ;
  check_dval
    "Int::random_v1 results in Ok"
    (exec_ast "(Int::random_v1 0 1)")
    (DResult (ResOk (Dval.dint 1))) ;
  check_dval
    "Int::random_v1 results in Error"
    (exec_ast "(Int::random_v1 1 0)")
    (DResult (ResError (Dval.dstr_of_string_exn "invalid range, start < end"))) ;
  check_dval
    "maperror error"
    (exec_ast
       "(Result::mapError (Error 'test') (\\x -> (String::append x '-appended')))")
    (DResult (ResError (Dval.dstr_of_string_exn "test-appended"))) ;
  check_dval
    "withDefault ok"
    (exec_ast "(Result::withDefault (Ok 6) 5)")
    (Dval.dint 6) ;
  check_dval
    "withDefault error"
    (exec_ast "(Result::withDefault (Error 'test') 5)")
    (Dval.dint 5) ;
  check_dval
    "fromOption just"
    (exec_ast "(Result::fromOption (Just 6) 'test')")
    (DResult (ResOk (Dval.dint 6))) ;
  check_dval
    "fromOption nothing"
    (exec_ast "(Result::fromOption (Nothing) 'test')")
    (DResult (ResError test_string)) ;
  check_dval
    "toOption ok"
    (exec_ast "(Result::toOption (Ok 6))")
    (DOption (OptJust (Dval.dint 6))) ;
  check_dval
    "toOption error"
    (exec_ast "(Result::toOption (Error 'test'))")
    (DOption OptNothing) ;
  check_dval
    "andThen ok,error"
    (exec_ast "(Result::andThen (Ok 5) (\\x -> (Error 'test')))")
    (DResult (ResError test_string)) ;
  check_dval
    "andThen ok,ok"
    (exec_ast "(Result::andThen (Ok 5) (\\x -> (Ok (+ 1 x))))")
    (DResult (ResOk (Dval.dint 6))) ;
  check_dval
    "andThen error,ok"
    (exec_ast "(Result::andThen (Error 'test') (\\x -> (Ok 5)))")
    (DResult (ResError test_string)) ;
  check_dval
    "andThen error,error"
    (exec_ast "(Result::andThen (Error 'test') (\\x -> (Error 'test')))")
    (DResult (ResError test_string)) ;
  AT.check
    AT.bool
    "andThen wrong type"
    ( match exec_ast "(Result::andThen (Ok 8) (\\x -> (Int::divide x 2)))" with
    | DError msg ->
        Prelude.String.contains
          ~substring:"Expected `f` to return a result"
          msg
    | _ ->
        false )
    true ;
  ()


let t_dict_stdlibs_work () =
  let dstr = Dval.dstr_of_string_exn in
  check_dval
    "dict keys"
    (exec_ast "(Dict::keys (obj (key1 'val1')))")
    (DList [dstr "key1"]) ;
  check_dval
    "dict values"
    (exec_ast "(Dict::values (obj (key1 'val1')))")
    (DList [dstr "val1"]) ;
  check_dval
    "dict get"
    (exec_ast "(Dict::get_v1 (obj (key1 'val1')) 'key1')")
    (DOption (OptJust (dstr "val1"))) ;
  check_dval
    "dict foreach"
    (exec_ast
       "(Dict::foreach (obj (key1 'val1') (key2 'val2')) (\\x -> (++ x '_append')))")
    (DObj
       (DvalMap.from_list
          [("key1", dstr "val1_append"); ("key2", dstr "val2_append")])) ;
  check_dval
    "dict map"
    (exec_ast
       "(Dict::map (obj (key1 'val1') (key2 'val2')) (\\k x -> (++ k x)))")
    (DObj
       (DvalMap.from_list [("key1", dstr "key1val1"); ("key2", dstr "key2val2")])) ;
  check_dval "dict empty" (exec_ast "(Dict::empty)") (DObj DvalMap.empty) ;
  check_dval
    "dict merge"
    (exec_ast "(Dict::merge (obj (key1 'val1')) (obj (key2 'val2')))")
    (DObj (DvalMap.from_list [("key1", dstr "val1"); ("key2", dstr "val2")])) ;
  check_dval
    "dict toJSON"
    (exec_ast "(Dict::toJSON (obj (key1 'val1') (key2 'val2')))")
    (dstr "{ \"key1\": \"val1\", \"key2\": \"val2\" }") ;
  ()


let t_password_hashing_and_checking_works () =
  let ast =
    "(let password 'password'
               (Password::check (Password::hash password)
               password))"
  in
  check_dval
    "A `Password::hash'd string `Password::check's against itself."
    (exec_ast ast)
    (DBool true)


let suite =
  [ ("Stdlib fns work", `Quick, t_stdlib_works)
  ; ("Option stdlibs work", `Quick, t_option_stdlibs_work)
  ; ("Result stdlibs work", `Quick, t_result_stdlibs_work)
  ; ("Dict stdlibs work", `Quick, t_dict_stdlibs_work)
  ; ( "End-user password hashing and checking works"
    , `Quick
    , t_password_hashing_and_checking_works ) ]
