open Core_kernel
open Libexecution
open Types.RuntimeT
open Utils

let t_stdlib_works () =
  check_dval
    "uniqueBy1"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> (Int::divide x 2)))")
    (DList [DInt 1; DInt 3; DInt 4]) ;
  check_dval
    "uniqueBy2"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> x))")
    (DList [DInt 1; DInt 2; DInt 3; DInt 4]) ;
  check_error_contains
    "base64decode"
    (exec_ast "(String::base64Decode 'random string')")
    "Not a valid base64 string" ;
  check_dval
    "getAt1"
    (exec_ast "(List::getAt (1 2 3 4) 0)")
    (DOption (OptJust (DInt 1))) ;
  check_dval
    "getAt2"
    (exec_ast "(List::getAt (1 2 3 4) 4)")
    (DOption OptNothing) ;
  ()


let t_option_stdlibs_work () =
  check_dval
    "map just"
    (exec_ast "(Option::map (Just 4) (\\x -> (Int::divide x 2)))")
    (DOption (OptJust (DInt 2))) ;
  check_dval
    "map nothing"
    (exec_ast "(Option::map (Nothing) (\\x -> (Int::divide x 2)))")
    (DOption OptNothing) ;
  check_dval
    "withDefault just"
    (exec_ast "(Option::withDefault (Just 6) 5)")
    (DInt 6) ;
  check_dval
    "withDefault nothing"
    (exec_ast "(Option::withDefault (Nothing) 5)")
    (DInt 5) ;
  check_dval
    "andThen just,nothing"
    (exec_ast "(Option::andThen (Just 5) (\\x -> (Nothing)))")
    (DOption OptNothing) ;
  check_dval
    "andThen just,just"
    (exec_ast "(Option::andThen (Just 5) (\\x -> (Just (+ 1 x))))")
    (DOption (OptJust (DInt 6))) ;
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
    (DResult (ResOk (DInt 2))) ;
  check_dval
    "map error"
    (exec_ast "(Result::map (Error 'test') (\\x -> (Int::divide x 2)))")
    (DResult (ResError test_string)) ;
  check_dval
    "maperror ok"
    (exec_ast "(Result::mapError (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (DInt 4))) ;
  check_dval
    "maperror error"
    (exec_ast
       "(Result::mapError (Error 'test') (\\x -> (String::append x '-appended')))")
    (DResult (ResError (Dval.dstr_of_string_exn "test-appended"))) ;
  check_dval
    "withDefault ok"
    (exec_ast "(Result::withDefault (Ok 6) 5)")
    (DInt 6) ;
  check_dval
    "withDefault error"
    (exec_ast "(Result::withDefault (Error 'test') 5)")
    (DInt 5) ;
  check_dval
    "fromOption just"
    (exec_ast "(Result::fromOption (Just 6) 'test')")
    (DResult (ResOk (DInt 6))) ;
  check_dval
    "fromOption nothing"
    (exec_ast "(Result::fromOption (Nothing) 'test')")
    (DResult (ResError test_string)) ;
  check_dval
    "toOption ok"
    (exec_ast "(Result::toOption (Ok 6))")
    (DOption (OptJust (DInt 6))) ;
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
    (DResult (ResOk (DInt 6))) ;
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


let suite =
  [ ("Stdlib fns work", `Quick, t_stdlib_works)
  ; ("Option stdlibs work", `Quick, t_option_stdlibs_work)
  ; ("Result stdlibs work", `Quick, t_result_stdlibs_work) ]
