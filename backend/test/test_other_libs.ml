open Core_kernel
open Libexecution
open Types.RuntimeT
open Prelude
open Utils
open Libshared.FluidShortcuts

let t_option_stdlibs_work () =
  check_dval
    "map just"
    (exec_ast
       (fn
          "Option::map"
          [just (int 4); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DOption (OptJust (Dval.dint 2))) ;
  check_dval
    "map nothing"
    (exec_ast
       (fn
          "Option::map"
          [nothing (); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DOption OptNothing) ;
  check_dval
    "map  v1 just"
    (exec_ast
       (fn
          "Option::map_v1"
          [just (int 4); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DOption (OptJust (Dval.dint 2))) ;
  check_dval
    "map v1 nothing"
    (exec_ast
       (fn
          "Option::map_v1"
          [nothing (); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DOption OptNothing) ;
  check_incomplete
    "map just incomplete"
    (exec_ast (fn "Option::map_v1" [blank (); lambda ["x"] (list [var "x"])])) ;
  check_dval
    "Option::map2 works (Just, Just)"
    (DOption (OptJust (Dval.dint 9)))
    (exec_ast
       (fn
          "Option::map2"
          [ just (int 10)
          ; just (int 1)
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "Option::map2 works (Just, Nothing)"
    (DOption OptNothing)
    (exec_ast
       (fn
          "Option::map2"
          [ just (int 10)
          ; nothing ()
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "Option::map2 works (Nothing, Just)"
    (DOption OptNothing)
    (exec_ast
       (fn
          "Option::map2"
          [ nothing ()
          ; just (int 1)
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "Option::map2 works (Nothing, Nothing)"
    (DOption OptNothing)
    (exec_ast
       (fn
          "Option::map2"
          [ nothing ()
          ; nothing ()
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "withDefault just"
    (exec_ast (fn "Option::withDefault" [just (int 6); int 5]))
    (Dval.dint 6) ;
  check_dval
    "withDefault nothing"
    (exec_ast (fn "Option::withDefault" [nothing (); int 5]))
    (Dval.dint 5) ;
  check_dval
    "andThen just,nothing"
    (exec_ast (fn "Option::andThen" [just (int 5); lambda ["x"] (nothing ())]))
    (DOption OptNothing) ;
  check_dval
    "andThen just,just"
    (exec_ast
       (fn
          "Option::andThen"
          [just (int 5); lambda ["x"] (just (binop "+" (int 1) (var "x")))]))
    (DOption (OptJust (Dval.dint 6))) ;
  check_dval
    "andThen nothing,just"
    (exec_ast (fn "Option::andThen" [nothing (); lambda ["x"] (just (int 5))]))
    (DOption OptNothing) ;
  check_dval
    "andThen nothing,nothing"
    (exec_ast (fn "Option::andThen" [nothing (); lambda ["x"] (nothing ())]))
    (DOption OptNothing) ;
  AT.check
    AT.bool
    "andThen wrong type"
    ( match
        exec_ast
          (fn
             "Option::andThen"
             [just (int 8); lambda ["x"] (fn "Int::divide" [var "x"; int 2])])
      with
    | DError (_, msg) ->
        Prelude.String.contains
          ~substring:"Expected `f` to return an option"
          msg
    | _ ->
        false )
    true ;
  ()


let t_result_stdlibs_work () =
  let dstr = Dval.dstr_of_string_exn in
  let test_string = dstr "test" in
  check_dval
    "map ok"
    (exec_ast
       (fn
          "Result::map"
          [ok (int 4); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DResult (ResOk (Dval.dint 2))) ;
  check_dval
    "map error"
    (exec_ast
       (fn
          "Result::map"
          [error (str "test"); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DResult (ResError test_string)) ;
  check_dval
    "map v1 ok"
    (exec_ast
       (fn
          "Result::map_v1"
          [ok (int 4); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DResult (ResOk (Dval.dint 2))) ;
  check_dval
    "map v1 error"
    (exec_ast
       (fn
          "Result::map_v1"
          [error (str "test"); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DResult (ResError test_string)) ;
  check_dval
    "map v1 incomplete"
    (exec_ast
       (fn
          "Result::map_v1"
          [blank (); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DIncomplete SourceNone) ;
  check_dval
    "Result::map2 works (Ok, Ok)"
    (DResult (ResOk (Dval.dint 9)))
    (exec_ast
       (fn
          "Result::map2"
          [ ok (int 10)
          ; ok (int 1)
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "Result::map2 works (Ok, Error)"
    (DResult (ResError (dstr "error2")))
    (exec_ast
       (fn
          "Result::map2"
          [ ok (int 10)
          ; error (str "error2")
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "Result::map2 works (Error, Ok)"
    (DResult (ResError (dstr "error1")))
    (exec_ast
       (fn
          "Result::map2"
          [ error (str "error1")
          ; ok (int 1)
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "Result::map2 works (Error, Error)"
    (DResult (ResError (dstr "error1")))
    (exec_ast
       (fn
          "Result::map2"
          [ error (str "error1")
          ; error (str "error2")
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "maperror ok"
    (exec_ast
       (fn
          "Result::mapError"
          [ok (int 4); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DResult (ResOk (Dval.dint 4))) ;
  check_dval
    "maperror error"
    (exec_ast
       (fn
          "Result::mapError"
          [ error (str "test")
          ; lambda ["x"] (fn "String::append" [var "x"; str "-appended"]) ]))
    (DResult (ResError (Dval.dstr_of_string_exn "test-appended"))) ;
  check_dval
    "maperror v1 ok"
    (exec_ast
       (fn
          "Result::mapError_v1"
          [ok (int 4); lambda ["x"] (fn "Int::divide" [var "x"; int 2])]))
    (DResult (ResOk (Dval.dint 4))) ;
  check_dval
    "maperror v1 error"
    (exec_ast
       (fn
          "Result::mapError_v1"
          [ error (str "test")
          ; lambda ["x"] (fn "String::append" [var "x"; str "-appended"]) ]))
    (DResult (ResError (Dval.dstr_of_string_exn "test-appended"))) ;
  check_dval
    "withDefault ok"
    (exec_ast (fn "Result::withDefault" [ok (int 6); int 5]))
    (Dval.dint 6) ;
  check_dval
    "withDefault error"
    (exec_ast (fn "Result::withDefault" [error (str "test"); int 5]))
    (Dval.dint 5) ;
  check_dval
    "fromOption just"
    (exec_ast (fn "Result::fromOption" [just (int 6); str "test"]))
    (DResult (ResOk (Dval.dint 6))) ;
  check_dval
    "fromOption nothing"
    (exec_ast (fn "Result::fromOption" [nothing (); str "test"]))
    (DResult (ResError test_string)) ;
  check_incomplete
    "fromOption_v1 propogates incomplete"
    (exec_ast (fn "Result::fromOption_v1" [just (blank ()); str "test"])) ;
  check_dval
    "fromOption_v1 propogates error"
    (exec_ast
       (fn "Result::fromOption_v1" [just (error (str "test")); str "test"]))
    (DResult (ResOk (DResult (ResError test_string)))) ;
  check_dval
    "toOption ok"
    (exec_ast (fn "Result::toOption" [ok (int 6)]))
    (DOption (OptJust (Dval.dint 6))) ;
  check_dval
    "toOption error"
    (exec_ast (fn "Result::toOption" [error (str "test")]))
    (DOption OptNothing) ;
  check_incomplete
    "toOption_v1 propogates incomplete"
    (exec_ast (fn "Result::toOption_v1" [ok (blank ())])) ;
  check_dval
    "toOption_v1 propogates errors"
    (exec_ast (fn "Result::toOption_v1" [error (str "test")]))
    (DOption OptNothing) ;
  check_dval
    "andThen ok,error"
    (exec_ast
       (fn "Result::andThen" [ok (int 5); lambda ["x"] (error (str "test"))]))
    (DResult (ResError test_string)) ;
  check_dval
    "andThen ok,ok"
    (exec_ast
       (fn
          "Result::andThen"
          [ok (int 5); lambda ["x"] (ok (binop "+" (int 1) (var "x")))]))
    (DResult (ResOk (Dval.dint 6))) ;
  check_dval
    "andThen error,ok"
    (exec_ast
       (fn "Result::andThen" [error (str "test"); lambda ["x"] (ok (int 5))]))
    (DResult (ResError test_string)) ;
  check_dval
    "andThen error,error"
    (exec_ast
       (fn
          "Result::andThen"
          [error (str "test"); lambda ["x"] (error (str "test"))]))
    (DResult (ResError test_string)) ;
  AT.check
    AT.bool
    "andThen wrong type"
    ( match
        exec_ast
          (fn
             "Result::andThen"
             [ok (int 8); lambda ["x"] (fn "Int::divide" [var "x"; int 2])])
      with
    | DError (_, msg) ->
        Prelude.String.contains ~substring:"Expected `f` to return a result" msg
    | _ ->
        false )
    true ;
  check_dval
    "andThen_v1 ok,error"
    (exec_ast
       (fn "Result::andThen_v1" [ok (int 5); lambda ["x"] (error (str "test"))]))
    (DResult (ResError test_string)) ;
  check_dval
    "andThen_v1 ok,ok"
    (exec_ast
       (fn
          "Result::andThen_v1"
          [ok (int 5); lambda ["x"] (ok (binop "+" (int 1) (var "x")))]))
    (DResult (ResOk (Dval.dint 6))) ;
  check_dval
    "andThen_v1 error,ok"
    (exec_ast
       (fn "Result::andThen_v1" [error (str "test"); lambda ["x"] (ok (int 5))]))
    (DResult (ResError test_string)) ;
  check_dval
    "andThen_v1 error,error"
    (exec_ast
       (fn
          "Result::andThen_v1"
          [error (str "test"); lambda ["x"] (error (str "test"))]))
    (DResult (ResError test_string)) ;
  AT.check
    AT.bool
    "andThen_v1 wrong type"
    ( match
        exec_ast
          (fn
             "Result::andThen_v1"
             [ok (int 8); lambda ["x"] (fn "Int::divide" [var "x"; int 2])])
      with
    | DError (_, msg) ->
        Prelude.String.contains ~substring:"Expected `f` to return a result" msg
    | _ ->
        false )
    true ;
  check_condition
    "andThen_v1 propogates errors"
    (exec_ast
       (fn "Result::andThen_v1" [ok (int 5); lambda ["x"] (list [blank ()])]))
    ~f:(fun x -> match x with DError _ -> true | _ -> false) ;
  ()


let t_dict_stdlibs_work () =
  let dstr = Dval.dstr_of_string_exn in
  let dint i = DInt (Dint.of_int i) in
  check_dval
    "Dict::singleton works"
    (DObj (DvalMap.from_list [("one", Dval.dint 1)]))
    (exec_ast (fn "Dict::singleton" [str "one"; int 1])) ;
  check_dval
    "Dict::member works (not present)"
    (DBool false)
    (exec_ast (fn "Dict::member" [record [("otherKey", int 5)]; str "someKey"])) ;
  check_dval
    "Dict::member works (present)"
    (DBool true)
    (exec_ast
       (fn
          "Dict::member"
          [record [("otherKey", int 5); ("someKey", int 5)]; str "someKey"])) ;
  check_dval
    "dict keys"
    (DList [dstr "key1"])
    (exec_ast (fn "Dict::keys" [record [("key1", str "val1")]])) ;
  check_dval
    "dict values"
    (DList [dstr "val1"])
    (exec_ast (fn "Dict::values" [record [("key1", str "val1")]])) ;
  check_dval
    "Dict::toList works (empty)"
    (DList [])
    (exec_ast (fn "Dict::toList" [record []])) ;
  check_dval
    "Dict::toList works (full)"
    (DList
       [ DList [dstr "a"; dint 1]
       ; DList [dstr "b"; dint 2]
       ; DList [dstr "c"; dint 3] ])
    (exec_ast
       (fn "Dict::toList" [record [("a", int 1); ("b", int 2); ("c", int 3)]])) ;
  check_dval
    "Dict::fromListOverwitingDuplicates works (empty)"
    (DObj (DvalMap.from_list []))
    (exec_ast (fn "Dict::fromListOverwritingDuplicates" [list []])) ;
  check_dval
    "Dict::fromListOverwitingDuplicates works (key-value pairs)"
    (DObj (DvalMap.from_list [("a", dint 1); ("b", dint 2); ("c", dint 3)]))
    (exec_ast
       (fn
          "Dict::fromListOverwritingDuplicates"
          [ list
              [ list [str "a"; int 1]
              ; list [str "b"; int 2]
              ; list [str "c"; int 3] ] ])) ;
  check_dval
    "Dict::fromListOverwitingDuplicates works (overwrites duplicates correctly)"
    (DObj (DvalMap.from_list [("a", dint 3); ("b", dint 2)]))
    (exec_ast
       (fn
          "Dict::fromListOverwritingDuplicates"
          [ list
              [ list [str "a"; int 1]
              ; list [str "b"; int 2]
              ; list [str "a"; int 3] ] ])) ;
  check_error_contains
    "Dict::fromListOverwitingDuplicates works (wrong length - identifies index)"
    (exec_ast
       (fn
          "Dict::fromListOverwritingDuplicates"
          [ list
              [ list [str "a"; int 1]
              ; list [str "b"; int 2]
              ; list [str "c"; int 3; int 3] ] ]))
    "Expected every value within the `entries` argument passed to `Dict::fromListOverwritingDuplicates` to be a `[key, value]` list. However, that is not the case for the value at index 2" ;
  check_error_contains
    "Dict::fromListOverwitingDuplicates works (wrong length - identifies length)"
    (exec_ast
       (fn
          "Dict::fromListOverwritingDuplicates"
          [ list
              [ list [str "a"; int 1]
              ; list [str "b"; int 2; int 3]
              ; list [str "c"; int 3] ] ]))
    "It has length 3 but must have length 2" ;
  check_error_contains
    "Dict::fromListOverwitingDuplicates works (wrong key type)"
    (exec_ast
       (fn
          "Dict::fromListOverwritingDuplicates"
          [ list
              [ list [int 1; int 5]
              ; list [int 2; int 5; int 3]
              ; list [int 3; int 5] ] ]))
    "Keys must be `String`s" ;
  check_error_contains
    "Dict::fromListOverwitingDuplicates works (wrong key-value type)"
    (exec_ast
       (fn
          "Dict::fromListOverwritingDuplicates"
          [list [list [str "a"; int 1]; int 2; list [str "c"; int 3]]]))
    "It is of type `Int` instead of `List`" ;
  check_dval
    "Dict::fromList works (empty)"
    (DOption (OptJust (DObj (DvalMap.from_list []))))
    (exec_ast (fn "Dict::fromList" [list []])) ;
  check_dval
    "Dict::fromList works (key-value pairs)"
    (DOption
       (OptJust
          (DObj
             (DvalMap.from_list [("a", dint 1); ("b", dint 2); ("c", dint 3)]))))
    (exec_ast
       (fn
          "Dict::fromList"
          [ list
              [ list [str "a"; int 1]
              ; list [str "b"; int 2]
              ; list [str "c"; int 3] ] ])) ;
  check_dval
    "Dict::fromList works (duplicate key)"
    (DOption OptNothing)
    (exec_ast
       (fn
          "Dict::fromList"
          [ list
              [ list [str "a"; int 1]
              ; list [str "b"; int 2]
              ; list [str "a"; int 3] ] ])) ;
  check_error_contains
    "Dict::fromList works (wrong length - identifies index)"
    (exec_ast
       (fn
          "Dict::fromList"
          [ list
              [ list [str "a"; int 1]
              ; list [str "b"; int 2]
              ; list [str "c"; int 3; int 3] ] ]))
    "Expected every value within the `entries` argument passed to `Dict::fromList` to be a `[key, value]` list. However, that is not the case for the value at index 2" ;
  check_error_contains
    "Dict::fromList works (wrong length - identifies length)"
    (exec_ast
       (fn
          "Dict::fromList"
          [ list
              [ list [str "a"; int 1]
              ; list [str "b"; int 2; int 3]
              ; list [str "c"; int 3] ] ]))
    "It has length 3 but must have length 2" ;
  check_error_contains
    "Dict::fromList works (wrong key type)"
    (exec_ast
       (fn
          "Dict::fromList"
          [ list
              [ list [int 1; int 5]
              ; list [int 2; int 5; int 3]
              ; list [int 3; int 5] ] ]))
    "Keys must be `String`s" ;
  check_error_contains
    "Dict::fromList works (wrong key-value type)"
    (exec_ast
       (fn
          "Dict::fromList"
          [list [list [str "a"; int 1]; int 2; list [str "c"; int 3]]]))
    "It is of type `Int` instead of `List`" ;
  check_dval
    "dict get"
    (DOption (OptJust (dstr "val1")))
    (exec_ast (fn "Dict::get_v1" [record [("key1", str "val1")]; str "key1"])) ;
  check_dval
    "dict foreach"
    (DObj
       (DvalMap.from_list
          [("key1", dstr "val1_append"); ("key2", dstr "val2_append")]))
    (exec_ast
       (fn
          "Dict::foreach"
          [ record [("key1", str "val1"); ("key2", str "val2")]
          ; lambda ["x"] (binop "++" (var "x") (str "_append")) ])) ;
  check_dval
    "dict map"
    (DObj
       (DvalMap.from_list
          [("key1", dstr "key1val1"); ("key2", dstr "key2val2")]))
    (exec_ast
       (fn
          "Dict::map"
          [ record [("key1", str "val1"); ("key2", str "val2")]
          ; lambda ["k"; "x"] (binop "++" (var "k") (var "x")) ])) ;
  check_dval "dict empty" (DObj DvalMap.empty) (exec_ast (fn "Dict::empty" [])) ;
  check_dval
    "Dict::isEmpty works (empty)"
    (DBool true)
    (exec_ast (fn "Dict::isEmpty" [record []])) ;
  check_dval
    "Dict::isEmpty works (full)"
    (DBool false)
    (exec_ast (fn "Dict::isEmpty" [record [("a", int 1)]])) ;
  check_dval
    "dict merge"
    (DObj (DvalMap.from_list [("key1", dstr "val1"); ("key2", dstr "val2")]))
    (exec_ast
       (fn
          "Dict::merge"
          [record [("key1", str "val1")]; record [("key2", str "val2")]])) ;
  check_dval
    "dict toJSON"
    (dstr "{ \"key1\": \"val1\", \"key2\": \"val2\" }")
    (exec_ast
       (fn "Dict::toJSON" [record [("key1", str "val1"); ("key2", str "val2")]])) ;
  check_dval
    "dict filter keeps val"
    (DObj (DvalMap.from_list [("key1", dstr "val1")]))
    (exec_ast
       (fn
          "Dict::filter_v1"
          [ record [("key1", str "val1"); ("key2", str "val2")]
          ; lambda ["k"; "v"] (binop "==" (var "v") (str "val1")) ])) ;
  check_dval
    "dict filter keeps key"
    (DObj (DvalMap.from_list [("key1", dstr "val1")]))
    (exec_ast
       (fn
          "Dict::filter_v1"
          [ record [("key1", str "val1"); ("key2", str "val2")]
          ; lambda ["k"; "v"] (binop "==" (var "k") (str "key1")) ])) ;
  check_incomplete
    "dict filter propagates incomplete from lambda"
    (exec_ast
       (fn
          "Dict::filter_v1"
          [ record [("key1", str "val1"); ("key2", str "val2")]
          ; lambda ["k"; "v"] (binop "==" (var "k") (blank ())) ])) ;
  check_dval
    "dict filter ignores incomplete from obj"
    (DObj (DvalMap.from_list [("key1", dint 1); ("key3", dint 3)]))
    (exec_ast
       (fn
          "Dict::filter_v1"
          [ record [("key1", int 1); ("key2", blank ()); ("key3", int 3)]
          ; lambda ["k"; "v"] (binop ">" (var "v") (int 0)) ])) ;
  check_dval
    "Dict::filterMap works (empty)"
    (DObj (DvalMap.from_list []))
    (exec_ast
       (fn "Dict::filterMap" [record []; lambda ["key"; "value"] (int 0)])) ;
  check_dval
    "Dict::filterMap works (concat)"
    (DObj (DvalMap.from_list [("a", dstr "ax"); ("c", dstr "cz")]))
    (exec_ast
       (fn
          "Dict::filterMap"
          [ record [("a", str "x"); ("b", str "y"); ("c", str "z")]
          ; lambda
              ["key"; "value"]
              (if'
                 (binop "==" (var "value") (str "y"))
                 (nothing ())
                 (just (binop "++" (var "key") (var "value")))) ])) ;
  check_incomplete
    "Dict::filterMap works (returns incomplete)"
    (exec_ast
       (fn
          "Dict::filterMap"
          [ record [("a", str "x"); ("b", str "y"); ("c", str "z")]
          ; lambda
              ["key"; "value"]
              (if'
                 (binop "==" (var "value") (str "y"))
                 (blank ())
                 (just (binop "++" (var "key") (var "value")))) ])) ;
  check_error_contains
    "Dict::filterMap works (wrong return type)"
    (exec_ast
       (fn
          "Dict::filterMap"
          [ record [("a", str "x"); ("b", str "y"); ("c", str "z")]
          ; lambda
              ["key"; "value"]
              (if'
                 (binop "==" (var "value") (str "y"))
                 (bool false)
                 (just (binop "++" (var "key") (var "value")))) ]))
    "Expected the argument `f` passed to `Dict::filterMap` to return `Just` or `Nothing` for every entry in `dict`" ;
  check_dval
    "Dict::size works (empty)"
    (Dval.dint 0)
    (exec_ast (fn "Dict::size" [record []])) ;
  check_dval
    "Dict::size works (3)"
    (Dval.dint 3)
    (exec_ast
       (fn "Dict::size" [record [("a", int 3); ("b", int 1); ("c", int 1)]])) ;
  ()


let t_list_stdlibs_work () =
  check_dval
    "List::singleton works"
    (DList [Dval.dint 1])
    (exec_ast (fn "List::singleton" [int 1])) ;
  check_incomplete
    "List::singleton works (incomplete)"
    (exec_ast (fn "List::singleton" [blank ()])) ;
  check_dval
    "List::tail works (empty)"
    (DOption OptNothing)
    (exec_ast (fn "List::tail" [list []])) ;
  check_dval
    "List::tail works (full)"
    (DOption (OptJust (DList [Dval.dint 2; Dval.dint 3])))
    (exec_ast (fn "List::tail" [list [int 1; int 2; int 3]])) ;
  check_dval
    "List::uniqueBy works (different evaluation for some)"
    (exec_ast
       (fn
          "List::uniqueBy"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda ["x"] (fn "Int::divide" [var "x"; int 2]) ]))
    (DList [Dval.dint 1; Dval.dint 3; Dval.dint 4]) ;
  check_dval
    "List::uniqueBy works (different evaluation for all)"
    (exec_ast
       (fn
          "List::uniqueBy"
          [list [int 1; int 2; int 3; int 4]; lambda ["x"] (var "x")]))
    (DList [Dval.dint 1; Dval.dint 2; Dval.dint 3; Dval.dint 4]) ;
  check_dval
    "List::getAt works (in range)"
    (exec_ast (fn "List::getAt" [list [int 1; int 2; int 3; int 4]; int 0]))
    (DOption (OptJust (Dval.dint 1))) ;
  check_dval
    "List::getAt works (index == length)"
    (exec_ast (fn "List::getAt" [list [int 1; int 2; int 3; int 4]; int 4]))
    (DOption OptNothing) ;
  check_dval
    "List::filter_v2 works (empty)"
    (DList [])
    (exec_ast (fn "List::filter_v2" [list []; lambda ["item"] (bool true)])) ;
  check_dval
    "List::filter_v2 works (match)"
    (DList [Dval.dint 1; Dval.dint 3])
    (exec_ast
       (fn
          "List::filter_v2"
          [ list [int 1; int 2; int 3]
          ; lambda
              ["item"]
              (match'
                 (var "item")
                 [(pInt 1, bool true); (pInt 2, bool false); (pInt 3, bool true)])
          ])) ;
  check_incomplete
    "List::filter_v2 works (returns incomplete)"
    (exec_ast
       (fn
          "List::filter_v2"
          [ list [int 1; int 2; int 3]
          ; lambda
              ["item"]
              (match'
                 (var "item")
                 [(pInt 1, bool true); (pInt 2, blank ()); (pInt 3, bool true)])
          ])) ;
  check_error_contains
    "List::filter_v2 works (wrong return type)"
    (exec_ast
       (fn
          "List::filter_v2"
          [ list [int 1; int 2; int 3]
          ; lambda
              ["item"]
              (match'
                 (var "item")
                 [ (pInt 1, nothing ())
                 ; (pInt 2, bool false)
                 ; (pInt 3, bool true) ]) ]))
    "Expected the argument `f` passed to `List::filter_v2` to return `true` or `false` for every value in `list`" ;
  check_dval
    "List::indexedMap works (empty)"
    (DList [])
    (exec_ast
       (fn
          "List::indexedMap"
          [list []; lambda ["i"; "v"] (binop "-" (var "v") (var "i"))])) ;
  check_dval
    "List::indexedMap works (full)"
    (DList [Dval.dint 3; Dval.dint 1; Dval.dint (-1)])
    (exec_ast
       (fn
          "List::indexedMap"
          [ list [int 3; int 2; int 1]
          ; lambda ["i"; "v"] (binop "-" (var "v") (var "i")) ])) ;
  check_dval
    "List::map2 works (length mismatch)"
    (DOption OptNothing)
    (exec_ast
       (fn
          "List::map2"
          [ list [int 10; int 20]
          ; list [int 1; int 2; int 3]
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "List::map2 works (length match)"
    (DOption (OptJust (DList [Dval.dint 9; Dval.dint 18; Dval.dint 27])))
    (exec_ast
       (fn
          "List::map2"
          [ list [int 10; int 20; int 30]
          ; list [int 1; int 2; int 3]
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "List::map2shortest works (length mismatch)"
    (DList [Dval.dint 9; Dval.dint 18])
    (exec_ast
       (fn
          "List::map2shortest"
          [ list [int 10; int 20]
          ; list [int 1; int 2; int 3]
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "List::map2shortest works (length match)"
    (DList [Dval.dint 9; Dval.dint 18; Dval.dint 27])
    (exec_ast
       (fn
          "List::map2shortest"
          [ list [int 10; int 20; int 30]
          ; list [int 1; int 2; int 3]
          ; lambda ["a"; "b"] (binop "-" (var "a") (var "b")) ])) ;
  check_dval
    "List::zip works (length mismatch)"
    (DOption OptNothing)
    (exec_ast
       (fn "List::zip" [list [int 10; int 20]; list [int 1; int 2; int 3]])) ;
  check_dval
    "List::zip works (length match)"
    (DOption
       (OptJust
          (DList
             [ DList [Dval.dint 10; Dval.dint 1]
             ; DList [Dval.dint 20; Dval.dint 2]
             ; DList [Dval.dint 30; Dval.dint 3] ])))
    (exec_ast
       (fn
          "List::zip"
          [list [int 10; int 20; int 30]; list [int 1; int 2; int 3]])) ;
  check_dval
    "List::zipShortest works (length mismatch)"
    (DList [DList [Dval.dint 10; Dval.dint 1]; DList [Dval.dint 20; Dval.dint 2]])
    (exec_ast
       (fn
          "List::zipShortest"
          [list [int 10; int 20]; list [int 1; int 2; int 3]])) ;
  check_dval
    "List::zipShortest works (length match)"
    (DList
       [ DList [Dval.dint 10; Dval.dint 1]
       ; DList [Dval.dint 20; Dval.dint 2]
       ; DList [Dval.dint 30; Dval.dint 3] ])
    (exec_ast
       (fn
          "List::zipShortest"
          [list [int 10; int 20; int 30]; list [int 1; int 2; int 3]])) ;
  check_dval
    "List::unzip works (length match)"
    (DList
       [ DList [Dval.dint 1; Dval.dint 2; Dval.dint 3]
       ; DList [Dval.dint 10; Dval.dint 20; Dval.dint 30] ])
    (exec_ast
       (fn
          "List::unzip"
          [ list
              [list [int 1; int 10]; list [int 2; int 20]; list [int 3; int 30]]
          ])) ;
  check_error_contains
    "List::unzip errors (incorrect length - identifies index)"
    (exec_ast
       (fn
          "List::unzip"
          [ list
              [ list [int 1; int 10]
              ; list [int 2; int 20]
              ; list [int 3; int 30; int 40] ] ]))
    "Expected every value within the `pairs` argument passed to `List::unzip` to be a list with exactly two values. However, that is not the case for the value at index 2" ;
  check_error_contains
    "List::unzip errors (incorrect length - identifies length)"
    (exec_ast
       (fn
          "List::unzip"
          [ list
              [ list [int 1; int 10]
              ; list [int 2; int 20]
              ; list [int 3; int 30; int 40] ] ]))
    "It has length 3 but must have length 2" ;
  check_error_contains
    "List::unzip errors (incorrect type)"
    (exec_ast
       (fn
          "List::unzip"
          [list [list [int 10; int 20]; int 10; list [int 3; int 30]]]))
    "It is of type `Int` instead of `List`." ;
  check_dval
    "List::filterMap works (empty)"
    (DList [])
    (exec_ast (fn "List::filterMap" [list []; lambda ["item"] (int 0)])) ;
  check_dval
    "List::filterMap works (double)"
    (DList [Dval.dint 2; Dval.dint 6])
    (exec_ast
       (fn
          "List::filterMap"
          [ list [int 1; int 2; int 3]
          ; lambda
              ["item"]
              (if'
                 (binop "==" (var "item") (int 2))
                 (nothing ())
                 (just (binop "*" (var "item") (int 2)))) ])) ;
  check_incomplete
    "List::filterMap works (returns incomplete)"
    (exec_ast
       (fn
          "List::filterMap"
          [ list [int 1; int 2; int 3]
          ; lambda
              ["item"]
              (if'
                 (binop "==" (var "item") (int 2))
                 (blank ())
                 (just (binop "*" (var "item") (int 2)))) ])) ;
  check_error_contains
    "List::filterMap works (wrong return type)"
    (exec_ast
       (fn
          "List::filterMap"
          [ list [int 1; int 2; int 3]
          ; lambda
              ["item"]
              (if'
                 (binop "==" (var "item") (int 2))
                 (bool false)
                 (just (binop "*" (var "item") (int 2)))) ]))
    "Expected the argument `f` passed to `List::filterMap` to return `Just` or `Nothing` for every value in `list`" ;
  check_dval
    "List::randomElement works (empty)"
    (DOption OptNothing)
    (exec_ast (fn "List::randomElement" [list []])) ;
  check_dval
    "List::randomElement works (1 value)"
    (DOption (OptJust (Dval.dint 1)))
    (* Can't check randomness deterministically in test so only 1 element*)
    (exec_ast (fn "List::randomElement" [list [int 1]])) ;
  check_dval
    "List:member works for empty lists"
    (DBool false)
    (exec_ast (fn "List::member" [list []; int 1])) ;
  check_dval
    "List::takeWhile works"
    (DList [Dval.dint 1; Dval.dint 2])
    (exec_ast
       (fn
          "List::takeWhile"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda ["item"] (binop "<" (var "item") (int 3)) ])) ;
  check_dval
    "List::takeWhile stops upon false"
    (DList [Dval.dint 1])
    (exec_ast
       (fn
          "List::takeWhile"
          [ list [int 1; int 5; int 2; int 2]
          ; lambda ["item"] (binop "<" (var "item") (int 3)) ])) ;
  check_dval
    "List::takeWhile works with empty input"
    (DList [])
    (exec_ast
       (fn
          "List::takeWhile"
          [list []; lambda ["item"] (binop "<" (var "item") (int 3))])) ;
  check_dval
    "List::takeWhile works with empty output"
    (DList [])
    (exec_ast
       (fn
          "List::takeWhile"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda ["item"] (binop "<" (var "item") (int 1)) ])) ;
  check_error_contains
    "List::takeWhile gives error with incorrect return type"
    (exec_ast
       (fn
          "List::takeWhile"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda ["item"] (binop "-" (int 0) (int 1)) ]))
    "Expected the argument `f` passed to `List::takeWhile` to return a boolean value for every value in `list`. However, it returned `-1` for the input `1`." ;
  check_dval
    "List::dropWhile works"
    (DList [Dval.dint 3; Dval.dint 4])
    (exec_ast
       (fn
          "List::dropWhile"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda ["item"] (binop "<" (var "item") (int 3)) ])) ;
  check_dval
    "List::dropWhile stops upon false"
    (DList [Dval.dint 5; Dval.dint 2; Dval.dint 2])
    (exec_ast
       (fn
          "List::dropWhile"
          [ list [int 1; int 5; int 2; int 2]
          ; lambda ["item"] (binop "<" (var "item") (int 3)) ])) ;

  check_dval
    "List::dropWhile works with empty input"
    (DList [])
    (exec_ast
       (fn
          "List::dropWhile"
          [list []; lambda ["item"] (binop "<" (var "item") (int 3))])) ;
  check_dval
    "List::dropWhile works with empty output"
    (DList [])
    (exec_ast
       (fn
          "List::dropWhile"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda ["item"] (binop ">=" (var "item") (int 1)) ])) ;
  check_error_contains
    "List::dropWhile gives error with incorrect return type"
    (exec_ast
       (fn
          "List::dropWhile"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda ["item"] (binop "-" (int 0) (int 1)) ]))
    "Expected the argument `f` passed to `List::dropWhile` to return a boolean value for every value in `list`. However, it returned `-1` for the input `1`." ;

  check_dval
    "List::flatten empty list works"
    (DList [])
    (exec_ast (fn "List::flatten" [list []])) ;
  check_dval
    "List::flatten empty nested list works"
    (DList [])
    (exec_ast (fn "List::flatten" [list [list []]])) ;
  check_dval
    "List::flatten empty doubly-nested list removes one layer"
    (DList [DList []])
    (exec_ast (fn "List::flatten" [list [list [list []]]])) ;
  check_dval
    "List::flatten flattens singly-nested lists"
    (DList [Dval.dint 1; Dval.dint 2; Dval.dint 3])
    (exec_ast
       (fn "List::flatten" [list [list [int 1]; list [int 2]; list [int 3]]])) ;
  check_dval
    "List::flatten flattens multi-nested lists properly"
    (DList [Dval.dint 1; DList [Dval.dint 2; Dval.dint 3]])
    (exec_ast
       (fn "List::flatten" [list [list [int 1]; list [list [int 2; int 3]]]])) ;
  ()


let t_string_stdlibs_work () =
  let dstr = Dval.dstr_of_string_exn in
  check_dval
    "String::isEmpty works (empty)"
    (DBool true)
    (exec_ast (fn "String::isEmpty" [str ""])) ;
  check_dval
    "String::isEmpty works (full)"
    (DBool false)
    (exec_ast (fn "String::isEmpty" [str "a"])) ;
  check_dval
    "String::append_v1 works (multicharacter)"
    (dstr "hello world")
    (exec_ast (fn "String::append_v1" [str "hello"; str " world"])) ;
  (* This is broken, hence String::append is deprecated *)
  (* check_dval
    "String::append works (normalizes)"
    (dstr "\xC3\xA2") (* â *)
    (exec_ast (fn "String::append" [str "\x61"; str "\xCC\x82"])) ; *)
  check_dval
    "String::append_v1 works (normalizes â)"
    (dstr "\xC3\xA2") (* â *)
    (exec_ast (fn "String::append_v1" [str "\x61"; str "\xCC\x82"])) ;
  check_dval
    "String::append_v1 works (normalizes hangul)"
    (dstr "\xea\xb0\x81") (* 각 *)
    (exec_ast (fn "String::append_v1" [str "\u{1100}"; str "\u{1161}\u{11A8}"])) ;
  check_dval
    "++ works (multicharacter)"
    (dstr "hello world")
    (exec_ast (binop "++" (str "hello") (str " world"))) ;
  check_dval
    "++ works (normalizes â)"
    (dstr "\xC3\xA2") (* â *)
    (exec_ast (binop "++" (str "\x61") (str "\xCC\x82"))) ;
  check_dval
    "++ works (normalizes hangul)"
    (dstr "\xea\xb0\x81") (* 각 *)
    (exec_ast (binop "++" (str "\u{1100}") (str "\u{1161}\u{11A8}"))) ;
  check_error_contains
    "String::base64decode errors on non-base64"
    (exec_ast (fn "String::base64Decode" [str "random string"]))
    "Not a valid base64 string" ;
  check_dval
    "String::slice works (pos, pos)"
    (dstr "c")
    (exec_ast (fn "String::slice" [str "abcd"; int 2; int 3])) ;
  check_dval
    "String::slice works (pos, > len)"
    (dstr "cd")
    (exec_ast (fn "String::slice" [str "abcd"; int 2; int 6])) ;
  check_dval
    "String::slice works (> len, > len)"
    (dstr "")
    (exec_ast (fn "String::slice" [str "abcd"; int 5; int 6])) ;
  check_dval
    "String::slice works (pos, neg)"
    (dstr "abc")
    (exec_ast (fn "String::slice" [str "abcd"; int 0; int (-1)])) ;
  check_dval
    "String::slice works (neg, neg)"
    (dstr "cd")
    (exec_ast (fn "String::slice" [str "abcd"; int (-2); int 4])) ;
  check_dval
    "String::slice works (<-len, pos)"
    (dstr "a")
    (exec_ast (fn "String::slice" [str "abcd"; int (-5); int 1])) ;
  check_dval
    "String::slice works (<-len, <-len)"
    (dstr "")
    (exec_ast (fn "String::slice" [str "abcd"; int (-5); int (-6)])) ;
  check_dval
    "String::slice works (swapped)"
    (dstr "")
    (exec_ast (fn "String::slice" [str "abcd"; int 3; int 2])) ;
  check_dval
    "String::first works (pos)"
    (dstr "abc")
    (exec_ast (fn "String::first" [str "abcd"; int 3])) ;
  check_dval
    "String::first works (0)"
    (dstr "")
    (exec_ast (fn "String::first" [str "abcd"; int 0])) ;
  check_dval
    "String::first works (neg)"
    (dstr "")
    (exec_ast (fn "String::first" [str "abcd"; int (-3)])) ;
  check_dval
    "String::last works (pos)"
    (dstr "bcd")
    (exec_ast (fn "String::last" [str "abcd"; int 3])) ;
  check_dval
    "String::last works (0)"
    (dstr "")
    (exec_ast (fn "String::last" [str "abcd"; int 0])) ;
  check_dval
    "String::last works (neg)"
    (dstr "")
    (exec_ast (fn "String::last" [str "abcd"; int (-3)])) ;
  check_dval
    "String::dropFirst works (pos)"
    (dstr "d")
    (exec_ast (fn "String::dropFirst" [str "abcd"; int 3])) ;
  check_dval
    "String::dropFirst works (0)"
    (dstr "abcd")
    (exec_ast (fn "String::dropFirst" [str "abcd"; int 0])) ;
  check_dval
    "String::dropFirst works (neg)"
    (dstr "abcd")
    (exec_ast (fn "String::dropFirst" [str "abcd"; int (-3)])) ;
  check_dval
    "String::dropLast works (pos)"
    (dstr "a")
    (exec_ast (fn "String::dropLast" [str "abcd"; int 3])) ;
  check_dval
    "String::dropLast works (0)"
    (dstr "abcd")
    (exec_ast (fn "String::dropLast" [str "abcd"; int 0])) ;
  check_dval
    "String::dropLast works (neg)"
    (dstr "abcd")
    (exec_ast (fn "String::dropLast" [str "abcd"; int (-3)])) ;
  check_error_contains
    "String::padStart works (errors on empty string)"
    (exec_ast (fn "String::padStart" [str "123"; str ""; int 10]))
    "Expected the argument `padWith` passed to `String::padStart` to be one character long. However, `\"\"` is 0 characters long." ;
  check_error_contains
    "String::padEnd works (errors on empty string)"
    (exec_ast (fn "String::padEnd" [str "123"; str ""; int 10]))
    "Expected the argument `padWith` passed to `String::padEnd` to be one character long. However, `\"\"` is 0 characters long." ;
  check_dval
    "String::padStart works (1 EGC)"
    (dstr "000123")
    (exec_ast (fn "String::padStart" [str "123"; str "0"; int 6])) ;
  check_dval
    "String::padEnd works (1 EGC)"
    (dstr "123000")
    (exec_ast (fn "String::padEnd" [str "123"; str "0"; int 6])) ;
  check_dval
    "String::padStart works (1 EGC; length too short)"
    (dstr "123")
    (exec_ast (fn "String::padStart" [str "123"; str "0"; int 3])) ;
  check_dval
    "String::padEnd works (1 EGC; length too short)"
    (dstr "123")
    (exec_ast (fn "String::padEnd" [str "123"; str "0"; int 3])) ;
  check_error_contains
    "String::padStart works (> 1 EGC errors)"
    (exec_ast (fn "String::padStart" [str "123"; str "_-"; int 4]))
    "Expected the argument `padWith` passed to `String::padStart` to be one character long. However, `\"_-\"` is 2 characters long." ;
  check_error_contains
    "String::padEnd works (> 1 EGC errors)"
    (exec_ast (fn "String::padEnd" [str "123"; str "_-"; int 4]))
    "Expected the argument `padWith` passed to `String::padEnd` to be one character long. However, `\"_-\"` is 2 characters long." ;
  check_dval
    "String::trimStart works (Noop)"
    (exec_ast (fn "String::trimStart" [str "foo"]))
    (dstr "foo") ;
  check_dval
    "String::trimEnd works (Noop)"
    (exec_ast (fn "String::trimEnd" [str "foo"]))
    (dstr "foo") ;
  check_dval
    "String::trimStart works (Start Trivial)"
    (exec_ast (fn "String::trimStart" [str "  foo"]))
    (dstr "foo") ;
  check_dval
    "String::trimEnd works (Start Trivial)"
    (exec_ast (fn "String::trimEnd" [str "  foo"]))
    (dstr "  foo") ;
  check_dval
    "String::trimStart works (End Trivial)"
    (exec_ast (fn "String::trimStart" [str "foo  "]))
    (dstr "foo  ") ;
  check_dval
    "String::trimEnd works (End Trivial)"
    (exec_ast (fn "String::trimEnd" [str "foo  "]))
    (dstr "foo") ;
  check_dval
    "String::trimStart works (Both Trivial)"
    (exec_ast (fn "String::trimStart" [str "  foo  "]))
    (dstr "foo  ") ;
  check_dval
    "String::trimEnd works (Both Trivial)"
    (exec_ast (fn "String::trimEnd" [str "  foo  "]))
    (dstr "  foo") ;
  check_dval
    "String::trimStart works (BothNotInner Trivial)"
    (exec_ast (fn "String::trimStart" [str "  foo bar  "]))
    (dstr "foo bar  ") ;
  check_dval
    "String::trimEnd works (BothNotInner Trivial)"
    (exec_ast (fn "String::trimEnd" [str "  foo bar  "]))
    (dstr "  foo bar") ;
  check_dval
    "String::trimStart works (Both Unicode)"
    (* Leading em-space, inner thin space, trailing space *)
    (exec_ast
       (fn
          "String::trimStart"
          [str " \xe2\x80\x83foo\xe2\x80\x83bar\xe2\x80\x83 "]))
    (dstr "foo\xe2\x80\x83bar\xe2\x80\x83 ") ;
  check_dval
    "String::trimEnd works (Both Unicode)"
    (* Leading em-space, inner thin space, trailing space *)
    (exec_ast
       (fn
          "String::trimEnd"
          [str " \xe2\x80\x83foo\xe2\x80\x83bar\xe2\x80\x83 "]))
    (dstr " \xe2\x80\x83foo\xe2\x80\x83bar") ;
  check_dval
    "String::trimStart works (All)"
    (exec_ast (fn "String::trimStart" [str "      "]))
    (dstr "") ;
  check_dval
    "String::trimEnd works (All)"
    (exec_ast (fn "String::trimEnd" [str "      "]))
    (dstr "") ;
  check_dval
    "String::trimStart works (PreservesEmoji)"
    (exec_ast
       (fn "String::trimStart" [str " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 "]))
    (dstr "\xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 ") ;
  check_dval
    "String::trimEnd works (PreservesEmoji)"
    (exec_ast
       (fn "String::trimEnd" [str " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 "]))
    (dstr " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84") ;
  ()


let t_password_hashing_and_checking_works () =
  let ast =
    let'
      "password"
      (str "password")
      (fn
         "Password::check"
         [fn "Password::hash" [var "password"]; var "password"])
  in
  check_dval
    "A `Password::hash'd string `Password::check's against itself."
    (exec_ast ast)
    (DBool true)


let t_jwt_functions_work () =
  let privatekey =
    "-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBAAKCAQEAvxW2wuTTK2d0ob5mu/ASJ9vYDc/SXy06QAIepF9x9eoVZZVZ
d8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqLn9/0Ag9ua4ml/ft7COprfEYA7klN
c+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGWCV7+3DF2RvDV2okk3x1ZKyBy2Rw2
uUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTwMlHbmVv9QMY5UetA9o05uPaAXH4B
CCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p4Ur22mtma+6ree45gsdnzlj1OASW
DQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ44wIDAQABAoIBAQC+0olj0a3MT5Fa
oNDpZ9JJubLmAB8e6wSbvUIqdiJRKUXa3y2sgNtVjLTzieKfNXhCaHIxUTdH5DWq
p0G7yo+qxbRghlaHz7tTitsQSUGzphjx3YQaewIujQ6EJXbDZZZBsNLqYHfQgbW+
1eV/qGvzyckLzd1G9OUrSv/mS+GrPQ00kpIJIX+EInFOPQ04DheppGNdlxoAUwQQ
XUUhE1LifY4DyyK71mNlUoYyCs+0ozLzbxQwr9n8PKnLKdukL6X0g3tlKEbqQWPv
vz2J8QZeSyhnZM9AjtYdVqTO6qs4l9dyWjdpDRIV9WylasOsIbb8XP8bv2NpH2Ua
6a54L/RJAoGBAPVWwU1jU6e86WrnocJf3miydkhF5VV1tporiuAi391N84zCG509
rWZWa0xsD2tq2+yNDry1qdqMGmvBXKoTJAx3cjpvK/uK7Tkd+tnislDLw8Wq/fCz
NBdSidGIuASXdh4Bo9OK8iYMBgfpUGXRKAs4rO45mwrS/+b0YYZSiX/1AoGBAMdj
amEa5SzXw7tSqtp4Vr4pp4H52YULKI84UKvEDQOROfazQrZMHxbtaSMXG69x7SBr
r48MuRYWd8KZ3iUkYjQLhr4n4zw5DS4AVJqgrLootVWHgt6Ey29Xa1g+B4pZOre5
PJcrxNsG0OjIAEUsTb+yeURSphVjYe+xlXlYD0Z3AoGACdxExKF7WUCEeSF6JN/J
hpe1nU4B259xiVy6piuAp9pcMYoTpgw2jehnQ5kMPZr739QDhZ4fh4MeBLquyL8g
McgTNToGoIOC6UrFLECqPgkSgz1OG4B4VX+hvmQqUTTtMGOMfBIXjWPqUiMUciMn
4tuSR7jU/GhilJu517Y1hIkCgYEAiZ5ypEdd+s+Jx1dNmbEJngM+HJYIrq1+9ytV
ctjEarvoGACugQiVRMvkj1W5xCSMGJ568+9CKJ6lVmnBTD2KkoWKIOGDE+QE1sVf
n8Jatbq3PitkBpX9nAHok2Vs6u6feoOd8HFDVDGmK6Uvmo7zsuZKkP/CpmyMAla9
5p0DHg0CgYEAg0Wwqo3sDFSyKii25/Sffjr6tf1ab+3gFMpahRslkUvyFE/ZweKb
T/YWcgYPzBA6q8LBfGRdh80kveFKRluUERb0PuK+jiHXz42SJ4zEIaToWeK1TQ6I
FW78LEsgtnna+JpWEr+ugcGN/FH8e9PLJDK7Z/HSLPtV8E6V/ls3VDM=
-----END RSA PRIVATE KEY-----"
  in
  let publickey =
    "-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvxW2wuTTK2d0ob5mu/AS
J9vYDc/SXy06QAIepF9x9eoVZZVZd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqL
n9/0Ag9ua4ml/ft7COprfEYA7klNc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGW
CV7+3DF2RvDV2okk3x1ZKyBy2Rw2uUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTw
MlHbmVv9QMY5UetA9o05uPaAXH4BCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p
4Ur22mtma+6ree45gsdnzlj1OASWDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ4
4wIDAQAB
-----END PUBLIC KEY-----"
  in
  let ast_v0 (privkey : string) (pubkey : string) : Libshared.FluidExpression.t
      =
    let'
      "privatekey"
      (str privkey)
      (let'
         "publickey"
         (str pubkey)
         (let'
            "payload"
            (record [("abc", str "def")])
            (let'
               "extraHeaders"
               (record [("ghi", str "jkl")])
               (fn
                  "JWT::verifyAndExtract"
                  [ var "publickey"
                  ; fn
                      "JWT::signAndEncodeWithHeaders"
                      [var "privatekey"; var "extraHeaders"; var "payload"] ]))))
  in
  let ast_v1 (privkey : string) (pubkey : string) : Libshared.FluidExpression.t
      =
    let'
      "privatekey"
      (str privkey)
      (let'
         "publickey"
         (str pubkey)
         (let'
            "payload"
            (record [("abc", str "def")])
            (let'
               "extraHeaders"
               (record [("ghi", str "jkl")])
               (fn
                  "JWT::verifyAndExtract_v1"
                  ~ster:Rail
                  [ var "publickey"
                  ; fn
                      "JWT::signAndEncodeWithHeaders_v1"
                      ~ster:Rail
                      [var "privatekey"; var "extraHeaders"; var "payload"] ]))))
  in
  check_dval
    "JWT::verifyAndExtract works on output of JWT::signAndEncodeWithheaders"
    ( [ ( "payload"
        , DObj (DvalMap.from_list [("abc", Dval.dstr_of_string_exn "def")]) )
      ; ( "header"
        , DObj
            (DvalMap.from_list
               [ ("type", Dval.dstr_of_string_exn "JWT")
               ; ("alg", Dval.dstr_of_string_exn "RS256")
               ; ("ghi", Dval.dstr_of_string_exn "jkl") ]) ) ]
    |> DvalMap.from_list
    |> fun x -> DOption (OptJust (DObj x)) )
    (exec_ast (ast_v0 privatekey publickey)) ;
  check_dval
    "JWT::verifyAndExtract_v1 works on output of JWT::signAndEncodeWithheaders"
    ( [ ( "payload"
        , DObj (DvalMap.from_list [("abc", Dval.dstr_of_string_exn "def")]) )
      ; ( "header"
        , DObj
            (DvalMap.from_list
               [ ("type", Dval.dstr_of_string_exn "JWT")
               ; ("alg", Dval.dstr_of_string_exn "RS256")
               ; ("ghi", Dval.dstr_of_string_exn "jkl") ]) ) ]
    |> DvalMap.from_list
    |> DObj )
    (exec_ast (ast_v1 privatekey publickey)) ;
  check_dval
    "JWT::signAndEncodeWithheaders_v1 gives error for private key"
    (DErrorRail
       (DResult
          (ResError
             (Dval.dstr_of_string_exn "Invalid private key: not an RSA key"))))
    (exec_ast (ast_v1 "invalid private key" publickey)) ;
  check_dval
    "JWT::verifyAndExtract_v1 gives error for pubkey"
    (DErrorRail
       (DResult (ResError (Dval.dstr_of_string_exn "Invalid public key"))))
    (exec_ast (ast_v1 privatekey "invalid public key"))


let t_date_functions_work () =
  let datestr = "2019-07-28T22:42:36Z" in
  let dateval = DDate (Util.date_of_isostring datestr) in
  let date = fn ~ster:Rail "Date::parse_v1" [str datestr] in
  let later_date =
    fn ~ster:Rail "Date::parse_v1" [str "2020-11-26T04:37:46Z"]
  in
  check_dval
    "Valid Date::parse_v0 produces a Date"
    dateval
    (exec_ast (fn "Date::parse" [str datestr])) ;
  check_dval
    "Invalid Date::parse_v0 produces an error"
    (DBool true)
    (exec_ast
       (pipe (fn "Date::parse" [str "asd"]) [fn "Bool::isError" [pipeTarget]])) ;
  check_dval
    "Valid Date::parse_v1 produces an Ok Date"
    (DResult (ResOk dateval))
    (exec_ast (fn ~ster:NoRail "Date::parse_v1" [str datestr])) ;
  check_dval
    "Invalid Date::parse_v1 produces an Error result"
    (DResult (ResError (Dval.dstr_of_string_exn "Invalid date format")))
    (exec_ast (fn ~ster:NoRail "Date::parse_v1" [str "asd"])) ;
  check_dval
    "Valid Date::parse_v1 roundtrips"
    (Dval.dstr_of_string_exn datestr)
    (exec_ast (fn "Date::toString" [date])) ;
  (* Subparts of a date *)
  check_dval "Year works" (Dval.dint 2019) (exec_ast (fn "Date::year" [date])) ;
  check_dval "Month works" (Dval.dint 7) (exec_ast (fn "Date::month" [date])) ;
  check_dval "Day works" (Dval.dint 28) (exec_ast (fn "Date::day" [date])) ;
  check_dval
    "Date::weekday works"
    (Dval.dint 7)
    (exec_ast (fn "Date::weekday" [date])) ;
  check_dval
    "Date::hour works"
    (Dval.dint 22)
    (exec_ast (fn "Date::hour" [date])) ;
  check_dval
    "Date::minute works"
    (Dval.dint 42)
    (exec_ast (fn "Date::minute" [date])) ;
  check_dval
    "Date::second works"
    (Dval.dint 36)
    (exec_ast (fn "Date::second" [date])) ;
  check_dval
    "Date::toSeconds roundtrips"
    (Dval.dstr_of_string_exn datestr)
    (exec_ast
       (pipe
          date
          [ fn "Date::toSeconds" [pipeTarget]
          ; fn "Date::fromSeconds" [pipeTarget]
          ; fn "toString" [pipeTarget] ])) ;
  check_dval
    "Date::fromSeconds roundtrips"
    (Dval.dint 1095379198)
    (exec_ast
       (pipe
          (int 1095379198)
          [ fn "Date::fromSeconds" [pipeTarget]
          ; fn "Date::toSeconds" [pipeTarget] ])) ;
  check_dval
    "Date::hour works - leif's test case"
    (Dval.dint 3)
    (exec_ast
       (pipe
          (fn ~ster:Rail "Date::parse_v1" [str "2019-12-27T03:27:36Z"])
          [fn "Date::hour_v1" [pipeTarget]])) ;
  check_dval
    "Date <= works"
    (DBool true)
    (exec_ast (binop "Date::<=" date later_date)) ;
  check_dval
    "Date <= works"
    (DBool true)
    (exec_ast (binop "Date::<=" date date)) ;
  check_dval
    "Date < works"
    (DBool true)
    (exec_ast (binop "Date::<" date later_date)) ;
  check_dval
    "Date < works - equality"
    (DBool false)
    (exec_ast (binop "Date::<" date date)) ;
  check_dval
    "Date > works"
    (DBool true)
    (exec_ast (binop "Date::>" later_date date)) ;
  check_dval
    "Date > works - equality"
    (DBool false)
    (exec_ast (binop "Date::>" date date)) ;
  check_dval
    "Date >= works"
    (DBool true)
    (exec_ast (binop "Date::>=" later_date date)) ;
  check_dval
    "Date >= works - equality"
    (DBool true)
    (exec_ast (binop "Date::>=" later_date later_date)) ;
  check_dval
    "Date::today works"
    (Dval.dstr_of_string_exn
       (Core.Time.format
          (Time.now ())
          "%Y-%m-%dT00:00:00Z"
          ~zone:Core.Time.Zone.utc))
    (exec_ast (pipe (fn "Date::today" []) [fn "toString" [pipeTarget]])) ;
  check_dval
    "Date::atStartOfDay works"
    (Dval.dstr_of_string_exn "2019-07-28T00:00:00Z")
    (exec_ast
       (pipe
          date
          [fn "Date::atStartOfDay" [pipeTarget]; fn "toString" [pipeTarget]])) ;
  ()


let t_sha256hmac_for_aws () =
  check_dval
    (* These values come from https://docs.aws.amazon.com/general/latest/gr/sigv4-calculate-signature.html*)
    "Crypto::sha256hmac behaves as AWS' example expects (link in test)"
    (Dval.dstr_of_string_exn
       "5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7")
    (exec_ast
       (let'
          "scope"
          (str "20150830/us-east-1/iam/aws4_request")
          (let'
             "content"
             (str
                "f536975d06c0309214f805bb90ccff089219ecd68b2577efef23edd43b7e1a59")
             (let'
                "strs"
                (list
                   [ str "AWS4-HMAC-SHA256"
                   ; str "20150830T123600Z"
                   ; var "scope"
                   ; var "content" ])
                (let'
                   "strToSign"
                   (fn "String::join" [var "strs"; fn "String::newline" []])
                   (let'
                      "secret"
                      (fn
                         "String::toBytes"
                         [str "AWS4wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"])
                      (let'
                         "data"
                         (fn "String::toBytes" [str "20150830"])
                         (let'
                            "date"
                            (fn "Crypto::sha256hmac" [var "secret"; var "data"])
                            (let'
                               "region"
                               (fn
                                  "Crypto::sha256hmac"
                                  [ var "date"
                                  ; fn "String::toBytes" [str "us-east-1"] ])
                               (let'
                                  "service"
                                  (fn
                                     "Crypto::sha256hmac"
                                     [ var "region"
                                     ; fn "String::toBytes" [str "iam"] ])
                                  (let'
                                     "signing"
                                     (fn
                                        "Crypto::sha256hmac"
                                        [ var "service"
                                        ; fn
                                            "String::toBytes"
                                            [str "aws4_request"] ])
                                     (let'
                                        "signed"
                                        (fn
                                           "Crypto::sha256hmac"
                                           [ var "signing"
                                           ; fn
                                               "String::toBytes"
                                               [var "strToSign"] ])
                                        (fn
                                           "String::toLowercase_v1"
                                           [fn "Bytes::hexEncode" [var "signed"]]))))))))))))) ;
  ()


let t_crypto_sha () =
  check_dval
    "Crypto::sha256 produces the correct digest"
    (Dval.dstr_of_string_exn
       "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855")
    (exec_ast
       (fn
          "Bytes::hexEncode"
          [fn "Crypto::sha256" [fn "String::toBytes" [str ""]]])) ;
  check_dval
    "Crypto::sha384 produces the correct digest"
    (Dval.dstr_of_string_exn
       "38B060A751AC96384CD9327EB1B1E36A21FDB71114BE07434C0CC7BF63F6E1DA274EDEBFE76F65FBD51AD2F14898B95B")
    (exec_ast
       (fn
          "Bytes::hexEncode"
          [fn "Crypto::sha384" [fn "String::toBytes" [str ""]]])) ;
  check_dval
    "Crypto::md5 produces the correct digest"
    (Dval.dstr_of_string_exn "D41D8CD98F00B204E9800998ECF8427E")
    (exec_ast
       (fn
          "Bytes::hexEncode"
          [fn "Crypto::md5" [fn "String::toBytes" [str ""]]])) ;
  ()


let t_libbytes () =
  check_dval
    "Length is right"
    (Dval.dint 6)
    (exec_ast (fn "Bytes::length" [fn "String::toBytes" [str "abcdef"]])) ;
  ()


let t_internal_functions () =
  Libbackend.Account.set_admin "test" true ;
  check_dval
    "We should get an error on failed validation"
    (DResult
       (ResError
          (Dval.dstr_of_string_exn
             "Invalid username 'Name with space', must match /^[a-z][a-z0-9_]{2,20}$/")))
    (exec_ast
       (fn
          "DarkInternal::upsertUser_v1"
          [ str "Name with space"
          ; str "valid@email.com"
          ; str "accidentalusername" ])) ;
  ()


let t_old_functions_deprecated () =
  let counts = ref StrDict.empty in
  List.iter (Core.String.Map.to_alist !Libs.static_fns) ~f:(fun (name, fn) ->
      let key = Str.global_replace (Str.regexp "_v[0-9]+") "" name in
      if not fn.deprecated
      then
        counts :=
          StrDict.update !counts ~key ~f:(fun count ->
              count |> Option.withDefault ~default:0 |> ( + ) 1 |> Some) ;
      ()) ;
  StrDict.iter !counts ~f:(fun name count ->
      AT.check AT.int (name ^ " only has one undeprecated fn") 1 count) ;
  ()


let t_url_encode () =
  let open Libshared.FluidShortcuts in
  check_dval
    "percent escaping works"
    (Dval.dstr_of_string_exn
       "https%3A%2F%2Fgoogle.com%3Fq%3Dleft%20shark%26l%3Den")
    (exec_ast
       (fn "Twitter::urlencode" [str "https://google.com?q=left shark&l=en"]))


let t_float_stdlibs () =
  let infinity' = binop "/" (float' 1 0) (float' 0 0) in
  let neg_infinity' = binop "/" (float' (-1) 0) (float' 0 0) in
  let nan' = binop "/" (float' 0 0) (float' 0 0) in
  check_dval
    "Float::sum works"
    (DFloat 1.2)
    (exec_ast (fn "Float::sum" [list [float' 1 0; float' 0 2]])) ;
  AT.check
    AT.bool
    "Float::sum fails on list elements that are not floats"
    true
    ( match exec_ast (fn "Float::sum" [list [float' 1 0; int 2]]) with
    | DError _ ->
        true
    | _ ->
        false ) ;
  check_dval
    "Float::ceiling works"
    (Dval.dint 2)
    (exec_ast (fn "Float::ceiling" [float' 1 3])) ;
  check_dval
    "Float::floor works"
    (Dval.dint 1)
    (exec_ast (fn "Float::floor" [float' 1 8])) ;
  check_dval
    "Float::round works"
    (Dval.dint 2)
    (exec_ast (fn "Float::round" [float' 1 5])) ;
  check_dval
    "Float::truncate works"
    (Dval.dint (-2367))
    (exec_ast (fn "Float::truncate" [float' (-2367) 9267])) ;
  check_dval
    "Float::min works (neg)"
    (DFloat (-10.0))
    (exec_ast (fn "Float::min" [float' (-10) 0; float' 1 0])) ;
  check_dval
    "Float::min works (pos)"
    (DFloat 1.0)
    (exec_ast (fn "Float::min" [float' 10 0; float' 1 0])) ;
  check_dval
    "Float::min works (nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::min" [float' 10 0; nan'])) ;
  check_dval
    "Float::min works (infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat 1.0)
    (exec_ast (fn "Float::min" [infinity'; float' 1 0])) ;
  check_dval
    "Float::max works (neg)"
    (DFloat 1.0)
    (exec_ast (fn "Float::max" [float' (-10) 0; float' 1 0])) ;
  check_dval
    "Float::max works (pos)"
    (DFloat 10.0)
    (exec_ast (fn "Float::max" [float' 10 0; float' 1 0])) ;
  check_dval
    "Float::max works (nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::max" [float' 10 0; nan'])) ;
  check_dval
    "Float::max works (infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.infinity)
    (exec_ast (fn "Float::max" [infinity'; float' 1 0])) ;
  check_dval
    "Float::clamp works (in bounds)"
    (DFloat (-2.0))
    (exec_ast (fn "Float::clamp" [float' (-2) 0; float' (-5) 0; float' 5 0])) ;
  check_dval
    "Float::clamp works (below min)"
    (DFloat (-2.0))
    (exec_ast (fn "Float::clamp" [float' (-3) 0; float' (-2) 0; float' 1 0])) ;
  check_dval
    "Float::clamp works (above max)"
    (DFloat 2.0)
    (exec_ast (fn "Float::clamp" [float' 3 0; float' 0 0; float' 2 0])) ;
  check_dval
    "Float::clamp works (limitA = limitB)"
    (DFloat 1.0)
    (exec_ast (fn "Float::clamp" [float' (-5) 0; float' 1 0; float' 1 0])) ;
  check_dval
    "Float::clamp works (limitB > limitA)"
    (DFloat 1.0)
    (exec_ast (fn "Float::clamp" [float' 1 0; float' 2 0; float' 1 0])) ;
  check_dval
    "Float::clamp works (val = infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat 0.5)
    (exec_ast (fn "Float::clamp" [infinity'; float' (-1) 0; float' 0 5])) ;
  check_dval
    "Float::clamp works (min = -infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat 0.5)
    (exec_ast (fn "Float::clamp" [float' 0 5; neg_infinity'; float' 1 0])) ;
  check_dval
    "Float::clamp works (val = infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat 0.5)
    (exec_ast (fn "Float::clamp" [infinity'; float' (-1) 0; float' 0 5])) ;
  check_dval
    "Float::clamp works (val = nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::clamp" [nan'; float' (-1) 0; float' 1 0])) ;
  check_error_contains
    "Float::clamp errors (limitA = nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (exec_ast (fn "Float::clamp" [float' 0 5; nan'; float' 1 0]))
    "Internal Float.clamp exception" ;
  check_error_contains
    "Float::clamp errors (limitB = nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (exec_ast (fn "Float::clamp" [float' 0 5; float' 1 0; nan']))
    "Internal Float.clamp exception" ;
  check_dval
    "Float::sqrt works"
    (DFloat 5.0)
    (exec_ast (fn "Float::sqrt" [float' 25 0])) ;
  check_dval
    "Float::power works"
    (DFloat 2.0)
    (exec_ast (fn "Float::power" [float' 4 0; float' 0 5])) ;
  check_dval
    "Float::power works"
    (DFloat 0.5)
    (* Writing -0.5 is currently awkward; this relies on string concatenation: *)
    (exec_ast (fn "Float::power" [float' 4 0; floatStr "-0" "5"])) ;
  check_dval
    "Float::absoluteValue works (nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::absoluteValue" [nan'])) ;
  check_dval
    "Float::absoluteValue works (infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.infinity)
    (exec_ast (fn "Float::absoluteValue" [neg_infinity'])) ;
  check_dval
    "Float::absoluteValue works (neg)"
    (DFloat 5.6)
    (exec_ast (fn "Float::absoluteValue" [float' (-5) 6])) ;
  check_dval
    "Float::absoluteValue works (pos)"
    (DFloat 5.6)
    (exec_ast (fn "Float::absoluteValue" [float' (-5) 6])) ;
  check_dval
    "Float::negate works (nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::negate" [nan'])) ;
  check_dval
    "Float::negate works (infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.neg_infinity)
    (exec_ast (fn "Float::negate" [infinity'])) ;
  check_dval
    "Float::negate works (neg)"
    (DFloat 5.6)
    (exec_ast (fn "Float::negate" [float' (-5) 6])) ;
  check_dval
    "Float::negate works (pos)"
    (DFloat (-5.6))
    (exec_ast (fn "Float::negate" [float' 5 6])) ;
  check_dval
    "Float::divide works"
    (DFloat 4.5)
    (exec_ast (fn "Float::divide" [float' 9 0; float' 2 0])) ;
  check_dval
    "Float::add works"
    (DFloat 2.5)
    (exec_ast (fn "Float::add" [float' 1 2; float' 1 3])) ;
  check_dval
    "Float::multiply works"
    (DFloat 13.0)
    (exec_ast (fn "Float::multiply" [float' 26 0; float' 0 5])) ;
  check_dval
    "Float::subtract works"
    (DFloat 0.8)
    (exec_ast (fn "Float::subtract" [float' 1 0; float' 0 2])) ;
  check_dval
    "Float::greaterThan works"
    (DBool true)
    (exec_ast (fn "Float::greaterThan" [float' 0 2; float' 0 1])) ;
  check_dval
    "Float::greaterThanOrEqualTo works"
    (DBool true)
    (exec_ast (fn "Float::greaterThanOrEqualTo" [float' 0 1; float' 0 1])) ;
  check_dval
    "Float::lessThan works"
    (DBool false)
    (exec_ast (fn "Float::lessThan" [float' 0 2; float' 0 1])) ;
  check_dval
    "Float::lessThanOrEqualTo works"
    (DBool true)
    (exec_ast (fn "Float::lessThanOrEqualTo" [float' 0 1; float' 0 1])) ;
  ()


let t_int_stdlibs () =
  let dstr = Dval.dstr_of_string_exn in
  check_dval
    "Int::mod works (sweep, pos)"
    (DList
       [ Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1
       ; Dval.dint 2
       ; Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1
       ; Dval.dint 2
       ; Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1 ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (fn "Int::mod" [var "v"; int 4]) ])) ;
  check_error_contains
    "Int::mod errors (_, 0)"
    (exec_ast (fn "Int::mod" [int 5; int 0]))
    "Expected the argument `b` argument passed to `Int::mod` to be positive, but it was `0`." ;
  check_error_contains
    "Int::mod errors (_, neg)"
    (exec_ast (fn "Int::mod" [int 5; int (-5)]))
    "Expected the argument `b` argument passed to `Int::mod` to be positive, but it was `-5`." ;
  check_dval
    "% works (sweep, pos)"
    (DList
       [ Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1
       ; Dval.dint 2
       ; Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1
       ; Dval.dint 2
       ; Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1 ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (binop "%" (var "v") (int 4)) ])) ;
  check_error_contains
    "% errors (_, 0)"
    (exec_ast (binop "%" (int 5) (int 0)))
    "Expected the argument `b` argument passed to `%` to be positive, but it was `0`." ;
  check_error_contains
    "% errors (_, neg)"
    (exec_ast (binop "%" (int 5) (int (-5))))
    "Expected the argument `b` argument passed to `%` to be positive, but it was `-5`." ;
  (*  (* Int::mod_v1 is not yet available; see implementation for why *)
  check_dval
    "Int::mod_v1 works (sweep, pos)"
    (DList
       [ DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1))
       ; DResult (ResOk (Dval.dint 2))
       ; DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1))
       ; DResult (ResOk (Dval.dint 2))
       ; DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1)) ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (fn "Int::mod_v1" [var "v"; int 4]) ])) ;
  check_dval
    "Int::mod_v1 errors (_, 0)"
    (DResult (ResError (dstr "`modulus` must be positive but was 0")))
    (exec_ast (fn "Int::mod_v1" [int 5; int 0])) ;
  check_dval
    "Int::mod_v1 errors (_, neg)"
    (DResult (ResError (dstr "`modulus` must be positive but was -5")))
    (exec_ast (fn "Int::mod_v1" [int 5; int (-5)])) ; *)
  check_dval
    "Int::remainder works (sweep, pos)"
    (DList
       [ DResult (ResOk (Dval.dint (-1)))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint (-3)))
       ; DResult (ResOk (Dval.dint (-2)))
       ; DResult (ResOk (Dval.dint (-1)))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1))
       ; DResult (ResOk (Dval.dint 2))
       ; DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1)) ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (fn "Int::remainder" [var "v"; int 4]) ])) ;
  check_dval
    "Int::remainder works (sweep, neg)"
    (DList
       [ DResult (ResOk (Dval.dint (-1)))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint (-3)))
       ; DResult (ResOk (Dval.dint (-2)))
       ; DResult (ResOk (Dval.dint (-1)))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1))
       ; DResult (ResOk (Dval.dint 2))
       ; DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1)) ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (fn "Int::remainder" [var "v"; int (-4)]) ])) ;
  check_dval
    "Int::remainder errors (0)"
    (DResult (ResError (dstr "`divisor` must be non-zero")))
    (exec_ast (fn "Int::remainder" [int 5; int 0])) ;
  check_dval
    "Int::max works"
    (Dval.dint 6)
    (exec_ast (fn "Int::max" [int 5; int 6])) ;
  check_dval
    "Int::min works"
    (exec_ast (fn "Int::min" [int 5; int 6]))
    (Dval.dint 5) ;
  check_dval
    "Int::absoluteValue works (neg)"
    (Dval.dint 5)
    (exec_ast (fn "Int::absoluteValue" [int (-5)])) ;
  check_dval
    "Int::absoluteValue works (pos)"
    (Dval.dint 5)
    (exec_ast (fn "Int::absoluteValue" [int 5])) ;
  check_dval
    "Int::clamp works (in bounds)"
    (Dval.dint (-2))
    (exec_ast (fn "Int::clamp" [int (-2); int (-5); int 5])) ;
  check_dval
    "Int::clamp works (below min)"
    (Dval.dint (-2))
    (exec_ast (fn "Int::clamp" [int (-3); int (-2); int 1])) ;
  check_dval
    "Int::clamp works (above max)"
    (Dval.dint 2)
    (exec_ast (fn "Int::clamp" [int 3; int 0; int 2])) ;
  check_dval
    "Int::clamp works (limitA = limitB)"
    (Dval.dint 1)
    (exec_ast (fn "Int::clamp" [int (-5); int 1; int 1])) ;
  check_dval
    "Int::clamp works (limitB > limitA)"
    (Dval.dint 1)
    (exec_ast (fn "Int::clamp" [int 1; int 2; int 1])) ;
  check_dval
    "Int::negate works (neg)"
    (Dval.dint 5)
    (exec_ast (fn "Int::negate" [int (-5)])) ;
  check_dval
    "Int::negate works (pos)"
    (Dval.dint (-5))
    (exec_ast (fn "Int::negate" [int 5])) ;
  ()


let t_bool_stdlibs () =
  check_dval
    "Bool::and works (true, false)"
    (exec_ast (fn "Bool::and" [bool true; bool false]))
    (DBool false) ;
  check_dval
    "Bool::and works (false, true)"
    (exec_ast (fn "Bool::and" [bool false; bool true]))
    (DBool false) ;
  check_dval
    "Bool::and works (false, false)"
    (exec_ast (fn "Bool::and" [bool false; bool false]))
    (DBool false) ;
  check_dval
    "Bool::and works (true, true)"
    (exec_ast (fn "Bool::and" [bool true; bool true]))
    (DBool true) ;
  check_dval
    "Bool::xor works (true, false)"
    (exec_ast (fn "Bool::xor" [bool true; bool false]))
    (DBool true) ;
  check_dval
    "Bool::xor works (false, true)"
    (exec_ast (fn "Bool::xor" [bool false; bool true]))
    (DBool true) ;
  check_dval
    "Bool::xor works (false, false)"
    (exec_ast (fn "Bool::xor" [bool false; bool false]))
    (DBool false) ;
  check_dval
    "Bool::xor works (true, true)"
    (exec_ast (fn "Bool::xor" [bool true; bool true]))
    (DBool false) ;
  check_dval
    "Bool::not works (true)"
    (exec_ast (fn "Bool::not" [bool true]))
    (DBool false) ;
  check_dval
    "Bool::not works (false)"
    (exec_ast (fn "Bool::not" [bool false]))
    (DBool true) ;
  check_dval
    "Bool::or works (true, false)"
    (exec_ast (fn "Bool::or" [bool true; bool false]))
    (DBool true) ;
  check_dval
    "Bool::or works (true, true)"
    (exec_ast (fn "Bool::or" [bool true; bool true]))
    (DBool true) ;
  check_dval
    "Bool::or works (false, false)"
    (exec_ast (fn "Bool::or" [bool false; bool false]))
    (DBool false) ;
  check_dval
    "Bool::isNull works (null)"
    (exec_ast (fn "Bool::isNull" [null]))
    (DBool true) ;
  check_dval
    "Bool::isNull works (not null)"
    (exec_ast (fn "Bool::isNull" [bool true]))
    (DBool false) ;
  ()


(* Test does not demonstrate how it'd be used with complex inputs/lambdas (say,
 * comparing two semvers); the goal is simply to demonstrate:
 * - a basic happy-path works
 * - guards for returning non-int or invalid int (not in {-1,0,1}) error *)
let t_liblist_sort_by_comparator_works () =
  let dlist_of_intlist (is : int list) : dval =
    is
    |> List.map ~f:(fun i -> Dint.of_int i |> DInt)
    |> DList
    |> ResOk
    |> DResult
  in
  let listSortByComparator (lambdaBody : Libshared.FluidExpression.t) :
      Libshared.FluidExpression.t =
    fn
      "List::sortByComparator"
      [list [int 3; int 1; int 2]; lambda ["a"; "b"] lambdaBody]
  in
  check_dval
    "List::sortByComparator works, a valid case"
    (exec_ast
       (listSortByComparator
          (if' (fn "Int::lessThan" [var "a"; var "b"]) (int (-1)) (int 1))))
    (dlist_of_intlist [1; 2; 3]) ;
  check_dval
    "List::sortByComparator returns a ResError if lambda returns non-ints"
    (exec_ast (listSortByComparator (float' 0 1)))
    (DResult
       (ResError
          (Dval.dstr_of_string_exn
             "`f` must return one of -1, 0, 1, but returned non-int: 0.1"))) ;
  check_dval
    "List::sortByComparator returns a ResError if lambda returns invalid ints"
    (exec_ast (listSortByComparator (int 3)))
    (DResult
       (ResError
          (Dval.dstr_of_string_exn
             "`f` must return one of -1, 0, 1, but returned another int: 3"))) ;
  ()


let t_math_stdlibs () =
  check_dval
    "Math::pi works"
    (DFloat 3.141592653589793)
    (exec_ast (fn "Math::pi" [])) ;
  check_dval
    "Math::tau works"
    (DFloat 6.283185307179586)
    (exec_ast (fn "Math::tau" [])) ;
  check_dval
    "Math::degrees works (pos)"
    (DFloat 6.283185307179586 (* tau *))
    (exec_ast (fn "Math::degrees" [float' 360 0])) ;
  check_dval
    "Math::degrees works (neg)"
    (DFloat (-3.141592653589793) (* -pi *))
    (exec_ast (fn "Math::degrees" [float' (-180) 0])) ;
  check_dval
    "Math::turns works (pos)"
    (DFloat 6.283185307179586 (* tau *))
    (exec_ast (fn "Math::turns" [float' 1 0])) ;
  check_dval
    "Math::turns works (neg)"
    (DFloat (-3.141592653589793) (* -pi *))
    (exec_ast (fn "Math::turns" [fn "Float::negate" [float' 0 5]])) ;
  check_dval
    "Math::radians works (pos)"
    (DFloat 6.283185307179586 (* tau *))
    (exec_ast (fn "Math::radians" [float' 6 283185307179586])) ;
  check_dval
    "Math::radians works (neg)"
    (DFloat (-3.141592653589793) (* -pi *))
    (exec_ast
       (fn "Math::radians" [fn "Float::negate" [float' 3 141592653589793]])) ;
  check_dval
    "Math::cos works (0)"
    (DFloat 1.0)
    (exec_ast (fn "Math::cos" [float' 0 0])) ;
  check_dval
    "Math::cos works (pi)"
    (DFloat (-1.0))
    (exec_ast (fn "Math::cos" [fn "Math::pi" []])) ;
  check_dval
    "Math::sin works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::sin" [float' 0 0])) ;
  check_dval
    "Math::sin works (pi/2)"
    (DFloat 1.0)
    (exec_ast (fn "Math::sin" [fn "/" [fn "Math::pi" []; float' 2 0]])) ;
  check_dval
    "Math::tan works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::tan" [float' 0 0])) ;
  check_dval
    "Math::tan works (pi/4)"
    (DFloat 0.9999999999999999 (* ~1.0 *))
    (exec_ast (fn "Math::tan" [fn "/" [fn "Math::pi" []; float' 4 0]])) ;
  check_dval
    "Math::acos works in range (1)"
    (DOption (OptJust (DFloat 0.0)))
    (exec_ast (fn "Math::acos" [float' 1 0])) ;
  check_dval
    "Math::acos works in range (-1)"
    (DOption (OptJust (DFloat 3.141592653589793 (* pi *))))
    (exec_ast (fn "Math::acos" [float' (-1) 0])) ;
  check_dval
    "Math::acos works (out of range)"
    (DOption OptNothing)
    (exec_ast (fn "Math::acos" [float' 5 0])) ;
  check_dval
    "Math::asin works (in range - 0)"
    (DOption (OptJust (DFloat 0.0)))
    (exec_ast (fn "Math::asin" [float' 0 0])) ;
  check_dval
    "Math::asin works (in range - 1)"
    (DOption (OptJust (DFloat 1.5707963267948966 (* pi/2 *))))
    (exec_ast (fn "Math::asin" [float' 1 0])) ;
  check_dval
    "Math::asin works (out of range)"
    (DOption OptNothing)
    (exec_ast (fn "Math::asin" [float' 5 0])) ;
  check_dval
    "Math::atan works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::atan" [float' 0 0])) ;
  check_dval
    "Math::atan works (1)"
    (DFloat 0.7853981633974483 (* pi/4 *))
    (exec_ast (fn "Math::atan" [float' 1 0])) ;
  check_dval
    "Math::atan2 works (0/1)"
    (DFloat 0.0)
    (exec_ast (fn "Math::atan2" [float' 0 0; float' 1 0])) ;
  check_dval
    "Math::atan2 works (1/1)"
    (DFloat 0.7853981633974483 (* pi/4 *))
    (exec_ast (fn "Math::atan2" [float' 1 0; float' 1 0])) ;
  check_dval
    "Math::atan2 works (-1/1)"
    (DFloat (-0.7853981633974483) (* -pi/4 *))
    (exec_ast (fn "Math::atan2" [float' (-1) 0; float' 1 0])) ;
  check_dval
    "Math::atan2 works (1/-1)"
    (DFloat 2.356194490192345 (* 3/4 pi *))
    (exec_ast (fn "Math::atan2" [float' 1 0; float' (-1) 0])) ;
  check_dval
    "Math::atan2 works (-1/-1)"
    (DFloat (-2.356194490192345) (* -3/4 pi *))
    (exec_ast (fn "Math::atan2" [float' (-1) 0; float' (-1) 0])) ;
  check_dval
    "Math::cosh works (0)"
    (DFloat 1.0)
    (exec_ast (fn "Math::cosh" [float' 0 0])) ;
  check_dval
    "Math::sinh works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::sinh" [float' 0 0])) ;
  check_dval
    "Math::tanh works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::tanh" [float' 0 0])) ;
  ()


let t_libhttp () =
  let dstr = Dval.dstr_of_string_exn in
  check_dval
    "Http::setCookie_v2 works (no params)"
    (DObj (DvalMap.from_list [("Set-Cookie", dstr "myCookie=myVal")]))
    (exec_ast
       (fn "Http::setCookie_v2" [str "myCookie"; str "myVal"; record []])) ;
  check_dval
    "Http::setCookie_v2 works (all params)"
    (DObj
       (DvalMap.from_list
          [ ( "Set-Cookie"
            , dstr
                "myCookie=myVal; Secure; SameSite=Strict; Path=/my/path; Max-Age=3600; HttpOnly; Expires=Sun, 28 Jul 2019 22:42:00 GMT; Domain=darklang.com"
            ) ]))
    (exec_ast
       (match'
          (fn "Date::parse_v1" [str "2019-07-28T22:42:00Z"])
          [ ( pOk (pVar "date")
            , fn
                "Http::setCookie_v2"
                [ str "myCookie"
                ; str "myVal"
                ; record
                    [ ("Expires", var "date")
                    ; ("Max-Age", int 3600)
                    ; ("Domain", str "darklang.com")
                    ; ("Path", str "/my/path")
                    ; ("Secure", bool true)
                    ; ("HttpOnly", bool true)
                    ; ("SameSite", str "Strict") ] ] ) ])) ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong expires type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Expires", int 5)]]))
    "Expected the Set-Cookie parameter `Expires` passed to `Http::setCookie_v2` to be a `Date`, but it had type `Int` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong max-age type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Max-Age", str "foo")]]))
    "Expected the Set-Cookie parameter `Max-Age` passed to `Http::setCookie_v2` to be an `Int` representing seconds, but it had type `String` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong domain type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Domain", int 5)]]))
    "Expected the Set-Cookie parameter `Domain` passed to `Http::setCookie_v2` to be a `String`, but it had type `Int` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong path type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Path", int 5)]]))
    "Expected the Set-Cookie parameter `Path` passed to `Http::setCookie_v2` to be a `String`, but it had type `Int` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong secure type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Secure", int 5)]]))
    "Expected the Set-Cookie parameter `Secure` passed to `Http::setCookie_v2` to have the value `true` or `false`, but it had the value `5` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong httponly type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("HttpOnly", int 5)]]))
    "Expected the Set-Cookie parameter `HttpOnly` passed to `Http::setCookie_v2` to have the value `true` or `false`, but it had the value `5` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong samesite type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("SameSite", int 5)]]))
    "Expected the Set-Cookie parameter `SameSite` passed to `Http::setCookie_v2` to have the value `\"Strict\"`, `\"Lax\"`, or `\"None\"`, but it had the value `5` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong samesite value)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [ str "myCookie"
          ; str "myVal"
          ; record [("SameSite", str "allthesecures")] ]))
    "Expected the Set-Cookie parameter `SameSite` passed to `Http::setCookie_v2` to have the value `\"Strict\"`, `\"Lax\"`, or `\"None\"`, but it had the value `\"allthesecures\"` instead." ;
  ()


let t_libhttpclient () =
  check_error_contains
    "HttpClient::get_v5 illegal urls"
    (exec_ast
       (fn
          "HttpClient::get_v5"
          [str "http://thenonexistingurlforsure.com"; record []; record []]))
    "Couldn't resolve host name" ;
  ()


(* This test doesn't bother to destructure + examine the contents of the dobj;
 * it's just intended to ensure that the thing runs and doesn't DError. Esp
 * since the contents of the DObj depend on the database's stats and those
 * aren't stable in the test env! We could do a spot check on the keys present,
 * but I don't think that's terribly useful right now, type guarantees are
 * enough. *)
let t_darkinternal_table_stats_works () =
  let ast = fn "DarkInternal::getAndLogTableSizes" [] in
  AT.check
    AT.bool
    "DarkInternal::table_stats works (no params)"
    true
    (match exec_ast ast with DObj _ -> true | _ -> false) ;
  ()


let suite =
  [ ("Option stdlibs work", `Quick, t_option_stdlibs_work)
  ; ("Result stdlibs work", `Quick, t_result_stdlibs_work)
  ; ("Dict stdlibs work", `Quick, t_dict_stdlibs_work)
  ; ("List stdlibs work", `Quick, t_list_stdlibs_work)
  ; ("String stdlibs work", `Quick, t_string_stdlibs_work)
  ; ( "End-user password hashing and checking works"
    , `Quick
    , t_password_hashing_and_checking_works )
  ; ("JWT lib works.", `Quick, t_jwt_functions_work)
  ; ("Date lib works", `Quick, t_date_functions_work)
  ; ("Functions deprecated correctly", `Quick, t_old_functions_deprecated)
  ; ("Internal functions work", `Quick, t_internal_functions)
  ; ("Crypto::sha digest functions work", `Quick, t_crypto_sha)
  ; ("Crypto::sha256hmac works for AWS", `Quick, t_sha256hmac_for_aws)
  ; ("URL percent encoding", `Quick, t_url_encode)
  ; ("Float stdlibs work", `Quick, t_float_stdlibs)
  ; ("Int stdlibs work", `Quick, t_int_stdlibs)
  ; ("Bool stdlibs work", `Quick, t_bool_stdlibs)
  ; ("Bytes stdlibs work", `Quick, t_libbytes)
  ; ("List::sortByComparator works", `Quick, t_liblist_sort_by_comparator_works)
  ; ("Math stdlibs work", `Quick, t_math_stdlibs)
  ; ("HTTP stdlibs work", `Quick, t_libhttp)
  ; ("DarkInternal::table_stats works", `Quick, t_darkinternal_table_stats_works)
  ; ("HttpClient error handling", `Quick, t_libhttpclient) ]
