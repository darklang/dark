open Core_kernel
open Libexecution
open Libbackend
open Types
open Types.RuntimeT
open Utils
open Libshared.FluidShortcuts

let t_db_add_roundtrip () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "old"
      (record [("x", null)])
      (let'
         "key"
         (fn "DB::add_v0" [var "old"; var "MyDB"])
         (fn "DB::get_v1" ~ster:Rail [var "key"; var "MyDB"]))
  in
  check_dval
    "equal_after_roundtrip"
    (DObj (DvalMap.singleton "x" DNull))
    (exec_handler ~ops ast)


let t_db_new_query_v1_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str")
    ; AddDBCol (dbid, colnameid2, coltypeid2)
    ; SetDBColName (dbid, colnameid2, "y")
    ; SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    let'
      "dontfind"
      (fn
         "DB::set_v1"
         [record [("x", str "foo"); ("y", str "bar")]; str "hello"; var "MyDB"])
      (let'
         "hopetofind"
         (fn
            "DB::set_v1"
            [ record [("x", str "bar"); ("y", str "foo")]
            ; str "findme"
            ; var "MyDB" ])
         (let'
            "results"
            (fn "DB::query_v1" [record [("x", str "bar")]; var "MyDB"])
            (binop
               "=="
               (list [list [str "findme"; var "hopetofind"]])
               (var "results"))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_new_query_v2_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str")
    ; AddDBCol (dbid, colnameid2, coltypeid2)
    ; SetDBColName (dbid, colnameid2, "y")
    ; SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    let'
      "dontfind"
      (fn
         "DB::set_v1"
         [record [("x", str "foo"); ("y", str "bar")]; str "hello"; var "MyDB"])
      (let'
         "hopetofind"
         (fn
            "DB::set_v1"
            [ record [("x", str "bar"); ("y", str "foo")]
            ; str "findme"
            ; var "MyDB" ])
         (let'
            "results"
            (fn "DB::query_v2" [record [("x", str "bar")]; var "MyDB"])
            (binop "==" (list [var "hopetofind"]) (var "results"))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_new_query_v3_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str")
    ; AddDBCol (dbid, colnameid2, coltypeid2)
    ; SetDBColName (dbid, colnameid2, "y")
    ; SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    let'
      "dontfind"
      (fn
         "DB::set_v1"
         [record [("x", str "foo"); ("y", str "bar")]; str "hello"; var "MyDB"])
      (let'
         "hopetofind"
         (fn
            "DB::set_v1"
            [ record [("x", str "bar"); ("y", str "foo")]
            ; str "findme"
            ; var "MyDB" ])
         (let'
            "results"
            (fn "DB::query_v3" [record [("x", str "bar")]; var "MyDB"])
            (binop "==" (list [var "hopetofind"]) (var "results"))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_set_does_upsert () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "old"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "hello"; var "MyDB"])
      (let'
         "new"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "hello"; var "MyDB"])
         (let'
            "results"
            (fn "DB::getAllWithKeys_v1" [var "MyDB"])
            (binop "==" (list [list [str "hello"; var "new"]]) (var "results"))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_get_all_with_keys_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid2)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "one"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "two"; var "MyDB"])
         (let'
            "results"
            (fn "DB::getAllWithKeys_v1" [var "MyDB"])
            (binop
               "=="
               (list [list [str "one"; var "one"]; list [str "two"; var "two"]])
               (var "results"))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_get_all_with_keys_v2_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid2)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "second"; var "MyDB"])
         (let'
            "results"
            (fn "DB::getAllWithKeys_v2" [var "MyDB"])
            (var "results")))
  in
  check_dval
    "equal_after_roundtrip"
    (DObj
       (DvalMap.from_list
          [ ( "second"
            , DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "bar")) )
          ; ( "first"
            , DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "foo")) ) ]))
    (exec_handler ~ops ast)


let t_db_get_many_with_keys_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "second"; var "MyDB"])
         (let'
            "fetched"
            (fn
               "DB::getManyWithKeys"
               [list [str "first"; str "second"]; var "MyDB"])
            (binop
               "=="
               (list
                  [list [str "first"; var "one"]; list [str "second"; var "two"]])
               (var "fetched"))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_get_many_with_keys_v1_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "second"; var "MyDB"])
         (let'
            "fetched"
            (fn
               "DB::getManyWithKeys_v1"
               [list [str "first"; str "second"]; var "MyDB"])
            (var "fetched")))
  in
  check_dval
    "equal_after_roundtrip"
    (DObj
       (DvalMap.from_list
          [ ( "second"
            , DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "bar")) )
          ; ( "first"
            , DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "foo")) ) ]))
    (exec_handler ~ops ast)


let t_db_get_existing_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "second"; var "MyDB"])
         (let'
            "fetched"
            (fn
               "DB::getExisting"
               [list [str "third"; str "second"]; var "MyDB"])
            (var "fetched")))
  in
  check_dval
    "equal_after_roundtrip"
    (DList [DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "bar"))])
    (exec_handler ~ops ast)


let t_db_get_many_v3_returns_nothing_if_none () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in

  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "second"; var "MyDB"])
         (let'
            "fetched"
            (fn "DB::getMany_v3" [list [str "third"; str "second"]; var "MyDB"])
            (var "fetched")))
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_get_many_v3_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in

  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "second"; var "MyDB"])
         (let'
            "fetched"
            (fn "DB::getMany_v3" [list [str "first"; str "second"]; var "MyDB"])
            (var "fetched")))
  in
  check_dval
    "equal_after_roundtrip"
    (DOption
       (OptJust
          (DList
             [ DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "foo"))
             ; DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "bar")) ])))
    (exec_handler ~ops ast)


let t_db_get_many_v2_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "second"; var "MyDB"])
         (let'
            "fetched"
            (fn "DB::getMany_v2" [list [str "first"; str "second"]; var "MyDB"])
            (var "fetched")))
  in
  check_dval
    "equal_after_roundtrip"
    (DList
       [ DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "foo"))
       ; DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "bar")) ])
    (exec_handler ~ops ast)


let t_db_get_many_v1_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "second"; var "MyDB"])
         (let'
            "fetched"
            (fn
               "DB::getManyWithKeys"
               [list [str "first"; str "second"]; var "MyDB"])
            (binop
               "=="
               (list
                  [list [str "first"; var "one"]; list [str "second"; var "two"]])
               (var "fetched"))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_queryWithKey_works_with_many () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str")
    ; AddDBCol (dbid, colnameid2, coltypeid2)
    ; SetDBColName (dbid, colnameid2, "sort_by")
    ; SetDBColType (dbid, coltypeid2, "Int") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    let'
      "one"
      (fn
         "DB::set_v1"
         [record [("x", str "foo"); ("sort_by", int 0)]; str "one"; var "MyDB"])
      (let'
         "two"
         (fn
            "DB::set_v1"
            [ record [("x", str "bar"); ("sort_by", int 1)]
            ; str "two"
            ; var "MyDB" ])
         (let'
            "three"
            (fn
               "DB::set_v1"
               [ record [("x", str "bar"); ("sort_by", int 2)]
               ; str "three"
               ; var "MyDB" ])
            (let'
               "fetched"
               (fn
                  "List::sortBy"
                  [ fn
                      "DB::queryWithKey_v1"
                      [record [("x", str "bar")]; var "MyDB"]
                  ; lambda
                      ["x"]
                      (fieldAccess (fn "List::last" [var "x"]) "sort_by") ])
               (binop
                  "=="
                  (list
                     [ list [str "two"; var "two"]
                     ; list [str "three"; var "three"] ])
                  (var "fetched")))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_queryWithKey_v2_works_with_many () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "one"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "two"; var "MyDB"])
         (let'
            "three"
            (fn
               "DB::set_v1"
               [record [("x", str "bar")]; str "three"; var "MyDB"])
            (let'
               "fetched"
               (fn
                  "DB::queryWithKey_v2"
                  [record [("x", str "bar")]; var "MyDB"])
               (var "fetched"))))
  in
  check_dval
    "equal_after_roundtrip"
    (DObj
       (DvalMap.from_list
          [ ("two", DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "bar")))
          ; ( "three"
            , DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "bar")) ) ]))
    (exec_handler ~ops ast)


let t_db_get_returns_nothing () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  check_dval
    "get_returns_nothing"
    (DOption OptNothing)
    (exec_handler ~ops (fn "DB::get_v1" [str "lol"; var "MyDB"]))


let t_db_queryOne_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (fn "DB::queryOne_v1" [record [("x", str "foo")]; var "MyDB"])
  in
  check_dval
    "equal_after_roundtrip"
    (DOption
       (OptJust (DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "foo")))))
    (exec_handler ~ops ast)


let t_db_queryOne_supports_Date_comparison () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "ts")
    ; SetDBColType (dbid, coltypeid, "Date") ]
  in
  let after = "2100-01-01T00:00:00Z" in
  let before = "1900-01-01T00:00:00Z" in
  let middle = "2000-01-01T00:00:00Z" in
  (* The Result::map_v1 and Option::andThen calls are necessary because
   * Date::parse_v2 returns a Result, and exec_ast doesn't leave those on
   * the rail. *)
  let ast op =
    let'
      "_"
      (fn
         "Result::map_v1"
         [ fn "Date::parse_v2" [str before]
         ; lambda
             ["date"]
             (fn
                "DB::set_v1"
                [record [("ts", var "date")]; str "before"; var "MyDB"]) ])
      (let'
         "_"
         (fn
            "Result::map_v1"
            [ fn "Date::parse_v2" [str after]
            ; lambda
                ["date"]
                (fn
                   "DB::set_v1"
                   [record [("ts", var "date")]; str "after"; var "MyDB"]) ])
         (let'
            "middle"
            (fn "Date::parse_v2" [str middle])
            (fn
               "Option::andThen"
               [ fn "Result::toOption" [var "middle"]
               ; lambda
                   ["middle"]
                   (fn
                      "DB::queryOne_v4"
                      [ var "MyDB"
                      ; lambda
                          ["value"]
                          (fn op [var "middle"; fieldAccess (var "value") "ts"])
                      ]) ])))
  in
  let expected (date : string) : dval =
    DOption
      (OptJust
         (DObj (DvalMap.singleton "ts" (DDate (date |> Util.date_of_isostring)))))
  in
  check_dval
    "Date::< middle ts.value gets us the after date"
    (expected after)
    (exec_ast ~ops (ast "Date::<")) ;
  check_dval
    "Date::> middle ts.value gets us the before date"
    (expected before)
    (exec_ast ~ops (ast "Date::>")) ;
  check_dval
    "Date::lessThan middle ts.value gets us the after date"
    (expected after)
    (exec_ast ~ops (ast "Date::lessThan")) ;
  check_dval
    "Date::greaterThan middle ts.value gets us the before date"
    (expected before)
    (exec_ast ~ops (ast "Date::greaterThan"))


let t_db_queryOne_returns_nothing_if_none () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (fn "DB::queryOne_v1" [record [("x", str "bar")]; var "MyDB"])
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOne_returns_nothing_multiple () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "one"
         (fn "DB::set_v1" [record [("x", str "foo")]; str "second"; var "MyDB"])
         (fn "DB::queryOne_v1" [record [("x", str "foo")]; var "MyDB"]))
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (fn "DB::queryOneWithKey_v1" [record [("x", str "foo")]; var "MyDB"])
  in
  check_dval
    "equal_after_roundtrip"
    (DOption
       (OptJust
          (DList
             [ Dval.dstr_of_string_exn "first"
             ; DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "foo")) ])))
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_v2_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (fn "DB::queryOneWithKey_v2" [record [("x", str "foo")]; var "MyDB"])
  in
  check_dval
    "equal_after_roundtrip"
    (DOption
       (OptJust
          (DObj
             (DvalMap.singleton
                "first"
                (DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "foo")))))))
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_returns_nothing_if_none () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (fn "DB::queryOneWithKey_v1" [record [("x", str "bar")]; var "MyDB"])
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_v2_returns_nothing_if_none () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (fn "DB::queryOneWithKey_v2" [record [("x", str "bar")]; var "MyDB"])
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_returns_nothing_multiple () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "one"
         (fn "DB::set_v1" [record [("x", str "foo")]; str "second"; var "MyDB"])
         (fn "DB::queryOneWithKey_v1" [record [("x", str "foo")]; var "MyDB"]))
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_v2_returns_nothing_multiple () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "one"
         (fn "DB::set_v1" [record [("x", str "foo")]; str "second"; var "MyDB"])
         (fn "DB::queryOneWithKey_v2" [record [("x", str "foo")]; var "MyDB"]))
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_getAll_v1_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str")
    ; AddDBCol (dbid, colnameid2, coltypeid2)
    ; SetDBColName (dbid, colnameid2, "sort_by")
    ; SetDBColType (dbid, coltypeid2, "Int") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    let'
      "one"
      (fn
         "DB::set_v1"
         [record [("x", str "foo"); ("sort_by", int 0)]; str "one"; var "MyDB"])
      (let'
         "two"
         (fn
            "DB::set_v1"
            [ record [("x", str "bar"); ("sort_by", int 1)]
            ; str "two"
            ; var "MyDB" ])
         (let'
            "three"
            (fn
               "DB::set_v1"
               [ record [("x", str "baz"); ("sort_by", int 2)]
               ; str "three"
               ; var "MyDB" ])
            (let'
               "fetched"
               (fn
                  "List::sortBy"
                  [ fn "DB::getAll_v1" [var "MyDB"]
                  ; lambda ["x"] (fieldAccess (var "x") "sort_by") ])
               (binop
                  "=="
                  (list
                     [ list [str "one"; var "one"]
                     ; list [str "three"; var "three"]
                     ; list [str "two"; var "two"] ])
                  (var "fetched")))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_getAll_v2_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str")
    ; AddDBCol (dbid, colnameid2, coltypeid2)
    ; SetDBColName (dbid, colnameid2, "sort_by")
    ; SetDBColType (dbid, coltypeid2, "Int") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    let'
      "one"
      (fn
         "DB::set_v1"
         [record [("x", str "foo"); ("sort_by", int 0)]; str "one"; var "MyDB"])
      (let'
         "two"
         (fn
            "DB::set_v1"
            [ record [("x", str "bar"); ("sort_by", int 1)]
            ; str "two"
            ; var "MyDB" ])
         (let'
            "three"
            (fn
               "DB::set_v1"
               [ record [("x", str "baz"); ("sort_by", int 2)]
               ; str "three"
               ; var "MyDB" ])
            (let'
               "fetched"
               (fn
                  "List::sortBy"
                  [ fn "DB::getAll_v2" [var "MyDB"]
                  ; lambda ["x"] (fieldAccess (var "x") "sort_by") ])
               (binop
                  "=="
                  (list [var "one"; var "two"; var "three"])
                  (var "fetched")))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_getAll_v3_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str")
    ; AddDBCol (dbid, colnameid2, coltypeid2)
    ; SetDBColName (dbid, colnameid2, "sort_by")
    ; SetDBColType (dbid, coltypeid2, "Int") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    let'
      "one"
      (fn
         "DB::set_v1"
         [record [("x", str "foo"); ("sort_by", int 0)]; str "one"; var "MyDB"])
      (let'
         "two"
         (fn
            "DB::set_v1"
            [ record [("x", str "bar"); ("sort_by", int 1)]
            ; str "two"
            ; var "MyDB" ])
         (let'
            "three"
            (fn
               "DB::set_v1"
               [ record [("x", str "baz"); ("sort_by", int 2)]
               ; str "three"
               ; var "MyDB" ])
            (let'
               "fetched"
               (fn
                  "List::sortBy"
                  [ fn "DB::getAll_v3" [var "MyDB"]
                  ; lambda ["x"] (fieldAccess (var "x") "sort_by") ])
               (binop
                  "=="
                  (list [var "one"; var "two"; var "three"])
                  (var "fetched")))))
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_getAllKeys_works () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid2)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    let'
      "one"
      (fn "DB::set_v1" [record [("x", str "foo")]; str "first"; var "MyDB"])
      (let'
         "two"
         (fn "DB::set_v1" [record [("x", str "bar")]; str "second"; var "MyDB"])
         (let'
            "results"
            (fn "DB::keys_v1" [var "MyDB"])
            (fn "List::sort" [var "results"])))
  in
  check_dval
    "equal_after_roundtrip"
    (DList [Dval.dstr_of_string_exn "first"; Dval.dstr_of_string_exn "second"])
    (exec_handler ~ops ast)


let t_sql_compiler_works () =
  let open Types in
  let open Prelude in
  let _, state, _ = Utils.test_execution_data [] in
  let check
      (msg : string)
      ?(paramName = "value")
      ?(dbFields = [])
      ?(symtable = [])
      (body : fluid_expr)
      (generated : string) : unit =
    let dbFields = StrDict.from_list dbFields in
    let symtable = StrDict.from_list symtable in
    let result =
      Sql_compiler.compile_lambda ~state symtable paramName dbFields body
    in
    AT.check AT.string msg result generated
  in
  let checkError
      (msg : string)
      ?(paramName = "value")
      ?(dbFields = [])
      ?(symtable = [])
      (body : fluid_expr)
      (expectedError : string) : unit =
    try check msg ~paramName ~dbFields ~symtable body "<error expected>"
    with Db.DBQueryException e -> AT.check AT.string msg e expectedError
  in
  let true' = bool true in
  check "true is true" true' "(true)" ;
  let field = fieldAccess (var "value") "myfield" in
  check
    "correct SQL for field access"
    ~dbFields:[("myfield", TBool)]
    field
    "(CAST(data::jsonb->>'myfield' as bool))" ;
  checkError
    "no field gives error"
    field
    "The datastore does not have a field named: myfield" ;
  let injection = "'; select * from user_data ;'field" in
  let field = binop "==" (fieldAccess (var "value") injection) (str "x") in
  check
    "field accesses are escaped"
    ~dbFields:[(injection, TStr)]
    field
    "((CAST(data::jsonb->>'''; select * from user_data ;''field' as text)) = ('x'))" ;
  let variable = binop "==" (var "var") (fieldAccess (var "value") "name") in
  check
    "symtable escapes correctly"
    ~dbFields:[("name", TStr)]
    ~symtable:[("var", Dval.dstr_of_string_exn "';select * from user_data;'")]
    variable
    "((''';select * from user_data;''') = (CAST(data::jsonb->>'name' as text)))" ;
  let thread =
    pipe
      (fieldAccess (var "value") "age")
      [ binop "-" pipeTarget (int 2)
      ; binop "+" pipeTarget (fieldAccess (var "value") "age")
      ; binop "<" pipeTarget (int 3) ]
  in
  check
    ~dbFields:[("age", TInt)]
    "pipes expand correctly into nested functions"
    thread
    "((((CAST(data::jsonb->>'age' as integer)) - (2)) + (CAST(data::jsonb->>'age' as integer))) < (3))" ;
  check_fluid_expr
    "canonicalize works on pipes with targets"
    (Sql_compiler.canonicalize thread)
    (binop
       "<"
       (binop
          "+"
          (binop "-" (fieldAccess (var "value") "age") (int 2))
          (fieldAccess (var "value") "age"))
       (int 3)) ;
  check_fluid_expr
    "inline works (with nested inlines)"
    (let expr =
       let'
         "x"
         (int 5)
         (let' "x" (int 6) (binop "+" (int 3) (let' "x" (int 7) (var "x"))))
     in
     Sql_compiler.inline "value" StrDict.empty expr)
    (binop "+" (int 3) (int 7)) ;
  check_fluid_expr
    "inline works (def at root)"
    (let expr =
       let'
         "y"
         (int 5)
         (let' "x" (int 6) (binop "+" (int 3) (let' "x" (int 7) (var "y"))))
     in
     Sql_compiler.inline "value" StrDict.empty expr)
    (binop "+" (int 3) (int 5)) ;
  ()


let t_db_query_works () =
  let open Libshared.FluidShortcuts in
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "Person")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "name")
    ; SetDBColType (dbid, coltypeid, "Str")
    ; AddDBCol (dbid, colnameid2, coltypeid2)
    ; SetDBColName (dbid, colnameid2, "human")
    ; SetDBColType (dbid, coltypeid2, "Bool")
    ; AddDBCol (dbid, colnameid3, coltypeid3)
    ; SetDBColName (dbid, colnameid3, "height")
    ; SetDBColType (dbid, coltypeid3, "Int")
    ; AddDBCol (dbid, colnameid4, coltypeid4)
    ; SetDBColName (dbid, colnameid4, "income")
    ; SetDBColType (dbid, coltypeid4, "Float")
    ; AddDBCol (dbid, colnameid5, coltypeid5)
    ; SetDBColName (dbid, colnameid5, "dob")
    ; SetDBColType (dbid, coltypeid5, "Date") ]
  in
  (* Prepopulate the DB for tests *)
  let ross_dob = str "1967-05-12T00:00:00Z" in
  let ross_expr =
    record
      [ ("height", int 73)
      ; ("name", str "Ross")
      ; ("human", bool true)
      ; ("dob", fn ~ster:Rail "Date::parse_v2" [ross_dob])
      ; ("income", float' 100 00) ]
  in
  let rachel_expr =
    record
      [ ("height", int 65)
      ; ("name", str "Rachel")
      ; ("human", bool true)
      ; ("dob", fn ~ster:Rail "Date::parse_v2" [str "1969-05-05T00:00:00Z"])
      ; ("income", float' 82 00) ]
  in
  let chandler_expr =
    record
      [ ("height", int 72)
      ; ("name", str " Chandler ")
      ; ("human", bool true)
      ; ("dob", fn ~ster:Rail "Date::parse_v2" [str "1969-08-19T10:00:00Z"])
      ; ("income", float' 83 00) ]
  in
  let cat_expr =
    record
      [ ("height", int 10)
      ; ("name", str "GrumpyCat")
      ; ("human", bool false)
      ; ("dob", fn ~ster:Rail "Date::parse_v2" [str "2012-04-04T00:00:00Z"])
      ; ("income", float' 0 00) ]
  in
  let null_expr =
    record
      [ ("height", null)
      ; ("name", null)
      ; ("human", null)
      ; ("dob", null)
      ; ("income", null) ]
  in
  let ross = exec_ast ross_expr in
  let rachel = exec_ast rachel_expr in
  let chandler = exec_ast chandler_expr in
  let cat = exec_ast cat_expr in
  let nullval = exec_ast null_expr in
  let expected =
    exec_handler
      ~ops
      (let'
         "_"
         (fn "DB::set_v1" [ross_expr; str "ross"; var "Person"])
         (let'
            "_"
            (fn "DB::set_v1" [rachel_expr; str "rachel"; var "Person"])
            (let'
               "_"
               (fn "DB::set_v1" [chandler_expr; str "chandler"; var "Person"])
               (let'
                  "_"
                  (fn "DB::set_v1" [cat_expr; str "cat"; var "Person"])
                  (let'
                     "_"
                     (fn "DB::set_v1" [null_expr; str "null"; var "Person"])
                     (int 5))))))
  in
  check_dval "setup worked" expected (Dval.dint 5) ;
  let query expr = fn "DB::query_v4" [var "Person"; expr] in
  let queryv body = fn "DB::query_v4" [var "Person"; lambda ["v"] body] in
  let field name field = fieldAccess (var name) field in
  let sort expr =
    pipe expr [fn "List::sortBy" [pipeTarget; lambda ["v"] (field "v" "height")]]
  in
  let exec expr = exec_handler ~ops expr in
  let execs expr = exec (expr |> sort) in
  let withvar (name : string) value ast = let' name value ast in
  check_dval
    "Find all"
    (DList [cat; rachel; chandler; ross; nullval])
    (queryv (bool true) |> execs) ;
  check_dval
    "Find all with condition"
    (DList [cat; rachel; chandler; ross])
    (queryv (binop ">" (field "v" "height") (int 3)) |> execs) ;
  check_dval
    "boolean"
    (DList [rachel; chandler; ross])
    (queryv (field "v" "human") |> execs) ;
  check_dval
    "different param name"
    (DList [rachel; chandler; ross])
    (query (lambda ["value"] (field "value" "human")) |> execs) ;
  check_dval
    "&&"
    (DList [chandler; ross])
    ( queryv
        (binop
           "&&"
           (field "v" "human")
           (binop ">" (field "v" "height") (int 66)))
    |> execs ) ;
  check_dval
    "inlining"
    (DList [rachel; chandler; ross])
    ( queryv
        (let'
           "x"
           (int 32)
           (binop "&&" (bool true) (binop ">" (field "v" "height") (var "x"))))
    |> execs ) ;
  check_dval
    "inlining - fieldAccesses"
    (DList [rachel; chandler; ross])
    ( queryv
        (let'
           "x"
           (field "v" "height")
           (binop "&&" (bool true) (binop ">" (var "x") (int 32))))
    |> execs ) ;
  check_dval
    "pipes"
    (DList [cat])
    ( queryv
        (pipe
           (field "v" "height")
           [binop "*" pipeTarget (int 2); binop "<" pipeTarget (int 40)])
    |> execs ) ;
  check_dval
    "external variable works"
    (DList [cat])
    ( queryv (pipe (field "v" "height") [binop "<" pipeTarget (var "x")])
    |> withvar "x" (int 20)
    |> execs ) ;
  check_dval
    "fieldAccess"
    (DList [rachel; chandler; ross])
    ( let'
        "myObj"
        (record [("x", int 42)])
        (queryv (binop ">" (field "v" "height") (field "myObj" "x")))
    |> execs ) ;
  check_dval
    "nested fieldAccess"
    (DList [rachel; chandler; ross])
    ( let'
        "myObj"
        (record [("field1", record [("field2", int 42)])])
        (queryv
           (binop
              ">"
              (field "v" "height")
              (fieldAccess (field "myObj" "field1") "field2")))
    |> execs ) ;
  check_error
    "not a bool"
    (queryv (str "x") |> exec)
    (Db.dbQueryExceptionToString
       (Db.DBQueryException
          "Incorrect type in `\"x\"`, expected Bool but got a Str in \"x\"")) ;
  check_error
    "bad variable name"
    ( queryv
        (let'
           "x"
           (int 32)
           (binop "&&" (bool true) (binop ">" (field "v" "height") (var "y"))))
    |> exec )
    (Db.dbQueryExceptionToString
       (Db.DBQueryException "This variable is not defined: y")) ;
  check_dval
    "sql injection"
    (DList [])
    ( queryv (binop "==" (str "; select * from users ;") (field "v" "name"))
    |> exec ) ;
  check_dval
    "null equality works"
    (DList [nullval])
    (queryv (binop "==" (field "v" "name") null) |> execs) ;
  check_dval
    "null inequality works"
    (DList [cat; rachel; chandler; ross])
    (queryv (binop "!=" (field "v" "name") null) |> execs) ;
  check_dval
    "null is not 'null'"
    (DList [cat; rachel; chandler; ross])
    (queryv (binop "!=" (field "v" "name") (str "null")) |> execs) ;
  (* Just check enough of the other functions to verify the signature - *)
  (* they all use they same function behind the scenes. *)
  check_dval
    "queryOne - multiple"
    (DOption OptNothing)
    ( fn "DB::queryOne_v4" [var "Person"; lambda ["v"] (field "v" "human")]
    |> exec ) ;
  check_dval
    "queryOne - none"
    (DOption OptNothing)
    ( fn
        "DB::queryOne_v4"
        [var "Person"; lambda ["v"] (binop "==" (str "bob") (field "v" "name"))]
    |> exec ) ;
  check_dval
    "queryOne - one"
    (DOption (OptJust rachel))
    ( fn
        "DB::queryOne_v4"
        [ var "Person"
        ; lambda ["v"] (binop "==" (str "Rachel") (field "v" "name")) ]
    |> exec ) ;
  check_dval
    "queryOneWithKey - multiple"
    (DOption OptNothing)
    ( fn
        "DB::queryOneWithKey_v3"
        [var "Person"; lambda ["v"] (field "v" "human")]
    |> exec ) ;
  check_dval
    "queryOneWithKey - none"
    (DOption OptNothing)
    ( fn
        "DB::queryOneWithKey_v3"
        [var "Person"; lambda ["v"] (binop "==" (str "bob") (field "v" "name"))]
    |> exec ) ;
  check_dval
    "queryOneWithKey - one"
    (DOption (OptJust rachel))
    ( pipe
        (fn
           "DB::queryOneWithKey_v3"
           [ var "Person"
           ; lambda ["v"] (binop "==" (str "Rachel") (field "v" "name")) ])
        [fn "Option::map_v1" [pipeTarget; lambda ["r"] (field "r" "rachel")]]
    |> exec ) ;
  check_dval
    "queryOneWithKey - empty"
    (DObj DvalMap.empty)
    ( fn
        "DB::queryWithKey_v3"
        [var "Person"; lambda ["v"] (binop "==" (str "bob") (field "v" "name"))]
    |> exec ) ;
  check_dval
    "queryWithKey - more than one"
    rachel
    ( pipe
        (fn
           "DB::queryWithKey_v3"
           [ var "Person"
           ; lambda ["v"] (binop "==" (str "Rachel") (field "v" "name")) ])
        [lambda ["r"] (field "r" "rachel")]
    |> exec ) ;
  check_dval
    "queryCount - empty"
    (Dval.dint 0)
    ( fn
        "DB::queryCount"
        [var "Person"; lambda ["v"] (binop "==" (str "bob") (field "v" "name"))]
    |> exec ) ;
  check_dval
    "queryCount"
    (Dval.dint 4)
    ( fn
        "DB::queryCount"
        [var "Person"; lambda ["v"] (binop ">" (field "v" "height") (int 3))]
    |> exec ) ;

  (* -------------- *)
  (* Test functions *)
  (* -------------- *)
  check_dval
    "float"
    (DList [ross])
    ( queryv (binop "Float::greaterThan" (field "v" "income") (float' 90 0))
    |> execs ) ;
  check_dval
    "int <="
    (DList [cat; rachel])
    ( queryv (binop "Int::lessThanOrEqualTo" (field "v" "height") (int 65))
    |> execs ) ;
  check_dval
    "float"
    (DList [cat; rachel])
    ( queryv
        (binop "Float::lessThanOrEqualTo" (field "v" "income") (float' 82 1))
    |> execs ) ;
  check_dval
    "string::tolower"
    (DList [rachel])
    ( queryv
        (binop
           "=="
           (fn "String::toLowercase_v1" [field "v" "name"])
           (str "rachel"))
    |> execs ) ;
  check_dval
    "string::reverse"
    (DList [rachel])
    ( queryv
        (binop "==" (fn "String::reverse" [field "v" "name"]) (str "lehcaR"))
    |> execs ) ;
  check_dval
    "string::length"
    (DList [cat; rachel; chandler])
    ( queryv (binop ">" (fn "String::length" [field "v" "name"]) (int 5))
    |> execs ) ;
  check_dval
    "string::isSubstring_v1"
    (DList [rachel; ross])
    (queryv (fn "String::isSubstring_v1" [field "v" "name"; str "R"]) |> execs) ;
  check_dval
    "string::isSubstring_v1 case-sensitive"
    (DList [])
    ( queryv (fn "String::isSubstring_v1" [field "v" "name"; str "ROSS"])
    |> execs ) ;
  check_dval
    "string::isSubstring_v1 when empty"
    (DList [])
    (queryv (fn "String::isSubstring_v1" [field "v" "name"; str "ZZZ"]) |> execs) ;
  check_dval
    "string::isSubstring_v1 empty arg"
    (DList [cat; rachel; chandler; ross])
    (* matches the ocaml version: "" is a substring of all strings *)
    (queryv (fn "String::isSubstring_v1" [field "v" "name"; str ""]) |> execs) ;
  check_dval
    "string::contains"
    (DList [rachel; ross])
    (queryv (fn "String::contains" [field "v" "name"; str "R"]) |> execs) ;
  check_dval
    "string::contains case-sensitive"
    (DList [])
    (queryv (fn "String::contains" [field "v" "name"; str "ROSS"]) |> execs) ;
  check_dval
    "string::contains when empty"
    (DList [])
    (queryv (fn "String::contains" [field "v" "name"; str "ZZZ"]) |> execs) ;
  check_dval
    "string::contains empty arg"
    (DList [cat; rachel; chandler; ross])
    (* matches the ocaml version: "" is a substring of all strings *)
    (queryv (fn "String::contains" [field "v" "name"; str ""]) |> execs) ;
  check_dval
    "date::lessThanOrEquals"
    (DList [rachel; chandler; ross])
    ( queryv
        (binop
           "Date::<="
           (field "v" "dob")
           (fn "Date::parse" [str "2000-01-01T01:02:03Z"]))
    |> execs ) ;
  check_dval
    "date::lessThanOrEquals - equality"
    (DList [ross])
    ( queryv (binop "Date::<=" (field "v" "dob") (fn "Date::parse" [ross_dob]))
    |> execs ) ;
  check_dval
    "date::lessThan - equality"
    (DList [])
    ( queryv (binop "Date::<" (field "v" "dob") (fn "Date::parse" [ross_dob]))
    |> execs ) ;
  check_dval
    "date::greaterThanOrEquals"
    (DList [cat])
    ( queryv
        (binop
           "Date::>="
           (field "v" "dob")
           (fn "Date::parse" [str "2000-01-01T01:02:03Z"]))
    |> execs ) ;
  check_dval
    "date::greaterThanOrEquals - equality"
    (DList [cat; rachel; chandler; ross])
    ( queryv (binop "Date::>=" (field "v" "dob") (fn "Date::parse" [ross_dob]))
    |> execs ) ;
  check_dval
    "date::greaterThan - equality"
    (DList [cat; rachel; chandler])
    ( queryv (binop "Date::>" (field "v" "dob") (fn "Date::parse" [ross_dob]))
    |> execs ) ;
  check_dval
    "Date::add"
    (DList [cat; rachel; chandler; ross])
    ( queryv
        (binop
           "Date::<="
           (field "v" "dob")
           (fn "Date::add" [fn "Date::now" []; int 1]))
    |> execs ) ;
  check_dval
    "Date::subtract"
    (DList [cat; rachel; chandler; ross])
    ( queryv
        (binop
           "Date::<="
           (field "v" "dob")
           (fn "Date::subtract" [fn "Date::now" []; int 1]))
    |> execs ) ;
  check_dval
    "string::trim"
    (DList [chandler])
    ( queryv (binop "==" (fn "String::trim" [field "v" "name"]) (str "Chandler"))
    |> execs ) ;
  check_dval
    "string::trimStart"
    (DList [chandler])
    ( queryv
        (binop
           "=="
           (fn "String::trimStart" [field "v" "name"])
           (str "Chandler "))
    |> execs ) ;
  check_dval
    "string::trimEnd"
    (DList [chandler])
    ( queryv
        (binop "==" (fn "String::trimEnd" [field "v" "name"]) (str " Chandler"))
    |> execs ) ;
  check_dval
    "date::hour"
    (DList [chandler])
    ( queryv (binop "==" (fn "Date::hour_v1" [field "v" "dob"]) (int 10))
    |> execs ) ;
  (* -------------- *)
  (* Test partial evaluation *)
  (* -------------- *)
  check_dval
    "partial evaluation - fieldAccesses outside query"
    (DList [cat; rachel; chandler])
    ( let'
        "x"
        (record [("y", record [("z", record [("a", int 5)])])])
        (queryv
           (binop
              "<"
              (fieldAccess (fieldAccess (fieldAccess (var "x") "y") "z") "a")
              (fn "String::length" [field "v" "name"])))
    |> execs ) ;
  check_dval
    "partial evaluation - fieldAccesses"
    (DList [cat; rachel; chandler])
    ( queryv
        (let'
           "x"
           (record [("y", record [("z", record [("a", int 5)])])])
           (binop
              "<"
              (fieldAccess (fieldAccess (fieldAccess (var "x") "y") "z") "a")
              (fn "String::length" [field "v" "name"])))
    |> execs ) ;
  check_dval
    "partial execution - List::length"
    (DList [cat; rachel; chandler])
    ( queryv
        (let'
           "x"
           (record
              [ ( "y"
                , record
                    [ ( "z"
                      , record [("a", list [int 1; int 2; int 3; int 4; int 5])]
                      ) ] ) ])
           (binop
              "<"
              (pipe
                 (fieldAccess (fieldAccess (fieldAccess (var "x") "y") "z") "a")
                 [fn "List::length" [pipeTarget]])
              (fn "String::length" [field "v" "name"])))
    |> execs ) ;
  ()


let suite =
  [ ("DB::getAll_v1 works", `Quick, t_db_getAll_v1_works)
  ; ("DB::getAll_v2 works", `Quick, t_db_getAll_v2_works)
  ; ("DB::getAll_v3 works", `Quick, t_db_getAll_v3_works)
  ; ("DB::add works", `Quick, t_db_add_roundtrip)
  ; ("New query_v1 function works", `Quick, t_db_new_query_v1_works)
  ; ("New query_v2 function works", `Quick, t_db_new_query_v2_works)
  ; ("New query_v3 function works", `Quick, t_db_new_query_v3_works)
  ; ("DB::set_v1 upserts", `Quick, t_db_set_does_upsert)
  ; ("DB::getAllWithKeys_v1 works", `Quick, t_db_get_all_with_keys_works)
  ; ("DB::getAllWithKeys_v2 works", `Quick, t_db_get_all_with_keys_v2_works)
  ; ("DB::getManyWithKeys works", `Quick, t_db_get_many_with_keys_works)
  ; ("DB::getManyWithKeys_v2 works", `Quick, t_db_get_many_with_keys_v1_works)
  ; ("DB::getMany_v1 works", `Quick, t_db_get_many_v1_works)
  ; ("DB::getMany_v2 works", `Quick, t_db_get_many_v2_works)
  ; ("DB::getMany_v3 works", `Quick, t_db_get_many_v3_works)
  ; ( "DB::getMany_v3 returns Nothing if any items are not found"
    , `Quick
    , t_db_get_many_v3_returns_nothing_if_none )
  ; ("DB::getExisting works", `Quick, t_db_get_existing_works)
  ; ( "DB::queryWithKey_v2 works with many items"
    , `Quick
    , t_db_queryWithKey_v2_works_with_many )
  ; ( "DB::queryWithKey_v1 works with many items"
    , `Quick
    , t_db_queryWithKey_works_with_many )
  ; ("DB::get_v1 returns Nothing if not found", `Quick, t_db_get_returns_nothing)
  ; ("DB::queryOne returns Some obj if found", `Quick, t_db_queryOne_works)
  ; ( "DB::queryOne_v4 supports Date:: comparison"
    , `Quick
    , t_db_queryOne_supports_Date_comparison )
  ; ( "DB::queryOne returns Nothing if not found"
    , `Quick
    , t_db_queryOne_returns_nothing_if_none )
  ; ( "DB::queryOne returns Nothing if more than one found"
    , `Quick
    , t_db_queryOne_returns_nothing_multiple )
  ; ( "DB::queryOneWithKey returns Just obj if found"
    , `Quick
    , t_db_queryOneWithKey_works )
  ; ( "DB::queryOneWithKey returns Nothing if not found"
    , `Quick
    , t_db_queryOneWithKey_returns_nothing_if_none )
  ; ( "DB::queryOneWithKey returns Nothing if more than one found"
    , `Quick
    , t_db_queryOneWithKey_returns_nothing_multiple )
  ; ( "DB::queryOneWithKey_v2 returns Just obj if found"
    , `Quick
    , t_db_queryOneWithKey_v2_works )
  ; ( "DB::queryOneWithKey_v2 returns Nothing if not found"
    , `Quick
    , t_db_queryOneWithKey_v2_returns_nothing_if_none )
  ; ( "DB::queryOneWithKey_v2 returns Nothing if more than one found"
    , `Quick
    , t_db_queryOneWithKey_v2_returns_nothing_multiple )
  ; ("t_db_getAllKeys_works returns List of keys", `Quick, t_db_getAllKeys_works)
  ; ("t_db_query works", `Quick, t_db_query_works)
  ; ("t_sql_compiler_works", `Quick, t_sql_compiler_works) ]
