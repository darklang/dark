open Core_kernel
open Libexecution
open Libbackend
open Types.RuntimeT
open Utils
open Libshared.FluidShortcuts

let t_db_add_roundtrip () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let old (obj (x null))
       (let key (DB::add_v0 old MyDB)
         (`DB::get_v1 key MyDB)))"
  in
  check_dval
    "equal_after_roundtrip"
    (DObj (DvalMap.singleton "x" DNull))
    (exec_handler ~ops ast)


let t_db_new_query_v1_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid2, "y")
    ; Op.SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    "(let dontfind (DB::set_v1 (obj (x 'foo') (y 'bar')) 'hello' MyDB)
              (let hopetofind (DB::set_v1 (obj (x 'bar') (y 'foo')) 'findme' MyDB)
              (let results (DB::query_v1 (obj (x 'bar')) MyDB)
                (== (('findme' hopetofind)) results))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_new_query_v2_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid2, "y")
    ; Op.SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    "(let dontfind (DB::set_v1 (obj (x 'foo') (y 'bar')) 'hello' MyDB)
               (let hopetofind (DB::set_v1 (obj (x 'bar') (y 'foo')) 'findme' MyDB)
                (let results (DB::query_v2 (obj (x 'bar')) MyDB)
                 (== (hopetofind) results))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_new_query_v3_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid2, "y")
    ; Op.SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    "(let dontfind (DB::set_v1 (obj (x 'foo') (y 'bar')) 'hello' MyDB)
                (let hopetofind (DB::set_v1 (obj (x 'bar') (y 'foo')) 'findme' MyDB)
                (let results (DB::query_v3 (obj (x 'bar')) MyDB)
                (== (hopetofind) results))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_set_does_upsert () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let old (DB::set_v1 (obj (x 'foo')) 'hello' MyDB)
               (let new (DB::set_v1 (obj (x 'bar')) 'hello' MyDB)
                (let results (DB::getAllWithKeys_v1 MyDB)
                 (== (('hello' new)) results))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_get_all_with_keys_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'one' MyDB)
              (let two (DB::set_v1 (obj (x 'bar')) 'two' MyDB)
               (let results (DB::getAllWithKeys_v1 MyDB)
                (== (('one' one) ('two' two)) results))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_get_all_with_keys_v2_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
    (let two (DB::set_v1 (obj (x 'bar')) 'second' MyDB)
      (let results (DB::getAllWithKeys_v2 MyDB)
      results)))"
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
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (let two (DB::set_v1 (obj (x 'bar')) 'second' MyDB)
               (let fetched (DB::getManyWithKeys ('first' 'second') MyDB)
                (== (('first' one) ('second' two)) fetched))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_get_many_with_keys_v1_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (let two (DB::set_v1 (obj (x 'bar')) 'second' MyDB)
                (let fetched (DB::getManyWithKeys_v1 ('first' 'second') MyDB)
                fetched)))"
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


let t_db_get_many_v2_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
                (let two (DB::set_v1 (obj (x 'bar')) 'second' MyDB)
                  (let fetched (DB::getMany_v2 ('first' 'second') MyDB)
                  fetched)))"
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
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (let two (DB::set_v1 (obj (x 'bar')) 'second' MyDB)
                (let fetched (DB::getManyWithKeys ('first' 'second') MyDB)
                (== (('first' one) ('second' two)) fetched))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_queryWithKey_works_with_many () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid2, "sort_by")
    ; Op.SetDBColType (dbid, coltypeid2, "Int") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo') (sort_by 0)) 'one' MyDB)
              (let two (DB::set_v1 (obj (x 'bar') (sort_by 1)) 'two' MyDB)
               (let three (DB::set_v1 (obj (x 'bar') (sort_by 2)) 'three' MyDB)
                (let fetched (List::sortBy (DB::queryWithKey_v1 (obj (x 'bar')) MyDB) (\\x -> (. (List::last x) sort_by)))
                 (== (('two' two) ('three' three)) fetched)))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_queryWithKey_v2_works_with_many () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'one' MyDB)
              (let two (DB::set_v1 (obj (x 'bar')) 'two' MyDB)
               (let three (DB::set_v1 (obj (x 'bar')) 'three' MyDB)
                (let fetched (DB::queryWithKey_v2 (obj (x 'bar')) MyDB)
                 fetched))))"
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
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  check_dval
    "get_returns_nothing"
    (DOption OptNothing)
    (exec_handler ~ops "(DB::get_v1 'lol' MyDB)")


let t_db_queryOne_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOne_v1 (obj (x 'foo')) MyDB))"
  in
  check_dval
    "equal_after_roundtrip"
    (DOption
       (OptJust (DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "foo")))))
    (exec_handler ~ops ast)


let t_db_queryOne_supports_Date_comparison () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "ts")
    ; Op.SetDBColType (dbid, coltypeid, "Date") ]
  in
  let after = "2100-01-01T00:00:00Z" in
  let before = "1900-01-01T00:00:00Z" in
  let middle = "2000-01-01T00:00:00Z" in
  (* The Result::map_v1 and Option::andThen calls are necessary because
   * Date::parse_v2 returns a Result, and exec_ast' doesn't leave those on
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
  let expected (date : string) : expr dval =
    DOption
      (OptJust
         (DObj (DvalMap.singleton "ts" (DDate (date |> Util.date_of_isostring)))))
  in
  check_dval
    "Date::< middle ts.value gets us the after date"
    (expected after)
    (exec_ast' ~ops (ast "Date::<")) ;
  check_dval
    "Date::> middle ts.value gets us the before date"
    (expected before)
    (exec_ast' ~ops (ast "Date::>")) ;
  check_dval
    "Date::lessThan middle ts.value gets us the after date"
    (expected after)
    (exec_ast' ~ops (ast "Date::lessThan")) ;
  check_dval
    "Date::greaterThan middle ts.value gets us the before date"
    (expected before)
    (exec_ast' ~ops (ast "Date::greaterThan"))


let t_db_queryOne_returns_nothing_if_none () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOne_v1 (obj (x 'bar')) MyDB))"
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOne_returns_nothing_multiple () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (let one (DB::set_v1 (obj (x 'foo')) 'second' MyDB)
               (DB::queryOne_v1 (obj (x 'foo')) MyDB)))"
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOneWithKey_v1 (obj (x 'foo')) MyDB))"
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
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOneWithKey_v2 (obj (x 'foo')) MyDB))"
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
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOneWithKey_v1 (obj (x 'bar')) MyDB))"
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_v2_returns_nothing_if_none () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOneWithKey_v2 (obj (x 'bar')) MyDB))"
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_returns_nothing_multiple () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (let one (DB::set_v1 (obj (x 'foo')) 'second' MyDB)
               (DB::queryOneWithKey_v1 (obj (x 'foo')) MyDB)))"
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_queryOneWithKey_v2_returns_nothing_multiple () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (let one (DB::set_v1 (obj (x 'foo')) 'second' MyDB)
                (DB::queryOneWithKey_v2 (obj (x 'foo')) MyDB)))"
  in
  check_dval
    "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)


let t_db_getAll_v1_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid2, "sort_by")
    ; Op.SetDBColType (dbid, coltypeid2, "Int") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo') (sort_by 0)) 'one' MyDB)
       (let two (DB::set_v1 (obj (x 'bar') (sort_by 1)) 'two' MyDB)
         (let three (DB::set_v1 (obj (x 'baz') (sort_by 2)) 'three' MyDB)
            (let fetched (List::sortBy (DB::getAll_v1 MyDB) (\\x -> (. x sort_by)))
            (== (('one' one) ('three' three) ('two' two)) fetched)))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_getAll_v2_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid2, "sort_by")
    ; Op.SetDBColType (dbid, coltypeid2, "Int") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo') (sort_by 0)) 'one' MyDB)
        (let two (DB::set_v1 (obj (x 'bar') (sort_by 1)) 'two' MyDB)
          (let three (DB::set_v1 (obj (x 'baz') (sort_by 2)) 'three' MyDB)
            (let fetched (List::sortBy (DB::getAll_v2 MyDB) (\\x -> (. x sort_by)))
              (== (one two three) fetched)))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_getAll_v3_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid2, "sort_by")
    ; Op.SetDBColType (dbid, coltypeid2, "Int") ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo') (sort_by 0)) 'one' MyDB)
          (let two (DB::set_v1 (obj (x 'bar') (sort_by 1)) 'two' MyDB)
            (let three (DB::set_v1 (obj (x 'baz') (sort_by 2)) 'three' MyDB)
              (let fetched (List::sortBy (DB::getAll_v3 MyDB) (\\x -> (. x sort_by)))
                (== (one two three) fetched)))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_getAllKeys_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid2, "Str") ]
  in
  let ast =
    "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
    (let two (DB::set_v1 (obj (x 'bar')) 'second' MyDB)
      (let results (DB::keys_v1 MyDB)
      (List::sort results))))"
  in
  check_dval
    "equal_after_roundtrip"
    (DList [Dval.dstr_of_string_exn "first"; Dval.dstr_of_string_exn "second"])
    (exec_handler ~ops ast)


let t_sql_compiler_works () =
  let open Types in
  let open Prelude in
  let f nexpr = Filled (Util.create_id (), nexpr) in
  let _, state, _ = Utils.test_execution_data [] in
  let check
      (msg : string)
      ?(paramName = "value")
      ?(dbFields = [])
      ?(symtable = [])
      (body : expr)
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
      (body : expr)
      (expectedError : string) : unit =
    try check msg ~paramName ~dbFields ~symtable body "<error expected>"
    with Db.DBQueryException e -> AT.check AT.string msg e expectedError
  in
  let true' = f (Value "true") in
  check "true is true" true' "(true)" ;
  let fieldAccess = f (FieldAccess (f (Variable "value"), f "myfield")) in
  check
    "correct SQL for field access"
    ~dbFields:[("myfield", TBool)]
    fieldAccess
    "(CAST(data::jsonb->>'myfield' as bool))" ;
  checkError
    "no field gives error"
    fieldAccess
    "The datastore does not have a field named: myfield" ;
  let injection = "'; select * from user_data ;'field" in
  let fieldAccess =
    f
      (FnCall
         ( "=="
         , [ f (FieldAccess (f (Variable "value"), f injection))
           ; f (Value "\"x\"") ] ))
  in
  check
    "field accesses are escaped"
    ~dbFields:[(injection, TStr)]
    fieldAccess
    "((CAST(data::jsonb->>'''; select * from user_data ;''field' as text)) = ('x'))" ;
  let var =
    f
      (FnCall
         ( "=="
         , [f (Variable "var"); f (FieldAccess (f (Variable "value"), f "name"))]
         ))
  in
  check
    "symtable escapes correctly"
    ~dbFields:[("name", TStr)]
    ~symtable:[("var", Dval.dstr_of_string_exn "';select * from user_data;'")]
    var
    "((''';select * from user_data;''') = (CAST(data::jsonb->>'name' as text)))" ;
  let thread =
    f
      (Thread
         [ f (FieldAccess (f (Variable "value"), f "age"))
         ; f (FnCall ("-", [f (Value "2")]))
         ; f (FnCall ("+", [f (FieldAccess (f (Variable "value"), f "age"))]))
         ; f (FnCall ("<", [f (Value "3")])) ])
  in
  check
    ~dbFields:[("age", TInt)]
    "pipes expand correctly into nested functions"
    thread
    "((((CAST(data::jsonb->>'age' as integer)) - (2)) + (CAST(data::jsonb->>'age' as integer))) < (3))" ;
  ()


let t_db_query_works () =
  let open Libshared.FluidShortcuts in
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "Person")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "name")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid, colnameid2, "human")
    ; Op.SetDBColType (dbid, coltypeid2, "Bool")
    ; Op.AddDBCol (dbid, colnameid3, coltypeid3)
    ; Op.SetDBColName (dbid, colnameid3, "height")
    ; Op.SetDBColType (dbid, coltypeid3, "Int")
    ; Op.AddDBCol (dbid, colnameid4, coltypeid4)
    ; Op.SetDBColName (dbid, colnameid4, "income")
    ; Op.SetDBColType (dbid, coltypeid4, "Float")
    ; Op.AddDBCol (dbid, colnameid5, coltypeid5)
    ; Op.SetDBColName (dbid, colnameid5, "dob")
    ; Op.SetDBColType (dbid, coltypeid5, "Date") ]
  in
  (* Prepopulate the DB for tests *)
  let expected =
    exec_handler'
      ~ops
      (let'
         "_"
         (fn
            "DB::set_v1"
            [ record
                [ ("height", int 73)
                ; ("name", str "Ross")
                ; ("human", bool true)
                ; ( "dob"
                  , fn ~ster:Rail "Date::parse_v2" [str "1967-05-12T00:00:00Z"]
                  )
                ; ("income", float' 100 00) ]
            ; str "ross"
            ; var "Person" ])
         (let'
            "_"
            (fn
               "DB::set_v1"
               [ record
                   [ ("height", int 65)
                   ; ("name", str "Rachel")
                   ; ("human", bool true)
                   ; ( "dob"
                     , fn
                         ~ster:Rail
                         "Date::parse_v2"
                         [str "1969-05-05T00:00:00Z"] )
                   ; ("income", float' 82 00) ]
               ; str "rachel"
               ; var "Person" ])
            (let'
               "_"
               (fn
                  "DB::set_v1"
                  [ record
                      [ ("height", int 10)
                      ; ("name", str "GrumpyCat")
                      ; ("human", bool false)
                      ; ( "dob"
                        , fn
                            ~ster:Rail
                            "Date::parse_v2"
                            [str "2012-04-04T00:00:00Z"] )
                      ; ("income", float' 0 00) ]
                  ; str "cat"
                  ; var "Person" ])
               (let'
                  "_"
                  (fn
                     "DB::set_v1"
                     [ record
                         [ ("height", null)
                         ; ("name", null)
                         ; ("human", null)
                         ; ("dob", null)
                         ; ("income", null) ]
                     ; str "null"
                     ; var "Person" ])
                  (int 5)))))
  in
  check_dval "setup worked" expected (Dval.dint 5) ;
  let query expr = fn "DB::query_v4" [var "Person"; expr] in
  let queryv body = fn "DB::query_v4" [var "Person"; lambda ["v"] body] in
  let field name field = fieldAccess (var name) field in
  let sort expr =
    pipe
      expr
      [ fn "List::map" [pipeTarget; lambda ["v"] (field "v" "height")]
      ; fn "List::sort" [pipeTarget] ]
  in
  let exec expr = exec_handler' ~ops expr in
  let execs expr = exec (expr |> sort) in
  let withvar (name : string) value ast = let' name value ast in
  let ross = Dval.dint 73 in
  let rachel = Dval.dint 65 in
  let cat = Dval.dint 10 in
  check_dval
    "Find all"
    (DList [cat; rachel; ross; DNull])
    (queryv (bool true) |> execs) ;
  check_dval
    "Find all with condition"
    (DList [cat; rachel; ross])
    (queryv (binop ">" (field "v" "height") (int 3)) |> execs) ;
  check_dval
    "boolean"
    (DList [rachel; ross])
    (queryv (field "v" "human") |> execs) ;
  check_dval
    "different param name"
    (DList [rachel; ross])
    (query (lambda ["value"] (field "value" "human")) |> execs) ;
  check_dval
    "&&"
    (DList [ross])
    ( queryv
        (binop
           "&&"
           (field "v" "human")
           (binop ">" (field "v" "height") (int 66)))
    |> execs ) ;
  check_dval
    "inlining"
    (DList [rachel; ross])
    ( queryv
        (let'
           "x"
           (int 32)
           (binop "&&" (bool true) (binop ">" (field "v" "height") (var "x"))))
    |> execs ) ;
  check_dval
    "inlining - fieldAccesses"
    (DList [rachel; ross])
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
    (DList [rachel; ross])
    ( let'
        "myObj"
        (record [("x", int 42)])
        (queryv (binop ">" (field "v" "height") (field "myObj" "x")))
    |> execs ) ;
  check_dval
    "nested fieldAccess"
    (DList [rachel; ross])
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
    (DList [DNull])
    (queryv (binop "==" (field "v" "name") null) |> execs) ;
  check_dval
    "null inequality works"
    (DList [cat; rachel; ross])
    (queryv (binop "!=" (field "v" "name") null) |> execs) ;
  check_dval
    "null is not 'null'"
    (DList [cat; rachel; ross])
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
    ( pipe
        (fn
           "DB::queryOne_v4"
           [ var "Person"
           ; lambda ["v"] (binop "==" (str "Rachel") (field "v" "name")) ])
        [fn "Option::map_v1" [pipeTarget; lambda ["r"] (field "r" "height")]]
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
        [ fn "Option::map_v1" [pipeTarget; lambda ["r"] (field "r" "rachel")]
        ; fn "Option::map_v1" [pipeTarget; lambda ["r"] (field "r" "height")] ]
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
        [lambda ["r"] (field "r" "rachel"); lambda ["r"] (field "r" "height")]
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
    (DList [cat; rachel])
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
    (DList [cat; rachel; ross])
    (* matches the ocaml version: "" is a substring of all strings *)
    (queryv (fn "String::isSubstring_v1" [field "v" "name"; str ""]) |> execs) ;
  check_dval
    "string::contains"
    (DList [Dval.dint 65; Dval.dint 73])
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
    (DList [Dval.dint 10; Dval.dint 65; Dval.dint 73])
    (* matches the ocaml version: "" is a substring of all strings *)
    (queryv (fn "String::contains" [field "v" "name"; str ""]) |> execs) ;
  (* -------------- *)
  (* Test partial evaluation *)
  (* -------------- *)
  check_dval
    "partial evaluation - fieldAccesses outside query"
    (DList [cat; rachel])
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
    (DList [cat; rachel])
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
    (DList [cat; rachel])
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
