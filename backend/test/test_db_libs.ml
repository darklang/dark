open Core_kernel
open Libexecution
open Libbackend
open Types.RuntimeT
open Utils

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
          [ ( "two"
            , DObj (DvalMap.singleton "x" (Dval.dstr_of_string_exn "bar")) )
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
      (let results (DB::getAllKeys MyDB)
      results)))"
  in
  check_dval
    "equal_after_roundtrip"
    (DList [Dval.dstr_of_string_exn "first"; Dval.dstr_of_string_exn "second"])
    (exec_handler ~ops ast)


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
  ; ( "DB::get_v1 returns Nothing if not found"
    , `Quick
    , t_db_get_returns_nothing )
  ; ("DB::queryOne returns Some obj if found", `Quick, t_db_queryOne_works)
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
  ; ( "t_db_getAllKeys_works returns List of keys"
    , `Quick
    , t_db_getAllKeys_works ) ]
