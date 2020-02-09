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
  let check
      (msg : string)
      ?(paramName = "value")
      ?(dbFields = [])
      ?(symtable = [])
      (body : expr)
      (generated : string) : unit =
    let dbFields = StrDict.from_list dbFields in
    let symtable = StrDict.from_list symtable in
    let result = Sql_compiler.compile_lambda symtable paramName dbFields body in
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
  let var = f (FnCall ("==", [f (Variable "var"); f (Value "\"x\"")])) in
  check
    "symtable escapes correctly"
    ~symtable:[("var", Dval.dstr_of_string_exn "';select * from user_data;'")]
    var
    "((''';select * from user_data;''') = ('x'))" ;
  let thread =
    f
      (Thread
         [ f (Value "5")
         ; f (FnCall ("-", [f (Value "2")]))
         ; f (FnCall ("+", [f (Value "3")]))
         ; f (FnCall ("<", [f (Value "3")])) ])
  in
  check
    "pipes expand correctly into nested functions"
    thread
    "((((5) - (2)) + (3)) < (3))" ;
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
    ; Op.SetDBColType (dbid, coltypeid3, "Int") ]
  in
  (* Prepopulate the DB for tests *)
  exec_handler'
    ~ops
    (let'
       "_"
       (fn
          "DB::set_v1"
          [ record
              [("height", int 73); ("name", str "Ross"); ("human", bool true)]
          ; str "ross"
          ; var "Person" ])
       (let'
          "_"
          (fn
             "DB::set_v1"
             [ record
                 [ ("height", int 65)
                 ; ("name", str "Rachel")
                 ; ("human", bool true) ]
             ; str "rachel"
             ; var "Person" ])
          (let'
             "_"
             (fn
                "DB::set_v1"
                [ record
                    [ ("height", int 10)
                    ; ("name", str "GrumpyCat")
                    ; ("human", bool false) ]
                ; str "cat"
                ; var "Person" ])
             (let'
                "_"
                (fn
                   "DB::set_v1"
                   [ record [("height", null); ("name", null); ("human", null)]
                   ; str "null"
                   ; var "Person" ])
                (EBlank (Libshared.Shared.gid ()))))))
  |> ignore ;
  let query lambda = fn "DB::query_v4" [var "Person"; lambda] in
  let sort expr =
    pipe
      expr
      [ fn
          "List::map"
          [pipeTarget; lambda ["v"] (fieldAccess (var "v") "height")]
      ; fn "List::sort" [pipeTarget] ]
  in
  let exec expr = exec_handler' ~ops expr in
  let withvar (name : string) value ast = let' name value ast in
  check_dval
    "Find all"
    (DList [Dval.dint 10; Dval.dint 65; Dval.dint 73; DNull])
    ( query (lambda ["value"] (bool true))
    |> sort
    |> Libcommon.Log.inspect ~f:Libshared.FluidExpression.show "expr"
    |> exec ) ;
  check_dval
    "Find all with condition"
    (DList [Dval.dint 10; Dval.dint 65; Dval.dint 73])
    ( query
        (lambda
           ["value"]
           (binop ">" (fieldAccess (var "value") "height") (int 3)))
    |> sort
    |> exec ) ;
  check_dval
    "boolean"
    (DList [Dval.dint 65; Dval.dint 73])
    ( query (lambda ["value"] (fieldAccess (var "value") "human"))
    |> sort
    |> exec ) ;
  check_dval
    "different param name"
    (DList [Dval.dint 65; Dval.dint 73])
    (query (lambda ["v"] (fieldAccess (var "v") "human")) |> sort |> exec) ;
  check_dval
    "&&"
    (DList [Dval.dint 73])
    ( query
        (lambda
           ["v"]
           (binop
              "&&"
              (fieldAccess (var "v") "human")
              (binop ">" (fieldAccess (var "v") "height") (int 66))))
    |> sort
    |> exec ) ;
  check_dval
    "inlining"
    (DList [Dval.dint 65; Dval.dint 73])
    ( query
        (lambda
           ["v"]
           (let'
              "x"
              (int 32)
              (binop
                 "&&"
                 (bool true)
                 (binop ">" (fieldAccess (var "v") "height") (var "x")))))
    |> sort
    |> exec ) ;
  check_dval
    "pipes"
    (DList [Dval.dint 10])
    ( query
        (lambda
           ["v"]
           (pipe
              (fieldAccess (var "v") "height")
              [binop "*" pipeTarget (int 2); binop "<" pipeTarget (int 40)]))
    |> sort
    |> exec ) ;
  check_dval
    "external variable works"
    (DList [Dval.dint 10])
    ( query
        (lambda
           ["v"]
           (pipe
              (fieldAccess (var "v") "height")
              [binop "<" pipeTarget (var "x")]))
    |> sort
    |> withvar "x" (int 20)
    |> exec ) ;
  check_error
    "not a bool"
    (query (lambda ["v"] (str "x")) |> exec)
    (Db.dbQueryExceptionToString
       (Db.DBQueryException
          "Incorrect type in `\"x\"`, expected Bool but got a str")) ;
  check_error
    "bad variable name"
    ( query
        (lambda
           ["v"]
           (let'
              "x"
              (int 32)
              (binop
                 "&&"
                 (bool true)
                 (binop ">" (fieldAccess (var "v") "height") (var "y")))))
    |> exec )
    (Db.dbQueryExceptionToString
       (Db.DBQueryException "This variable is not defined: y")) ;
  check_dval
    "sql injection"
    (DList [])
    ( query
        (lambda
           ["v"]
           (binop
              "=="
              (str "; select * from users ;")
              (fieldAccess (var "v") "name")))
    |> exec ) ;
  check_dval
    "null equality works"
    (DList [DNull])
    ( query (lambda ["v"] (binop "==" (fieldAccess (var "v") "name") null))
    |> sort
    |> exec ) ;
  check_dval
    "null inequality works"
    (DList [Dval.dint 10; Dval.dint 65; Dval.dint 73])
    ( query (lambda ["v"] (binop "!=" (fieldAccess (var "v") "name") null))
    |> sort
    |> exec ) ;
  check_dval
    "null is not 'null'"
    (DList [Dval.dint 10; Dval.dint 65; Dval.dint 73])
    ( query
        (lambda ["v"] (binop "!=" (fieldAccess (var "v") "name") (str "null")))
    |> sort
    |> exec ) ;
  (* Just check enough of the other functions to verify the signature -
   * they all use they same function behind the scenes. *)
  let _queryWithKeys code = "(DB::queryWithKeys_v3 Person (" ^ code ^ "))" in
  check_dval
    "queryOne - multiple"
    (DOption OptNothing)
    ( fn
        "DB::queryOne_v3"
        [var "Person"; lambda ["v"] (fieldAccess (var "v") "human")]
    |> exec ) ;
  check_dval
    "queryOne - none"
    (DOption OptNothing)
    ( fn
        "DB::queryOne_v3"
        [ var "Person"
        ; lambda ["v"] (binop "==" (str "bob") (fieldAccess (var "v") "name"))
        ]
    |> exec ) ;
  check_dval
    "queryOne - one"
    (DOption (OptJust (Dval.dint 65)))
    ( pipe
        (fn
           "DB::queryOne_v3"
           [ var "Person"
           ; lambda
               ["v"]
               (binop "==" (str "Rachel") (fieldAccess (var "v") "name")) ])
        [ fn
            "Option::map_v1"
            [pipeTarget; lambda ["r"] (fieldAccess (var "r") "height")] ]
    |> exec ) ;
  check_dval
    "queryOneWithKey - multiple"
    (DOption OptNothing)
    ( fn
        "DB::queryOneWithKey_v3"
        [var "Person"; lambda ["v"] (fieldAccess (var "v") "human")]
    |> exec ) ;
  check_dval
    "queryOneWithKey - none"
    (DOption OptNothing)
    ( fn
        "DB::queryOneWithKey_v3"
        [ var "Person"
        ; lambda ["v"] (binop "==" (str "bob") (fieldAccess (var "v") "name"))
        ]
    |> exec ) ;
  check_dval
    "queryOneWithKey - one"
    (DOption (OptJust (Dval.dint 65)))
    ( pipe
        (fn
           "DB::queryOneWithKey_v3"
           [ var "Person"
           ; lambda
               ["v"]
               (binop "==" (str "Rachel") (fieldAccess (var "v") "name")) ])
        [ fn
            "Option::map_v1"
            [pipeTarget; lambda ["r"] (fieldAccess (var "r") "rachel")]
        ; fn
            "Option::map_v1"
            [pipeTarget; lambda ["r"] (fieldAccess (var "r") "height")] ]
    |> exec ) ;
  check_dval
    "queryOneWithKey - empty"
    (DObj DvalMap.empty)
    ( fn
        "DB::queryWithKey_v3"
        [ var "Person"
        ; lambda ["v"] (binop "==" (str "bob") (fieldAccess (var "v") "name"))
        ]
    |> exec ) ;
  check_dval
    "queryWithKey - more than one"
    (Dval.dint 65)
    ( pipe
        (fn
           "DB::queryWithKey_v3"
           [ var "Person"
           ; lambda
               ["v"]
               (binop "==" (str "Rachel") (fieldAccess (var "v") "name")) ])
        [ lambda ["r"] (fieldAccess (var "r") "rachel")
        ; lambda ["r"] (fieldAccess (var "r") "height") ]
    |> exec ) ;
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
