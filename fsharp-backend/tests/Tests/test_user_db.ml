let t_nulls_added_to_missing_column () =
  (* Test for the hack that columns get null in all rows to start *)
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "MyDB")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "x")
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  ignore
    (exec_handler
       ~ops
       (fn "DB::set_v1" [record [("x", str "v")]; str "i"; var "MyDB"])) ;
  let ops =
    ops
    @ [ AddDBCol (dbid, colnameid2, coltypeid2)
      ; SetDBColName (dbid, colnameid2, "y")
      ; SetDBColType (dbid, coltypeid2, "Str") ]
  in
  check_dval
    "equal_after_fetchall"
    (DList
       [ Dval.dstr_of_string_exn "i"
       ; DObj
           (DvalMap.from_list
              [("x", Dval.dstr_of_string_exn "v"); ("y", DNull)]) ])
    (exec_handler
       ~ops
       (fn "List::head" [fn "DB::getAllWithKeys_v1" [var "MyDB"]]))
