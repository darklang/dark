open Core_kernel
open Libcommon
open Libexecution
open Libbackend
open Libshared.FluidShortcuts
open Types
open Types.RuntimeT
open Utils
module AT = Alcotest

let t_case_insensitive_db_roundtrip () =
  clear_test_data () ;
  let colname = "cOlUmNnAmE" in
  let value = Dval.dstr_of_string_exn "some value" in
  let ops =
    [ CreateDB (dbid, pos, "TestUnicode")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, colname)
    ; SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    let'
      "_"
      (fn
         "DB::add_v0"
         [record [("cOlUmNnAmE", str "some value")]; var "TestUnicode"])
      (fn "DB::getAll_v2" [var "TestUnicode"])
  in
  match exec_handler ~ops ast with
  | DList [DObj v] ->
      (let open AT in
      check bool)
        "matched"
        true
        (List.mem ~equal:( = ) (DvalMap.values v) value)
  | other ->
      Log.erroR "error" ~data:(Dval.to_developer_repr_v0 other) ;
      (let open AT in
      check bool)
        "failed"
        true
        false


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


let t_uuid_db_roundtrip () =
  clear_test_data () ;
  let ops =
    [ CreateDB (dbid, pos, "Ids")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; SetDBColName (dbid, colnameid, "uu")
    ; SetDBColType (dbid, coltypeid, "UUID") ]
  in
  let ast =
    let'
      "i"
      (fn "Uuid::generate" [])
      (let'
         "_"
         (fn "DB::add_v0" [record [("uu", var "i")]; var "Ids"])
         (let'
            "fetched"
            (fieldAccess
               (fn "List::head" [fn "DB::getAll_v2" [var "Ids"]])
               "uu")
            (list [var "i"; var "fetched"])))
  in
  AT.check
    AT.int
    "A generated UUID can round-trip from the DB"
    0
    ( match exec_handler ~ops ast with
    | DList [p1; p2] ->
        compare_dval p1 p2
    | _ ->
        1 )
