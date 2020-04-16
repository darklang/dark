open Core_kernel
open Libcommon
open Libexecution
open Libbackend
open Types.RuntimeT
open Utils
module AT = Alcotest

let t_case_insensitive_db_roundtrip () =
  clear_test_data () ;
  let colname = "cOlUmNnAmE" in
  let value = Dval.dstr_of_string_exn "some value" in
  let ops =
    [ Op.CreateDB (dbid, pos, "TestUnicode")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, colname)
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let _
            (DB::add_v0 (obj (cOlUmNnAmE 'some value')) TestUnicode)
            (DB::getAll_v2 TestUnicode))"
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


let t_nulls_allowed_in_db () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let old (DB::set_v1 (obj (x null)) 'hello' MyDB)
               (let new (`DB::get_v1 'hello' MyDB)
                 (== old new)))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_inserting_object_to_missing_col_gives_good_error () =
  clear_test_data () ;
  check_error_contains
    "error is expected"
    (exec_handler
       "(DB::add_v0 (obj (col (obj))) TestDB)"
       ~ops:[Op.CreateDB (dbid, pos, "TestDB")])
    "Found but did not expect: [col]"


let t_nulls_added_to_missing_column () =
  (* Test for the hack that columns get null in all rows to start *)
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  ignore (exec_handler ~ops "(DB::set_v1 (obj (x 'v')) 'i' MyDB)") ;
  let ops =
    ops
    @ [ Op.AddDBCol (dbid, colnameid2, coltypeid2)
      ; Op.SetDBColName (dbid, colnameid2, "y")
      ; Op.SetDBColType (dbid, coltypeid2, "Str") ]
  in
  check_dval
    "equal_after_fetchall"
    (DList
       [ Dval.dstr_of_string_exn "i"
       ; DObj
           (DvalMap.from_list
              [("x", Dval.dstr_of_string_exn "v"); ("y", DNull)]) ])
    (exec_handler ~ops "(List::head (DB::getAllWithKeys_v1 MyDB))")


let t_uuid_db_roundtrip () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "Ids")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "uu")
    ; Op.SetDBColType (dbid, coltypeid, "UUID") ]
  in
  let ast =
    "(let i (Uuid::generate)
               (let _ (DB::add_v0 (obj (uu i)) Ids)
                 (let fetched (. (List::head (DB::getAll_v2 Ids)) uu)
                   (i fetched))))"
  in
  AT.check
    AT.int
    "A generated UUID can round-trip from the DB"
    0
    ( match exec_handler ~ops ast with
    | DList [p1; p2] ->
        compare_dval compare_expr p1 p2
    | _ ->
        1 )


let t_password_hash_db_roundtrip () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "Passwords")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "password")
    ; Op.SetDBColType (dbid, coltypeid, "Password") ]
  in
  let ast =
    "(let pw (Password::hash 'password')
               (let _ (DB::add_v0 (obj (password pw)) Passwords)
                 (let fetched (. (List::head (DB::getAll_v2 Passwords)) password)
                   (pw fetched))))"
  in
  AT.check
    AT.int
    "A Password::hash'd string can get stored in and retrieved from a user datastore."
    0
    ( match exec_handler ~ops ast with
    | DList [p1; p2] ->
        compare_dval compare_expr p1 p2
    | _ ->
        1 )


let t_escape_pg_escaping () =
  AT.check AT.string "no quotes" "asdd" (Db.escape_string "asdd") ;
  AT.check AT.string "single" "as''dd" (Db.escape_string "as'dd") ;
  AT.check AT.string "double" "as\"dd" (Db.escape_string "as\"dd") ;
  ()


let suite =
  [ ("DB case-insensitive roundtrip", `Quick, t_case_insensitive_db_roundtrip)
  ; ( "Good error when inserting badly"
    , `Quick
    , t_inserting_object_to_missing_col_gives_good_error )
  ; ("Nulls allowed in DB", `Quick, t_nulls_allowed_in_db)
  ; ("UUIDs round-trip to the DB", `Quick, t_uuid_db_roundtrip)
  ; ("Nulls for missing column", `Quick, t_nulls_added_to_missing_column)
  ; ( "Password hashes can be stored in and retrieved from the DB"
    , `Quick
    , t_password_hash_db_roundtrip )
  ; ("Test postgres escaping", `Quick, t_escape_pg_escaping) ]
