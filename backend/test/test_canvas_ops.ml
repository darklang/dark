(* Canvases and ops *)

open Core_kernel
open Libexecution
open Libbackend
open Libshared.FluidShortcuts
open Types
open Types.RuntimeT
open Utils
module Op = Libserialize.Op
module C = Canvas
module TL = Toplevel
module AT = Alcotest

let t_undo_fns () =
  clear_test_data () ;
  let n1 = TLSavepoint tlid in
  let n2 = hop (handler (binop "-" (blank ()) (blank ()))) in
  let n3 = hop (handler (binop "-" (int 3) (blank ()))) in
  let n4 = hop (handler (binop "-" (int 3) (int 4))) in
  let u = UndoTL tlid in
  let ops (c : C.canvas ref) = !c.ops |> List.hd_exn |> Tuple.T2.get2 in
  AT.check
    AT.int
    "undocount"
    3
    (Undo.undo_count
       (ops2c_exn "test" [n1; n1; n1; n1; n2; n3; n4; u; u; u] |> ops)
       tlid)


let t_undo () =
  clear_test_data () ;
  let ha ast = hop (handler ast) in
  let sp = TLSavepoint tlid in
  let u = UndoTL tlid in
  let r = RedoTL tlid in
  let o1 = int 1 in
  let o2 = int 2 in
  let o3 = int 3 in
  let o4 = int 4 in
  let o5 = int 5 in
  let ops = [sp; ha o1; sp; ha o2; sp; ha o3; sp; ha o4; sp; ha o5] in
  (* Check assumptions *)
  execute_ops ops |> check_dval "t_undo_1" (Dval.dint 5) ;
  (* First undo *)
  execute_ops (List.concat [ops; [u]]) |> check_dval "t_undo_3" (Dval.dint 4) ;
  (* Second undo *)
  execute_ops (List.concat [ops; [u; u]]) |> check_dval "t_undo_4" (Dval.dint 3) ;
  (* First redo *)
  execute_ops (List.concat [ops; [u; u; r]])
  |> check_dval "t_undo_5" (Dval.dint 4) ;
  (* Second redo *)
  execute_ops (List.concat [ops; [u; u; r; r]])
  |> check_dval "t_undo_6" (Dval.dint 5) ;
  (* Another undo *)
  execute_ops (List.concat [ops; [u; u; r; r; u]])
  |> check_dval "t_undo_7" (Dval.dint 4) ;
  (* Another redo *)
  execute_ops (List.concat [ops; [u; u; r; r; u; r]])
  |> check_dval "t_undo_8" (Dval.dint 5)


let t_db_oplist_roundtrip () =
  clear_test_data () ;
  let host = "test-db_oplist_roundtrip" in
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let oplist = [UndoTL tlid; RedoTL tlid; UndoTL tlid; RedoTL tlid] in
  Serialize.save_toplevel_oplist
    oplist
    ~binary_repr:None
    ~tlid
    ~deleted:None
    ~pos:None
    ~canvas_id
    ~account_id:owner
    ~tipe:TL.TLHandler
    ~name:None
    ~module_:None
    ~modifier:None ;
  let ops = Serialize.load_all_from_db ~canvas_id ~host () in
  check_tlid_oplists "db_oplist roundtrip" [(tlid, oplist)] ops


let t_http_oplist_roundtrip () =
  clear_test_data () ;
  let host = "test-http_oplist_roundtrip" in
  let handler = http_route_handler () in
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let oplist = [SetHandler (tlid, pos, handler)] in
  let c1 = ops2c_exn host oplist in
  Canvas.serialize_only [tlid] !c1 ;
  let c2 =
    Canvas.load_http_from_cache
      ~canvas_id
      ~owner
      ~path:http_request_path
      ~verb:"GET"
      host
    |> Result.map_error ~f:(String.concat ~sep:", ")
    |> Prelude.Result.ok_or_internal_exception "Canvas load error"
  in
  (* Can tell it was loaded from the cache, as the canvas object has no
   * oplists *)
  AT.check AT.bool "handler is loaded from cache #1" true (!c2.ops = []) ;
  AT.check
    testable_handler
    "handler is loaded correctly from cache #1"
    handler
    ( !c2.handlers
    |> IDMap.data
    |> List.hd_exn
    |> Toplevel.as_handler
    |> Option.value_exn )


let t_http_oplist_loads_user_tipes () =
  clear_test_data () ;
  let host = "test-http_oplist_loads_user_tipes" in
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let tipe = user_record "test-tipe" [] in
  let oplist = [SetHandler (tlid, pos, http_route_handler ()); SetType tipe] in
  let c1 = ops2c_exn host oplist in
  Canvas.serialize_only [tlid; tipe.tlid] !c1 ;
  let c2 =
    Canvas.load_http_from_cache
      ~canvas_id
      ~owner
      ~path:http_request_path
      ~verb:"GET"
      host
    |> Result.map_error ~f:(String.concat ~sep:", ")
    |> Prelude.Result.ok_or_internal_exception "Canvas load error"
  in
  AT.check
    AT.bool
    "handlers and types are loaded from cache #2"
    true
    (!c2.ops = []) ;
  AT.check
    (AT.list (AT.testable pp_user_tipe equal_user_tipe))
    "user tipes"
    [tipe]
    (IDMap.data !c2.user_tipes)


let t_http_load_ignores_deleted_fns () =
  clear_test_data () ;
  let host = "test-http_load_ignores_deleted_fns_and_dbs" in
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let handler = http_route_handler () in
  let f = user_fn ~tlid:tlid2 "testfn" [] (binop "+" (int 5) (int 3)) in
  let f2 = user_fn ~tlid:tlid3 "testfn" [] (binop "+" (int 6) (int 4)) in
  let oplist =
    [ SetHandler (tlid, pos, handler)
    ; SetFunction f
    ; DeleteFunction tlid2
    ; SetFunction f2 ]
  in
  let c1 = ops2c_exn host oplist in
  Canvas.serialize_only [tlid; tlid2; tlid3] !c1 ;
  let c2 =
    Canvas.load_http_from_cache
      ~canvas_id
      ~owner
      ~path:http_request_path
      ~verb:"GET"
      host
    |> Result.map_error ~f:(String.concat ~sep:", ")
    |> Prelude.Result.ok_or_internal_exception "Canvas load error"
  in
  AT.check AT.bool "handler is loaded from cache #3" true (!c2.ops = []) ;
  AT.check
    testable_handler
    "handler is loaded correctly from cache #3"
    handler
    ( !c2.handlers
    |> IDMap.data
    |> List.hd_exn
    |> Toplevel.as_handler
    |> Option.value_exn ) ;
  AT.check
    AT.int
    "only one function is loaded from cache"
    1
    (IDMap.length !c2.user_functions) ;
  AT.check
    AT.bool
    "the most recent function is loaded from the cache"
    true
    (f2 = (!c2.user_functions |> IDMap.data |> List.hd_exn))


let t_db_create_with_orblank_name () =
  clear_test_data () ;
  let ops =
    [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
    ; AddDBCol (dbid, colnameid, coltypeid) ]
  in
  let _, state, _ = test_execution_data ops in
  AT.check AT.bool "datastore is created" true (state.dbs <> [])


let t_db_rename () =
  clear_test_data () ;
  let ops =
    [ CreateDBWithBlankOr (dbid, pos, nameid, "ElmCode")
    ; AddDBCol (dbid, colnameid, coltypeid)
    ; RenameDBname (dbid, "BsCode") ]
  in
  let _, state, _ = test_execution_data ops in
  match List.hd state.dbs with
  | Some db ->
      let newname =
        match db.name with
        | Filled (_, name) ->
            name
        | Partial _ | Blank _ ->
            ""
      in
      AT.check AT.string "datastore rename success" "BsCode" newname
  | None ->
      AT.check AT.bool "fail to rename datastore" true false


let t_set_after_delete () =
  let check_empty msg tls = AT.check AT.int msg (IDMap.length tls) 0 in
  let check_single msg tls = AT.check AT.int msg (IDMap.length tls) 1 in
  (* handlers *)
  clear_test_data () ;
  let h1 = handler (binop "+" (int 5) (int 3)) in
  let h2 = handler (binop "+" (int 5) (int 2)) in
  let op1 = SetHandler (tlid, pos, h1) in
  let op2 = DeleteTL tlid in
  let op3 = SetHandler (tlid, pos, h2) in
  check_dval "first handler is right" (execute_ops [op1]) (Dval.dint 8) ;
  check_empty "deleted not in handlers" !(ops2c_exn "test" [op1; op2]).handlers ;
  check_single
    "delete in deleted"
    !(ops2c_exn "test" [op1; op2]).deleted_handlers ;
  check_single
    "deleted in handlers"
    !(ops2c_exn "test" [op1; op2; op3]).handlers ;
  check_empty
    "deleted not in deleted "
    !(ops2c_exn "test" [op1; op2; op3]).deleted_handlers ;
  check_dval
    "second handler is right"
    (execute_ops [op1; op2; op3])
    (Dval.dint 7) ;
  (* same thing for functions *)
  clear_test_data () ;
  let h1 = user_fn "testfn" [] (binop "+" (int 5) (int 3)) in
  let h2 = user_fn "testfn" [] (binop "+" (int 5) (int 2)) in
  let op1 = SetFunction h1 in
  let op2 = DeleteFunction tlid in
  let op3 = SetFunction h2 in
  check_empty "deleted not in fns" !(ops2c_exn "test" [op1; op2]).user_functions ;
  check_single
    "delete in deleted"
    !(ops2c_exn "test" [op1; op2]).deleted_user_functions ;
  check_single
    "deleted in fns"
    !(ops2c_exn "test" [op1; op2; op3]).user_functions ;
  check_empty
    "deleted not in deleted "
    !(ops2c_exn "test" [op1; op2; op3]).deleted_user_functions ;
  ()


let t_load_all_dbs_from_cache () =
  clear_test_data () ;
  let host = "test-http_oplist_loads_user_tipes" in
  let oplist =
    [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
    ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books2")
    ; CreateDBWithBlankOr (dbid3, pos, nameid3, "Books3")
    ; DeleteTL dbid ]
  in
  let c1 = ops2c_exn host oplist in
  Canvas.serialize_only [dbid; dbid2; dbid3] !c1 ;
  let c2 =
    Canvas.load_all_dbs_from_cache host
    |> Result.map_error ~f:(String.concat ~sep:", ")
    |> Prelude.Result.ok_or_internal_exception "Canvas load error"
  in
  AT.check AT.bool "dbs are loaded from cache" true (!c2.ops = []) ;
  AT.check
    (AT.list testable_id)
    "Loaded only undeleted dbs"
    (List.sort ~compare:compare_id [dbid2; dbid3])
    (!c2.dbs |> IDMap.keys |> List.sort ~compare:compare_id)


let t_canvas_verification_duplicate_creation () =
  let ops =
    [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
    ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books") ]
  in
  let c = ops2c "test-verify_create" ops in
  AT.check AT.bool "should not verify" false (Result.is_ok c)


let t_canvas_verification_duplicate_creation_off_disk () =
  clear_test_data () ;
  let host = "test-verify_rename" in
  let ops = [CreateDBWithBlankOr (dbid, pos, nameid, "Books")] in
  let c1 = ops2c_exn host ops in
  Canvas.serialize_only [dbid] !c1 ;
  let c2 =
    let ops = [CreateDBWithBlankOr (dbid2, pos, nameid, "Books")] in
    match Op.required_context_to_validate_oplist ops with
    | NoContext ->
        Canvas.load_only_tlids ~tlids:[dbid2] host ops
    | AllDatastores ->
        Canvas.load_with_dbs ~tlids:[dbid2] host ops
  in
  AT.check AT.bool "should not verify" false (Result.is_ok c2)


let t_canvas_verification_duplicate_renaming () =
  let ops =
    [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
    ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books2")
    ; RenameDBname (dbid2, "Books") ]
  in
  let c = ops2c "test-verify_rename" ops in
  AT.check AT.bool "should not verify" false (Result.is_ok c)


let t_canvas_verification_no_error () =
  let ops =
    [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
    ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books2") ]
  in
  let c = ops2c "test-verify_okay" ops in
  AT.check AT.bool "should verify" true (Result.is_ok c)


let t_canvas_verification_undo_rename_duped_name () =
  let ops1 =
    [ CreateDBWithBlankOr (dbid, pos, nameid, "Books")
    ; TLSavepoint dbid
    ; DeleteTL dbid
    ; CreateDBWithBlankOr (dbid2, pos, nameid2, "Books") ]
  in
  let c = ops2c "test-verify_undo_1" ops1 in
  AT.check AT.bool "should initially verify" true (Result.is_ok c) ;
  let ops2 = ops1 @ [UndoTL dbid] in
  let c2 = ops2c "test-verify_undo_2" ops2 in
  AT.check AT.bool "should then fail to verify" false (Result.is_ok c2)


let suite =
  [ ("undo", `Quick, t_undo)
  ; ("undo_fns", `Quick, t_undo_fns)
  ; ("db binary oplist roundtrip", `Quick, t_db_oplist_roundtrip)
  ; ("http oplist roundtrip", `Quick, t_http_oplist_roundtrip)
  ; ( "Can create new DB with Op CreateDBWithBlankOr"
    , `Quick
    , t_db_create_with_orblank_name )
  ; ("Can rename DB with Op RenameDBname", `Quick, t_db_rename)
  ; ("set after delete doesn't crash", `Quick, t_set_after_delete)
  ; ( "Canvas verification catches duplicate DB name via creation"
    , `Quick
    , t_canvas_verification_duplicate_creation )
  ; ( "Canvas verification catches duplicate DB name via renaming"
    , `Quick
    , t_canvas_verification_duplicate_renaming )
  ; ( "Canvas verification returns Ok if no error"
    , `Quick
    , t_canvas_verification_no_error )
  ; ( "Canvas verification catches inconsistency post undo"
    , `Quick
    , t_canvas_verification_undo_rename_duped_name )
  ; ( "Loading handler via HTTP router loads user tipes"
    , `Quick
    , t_http_oplist_loads_user_tipes )
  ; ( "Loading handler via HTTP router ignores deleted fns"
    , `Quick
    , t_http_load_ignores_deleted_fns )
  ; ( "Loading dbs from load_all_dbs_from_cache ignores deleted dbs"
    , `Quick
    , t_load_all_dbs_from_cache )
  ; ( "Adding a DB with a duplicate name fails to verify"
    , `Quick
    , t_canvas_verification_duplicate_creation_off_disk ) ]
