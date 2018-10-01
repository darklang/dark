open Core_kernel
open Libcommon
open Libexecution
open Libbackend

open Types
open Types.RuntimeT
open Ast

open Lwt
module Resp = Cohttp_lwt_unix.Response
module Req = Cohttp_lwt_unix.Request
module Header = Cohttp.Header
module Code = Cohttp.Code

module C = Canvas
module RT = Runtime
module TL = Toplevel
module Map = Map.Poly
module AT = Alcotest



(* ------------------- *)
(* Misc fns *)
(* ------------------- *)
let clear_test_data () : unit =
  let owner = Account.for_host "test" in
  let canvas = Serialize.fetch_canvas_id owner "test" in
  Db.run ~params:[Uuid canvas] ~name:"clear_events_test_data"
    "DELETE FROM events where canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_stored_events_test_data"
    "DELETE FROM stored_events where canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_function_results_test_data"
    "DELETE FROM function_results where canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_user_data_test_data"
    "DELETE FROM user_data where canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_cron_records_test_data"
    "DELETE FROM cron_records where canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_toplevel_oplists_test_data"
    "DELETE FROM toplevel_oplists WHERE canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_function_arguments"
    "DELETE FROM function_arguments WHERE canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_canvases_test_data"
    "DELETE FROM canvases where id = $1";
  ()


(* ------------------- *)
(* Test fns *)
(* ------------------- *)

let at_dval = AT.testable
    (fun fmt dv -> Fmt.pf fmt "%s" (Dval.to_repr dv))
    (fun a b -> compare_dval a b = 0)
let check_dval = AT.check at_dval
let check_dval_list = AT.check (AT.list at_dval)
let check_oplist = AT.check (AT.of_pp Op.pp_oplist)
let check_tlid_oplists = AT.check (AT.of_pp Op.pp_tlid_oplists)
let check_exception ?(check=(fun _ -> true)) ~(f:unit -> dval) msg =
  let e =
    try
      let r = f () in
      Log.erroR "result" ~data:(Dval.to_repr r);
      Some "no exception"
    with
    | Exception.DarkException ed ->
      if check ed
      then None
      else
        (Log.erroR "check failed" ~data:(Log.dump ed);
        Some "Check failed")
    | e ->
      let bt = Backtrace.Exn.most_recent () in
      let msg = Exn.to_string e in
      print_endline (Backtrace.to_string bt);
      Log.erroR "different exception" ~data:msg;
      Some "different exception"
  in
  AT.check (AT.option AT.string) msg None e

let check_error_contains (name: string) (result: dval) (substring: string) =
  let strresult = Dval.as_string result in
  AT.(check bool)
    (name ^ ": (\"" ^ strresult ^ "\" contains \"" ^ substring ^ "\"")
    true
    (String.is_substring ~substring (Dval.as_string result))




(* ------------------- *)
(* Set up test data *)
(* ------------------- *)

let fid = Util.create_id
let v str = Filled (fid (), Value str)
let b () = Blank (fid ())
let f a = Filled (fid (), a)
let fncall (a,b) = f (FnCall (a,b))
let tlid = Int63.of_int 7
let dbid = Int63.of_int 89
let dbid2 = Int63.of_int 189
let colnameid = Int63.of_int 11
let coltypeid = Int63.of_int 12
let colnameid2 = Int63.of_int 13
let coltypeid2 = Int63.of_int 14
let colnameid3 = Int63.of_int 15
let coltypeid3 = Int63.of_int 16
let pos = {x=0;y=0}
let execution_id = Int63.of_int 6542

let ast_for = Expr_dsl.ast_for

let handler ast : HandlerT.handler =
  { tlid = tlid
  ; ast = ast
  ; spec = { module_ = b ()
           ; name = b ()
           ; modifier = b ()
           ; types = { input = b ()
                     ; output = b () }}}

let http_handler ast : HandlerT.handler =
  { tlid = tlid
  ; ast = ast
  ; spec = { module_ = f "HTTP"
           ; name = f "/test"
           ; modifier = f "GET"
           ; types = { input = b ()
                     ; output = b () }}}

let http_route = "/some/vars/and/such"
let http_route_handler : HandlerT.handler =
  { tlid = tlid
  ; ast = f (Value "5")
  ; spec = { module_ = f "HTTP"
           ; name = f "/some/:vars/:and/such"
           ; modifier = f "GET"
           ; types = { input = b ()
                     ; output = b () }}}

let daily_cron ast : HandlerT.handler =
  { tlid = tlid
  ; ast = ast
  ; spec = { module_ = f "CRON"
           ; name = f "test"
           ; modifier = f "Daily"
           ; types = { input = b ()
                     ; output = b () }}}

let hop h =
  Op.SetHandler (tlid, pos, h)

let user_fn name params ast : user_fn =
  { tlid = tlid
  ; ast = ast
  ; metadata = { name = f name
               ; parameters = List.map params
                                ~f:(fun p ->
                                      { name = f p
                                      ; tipe = f TAny
                                      ; block_args = []
                                      ; optional = false
                                      ; description = "test"})
               ; return_type = f TAny
               ; description = "test user fn"
               ; infix = false}}



(* ------------------- *)
(* Execution *)
(* ------------------- *)
let ops2c (host: string) (ops: Op.op list) : C.canvas ref =
  C.init host ops

let test_execution_data ops : (C.canvas ref * exec_state * input_vars) =
  let c = ops2c "test" ops in
  let vars = Execution.dbs_as_input_vars (TL.dbs !c.dbs) in
  let canvas_id = !c.id in
  let trace_id = Util.create_uuid () in
  let state =
        { tlid
        ; account_id = !c.owner
        ; canvas_id = !c.id
        ; user_fns = !c.user_functions
        ; fail_fn = None
        ; dbs = TL.dbs !c.dbs
        ; execution_id
        ; load_fn_result = Execution.load_no_results
        ; store_fn_result = Stored_function_result.store ~canvas_id ~trace_id
        ; load_fn_arguments = Execution.load_no_arguments
        ; store_fn_arguments = Stored_function_arguments.store ~canvas_id ~trace_id
        }
  in
  (c, state, vars)

let execute_ops (ops : Op.op list) : dval =
  let (c, { tlid; execution_id ; dbs ; user_fns ; account_id ; canvas_id }, input_vars) =
    test_execution_data ops in
  let h = !c.handlers
          |> TL.handlers
          |> List.hd_exn in
  Execution.execute_handler h
    ~tlid
    ~execution_id
    ~dbs
    ~user_fns
    ~account_id
    ~canvas_id
    ~input_vars:[] (* already provided in execute_handler *)

let exec_handler ?(ops=[]) (prog: string) : dval =
  prog
  |> ast_for
  (* |> Log.pp ~f:show_expr *)
  |> handler
  |> hop
  |> fun h -> execute_ops (ops @ [h])

let exec_ast (prog: string) : dval =
  let (c, state, input_vars) = test_execution_data [] in
  Ast.execute_ast input_vars state (ast_for prog)

let exec_userfn (prog: string) : dval =
  let name = "test_function" in
  let ast = ast_for prog in
  let fn = user_fn name [] ast in
  let (c, state, _) = test_execution_data [SetFunction fn] in
  Ast.execute_userfn state name execution_id []





(* ----------------------- *)
(* The tests *)
(* ----------------------- *)

let t_undo_fns () =
  clear_test_data ();
  let n1 = Op.TLSavepoint tlid in
  let n2 = hop (handler (ast_for "(- _ _)")) in
  let n3 = hop (handler (ast_for "(- 3 _)")) in
  let n4 = hop (handler (ast_for "(- 3 4)")) in
  let u = Op.UndoTL tlid in
  let r = Op.RedoTL tlid in

  let ops (c:C.canvas ref) = !c.ops |> List.hd_exn |> Tuple.T2.get2 in

  AT.check AT.int "undocount" 3
    (Undo.undo_count
       (ops2c "test" [n1; n1; n1; n1; n2; n3; n4; u; u; u] |> ops) tlid);

  AT.check AT.bool "redoable" true
    (Undo.is_redoable (ops2c "test" [n1; n2; n3; n4; u] |> ops) tlid);
  AT.check AT.bool "undoable" true
    (Undo.is_undoable (ops2c "test" [n1; n2; n3; n4] |> ops) tlid);


  AT.check AT.bool "not redoable" false
    (Undo.is_redoable (ops2c "test" [n1; n2; n3; n4; u; r] |> ops) tlid);
  AT.check AT.bool "not undoable" false
    (Undo.is_undoable (ops2c "test" [n1; n2; n3; n4; u] |> ops) tlid);


  let both = ops2c "test" [n1; n1; n2; n3; n4; u; r; u] |> ops in
  AT.check AT.bool "both_undo" true (Undo.is_undoable both tlid);
  AT.check AT.bool "both_redo" true (Undo.is_redoable both tlid);

  let neither = ops2c "test" [n2; n3; n4] |> ops in
  AT.check AT.bool "neither_undo" false (Undo.is_undoable neither tlid);
  AT.check AT.bool "neither_redo" false (Undo.is_redoable neither tlid)

let t_undo () =
  clear_test_data ();
  let ha ast = hop (handler ast) in
  let sp = Op.TLSavepoint tlid in
  let u = Op.UndoTL tlid in
  let r = Op.RedoTL tlid in
  let o1 = v "1" in
  let o2 = v "2" in
  let o3 = v "3" in
  let o4 = v "4" in
  let o5 = v "5" in
  let ops = [sp; ha o1; sp; ha o2; sp; ha o3; sp; ha o4; sp; ha o5] in

  (* Check assumptions *)
  execute_ops ops
  |> check_dval "t_undo_1" (DInt 5);

  (* First undo *)
  execute_ops (List.concat [ops; [u]])
  |> check_dval "t_undo_3" (DInt 4);

  (* Second undo *)
  execute_ops (List.concat [ops; [u;u]])
  |> check_dval "t_undo_4" (DInt 3);

  (* First redo *)
  execute_ops (List.concat [ops; [u;u;r]])
  |> check_dval "t_undo_5" (DInt 4);

  (* Second redo *)
  execute_ops (List.concat [ops; [u;u;r;r]])
  |> check_dval "t_undo_6" (DInt 5);

  (* Another undo *)
  execute_ops (List.concat [ops; [u;u;r;r;u]])
  |> check_dval "t_undo_7" (DInt 4);

  (* Another redo *)
  execute_ops (List.concat [ops; [u;u;r;r;u;r]])
  |> check_dval "t_undo_8" (DInt 5)

let t_inserting_object_to_missing_col_gives_good_error () =
  clear_test_data ();
  check_error_contains "error is expected"
    (exec_handler "(DB::insert (obj (col (obj))) TestDB)"
      ~ops:[Op.CreateDB (dbid, pos, "TestDB")])
    "Found but did not expect: [col]"


let t_int_add_works () =
  (* Couldn't call Int::add *)
  check_dval "int_add" (DInt 8) (exec_ast "(+ 5 3)")

let t_stdlib_works () =
  check_dval "uniqueBy1"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> (Int::divide x 2)))")
    (DList [DInt 1; DInt 3; DInt 4]);
  check_dval "uniqueBy2"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> x))")
    (DList [DInt 1; DInt 2; DInt 3; DInt 4]);
  check_error_contains "base64decode"
    (exec_ast "(String::base64Decode 'random string')")
    "Not a valid base64 string";
  ()


let t_derror_roundtrip () =
  let x = DError "test" in
  let converted = x
                |> Dval.unsafe_dval_to_yojson
                |> Dval.unsafe_dval_of_yojson
                |> Result.ok_or_failwith in
  check_dval "roundtrip" converted x



let t_db_oplist_roundtrip () =
  clear_test_data ();
  let host = "test-db_oplist_roundtrip" in
  let owner = Account.for_host host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let oplist = [ Op.UndoTL tlid
               ; Op.RedoTL tlid
               ; Op.UndoTL tlid
               ; Op.RedoTL tlid] in
  Serialize.save_toplevel_oplist oplist
    ~tlid ~canvas_id ~account_id:owner
    ~tipe:`Handler
    ~name:None ~module_:None ~modifier:None;
  let ops = Serialize.load_all_from_db ~canvas_id ~host () in
  check_tlid_oplists "db_oplist roundtrip" [(tlid, oplist)] ops

let t_http_oplist_roundtrip () =
  clear_test_data ();
  let host = "test-http_oplist_roundtrip" in
  let oplist = [ Op.SetHandler (tlid, pos, http_route_handler) ] in
  let c1 = Canvas.init host oplist in
  Canvas.serialize_only [tlid] !c1;
  let c2 = Canvas.load_http ~path:http_route ~verb:"GET" host in
  check_tlid_oplists "http_oplist roundtrip" !c1.ops !c2.ops


let t_case_insensitive_db_roundtrip () =
  clear_test_data ();
  let colname = "cOlUmNnAmE" in
  let value = DStr "some value" in
  let ops = [ Op.CreateDB (dbid, pos, "TestUnicode")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, colname)
            ; Op.SetDBColType (dbid, coltypeid, "Str")]
  in
  let ast =
      "(let _
            (DB::insert (obj (cOlUmNnAmE 'some value')) TestUnicode)
            (DB::fetchAll TestUnicode))"
  in
  match exec_handler ~ops ast with
  | DList [DObj v] ->
    AT.(check bool) "matched" true
      (List.mem ~equal:(=) (DvalMap.data v) value)
  | other ->
    Log.erroR "error" ~data:(Dval.to_repr other);
    AT.(check bool) "failed" true false


let t_lambda_with_foreach () =
  check_dval "lambda_with_foreach"
    (DStr "SOME STRING")
    (exec_ast
       "(String::foreach 'some string'
          (\\var -> (Char::toUppercase var)))")

module SE = Stored_event
let t_stored_event_roundtrip () =
  clear_test_data ();
  let owner : Uuidm.t = Account.owner ~auth_domain:"test"
                        |> fun x -> Option.value_exn x in
  let id1 = Serialize.fetch_canvas_id owner "host" in
  let id2 = Serialize.fetch_canvas_id owner "host2" in
  let t1 = Util.create_uuid () in
  let t2 = Util.create_uuid () in
  let t3 = Util.create_uuid () in
  let t4 = Util.create_uuid () in
  let t5 = Util.create_uuid () in
  SE.clear_all_events ~canvas_id:id1 ();
  SE.clear_all_events ~canvas_id:id2 ();
  let desc1 = ("HTTP", "/path", "GET") in
  let desc2 = ("HTTP", "/path2", "GET") in
  let desc3 = ("HTTP", "/path", "POST") in
  SE.store_event ~canvas_id:id1 ~trace_id:t1 desc1 (DStr "1");
  SE.store_event ~canvas_id:id1 ~trace_id:t2 desc1 (DStr "2");
  SE.store_event ~canvas_id:id1 ~trace_id:t3 desc3 (DStr "3");
  SE.store_event ~canvas_id:id1 ~trace_id:t4 desc2 (DStr "3");
  SE.store_event ~canvas_id:id2 ~trace_id:t5 desc2 (DStr "3");

  let at_desc = AT.of_pp SE.pp_event_desc in

  let listed = SE.list_events ~canvas_id:id1 () in
  AT.check
    (AT.list at_desc) "list host events"
    (List.sort ~compare [desc1; desc2; desc3])
    (List.sort ~compare listed);

  let loaded1 = SE.load_events ~canvas_id:id1 desc1
                |> List.map ~f:Tuple.T2.get2
  in
  check_dval_list "load GET events" [DStr "2"; DStr "1"] loaded1;

  let loaded2 = SE.load_events ~canvas_id:id1 desc3
                |> List.map ~f:Tuple.T2.get2
  in
  check_dval_list "load POST events" [DStr "3"] loaded2;

  let loaded3 = SE.load_events ~canvas_id:id2 desc3
                |> List.map ~f:Tuple.T2.get2
  in
  check_dval_list "load no host2 events" [] loaded3;

  let loaded4 = SE.load_events ~canvas_id:id2 desc2
                |> List.map ~f:Tuple.T2.get2
  in
  check_dval_list "load host2 events" [DStr "3"] loaded4;

  ()

(* module EQ = Event_queue *)
(* let t_event_queue_roundtrip () = *)
(*   clear_test_data (); *)
(*   let dval = DInt 345 in *)
(*   let exec_id = 147 in *)
(*   let space = "TEST_SPACE" in *)
(*   let name = "test_name" in *)
(*   let c = ops2c "test-event_queue" [] in *)
(*   let state = Execution.state_for_execution ~c:!c tlid *)
(*       ~execution_id ~env:DvalMap.empty *)
(*   in *)
(*   EQ.enqueue state space name dval; *)
(*   let v = *)
(*     EQ.dequeue ~canvas:!c.id ~account:!c.owner exec_id space name *)
(*     |> fun x -> Option.value_exn x *)
(*   in *)

(*   check_dval "v" v.value dval; *)

(*   () *)

let t_bad_ssl_cert _ =
  check_error_contains "should get bad_ssl"
    (exec_ast "(HttpClient::get 'https://self-signed.badssl.com' {} {} {})")
    "Bad HTTP request: Peer certificate cannot be authenticated with given CA certificates"


let t_hmac_signing _ =
  let url = "https://api.twitter.com/1.1/statuses/update.json" in
  let ts = "1318622958" in
  let nonce = "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg" in
  let secret : Secret.twitter_secret =
    { consumer_key = "xvz1evFS4wEEPTGEFPHBog"
    ; consumer_secret = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw"
    ; access_token = "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"
    ; access_token_secret = "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE"
    } in
  let k1 = "status" in
  let v1 = "Hello Ladies + Gentlemen, a signed OAuth request!" in
  let k2 = "include_entities" in
  let v2 = "true" in

  (* Test 1 - just the sig *)
  AT.check AT.string "hmac_signing_1"
    "hCtSmYh+iHYCEqBWrE7C7hYmtUk="
    (Twitter.sign
       secret.consumer_secret
       secret.access_token_secret
       url
       "POST"
       [ (k1, v1)
       ; (k2, v2)
       ; ("oauth_consumer_key", secret.consumer_key)
       ; ("oauth_nonce", nonce)
       ; ("oauth_signature_method", "HMAC-SHA1")
       ; ("oauth_timestamp", ts)
       ; ("oauth_token", secret.access_token)
       ; ("oauth_version", "1.0")]);


  (* Test 2 - full header *)
  let url = "https://api.twitter.com/1.1/statuses/update.json" in
  Mock.set_string "ts" ts;
  Mock.set_string "nonce" nonce;
  let args = DvalMap.of_alist_exn [ (k1, DStr v1)
                                  ; (k2, DStr v2)] in

  let expected_header =
    "OAuth oauth_consumer_key=\"xvz1evFS4wEEPTGEFPHBog\", oauth_nonce=\"kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg\", oauth_signature=\"hCtSmYh%2BiHYCEqBWrE7C7hYmtUk%3D\", oauth_signature_method=\"HMAC-SHA1\", oauth_timestamp=\"1318622958\", oauth_token=\"370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb\", oauth_version=\"1.0\"" in
  let actual =
    Twitter.oauth_header
      secret
      url
      "POST"
      args in
  AT.check AT.string "hmac_signing_2" expected_header actual

let t_cron_sanity () =
  clear_test_data ();
  let h = daily_cron (ast_for "(+ 5 3)") in
  let c = ops2c "test-cron_works" [hop h] in
  let handler = !c.handlers |> TL.handlers |> List.hd_exn in
  let should_run =
    Cron.should_execute !c.id handler
  in
  AT.check AT.bool "should_run should be true" should_run true;
  ()

let t_cron_just_ran () =
  clear_test_data ();
  let h = daily_cron (ast_for "(+ 5 3)") in
  let c = ops2c "test-cron_works" [hop h] in
  let handler = !c.handlers |> TL.handlers |> List.hd_exn in
  Cron.record_execution !c.id handler;
  let should_run =
    Cron.should_execute !c.id handler
  in
  AT.check AT.bool "should_run should be false" should_run false;
  ()


let t_roundtrip_user_data_using_deprecated_functions () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let v 'lasd;04mr'
               (let old (DB::insert (obj (x v)) MyDB)
               (let new (DB::fetchOneBy v 'x' MyDB)
               (== old new))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_escape_pg_escaping () =
  AT.check AT.string "no quotes" "asdd" (Db.escape_single "asdd");
  AT.check AT.string "single" "as''dd" (Db.escape_single "as'dd");
  AT.check AT.string "double" "as\"dd" (Db.escape_single "as\"dd");
  ()

let t_nulls_allowed_in_db () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let old (DB::set_v1 (obj (x null)) 'hello' MyDB)
               (let new (`DB::get_v1 'hello' MyDB)
                 (== old new)))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_nulls_added_to_missing_column () =
  (* Test for the hack that columns get null in all rows to start *)
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")]
  in
  ignore (exec_handler ~ops "(DB::set_v1 (obj (x 'v')) 'i' MyDB)");

  let ops = ops @ [ Op.AddDBCol (dbid, colnameid2, coltypeid2)
                  ; Op.SetDBColName (dbid, colnameid2, "y")
                  ; Op.SetDBColType (dbid, coltypeid2, "Str")]
  in
  check_dval "equal_after_fetchall"
    (DList [DStr "i"; (DObj (DvalMap.of_alist_exn ["x", DStr "v"; "y", DNull]))])
    (exec_handler ~ops "(List::head (DB::getAllWithKeys_v1 MyDB))")

let t_unsafe_dval_of_yojson_doesnt_care_about_order () =
  check_dval "dval_of_json_string doesn't care about key order"
    (Dval.unsafe_dval_of_json_string
       "{
         \"type\": \"url\",
         \"value\": \"https://example.com\"
        }")
    (Dval.unsafe_dval_of_json_string
       "{
         \"value\": \"https://example.com\",
         \"type\": \"url\"
        }")

let t_dval_yojson_roundtrips () =
  let unsafe_rt v = v
                    |> Dval.unsafe_dval_to_yojson
                    |> Dval.unsafe_dval_of_yojson
                    |> Result.ok_or_failwith
  in
  (* Don't really need to check this but what harm *)
  let safe_rt v = v
                  |> dval_to_yojson
                  |> dval_of_yojson
                  |> Result.ok_or_failwith
  in
  let check name (v: dval) =
    check_dval name v (safe_rt v);
    check_dval ("unsafe " ^ name) v (unsafe_rt v);
    ()
  in
  check "int" (DInt 5);
  check "int" (DInt 5);
  check "obj" (DObj (DvalMap.of_alist_exn [("foo", DInt 5)]));
  check "date" (DDate (Time.of_string "2018-09-14T00:31:41Z"));
  check "incomplete" DIncomplete;
  check "float" (DFloat 7.2);
  check "true" (DBool true);
  check "false" (DBool false);
  check "string" (DStr "incredibly this was broken");
  check "null" DNull;
  check "id" (DID (Util.uuid_of_string "7d9e5495-b068-4364-a2cc-3633ab4d13e6"));
  check "uuid" (DUuid (Util.uuid_of_string "7d9e5495-b068-4364-a2cc-3633ab4d13e6"));
  check "title" (DTitle "some title");
  check "errorrail" (DErrorRail (DInt 5));
  check "option" (DOption OptNothing);
  check "option" (DOption (OptJust (DInt 15)));
  check "db" (DDB "Visitors");
  check "list" (DList [DDB "Visitors"; DInt 4]);
  check "redirect" (DResp (Redirect "/home", DNull));
  check "httpresponse" (DResp (Response (200, []), DStr "success"));
  check "weird assoc 1" (DObj
                           (DvalMap.of_alist_exn [ ("type", DStr "weird")
                                                 ; ("value", DNull)]));
  check "weird assoc 2" (DObj
                           (DvalMap.of_alist_exn [ ("type", DStr "weird")
                                                 ; ("value", DStr "x")]));
  ()


let t_password_hashing_and_checking_works () =
  let ast = "(let password 'password'
               (Password::check (Password::hash password)
               password))"
  in
  check_dval "A `Password::hash'd string `Password::check's against itself."
    (exec_ast ast)
    (DBool true)

let t_password_hash_db_roundtrip () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "Passwords")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "password")
            ; Op.SetDBColType (dbid, coltypeid, "Password")]
  in
  let ast = "(let pw (Password::hash 'password')
               (let _ (DB::insert (obj (password pw)) Passwords)
                 (let fetched (. (List::head (DB::fetchAll Passwords)) password)
                   (pw fetched))))"
  in
  AT.check AT.int
    "A Password::hash'd string can get stored in and retrieved from a user database."
    0 (match exec_handler ~ops ast with
         DList [p1; p2;] -> compare_dval p1 p2
       | _ -> 1)


let t_passwords_dont_serialize () =
  let password = DPassword (Bytes.of_string "x") in
  AT.check AT.bool "Passwords don't serialize by default"
    true
    (let serialized = password
                      |> Dval.unsafe_dval_to_yojson (* ~redact:true by default *)
                      |> Yojson.Safe.sort in
     match serialized with
       `Assoc [("type", `String "password");
               ("value", `Null)] -> true
      |_ -> false)

let t_passwords_serialize () =
  let password = DPassword (Bytes.of_string "x") in
  AT.check (AT.option AT.string) "Passwords serialize if you turn off redaction "
    (Some "x")
    (let serialized = password
                      |> Dval.unsafe_dval_to_yojson ~redact:false
                      |> Yojson.Safe.sort in
     match serialized with
       `Assoc [("type", `String "password");
               ("value", `String x)] -> Some (B64.decode x)
      |_ -> None)

let t_password_json_round_trip_forwards () =
  let password = DPassword (Bytes.of_string "x") in
  check_dval
    "Passwords serialize and deserialize if there's no redaction."
    password
    (password
     |> Dval.unsafe_dval_to_json_string ~redact:false
     |> Dval.unsafe_dval_of_json_string)

let t_password_json_round_trip_backwards () =
  let json = "x"
      |> Bytes.of_string
      |> fun p -> DPassword p
      |> Dval.unsafe_dval_to_json_string ~redact:false
  in
  AT.check AT.string
    "Passwords deserialize and serialize if there's no redaction."
    json
    (json
     |> Dval.unsafe_dval_of_json_string
     |> Dval.unsafe_dval_to_json_string ~redact:false)

let t_incomplete_propagation () =
  check_dval "Fn with incomplete return incomplete"
    DIncomplete
    (exec_ast "(List::head _)");
  check_dval "Incompletes stripped from lists"
    (DList [DInt 5; DInt 6])
    (exec_ast "(5 6 (List::head _))");
  check_dval "Blanks stripped from lists"
    (DList [DInt 5; DInt 6])
    (exec_ast "(5 6 _)");
  check_dval "Blanks stripped from objects"
    (DObj (DvalMap.of_alist_exn ["m", DInt 5; "n", DInt 6]))
    (exec_ast "(obj (i _) (m 5) (j (List::head _)) (n 6))");
  check_dval "incomplete if conds are incomplete"
    DIncomplete
    (exec_ast "(if _ 5 6)");
  check_dval "blanks in threads are ignored"
    (DInt 8)
    (exec_ast "(| 5 _ (+ 3))");
  check_dval "incomplete in the middle of a thread is skipped"
    (DInt 8)
    (exec_ast "(| 5 (+ _) (+ 3))");
  check_dval "incomplete at the end of a thread is skipped"
    (DInt 5)
    (exec_ast "(| 5 (+ _))");
  check_dval "empty thread is incomplete"
    DIncomplete
    (exec_ast "(|)");
  check_dval "incomplete obj in field access is incomplete"
    DIncomplete
    (exec_ast "(. (List::head _) field)");
  check_dval "incomplete name in field access is incomplete"
    DIncomplete
    (exec_ast "(. (obj (i 5)) _)");
  ()

let t_html_escaping () =
  check_dval "html escaping works"
    (* TODO: add back in check that `'` is correctly escaped. It didn't
     * play nice with our hacky `'` removal in the DSL parser *)
    (DStr "test&lt;&gt;&amp;&quot;")
    (exec_ast "(String::htmlEscape 'test<>&\\\"')")

let t_curl_file_urls () =
  AT.check (AT.option AT.string) "aaa"
    (* Before we limited the protocols for curl, .info.error was "",
       since Httpclient.http_call checked for a 2xx HTTP code. But the file
       contents ended up in the error message. Now we've restricted the URL
       protocols, so we get CURLE_UNSUPPORTED_PROTOCOL before a request
       is even sent. *)
    (Some "Unsupported protocol")
    (try
       ignore (Httpclient.http_call "file://localhost/etc/passwd"
                 [] Httpclient.GET [] "");
       None
     with
       Exception.DarkException i -> List.Assoc.find i.info ~equal:(=) "error"
     | _ -> None)

let t_authenticate_user () =
  AT.check AT.bool "Account.authenticate_user works for the test user"
    true
    (Account.authenticate "test" "fVm2CUePzGKCwoEQQdNJktUQ"
     && not (Account.authenticate "test_unhashed" "fVm2CUePzGKCwoEQQdNJktUQ")
     && not (Account.authenticate "test" "no")
     && not (Account.authenticate "test_unhashed" "no"))

let t_uuid_db_roundtrip () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "Ids")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "uu")
            ; Op.SetDBColType (dbid, coltypeid, "UUID")
            ]
  in
  let ast = "(let i (Uuid::generate)
               (let _ (DB::insert (obj (uu i)) Ids)
                 (let fetched (. (List::head (DB::fetchAll Ids)) uu)
                   (i fetched))))"
  in
  AT.check AT.int "A generated UUID can round-trip from the DB"
    0
    (match exec_handler ~ops ast with
     | DList [p1; p2;] -> compare_dval p1 p2
     | _ -> 1)

let t_uuid_string_roundtrip () =
  let ast = "(let i (Uuid::generate)
               (let s (toString i)
                 (let parsed (String::toUUID s)
                   (i parsed))))"
  in
  AT.check AT.int "A generated id can round-trip"
    0 (match exec_ast ast with
         DList [p1; p2;] -> compare_dval p1 p2
       | _ -> 1)

let t_should_use_https () =
  AT.check (AT.list AT.bool) "should_use_https works"
    (List.map
       ~f:(fun x -> Server.should_use_https (Uri.of_string x))
       [ "http://builtwithdark.com"
       ; "http://test.builtwithdark.com"
       ; "http://localhost"
       ; "http://test.localhost"
    ])
    [ true
    ; true
    ; false
    ; false
    ]

let t_redirect_to () =
  AT.check (AT.list (AT.option AT.string)) "redirect_to works"
    (List.map
       ~f:(fun x ->
           x
           |> Uri.of_string
           |> Server.redirect_to
           |> Option.map ~f:Uri.to_string)
       [ "http://example.com"
       ; "http://builtwithdark.com"
       ; "https://builtwithdark.com"
       ; "http://test.builtwithdark.com"
       ; "https://test.builtwithdark.com"
       ; "http://test.builtwithdark.com/x/y?z=a"
       ])
    [ None
    ; Some "https://builtwithdark.com"
    ; None
    ; Some "https://test.builtwithdark.com"
    ; None
    ; Some "https://test.builtwithdark.com/x/y?z=a"
    ]

let t_errorrail_simple () =
  check_dval "rail"
    (DErrorRail (DOption OptNothing))
    (exec_ast "(`List::last_v1 [])");

  check_dval "no rail"
    (DOption OptNothing)
    (exec_ast "(Dict::get_v1 {} 'i')");

  check_dval "no rail deeply nested"
    (DInt 8)
    (exec_ast "(| (5)
                  (`List::head_v1)
                  (+ 3)
                  (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
               )");

  check_dval "to rail deeply nested"
    (DErrorRail (DOption OptNothing))
    (exec_ast "(| ()
                  (`List::head_v1)
                  (+ 3)
                  (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
               )");
  ()

let t_errorrail_toplevel () =
  check_dval "Errorrail goes to 404"
    (DResp (Response (404, []), DStr "Not found"))
    (exec_handler "(| ()
                      (`List::head_v1)
                      (+ 3)
                      (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
                    )");

  check_dval "No errorrail goes to option"
    (DOption OptNothing)
    (exec_handler "(List::head_v1 [])");
  ()



let t_errorrail_userfn () =
  check_dval "userfn unwraps"
    (DOption OptNothing)
    (exec_userfn "(| ()
                     (`List::head_v1)
                     (+ 3)
                     (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
                   )");
  ()

let t_nothing () =
  check_dval "can specifiy nothing"
    (DOption OptNothing)
    (exec_ast "nothing");

  check_dval "nothing works as expected"
    (DBool true)
    (exec_ast "(== (List::head_v1 []) nothing)");

  ()

let t_authenticate_then_handle_code_and_cookie () =
  (* basic auth headers *)
  let basic a b = Header.add_authorization (Header.init ()) (`Basic (a, b)) in
  (* sample execution id, makes grepping test logs easier *)
  let test_id = Types.id_of_int 1234 in
  (* uri doesn't matter very much since this should be uri-agnostic *)
  (* takes a req, returns the status code and the  parameters for Set-cookie: __session=whatever; [...] *)
  let ath_cookie (req : Req.t) : int * string option =
    Lwt_main.run
      (let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let%lwt (resp, _) =  Server.authenticate_then_handle
                              ~execution_id:test_id
                              (fun ~username req ->
                                Server.respond
                                  ~execution_id:test_id
                                  `OK
                                  "test handler")
                              req in
       let code = resp |> Resp.status |> Code.code_of_status in
       resp
       |> Resp.headers
       |> (fun x -> Header.get x "set-cookie")
       |> (fun x -> Option.bind x
                   ~f:(fun sc -> let (first, params) = String.lsplit2_exn ~on:';' sc in
                              let (name, value) = String.lsplit2_exn ~on:'=' first in
                              (* make sure some other cookie isn't getting set *)
                              if name = "__session" then Some (String.lstrip params) else None))
       |> (fun x -> return (code, x)))
  in
  AT.check (AT.list  (AT.pair AT.int (AT.option AT.string)))
    "authenticate_then_handle sets status codes and cookies correctly"
    (List.map
       ~f:ath_cookie

       (* valid basic auth login on darklang.com *)
       [  Req.make ~headers:(basic "test" "fVm2CUePzGKCwoEQQdNJktUQ")
            (Uri.of_string "http://darklang.com/a/test")

       (* valid basic auth login on localhost *)
        ; Req.make ~headers:(basic "test" "fVm2CUePzGKCwoEQQdNJktUQ")
            (Uri.of_string "http://darklang.localhost/a/test")

       (* invalid basic auth logins *)
       ; Req.make ~headers:(basic "test" "")
           (Uri.of_string "http://darklang.com/a/test")

       ; Req.make ~headers:(basic "" "fVm2CUePzGKCwoEQQdNJktUQ")
           (Uri.of_string "http://darklang.com/a/test")

       (* plain request, no auth *)
       ; Req.make (Uri.of_string "http://test.builtwithdark.com/a/test")
    ])

    [ 200, Some "Max-Age=604800; path=/; secure; httponly"
    ; 200, Some "Max-Age=604800; path=/; httponly"
    ; 401, None
    ; 401, None
    ; 401, None
    ]

let admin_handler_code ?(meth=`GET) ?(body="") (username, endpoint) =
  (* sample execution id, makes grepping test logs easier *)
  let test_id = Types.id_of_int 1234 in
    Lwt_main.run
      (let stop, stopper = Lwt.wait () in
       let uri = Uri.of_string ("http://builtwithdark.localhost:8000" ^ endpoint) in
       let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let%lwt (resp, _) =  Server.admin_handler
                              ~execution_id:test_id
                              ~uri
                              ~stopper
                              ~body
                              ~username
                              (Req.make ~meth uri) in
       resp |> Resp.status |> Code.code_of_status |> return)

let t_admin_handler_ui () =
  let ah_ui_response (username, canvas)  = admin_handler_code (username, "/a/" ^ canvas ^ "/")
  in
  AT.check (AT.list AT.int)
    "UI routes in admin_handler check authorization correctly."
    (List.map
       ~f:ah_ui_response
       [ "test", "test"
       (* everyone can edit demo *)
       ; "test", "demo"
       (* a la dabblefox *)
       ; "test", "test-something"
       (* arbitrary canvas belonging to another user *)
       ; "test", "test_admin"
       (* admin can look at test *)
       ; "test_admin", "test"
       ])

    [ 200
    ; 200
    ; 200
    ; 401
    ; 200
    ]

let t_admin_handler_ops () =
  AT.check (AT.list AT.int)
    "/ops/ routes in admin_handler check authorization correctly."
    (List.map
       ~f:admin_handler_code
       [ "test", "/ops/check-all-canvases"
       ; "test_admin", "/ops/check-all-canvases"
    ])
    [ 401
    ; 200
    ]

let t_admin_handler_api () =
  let ah_api_response (username, endpoint, body) =
    admin_handler_code ~meth:`POST ~body (username, endpoint)
  in
  AT.check (AT.list AT.int)
    "/api/ routes in admin_handler check authorization correctly."
    (List.map
       ~f:ah_api_response
       [ "test", "/api/test/initial_load", ""
       ; "test", "/api/test_admin/initial_load", ""
    ])
    [ 200
    ; 401
    ]

let t_db_write_deprecated_read_new () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  (* DID and DUUID deliberately do not unify, but we don't want to break
   * the contract that the old DB functions return DID, so we have to stringify *)
  let ast = "(let old (DB::insert (obj (x 'foo')) MyDB)
              (let stringified_id (toString (. old id))
               (let new (`DB::get_v1 stringified_id MyDB)
                (let mutated_new (assoc new 'id' stringified_id)
                 (let mutated_old (assoc old 'id' stringified_id)
                  (== mutated_new mutated_old))))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_read_deprecated_write_new_duuid () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let new_write (DB::set_v1 (obj (x 'foo')) (toString (Uuid::generate)) MyDB)
               (let old_read (DB::fetchOneBy 'foo' 'x' MyDB)
                 (let mutated_old_read (dissoc old_read 'id')
                   ((== new_write mutated_old_read) (. old_read id)))))"
  in
  let result = exec_handler ~ops ast in
  AT.check AT.int
  "Deprecated reads from an object with a new write give expected value and right type"
  0 (match result with
     | DList [DBool true; a] when Dval.tipe_of a = TID -> 0
     | _ -> 1)

let t_db_new_query_v2_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
            ; Op.SetDBColName (dbid, colnameid2, "y")
            ; Op.SetDBColType (dbid, coltypeid2, "Str")
            ]
  in
  let ast = "(let dontfind (DB::set_v1 (obj (x 'foo') (y 'bar')) 'hello' MyDB)
               (let hopetofind (DB::set_v1 (obj (x 'bar') (y 'foo')) 'findme' MyDB)
                (let results (DB::query_v2 (obj (x 'bar')) MyDB)
                 (== (hopetofind) results))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_set_does_upsert () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let old (DB::set_v1 (obj (x 'foo')) 'hello' MyDB)
               (let new (DB::set_v1 (obj (x 'bar')) 'hello' MyDB)
                (let results (DB::getAllWithKeys_v1 MyDB)
                 (== (('hello' new)) results))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_get_all_with_keys_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid2)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid2, "Str")
            ]
  in
  let ast = "(let one (DB::set_v1 (obj (x 'foo')) 'one' MyDB)
              (let two (DB::set_v1 (obj (x 'bar')) 'two' MyDB)
               (let results (DB::getAllWithKeys_v1 MyDB)
                (== (('one' one) ('two' two)) results))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_deprecated_belongs_to_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ; Op.CreateDB (dbid2, pos, "SecondDB")
            ; Op.AddDBCol (dbid2, colnameid2, coltypeid2)
            ; Op.SetDBColName (dbid2, colnameid2, "y")
            ; Op.SetDBColType (dbid2, coltypeid2, "Int")
            ; Op.AddDBCol (dbid, colnameid3, coltypeid3)
            ; Op.SetDBColName (dbid, colnameid3, "relation")
            ; Op.SetDBColType (dbid, coltypeid3, "SecondDB")
            ]
  in
  let ast = "(let oldin (DB::insert (obj (x 'foo') (relation (obj (y 4)))) MyDB)
               (List::head (DB::fetchAll MyDB)))"
  in
  let result = exec_handler ~ops ast in
  AT.check AT.int
  "Deprecated BelongsTo works"
  0 (match result with
     | DObj o ->
       (match (DvalMap.find o "x", DvalMap.find o "relation") with
        | (Some (DStr "foo"), Some (DObj inner)) ->
          (match (DvalMap.find inner "y") with
           | Some (DInt 4) -> 0
           | _ -> 1)
        | _ -> 1)
     | _ -> 1)

let t_db_deprecated_has_many_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ; Op.CreateDB (dbid2, pos, "SecondDB")
            ; Op.AddDBCol (dbid2, colnameid2, coltypeid2)
            ; Op.SetDBColName (dbid2, colnameid2, "y")
            ; Op.SetDBColType (dbid2, coltypeid2, "Int")
            ; Op.AddDBCol (dbid, colnameid3, coltypeid3)
            ; Op.SetDBColName (dbid, colnameid3, "relations")
            ; Op.SetDBColType (dbid, coltypeid3, "[SecondDB]")
            ]
  in
  let ast = "(let oldin (DB::insert (obj (x 'foo') (relations ((obj (y 4)) (obj (y 6))))) MyDB)
               (List::head (DB::fetchAll MyDB)))"
  in
  let result = exec_handler ~ops ast in
  AT.check AT.int
  "Deprecated HasMany works"
  0 (match result with
     | DObj o ->
       (match (DvalMap.find o "x", DvalMap.find o "relations") with
        | (Some (DStr "foo"), Some (DList inners)) ->
          (match inners with
           | [DObj fst; DObj snd] ->
             (try
                let sorted_list =
                    List.sort
                        ~compare:compare_dval
                        [DvalMap.find_exn fst "y"; DvalMap.find_exn snd "y"]
                in
                (match sorted_list with
                 | [DInt 4; DInt 6] -> 0
                 | _ -> 1)
             with e -> 1)
           | _ -> 1)
        | _ -> 1)
     | _ -> 1)

let t_db_deprecated_fetch_by_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
            ; Op.SetDBColName (dbid, colnameid2, "sort_by")
            ; Op.SetDBColType (dbid, coltypeid2, "Int")
            ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast = "(let one (DB::insert (obj (x 'foo') (sort_by 0)) MyDB)
              (let two (DB::insert (obj (x 'bar') (sort_by 1)) MyDB)
               (let three (DB::insert (obj (x 'bar') (sort_by 2)) MyDB)
                (let fetched (List::sortBy (DB::fetchBy 'bar' 'x' MyDB) (\\x -> (. x sort_by)))
                (== (two three) fetched)))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_deprecated_fetch_by_id_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::insert (obj (x 'foo')) MyDB)
              (let fetched (DB::fetchOneBy (. one id) 'id' MyDB)
                (== one fetched)))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_get_many_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (let two (DB::set_v1 (obj (x 'bar')) 'second' MyDB)
               (let fetched (DB::getMany_v1 ('first' 'second') MyDB)
                (== (('first' one) ('second' two)) fetched))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_queryWithKey_works_with_many () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
            ; Op.SetDBColName (dbid, colnameid2, "sort_by")
            ; Op.SetDBColType (dbid, coltypeid2, "Int")
            ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast = "(let one (DB::set_v1 (obj (x 'foo') (sort_by 0)) 'one' MyDB)
              (let two (DB::set_v1 (obj (x 'bar') (sort_by 1)) 'two' MyDB)
               (let three (DB::set_v1 (obj (x 'bar') (sort_by 2)) 'three' MyDB)
                (let fetched (List::sortBy (DB::queryWithKey_v1 (obj (x 'bar')) MyDB) (\\x -> (. (List::last x) sort_by)))
                 (== (('two' two) ('three' three)) fetched)))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_deprecated_delete_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::insert (obj (x 'foo')) MyDB)
              (let fetched (DB::delete one MyDB)
               (== 0 (List::length (DB::fetchAll MyDB)))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_deprecated_update_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::insert (obj (x 'foo')) MyDB)
              (let update (DB::update (assoc one 'x' 'bar') MyDB)
               (== 1 (List::length (DB::fetchAll MyDB)))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)

let t_db_get_returns_nothing () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  check_dval "get_returns_nothing"
    (DOption OptNothing)
    (exec_handler ~ops "(DB::get_v1 'lol' MyDB)")

let t_feature_flags_work () =
  check_dval "flag shows new for true"
    (DInt 1)
    (exec_ast "(flag _ true 2 1)");

  check_dval "flag shows old for false"
    (DInt 2)
    (exec_ast "(flag _ false 2 1)");

  check_dval "flag shows old for incomplete cond"
    (DInt 2)
    (exec_ast "(flag _ _ 2 1)");

  check_dval "flag shows old for null"
    (DInt 2)
    (exec_ast "(flag _ null 2 1)");

  check_dval "flag shows old for error"
    (DInt 2)
    (exec_ast "(flag _ (List::head) 2 1)");

  check_dval "flag shows old for errorrail"
    (DInt 2)
    (exec_ast "(flag _ (`List::head []) 2 1)");

  check_dval "flag shows old for object"
    (DInt 2)
    (exec_ast "(flag _ (obj (x true)) 2 1)");

  check_dval "flag shows old for list"
    (DInt 2)
    (exec_ast "(flag _ [] 2 1)");

  ()

let t_db_queryOne_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOne_v1 (obj (x 'foo')) MyDB))"
  in
  check_dval "equal_after_roundtrip"
    (DOption
       (OptJust
          (DObj (DvalMap.singleton "x" (DStr "foo")))
       )
    )
    (exec_handler ~ops ast)

let t_db_queryOne_returns_nothing_if_none () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOne_v1 (obj (x 'bar')) MyDB))"
  in
  check_dval "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)

let t_db_queryOne_returns_nothing_multiple () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (let one (DB::set_v1 (obj (x 'foo')) 'second' MyDB)
               (DB::queryOne_v1 (obj (x 'foo')) MyDB)))"
  in
  check_dval "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)

let t_db_queryOneWithKey_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOneWithKey_v1 (obj (x 'foo')) MyDB))"
  in
  check_dval "equal_after_roundtrip"
    (DOption
       (OptJust
          (DList
             [DStr "first"
             ;DObj (DvalMap.singleton "x" (DStr "foo"))
             ]
          )
       )
    )
    (exec_handler ~ops ast)

let t_db_queryOneWithKey_returns_nothing_if_none () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (DB::queryOneWithKey_v1 (obj (x 'bar')) MyDB))"
  in
  check_dval "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)

let t_db_queryOneWithKey_returns_nothing_multiple () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ]
  in
  let ast = "(let one (DB::set_v1 (obj (x 'foo')) 'first' MyDB)
              (let one (DB::set_v1 (obj (x 'foo')) 'second' MyDB)
               (DB::queryOneWithKey_v1 (obj (x 'foo')) MyDB)))"
  in
  check_dval "equal_after_roundtrip"
    (DOption OptNothing)
    (exec_handler ~ops ast)

let t_db_getAll_v2_works () =
  clear_test_data ();
  let ops = [ Op.CreateDB (dbid, pos, "MyDB")
            ; Op.AddDBCol (dbid, colnameid, coltypeid)
            ; Op.SetDBColName (dbid, colnameid, "x")
            ; Op.SetDBColType (dbid, coltypeid, "Str")
            ; Op.AddDBCol (dbid, colnameid2, coltypeid2)
            ; Op.SetDBColName (dbid, colnameid2, "sort_by")
            ; Op.SetDBColType (dbid, coltypeid2, "Int")
            ]
  in
  (* sorting to ensure the test isn't flakey *)
  let ast = "(let one (DB::set_v1 (obj (x 'foo') (sort_by 0)) 'one' MyDB)
              (let two (DB::set_v1 (obj (x 'bar') (sort_by 1)) 'two' MyDB)
               (let three (DB::set_v1 (obj (x 'baz') (sort_by 2)) 'three' MyDB)
                (let fetched (List::sortBy (DB::getAll_v2 MyDB) (\\x -> (. (List::last x) sort_by)))
                 (== (one two three) fetched)))))"
  in
  check_dval "equal_after_roundtrip"
    (DBool true)
    (exec_handler ~ops ast)



(* ------------------- *)
(* Test setup *)
(* ------------------- *)

let suite =
  [ "hmac signing works", `Quick, t_hmac_signing
  ; "undo", `Quick, t_undo
  ; "undo_fns", `Quick, t_undo_fns
  ; "int_add_works", `Quick, t_int_add_works
  ; "lambda_with_foreach", `Quick, t_lambda_with_foreach
  ; "stored_events", `Quick, t_stored_event_roundtrip
  (* ; "event_queue roundtrip", `Quick, t_event_queue_roundtrip *)
  ; "bad ssl cert", `Slow, t_bad_ssl_cert
  ; "db binary oplist roundtrip", `Quick, t_db_oplist_roundtrip
  ; "http oplist roundtrip", `Quick, t_http_oplist_roundtrip
  ; "derror roundtrip", `Quick, t_derror_roundtrip
  ; "DB case-insensitive roundtrip", `Quick,
    t_case_insensitive_db_roundtrip
  ; "Good error when inserting badly", `Quick,
    t_inserting_object_to_missing_col_gives_good_error
  ; "Stdlib fns work", `Quick, t_stdlib_works
  ; "Feature flags work", `Quick, t_feature_flags_work
  ; "Cron should run sanity", `Quick, t_cron_sanity
  ; "Cron just ran", `Quick, t_cron_just_ran
  ; "Roundtrip user_data into jsonb using deprecated funcs", `Quick, t_roundtrip_user_data_using_deprecated_functions
  ; "Test postgres escaping", `Quick, t_escape_pg_escaping
  ; "Nulls allowed in DB", `Quick, t_nulls_allowed_in_db
  ; "Nulls for missing column", `Quick, t_nulls_added_to_missing_column
  ; "Parsing JSON to DVals doesn't care about key order", `Quick,
    t_unsafe_dval_of_yojson_doesnt_care_about_order
  ; "End-user password hashing and checking works", `Quick,
    t_password_hashing_and_checking_works
  ; "Password hashes can be stored in and retrieved from the DB", `Quick,
     t_password_hash_db_roundtrip
  ; "Passwords don't serialize by default", `Quick, t_passwords_dont_serialize
  ; "Passwords serialize if you turn off redaction", `Quick, t_passwords_serialize
  ; "Passwords serialize and deserialize if there's no redaction.", `Quick,
    t_password_json_round_trip_forwards
  ; "Passwords deserialize and serialize if there's no redaction.", `Quick,
    t_password_json_round_trip_backwards
  ; "Incompletes propagate correctly", `Quick, t_incomplete_propagation
  ; "HTML escaping works reasonably", `Quick, t_html_escaping
  ; "Dark code can't curl file:// urls", `Quick, t_curl_file_urls
  ; "Account.authenticate_user works when it should", `Quick,
    t_authenticate_user
  ; "UUIDs round-trip to the DB", `Quick, t_uuid_db_roundtrip
  ; "UUIDs round-trip to/from strings", `Quick, t_uuid_string_roundtrip
  ; "Server.should_use_https works", `Quick,  t_should_use_https
  ; "Server.redirect_to works", `Quick, t_redirect_to
  ; "Errorrail simple", `Quick, t_errorrail_simple
  ; "Errorrail works in toplevel", `Quick, t_errorrail_toplevel
  ; "Errorrail works in user_function", `Quick, t_errorrail_userfn
  ; "Handling nothing in code works", `Quick, t_nothing
  ; "authenticate_then_handle sets status codes and cookies correctly ", `Quick, t_authenticate_then_handle_code_and_cookie
  ; "UI routes in admin_handler work ", `Quick, t_admin_handler_ui
  ; "/ops/ routes in admin_handler work ", `Quick, t_admin_handler_ops
  ; "/api/ routes in admin_handler work ", `Quick, t_admin_handler_api
  ; "New DB code can read old writes", `Quick, t_db_write_deprecated_read_new
  ; "Old DB code can read new writes with UUID key", `Quick, t_db_read_deprecated_write_new_duuid
  ; "New query function works", `Quick, t_db_new_query_v2_works
  ; "DB::set_v1 upserts", `Quick, t_db_set_does_upsert
  ; "DB::getAllWithKeys_v1 works", `Quick, t_db_get_all_with_keys_works
  ; "Deprecated BelongsTo works", `Quick, t_db_deprecated_belongs_to_works
  ; "Deprecated HasMany works", `Quick, t_db_deprecated_has_many_works
  ; "Deprecated fetchBy works", `Quick, t_db_deprecated_fetch_by_works
  ; "Deprecated fetchBy works with an id", `Quick, t_db_deprecated_fetch_by_id_works
  ; "DB::getMany_v1 works", `Quick, t_db_get_many_works
  ; "DB::queryWithKey_v1 works with many items", `Quick, t_db_queryWithKey_works_with_many
  ; "Deprecated delete works", `Quick, t_db_deprecated_delete_works
  ; "Deprecated update works", `Quick, t_db_deprecated_update_works
  ; "DB::get_v1 returns Nothing if not found", `Quick, t_db_get_returns_nothing
  ; "DB::queryOne returns Some obj if found", `Quick, t_db_queryOne_works
  ; "DB::queryOne returns Nothing if not found", `Quick, t_db_queryOne_returns_nothing_if_none
  ; "DB::queryOne returns Nothing if more than one found", `Quick, t_db_queryOne_returns_nothing_multiple
  ; "DB::queryOneWithKey returns Just obj if found", `Quick, t_db_queryOneWithKey_works
  ; "DB::queryOneWithKey returns Nothing if not found", `Quick, t_db_queryOneWithKey_returns_nothing_if_none
  ; "DB::queryOneWithKey returns Nothing if more than one found", `Quick, t_db_queryOneWithKey_returns_nothing_multiple
  ; "Dvals roundtrip to yojson correctly", `Quick, t_dval_yojson_roundtrips
  ; "DB::getAll_v2 works", `Quick, t_db_getAll_v2_works
  ]

let () =
  Libbackend.Init.init ~run_side_effects:true;
  Log.set_level `All;
  Account.init_testing ();

  let wrap f =
    fun () ->
      try
        f ()
      with e ->
        Exception.reraise_after e (fun bt ->
          print_endline (Exception.to_string e);
          print_endline (Exception.backtrace_to_string bt))
  in
  let wrapped_suite =
    List.map suite ~f:(fun (n, m, t) -> (n, m, wrap t))
  in

  let (suite, exit) =
    Junit_alcotest.run_and_report "suite" ["tests", wrapped_suite] in
  let report = Junit.make [suite] in
  File.mkdir ~root:Testresults "";
  let file = File.check_filename ~mode:`Write ~root:Testresults "backend.xml" in
  Junit.to_file report file;
  exit ()


