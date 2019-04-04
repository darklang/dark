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
  Db.run
    ~params:[Uuid canvas]
    ~name:"clear_events_test_data"
    "DELETE FROM events where canvas_id = $1" ;
  Db.run
    ~params:[Uuid canvas]
    ~name:"clear_stored_events_test_data"
    "DELETE FROM stored_events_v2 where canvas_id = $1" ;
  Db.run
    ~params:[Uuid canvas]
    ~name:"clear_function_results_test_data"
    "DELETE FROM function_results_v2 where canvas_id = $1" ;
  Db.run
    ~params:[Uuid canvas]
    ~name:"clear_user_data_test_data"
    "DELETE FROM user_data where canvas_id = $1" ;
  Db.run
    ~params:[Uuid canvas]
    ~name:"clear_cron_records_test_data"
    "DELETE FROM cron_records where canvas_id = $1" ;
  Db.run
    ~params:[Uuid canvas]
    ~name:"clear_toplevel_oplists_test_data"
    "DELETE FROM toplevel_oplists WHERE canvas_id = $1" ;
  Db.run
    ~params:[Uuid canvas]
    ~name:"clear_function_arguments"
    "DELETE FROM function_arguments WHERE canvas_id = $1" ;
  Db.run
    ~params:[Uuid canvas]
    ~name:"clear_canvases_test_data"
    "DELETE FROM canvases where id = $1" ;
  ()


(* ------------------- *)
(* Test fns *)
(* ------------------- *)

let at_dval =
  AT.testable
    (fun fmt dv -> Fmt.pf fmt "%s" (Dval.show dv))
    (fun a b -> compare_dval a b = 0)


let check_dval = AT.check at_dval

let check_dval_list = AT.check (AT.list at_dval)

let check_oplist = AT.check (AT.of_pp Op.pp_oplist)

let check_tlid_oplists = AT.check (AT.of_pp Op.pp_tlid_oplists)

let check_exception ?(check = fun _ -> true) ~(f : unit -> dval) msg =
  let e =
    try
      let r = f () in
      Log.erroR "result" ~data:(Dval.to_developer_repr_v0 r) ;
      Some "no exception"
    with
    | Exception.DarkException ed ->
        if check ed
        then None
        else (
          Log.erroR "check failed" ~data:(Log.dump ed) ;
          Some "Check failed" )
    | e ->
        let bt = Backtrace.Exn.most_recent () in
        let msg = Exn.to_string e in
        print_endline (Backtrace.to_string bt) ;
        Log.erroR "different exception" ~data:msg ;
        Some "different exception"
  in
  AT.check (AT.option AT.string) msg None e


let check_error_contains (name : string) (result : dval) (substring : string) =
  let strresult = Dval.to_developer_repr_v0 result in
  AT.(check bool)
    (name ^ ": (\"" ^ strresult ^ "\" contains \"" ^ substring ^ "\"")
    true
    (String.is_substring ~substring strresult)


(* ------------------- *)
(* Set up test data *)
(* ------------------- *)

let fid = Util.create_id

let v str = Filled (fid (), Value str)

let b () = Blank (fid ())

let f a = Filled (fid (), a)

let fncall (a, b) = f (FnCall (a, b))

let tlid = Int63.of_int 7

let tipe_id = Int63.of_int 9

let dbid = Int63.of_int 89

let dbid2 = Int63.of_int 189

let colnameid = Int63.of_int 11

let coltypeid = Int63.of_int 12

let colnameid2 = Int63.of_int 13

let coltypeid2 = Int63.of_int 14

let colnameid3 = Int63.of_int 15

let coltypeid3 = Int63.of_int 16

let nameid = Int63.of_int 17

let pos = {x = 0; y = 0}

let execution_id = Int63.of_int 6542

let ast_for = Expr_dsl.ast_for

let handler ast : HandlerT.handler =
  { tlid
  ; ast
  ; spec =
      { module_ = b ()
      ; name = b ()
      ; modifier = b ()
      ; types = {input = b (); output = b ()} } }


let http_handler ast : HandlerT.handler =
  { tlid
  ; ast
  ; spec =
      { module_ = f "HTTP"
      ; name = f "/test"
      ; modifier = f "GET"
      ; types = {input = b (); output = b ()} } }


let http_request_path = "/some/vars/and/such"

let http_route = "/some/:vars/:and/such"

let http_route_handler ?(route = http_route) () : HandlerT.handler =
  { tlid
  ; ast = f (Value "5")
  ; spec =
      { module_ = f "HTTP"
      ; name = f http_route
      ; modifier = f "GET"
      ; types = {input = b (); output = b ()} } }


let daily_cron ast : HandlerT.handler =
  { tlid
  ; ast
  ; spec =
      { module_ = f "CRON"
      ; name = f "test"
      ; modifier = f "Daily"
      ; types = {input = b (); output = b ()} } }


let hop h = Op.SetHandler (tlid, pos, h)

let user_fn name params ast : user_fn =
  { tlid
  ; ast
  ; metadata =
      { name = f name
      ; parameters =
          List.map params ~f:(fun p ->
              { name = f p
              ; tipe = f TAny
              ; block_args = []
              ; optional = false
              ; description = "test" } )
      ; return_type = f TAny
      ; description = "test user fn"
      ; infix = false } }


let user_record name fields : user_tipe =
  {tlid = tipe_id; version = 0; name = f name; definition = UTRecord fields}


let t4_get1st (x, _, _, _) = x

let t4_get4th (_, _, _, x) = x

let sample_dvals =
  [ ("int", DInt 5)
  ; ("int2", DInt (-1))
  ; ("float", DFloat 7.2)
  ; ("float2", DFloat (-7.2))
  ; ("true", DBool true)
  ; ("false", DBool false)
  ; ("null", DNull)
  ; ("string", Dval.dstr_of_string_exn "incredibly this was broken")
  ; ("list", DList [DDB "Visitors"; DInt 4])
  ; ("obj", DObj (DvalMap.of_alist_exn [("foo", DInt 5)]))
  ; ( "obj2"
    , DObj
        (DvalMap.of_alist_exn
           [("type", Dval.dstr_of_string_exn "weird"); ("value", DNull)]) )
  ; ( "obj3"
    , DObj
        (DvalMap.of_alist_exn
           [ ("type", Dval.dstr_of_string_exn "weird")
           ; ("value", Dval.dstr_of_string_exn "x") ]) )
  ; ("incomplete", DIncomplete)
  ; ("error", DError "some error string")
  ; ("block", DBlock (fun _args -> DNull))
  ; ("errorrail", DErrorRail (DInt 5))
  ; ("redirect", DResp (Redirect "/home", DNull))
  ; ( "httpresponse"
    , DResp (Response (200, []), Dval.dstr_of_string_exn "success") )
  ; ("db", DDB "Visitors")
  ; ("id", DID (Util.uuid_of_string "7d9e5495-b068-4364-a2cc-3633ab4d13e6"))
  ; ("date", DDate (Time.of_string "2018-09-14T00:31:41Z"))
  ; ("password", DPassword (PasswordBytes.of_string "somebytes"))
  ; ("uuid", DUuid (Util.uuid_of_string "7d9e5495-b068-4364-a2cc-3633ab4d13e6"))
  ; ("option", DOption OptNothing)
  ; ("option2", DOption (OptJust (DInt 15)))
  ; ("character", DCharacter (Unicode_string.Character.unsafe_of_string "s"))
  ; ("result", DResult (ResOk (DInt 15)))
  ; ( "result2"
    , DResult
        (ResError (DList [Dval.dstr_of_string_exn "dunno if really supported"]))
    ) ]


(* ------------------- *)
(* Execution *)
(* ------------------- *)
let ops2c (host : string) (ops : Op.op list) : C.canvas ref = C.init host ops

let test_execution_data ?(canvas_name = "test") ops :
    C.canvas ref * exec_state * input_vars =
  let c = ops2c canvas_name ops in
  let vars = Execution.dbs_as_input_vars (TL.dbs !c.dbs) in
  let canvas_id = !c.id in
  let trace_id = Util.create_uuid () in
  let state =
    { tlid
    ; account_id = !c.owner
    ; canvas_id = !c.id
    ; user_fns = !c.user_functions
    ; user_tipes = !c.user_tipes
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
  let ( c
      , {tlid; execution_id; dbs; user_fns; user_tipes; account_id; canvas_id}
      , input_vars ) =
    test_execution_data ops
  in
  let h = !c.handlers |> TL.handlers |> List.hd_exn in
  Execution.execute_handler
    h
    ~tlid
    ~execution_id
    ~dbs
    ~user_fns
    ~user_tipes
    ~account_id
    ~canvas_id
    ~input_vars:[]


(* already provided in execute_handler *)

let exec_handler ?(ops = []) (prog : string) : dval =
  prog
  |> ast_for
  (* |> Log.pp ~f:show_expr *)
  |> handler
  |> hop
  |> fun h -> execute_ops (ops @ [h])


let exec_ast ?(canvas_name = "test") (prog : string) : dval =
  let c, state, input_vars = test_execution_data ~canvas_name [] in
  Ast.execute_ast input_vars state (ast_for prog)


let exec_userfn (prog : string) : dval =
  let name = "test_function" in
  let ast = ast_for prog in
  let fn = user_fn name [] ast in
  let c, state, _ = test_execution_data [SetFunction fn] in
  Ast.execute_userfn state name execution_id []


(* ----------------------- *)
(* The tests *)
(* ----------------------- *)

let t_undo_fns () =
  clear_test_data () ;
  let n1 = Op.TLSavepoint tlid in
  let n2 = hop (handler (ast_for "(- _ _)")) in
  let n3 = hop (handler (ast_for "(- 3 _)")) in
  let n4 = hop (handler (ast_for "(- 3 4)")) in
  let u = Op.UndoTL tlid in
  let ops (c : C.canvas ref) = !c.ops |> List.hd_exn |> Tuple.T2.get2 in
  AT.check
    AT.int
    "undocount"
    3
    (Undo.undo_count
       (ops2c "test" [n1; n1; n1; n1; n2; n3; n4; u; u; u] |> ops)
       tlid)


let t_undo () =
  clear_test_data () ;
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
  execute_ops ops |> check_dval "t_undo_1" (DInt 5) ;
  (* First undo *)
  execute_ops (List.concat [ops; [u]]) |> check_dval "t_undo_3" (DInt 4) ;
  (* Second undo *)
  execute_ops (List.concat [ops; [u; u]]) |> check_dval "t_undo_4" (DInt 3) ;
  (* First redo *)
  execute_ops (List.concat [ops; [u; u; r]]) |> check_dval "t_undo_5" (DInt 4) ;
  (* Second redo *)
  execute_ops (List.concat [ops; [u; u; r; r]])
  |> check_dval "t_undo_6" (DInt 5) ;
  (* Another undo *)
  execute_ops (List.concat [ops; [u; u; r; r; u]])
  |> check_dval "t_undo_7" (DInt 4) ;
  (* Another redo *)
  execute_ops (List.concat [ops; [u; u; r; r; u; r]])
  |> check_dval "t_undo_8" (DInt 5)


let t_inserting_object_to_missing_col_gives_good_error () =
  clear_test_data () ;
  check_error_contains
    "error is expected"
    (exec_handler
       "(DB::insert (obj (col (obj))) TestDB)"
       ~ops:[Op.CreateDB (dbid, pos, "TestDB")])
    "Found but did not expect: [col]"


let t_int_add_works () =
  (* Couldn't call Int::add *)
  check_dval "int_add" (DInt 8) (exec_ast "(+ 5 3)")


let t_stdlib_works () =
  check_dval
    "uniqueBy1"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> (Int::divide x 2)))")
    (DList [DInt 1; DInt 3; DInt 4]) ;
  check_dval
    "uniqueBy2"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> x))")
    (DList [DInt 1; DInt 2; DInt 3; DInt 4]) ;
  check_error_contains
    "base64decode"
    (exec_ast "(String::base64Decode 'random string')")
    "Not a valid base64 string" ;
  ()


let t_multiple_copies_of_same_name () =
  check_dval
    "object field names"
    (exec_ast "(obj (col1 1) (col1 2))")
    (DError "The same key occurs multiple times") ;
  let ops =
    [Op.CreateDB (dbid, pos, "TestDB"); Op.CreateDB (dbid, pos, "TestDB")]
  in
  check_exception (fun _ -> exec_handler ~ops "_") "db names" ;
  ()


let t_derror_roundtrip () =
  let x = DError "test" in
  let converted =
    x
    |> Dval.to_internal_roundtrippable_v0
    |> Dval.of_internal_roundtrippable_v0
  in
  check_dval "roundtrip" converted x


let t_db_oplist_roundtrip () =
  clear_test_data () ;
  let host = "test-db_oplist_roundtrip" in
  let owner = Account.for_host host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let oplist =
    [Op.UndoTL tlid; Op.RedoTL tlid; Op.UndoTL tlid; Op.RedoTL tlid]
  in
  Serialize.save_toplevel_oplist
    oplist
    ~tlid
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
  let oplist = [Op.SetHandler (tlid, pos, http_route_handler ())] in
  let c1 = Canvas.init host oplist in
  Canvas.serialize_only [tlid] !c1 ;
  let c2 = Canvas.load_http ~path:http_request_path ~verb:"GET" host in
  check_tlid_oplists "http_oplist roundtrip" !c1.ops !c2.ops


let t_http_oplist_loads_user_tipes () =
  clear_test_data () ;
  let host = "test-http_oplist_loads_user_tipes" in
  let tipe = user_record "test-tipe" [] in
  let oplist =
    [Op.SetHandler (tlid, pos, http_route_handler ()); Op.SetType tipe]
  in
  let c1 = Canvas.init host oplist in
  Canvas.serialize_only [tlid; tipe.tlid] !c1 ;
  let c2 = Canvas.load_http ~path:http_request_path ~verb:"GET" host in
  AT.check
    (AT.list (AT.testable pp_user_tipe equal_user_tipe))
    "user tipes"
    [tipe]
    !c2.user_tipes


let date_migration_has_correct_formats () =
  let str = "2019-03-08T08:26:14Z" in
  let date = DDate (Util.date_of_isostring str) in
  let expected =
    Yojson.pretty_to_string
      (`Assoc [("type", `String "date"); ("value", `String str)])
  in
  AT.check
    AT.string
    "old format"
    expected
    (Legacy.PrettyResponseJsonV0.to_pretty_response_json_v0 date) ;
  AT.check
    AT.string
    "new format"
    ("\"" ^ str ^ "\"")
    (Dval.to_pretty_machine_json_v1 date)


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
            (DB::insert (obj (cOlUmNnAmE 'some value')) TestUnicode)
            (DB::fetchAll TestUnicode))"
  in
  match exec_handler ~ops ast with
  | DList [DObj v] ->
      AT.(check bool)
        "matched"
        true
        (List.mem ~equal:( = ) (DvalMap.data v) value)
  | other ->
      Log.erroR "error" ~data:(Dval.to_developer_repr_v0 other) ;
      AT.(check bool) "failed" true false


let t_lambda_with_foreach () =
  check_dval
    "lambda_with_foreach"
    (Dval.dstr_of_string_exn "SOME STRING")
    (exec_ast
       "(String::join
       (List::foreach (String::toList_v1 'some string') (\\var ->
(String::toUppercase (String::fromChar_v1 var)))) '')")


module SE = Stored_event

let t_stored_event_roundtrip () =
  clear_test_data () ;
  let owner : Uuidm.t =
    Account.owner ~auth_domain:"test" |> fun x -> Option.value_exn x
  in
  let id1 = Serialize.fetch_canvas_id owner "host" in
  let id2 = Serialize.fetch_canvas_id owner "host2" in
  let t1 = Util.create_uuid () in
  let t2 = Util.create_uuid () in
  let t3 = Util.create_uuid () in
  let t4 = Util.create_uuid () in
  let t5 = Util.create_uuid () in
  SE.clear_all_events ~canvas_id:id1 () ;
  SE.clear_all_events ~canvas_id:id2 () ;
  let desc1 = ("HTTP", "/path", "GET") in
  let desc2 = ("HTTP", "/path2", "GET") in
  let desc3 = ("HTTP", "/path", "POST") in
  ignore
    (SE.store_event
       ~canvas_id:id1
       ~trace_id:t1
       desc1
       (Dval.dstr_of_string_exn "1")) ;
  ignore
    (SE.store_event
       ~canvas_id:id1
       ~trace_id:t2
       desc1
       (Dval.dstr_of_string_exn "2")) ;
  ignore
    (SE.store_event
       ~canvas_id:id1
       ~trace_id:t3
       desc3
       (Dval.dstr_of_string_exn "3")) ;
  ignore
    (SE.store_event
       ~canvas_id:id1
       ~trace_id:t4
       desc2
       (Dval.dstr_of_string_exn "3")) ;
  ignore
    (SE.store_event
       ~canvas_id:id2
       ~trace_id:t5
       desc2
       (Dval.dstr_of_string_exn "3")) ;
  let at_trace_id = AT.of_pp Uuidm.pp_string in
  let to_trace_id (t1, t2, t3, t4, t5) = t5 in
  let listed = SE.list_events ~limit:`All ~canvas_id:id1 () in
  AT.check
    (AT.list at_trace_id)
    "list host events"
    (List.sort ~compare [t1; t3; t4])
    (List.sort ~compare (List.map ~f:to_trace_id listed)) ;
  let loaded1 = SE.load_events ~canvas_id:id1 desc1 |> List.map ~f:t4_get4th in
  check_dval_list
    "load GET events"
    [Dval.dstr_of_string_exn "2"; Dval.dstr_of_string_exn "1"]
    loaded1 ;
  let loaded2 = SE.load_events ~canvas_id:id1 desc3 |> List.map ~f:t4_get4th in
  check_dval_list "load POST events" [Dval.dstr_of_string_exn "3"] loaded2 ;
  let loaded3 = SE.load_events ~canvas_id:id2 desc3 |> List.map ~f:t4_get4th in
  check_dval_list "load no host2 events" [] loaded3 ;
  let loaded4 = SE.load_events ~canvas_id:id2 desc2 |> List.map ~f:t4_get4th in
  check_dval_list "load host2 events" [Dval.dstr_of_string_exn "3"] loaded4 ;
  ()


(* This doesn't actually test input, since it's a cron handler and not an actual
 * event handler *)
let t_event_queue_roundtrip () =
  clear_test_data () ;
  let h = daily_cron (ast_for "123") in
  let c = ops2c "test-event_queue" [hop h] in
  Canvas.save_all !c ;
  Event_queue.enqueue
    "CRON"
    "test"
    "Daily"
    DNull (* I don't believe crons take inputs? *)
    ~account_id:!c.owner
    ~canvas_id:!c.id ;
  let result = Queue_worker.run execution_id in
  ( match result with
  | Ok (Some result_dval) ->
      check_dval "Round tripped value" (DInt 123) result_dval
  | Ok None ->
      AT.fail "Failed: expected Some, got None"
  | Error e ->
      AT.fail ("Failed: got error: " ^ Log.dump e) ) ;
  ()


let t_bad_ssl_cert _ =
  check_error_contains
    "should get bad_ssl"
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
    ; access_token_secret = "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE" }
  in
  let k1 = "status" in
  let v1 = "Hello Ladies + Gentlemen, a signed OAuth request!" in
  let k2 = "include_entities" in
  let v2 = "true" in
  (* Test 1 - just the sig *)
  AT.check
    AT.string
    "hmac_signing_1"
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
       ; ("oauth_version", "1.0") ]) ;
  (* Test 2 - full header *)
  let url = "https://api.twitter.com/1.1/statuses/update.json" in
  Mock.set_string "ts" ts ;
  Mock.set_string "nonce" nonce ;
  let args =
    DvalMap.of_alist_exn
      [(k1, Dval.dstr_of_string_exn v1); (k2, Dval.dstr_of_string_exn v2)]
  in
  let expected_header =
    "OAuth oauth_consumer_key=\"xvz1evFS4wEEPTGEFPHBog\", oauth_nonce=\"kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg\", oauth_signature=\"hCtSmYh%2BiHYCEqBWrE7C7hYmtUk%3D\", oauth_signature_method=\"HMAC-SHA1\", oauth_timestamp=\"1318622958\", oauth_token=\"370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb\", oauth_version=\"1.0\""
  in
  let actual = Twitter.oauth_header secret url "POST" args in
  AT.check AT.string "hmac_signing_2" expected_header actual


let t_cron_sanity () =
  clear_test_data () ;
  let h = daily_cron (ast_for "(+ 5 3)") in
  let c = ops2c "test-cron_works" [hop h] in
  let handler = !c.handlers |> TL.handlers |> List.hd_exn in
  let should_run = Cron.should_execute !c.id handler in
  AT.check AT.bool "should_run should be true" should_run true ;
  ()


let t_cron_just_ran () =
  clear_test_data () ;
  let h = daily_cron (ast_for "(+ 5 3)") in
  let c = ops2c "test-cron_works" [hop h] in
  let handler = !c.handlers |> TL.handlers |> List.hd_exn in
  Cron.record_execution !c.id handler ;
  let should_run = Cron.should_execute !c.id handler in
  AT.check AT.bool "should_run should be false" should_run false ;
  ()


let t_roundtrip_user_data_using_deprecated_functions () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let v 'lasd;04mr'
               (let old (DB::insert (obj (x v)) MyDB)
               (let new (DB::fetchOneBy v 'x' MyDB)
               (== old new))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_escape_pg_escaping () =
  AT.check AT.string "no quotes" "asdd" (Db.escape_single "asdd") ;
  AT.check AT.string "single" "as''dd" (Db.escape_single "as'dd") ;
  AT.check AT.string "double" "as\"dd" (Db.escape_single "as\"dd") ;
  ()


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
    (DObj (DvalMap.of_alist_exn [("x", DNull)]))
    (exec_handler ~ops ast)


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
           (DvalMap.of_alist_exn
              [("x", Dval.dstr_of_string_exn "v"); ("y", DNull)]) ])
    (exec_handler ~ops "(List::head (DB::getAllWithKeys_v1 MyDB))")


let t_internal_roundtrippable_doesnt_care_about_order () =
  check_dval
    "internal_roundtrippable doesn't care about key order"
    (Dval.of_internal_roundtrippable_v0
       "{
         \"type\": \"weird\",
         \"value\": \"x\"
        }")
    (Dval.of_internal_roundtrippable_v0
       "{
         \"value\": \"x\",
         \"type\": \"weird\"
        }")


let t_dval_yojson_roundtrips () =
  let roundtrippable_rt v =
    v
    |> Dval.to_internal_roundtrippable_v0
    |> Dval.of_internal_roundtrippable_v0
  in
  let queryable_rt v =
    v |> Dval.to_internal_queryable_v0 |> Dval.of_internal_queryable_v0
  in
  (* Don't really need to check this but what harm *)
  let safe_rt v =
    v |> dval_to_yojson |> dval_of_yojson |> Result.ok_or_failwith
  in
  let check name (v : dval) =
    check_dval ("safe: " ^ name) v (safe_rt v) ;
    check_dval ("roundtrippable: " ^ name) v (roundtrippable_rt v) ;
    check_dval ("queryable: " ^ name) v (queryable_rt v) ;
    ()
  in
  sample_dvals
  |> List.filter ~f:(function
         | _, DBlock _ | _, DPassword _ ->
             false
         | _ ->
             true )
  |> List.iter ~f:(fun (name, dv) -> check name dv)


let t_password_hashing_and_checking_works () =
  let ast =
    "(let password 'password'
               (Password::check (Password::hash password)
               password))"
  in
  check_dval
    "A `Password::hash'd string `Password::check's against itself."
    (exec_ast ast)
    (DBool true)


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
               (let _ (DB::insert (obj (password pw)) Passwords)
                 (let fetched (. (List::head (DB::fetchAll Passwords)) password)
                   (pw fetched))))"
  in
  AT.check
    AT.int
    "A Password::hash'd string can get stored in and retrieved from a user database."
    0
    ( match exec_handler ~ops ast with
    | DList [p1; p2] ->
        compare_dval p1 p2
    | _ ->
        1 )


let t_password_serialization () =
  let does_serialize name expected f =
    let bytes = Bytes.of_string "encryptedbytes" in
    let password = DPassword bytes in
    AT.check
      AT.bool
      ("Passwords serialize in non-redaction function: " ^ name)
      expected
      (String.is_substring
         ~substring:(B64.encode "encryptedbytes")
         (f password))
  in
  let roundtrips name serialize deserialize =
    let bytes = Bytes.of_string "encryptedbytes" in
    let password = DPassword bytes in
    AT.check
      at_dval
      ("Passwords serialize in non-redaction function: " ^ name)
      password
      (password |> serialize |> deserialize |> serialize |> deserialize)
  in
  (* doesn't redact *)
  does_serialize
    "to_internal_roundtrippable_v0"
    true
    Dval.to_internal_roundtrippable_v0 ;
  does_serialize "to_internal_queryable_v0" true Dval.to_internal_queryable_v0 ;
  (* roundtrips *)
  roundtrips
    "to_internal_roundtrippable_v0"
    Dval.to_internal_roundtrippable_v0
    Dval.of_internal_roundtrippable_v0 ;
  roundtrips
    "to_internal_queryable_v0"
    Dval.to_internal_queryable_v0
    Dval.of_internal_roundtrippable_v0 ;
  (* redacting *)
  does_serialize
    "to_enduser_readable_text_v0"
    false
    Dval.to_enduser_readable_text_v0 ;
  does_serialize
    "to_enduser_readable_html_v0"
    false
    Dval.to_enduser_readable_html_v0 ;
  does_serialize "to_developer_repr_v0" false Dval.to_developer_repr_v0 ;
  does_serialize
    "to_pretty_machine_json_v1"
    false
    Dval.to_pretty_machine_json_v1 ;
  does_serialize
    "to_pretty_request_json_v0"
    false
    Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0 ;
  does_serialize
    "to_pretty_response_json_v1"
    false
    Legacy.PrettyResponseJsonV0.to_pretty_response_json_v0 ;
  ()


let t_password_json_round_trip_forwards () =
  let password = DPassword (Bytes.of_string "x") in
  check_dval
    "Passwords serialize and deserialize if there's no redaction."
    password
    ( password
    |> Dval.to_internal_roundtrippable_v0
    |> Dval.of_internal_roundtrippable_v0
    |> Dval.to_internal_roundtrippable_v0
    |> Dval.of_internal_roundtrippable_v0 )


let t_incomplete_propagation () =
  check_dval
    "Fn with incomplete return incomplete"
    DIncomplete
    (exec_ast "(List::head _)") ;
  check_dval
    "Incompletes stripped from lists"
    (DList [DInt 5; DInt 6])
    (exec_ast "(5 6 (List::head _))") ;
  check_dval
    "Blanks stripped from lists"
    (DList [DInt 5; DInt 6])
    (exec_ast "(5 6 _)") ;
  check_dval
    "Blanks stripped from objects"
    (DObj (DvalMap.of_alist_exn [("m", DInt 5); ("n", DInt 6)]))
    (exec_ast "(obj (i _) (m 5) (j (List::head _)) (n 6))") ;
  check_dval
    "incomplete if conds are incomplete"
    DIncomplete
    (exec_ast "(if _ 5 6)") ;
  check_dval
    "blanks in threads are ignored"
    (DInt 8)
    (exec_ast "(| 5 _ (+ 3))") ;
  check_dval
    "incomplete in the middle of a thread is skipped"
    (DInt 8)
    (exec_ast "(| 5 (+ _) (+ 3))") ;
  check_dval
    "incomplete at the end of a thread is skipped"
    (DInt 5)
    (exec_ast "(| 5 (+ _))") ;
  check_dval "empty thread is incomplete" DIncomplete (exec_ast "(|)") ;
  check_dval
    "incomplete obj in field access is incomplete"
    DIncomplete
    (exec_ast "(. (List::head _) field)") ;
  check_dval
    "incomplete name in field access is incomplete"
    DIncomplete
    (exec_ast "(. (obj (i 5)) _)") ;
  ()


let t_html_escaping () =
  check_dval
    "html escaping works"
    (* TODO: add back in check that `'` is correctly escaped. It didn't
     * play nice with our hacky `'` removal in the DSL parser *)
    (Dval.dstr_of_string_exn "test&lt;&gt;&amp;&quot;")
    (exec_ast "(String::htmlEscape 'test<>&\\\"')")


let t_curl_file_urls () =
  AT.check
    (AT.option AT.string)
    "aaa"
    (* Before we limited the protocols for curl, .info.error was "",
       since Httpclient.http_call checked for a 2xx HTTP code. But the file
       contents ended up in the error message. Now we've restricted the URL
       protocols, so we get CURLE_UNSUPPORTED_PROTOCOL before a request
       is even sent. *)
    (Some "Unsupported protocol")
    ( try
        ignore
          (Httpclient.http_call
             "file://localhost/etc/passwd"
             []
             Httpclient.GET
             []
             "") ;
        None
      with
    | Exception.DarkException i ->
        List.Assoc.find i.info ~equal:( = ) "error"
    | _ ->
        None )


let t_authenticate_user () =
  AT.check
    AT.bool
    "Account.authenticate_user works for the test user"
    true
    ( Account.authenticate "test" "fVm2CUePzGKCwoEQQdNJktUQ"
    && (not (Account.authenticate "test_unhashed" "fVm2CUePzGKCwoEQQdNJktUQ"))
    && (not (Account.authenticate "test" "no"))
    && not (Account.authenticate "test_unhashed" "no") )


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
               (let _ (DB::insert (obj (uu i)) Ids)
                 (let fetched (. (List::head (DB::fetchAll Ids)) uu)
                   (i fetched))))"
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


let t_uuid_string_roundtrip () =
  let ast =
    "(let i (Uuid::generate)
               (let s (toString i)
                 (let parsed (String::toUUID s)
                   (i parsed))))"
  in
  AT.check
    AT.int
    "A generated id can round-trip"
    0
    (match exec_ast ast with DList [p1; p2] -> compare_dval p1 p2 | _ -> 1)


let t_should_use_https () =
  AT.check
    (AT.list AT.bool)
    "should_use_https works"
    (List.map
       ~f:(fun x -> Webserver.should_use_https (Uri.of_string x))
       [ "http://builtwithdark.com"
       ; "http://test.builtwithdark.com"
       ; "http://localhost"
       ; "http://test.localhost" ])
    [true; true; false; false]


let t_redirect_to () =
  AT.check
    (AT.list (AT.option AT.string))
    "redirect_to works"
    (List.map
       ~f:(fun x ->
         x
         |> Uri.of_string
         |> Webserver.redirect_to
         |> Option.map ~f:Uri.to_string )
       [ "http://example.com"
       ; "http://builtwithdark.com"
       ; "https://builtwithdark.com"
       ; "http://test.builtwithdark.com"
       ; "https://test.builtwithdark.com"
       ; "http://test.builtwithdark.com/x/y?z=a" ])
    [ None
    ; Some "https://builtwithdark.com"
    ; None
    ; Some "https://test.builtwithdark.com"
    ; None
    ; Some "https://test.builtwithdark.com/x/y?z=a" ]


let t_errorrail_simple () =
  check_dval
    "rail"
    (DErrorRail (DOption OptNothing))
    (exec_ast "(`List::last_v1 [])") ;
  check_dval "no rail" (DOption OptNothing) (exec_ast "(Dict::get_v1 {} 'i')") ;
  check_dval
    "no rail deeply nested"
    (DInt 8)
    (exec_ast
       "(| (5)
                  (`List::head_v1)
                  (+ 3)
                  (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
               )") ;
  check_dval
    "to rail deeply nested"
    (DErrorRail (DOption OptNothing))
    (exec_ast
       "(| ()
                  (`List::head_v1)
                  (+ 3)
                  (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
               )") ;
  ()


let t_errorrail_toplevel () =
  check_dval
    "Errorrail goes to 404"
    (DResp (Response (404, []), Dval.dstr_of_string_exn "Not found"))
    (exec_handler
       "(| ()
                      (`List::head_v1)
                      (+ 3)
                      (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
                    )") ;
  check_dval
    "No errorrail goes to option"
    (DOption OptNothing)
    (exec_handler "(List::head_v1 [])") ;
  ()


let t_errorrail_userfn () =
  check_dval
    "userfn unwraps"
    (DOption OptNothing)
    (exec_userfn
       "(| ()
                     (`List::head_v1)
                     (+ 3)
                     (\\x -> (if (> (+ x 4) 1) x (+ 1 x)))
                   )") ;
  ()


let t_nothing () =
  check_dval "can specifiy nothing" (DOption OptNothing) (exec_ast "nothing") ;
  check_dval
    "nothing works as expected"
    (DBool true)
    (exec_ast "(== (List::head_v1 []) nothing)") ;
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
       let%lwt resp, _ =
         Webserver.authenticate_then_handle
           ~execution_id:test_id
           (fun ~session ~csrf_token req ->
             Webserver.respond ~execution_id:test_id `OK "test handler" )
           req
       in
       let code = resp |> Resp.status |> Code.code_of_status in
       resp
       |> Resp.headers
       |> (fun x -> Header.get x "set-cookie")
       |> (fun x ->
            Option.bind x ~f:(fun sc ->
                let first, params = String.lsplit2_exn ~on:';' sc in
                let name, value = String.lsplit2_exn ~on:'=' first in
                (* make sure some other cookie isn't getting set *)
                if name = "__session"
                then Some (String.lstrip params)
                else None ) )
       |> fun x -> return (code, x))
  in
  AT.check
    (AT.list (AT.pair AT.int (AT.option AT.string)))
    "authenticate_then_handle sets status codes and cookies correctly"
    (List.map
       ~f:ath_cookie
       (* valid basic auth login on darklang.com *)
       [ Req.make
           ~headers:(basic "test" "fVm2CUePzGKCwoEQQdNJktUQ")
           (Uri.of_string "http://darklang.com/a/test")
         (* valid basic auth login on localhost *)
       ; Req.make
           ~headers:(basic "test" "fVm2CUePzGKCwoEQQdNJktUQ")
           (Uri.of_string "http://darklang.localhost/a/test")
         (* invalid basic auth logins *)
       ; Req.make
           ~headers:(basic "test" "")
           (Uri.of_string "http://darklang.com/a/test")
       ; Req.make
           ~headers:(basic "" "fVm2CUePzGKCwoEQQdNJktUQ")
           (Uri.of_string "http://darklang.com/a/test")
         (* plain request, no auth *)
       ; Req.make (Uri.of_string "http://test.builtwithdark.com/a/test") ])
    [ (200, Some "Max-Age=604800; path=/; secure; httponly")
    ; (200, Some "Max-Age=604800; path=/; httponly")
    ; (401, None)
    ; (401, None)
    ; (401, None) ]


let t_check_csrf_then_handle () =
  (* csrf header *)
  let csrf token = Header.of_list [("X-CSRF-Token", token)] in
  let test_session = Lwt_main.run (Auth.Session.new_for_username "test") in
  let correct_token = Auth.Session.csrf_token_for test_session in
  (* sample execution id, makes grepping test logs easier *)
  let test_id = Types.id_of_int 1234 in
  (* Fake URL; this should be url-agnostic *)
  let url = Uri.of_string "http://darklang.com/a/test" in
  let ccth ((username, req) : string * Req.t) : int =
    Lwt_main.run
      (let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let%lwt resp, _ =
         Webserver.check_csrf_then_handle
           ~execution_id:test_id
           ~session:test_session
           (fun req ->
             Webserver.respond ~execution_id:test_id `OK "test handler" )
           req
       in
       resp |> Resp.status |> Code.code_of_status |> return)
  in
  AT.check
    (AT.list AT.int)
    "authenticate_then_handle sets status codes and cookies correctly"
    (List.map
       ~f:ccth
       (* GET works, with no token *)
       [ ("test", Req.make ~meth:`GET url) (* POST works with the right token *)
       ; ("test", Req.make ~headers:(csrf correct_token) ~meth:`POST url)
         (* But not with no token *)
       ; ("test", Req.make ~meth:`POST url) (* And not with the wrong token. *)
       ; ("test", Req.make ~headers:(csrf "x") ~meth:`POST url) ])
    [200; 200; 401; 401]


let admin_handler_code
    ?(meth = `GET) ?(body = "") ?(csrf = true) (username, endpoint) =
  (* sample execution id, makes grepping test logs easier *)
  let test_id = Types.id_of_int 1234 in
  let session = Lwt_main.run (Auth.Session.new_for_username username) in
  Lwt_main.run
    (let stop, stopper = Lwt.wait () in
     let uri =
       Uri.of_string ("http://builtwithdark.localhost:8000" ^ endpoint)
     in
     let headers =
       Header.of_list
         ( if csrf
         then [("X-CSRF-Token", Auth.Session.csrf_token_for session)]
         else [] )
     in
     let%lwt () = Nocrypto_entropy_lwt.initialize () in
     let%lwt resp, _ =
       Webserver.admin_handler
         ~execution_id:test_id
         ~uri
         ~stopper
         ~body
         ~session
         ~csrf_token:(Auth.Session.csrf_token_for session)
         (Req.make ~meth ~headers uri)
     in
     resp |> Resp.status |> Code.code_of_status |> return)


let t_admin_handler_ui () =
  let ah_ui_response (username, canvas) =
    admin_handler_code (username, "/a/" ^ canvas ^ "/")
  in
  AT.check
    (AT.list AT.int)
    "UI routes in admin_handler check authorization correctly."
    (List.map
       ~f:ah_ui_response
       [ ("test", "test") (* everyone can edit sample *)
       ; ("test", "sample") (* a la dabblefox *)
       ; ("test", "test-something")
         (* arbitrary canvas belonging to another user *)
       ; ("test", "test_admin") (* admin can look at test *)
       ; ("test_admin", "test") ])
    [200; 200; 200; 401; 200]


let t_admin_handler_api () =
  let ah_api_response (username, endpoint, body) =
    admin_handler_code ~meth:`POST ~body (username, endpoint)
  in
  AT.check
    (AT.list AT.int)
    "/api/ routes in admin_handler check authorization correctly."
    (List.map
       ~f:ah_api_response
       [ ("test", "/api/test/initial_load", "")
       ; ("test", "/api/test_admin/initial_load", "") ])
    [200; 401]


let t_db_write_deprecated_read_new () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  (* DID and DUUID deliberately do not unify, but we don't want to break
   * the contract that the old DB functions return DID, so we have to stringify *)
  let ast =
    "(let old (DB::insert (obj (x 'foo')) MyDB)
              (let stringified_id (toString (. old id))
               (let new (`DB::get_v1 stringified_id MyDB)
                (let mutated_new (assoc new 'id' stringified_id)
                 (let mutated_old (assoc old 'id' stringified_id)
                  (== mutated_new mutated_old))))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_read_deprecated_write_new_duuid () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let new_write (DB::set_v1 (obj (x 'foo')) (toString (Uuid::generate)) MyDB)
               (let old_read (DB::fetchOneBy 'foo' 'x' MyDB)
                 (let mutated_old_read (dissoc old_read 'id')
                   ((== new_write mutated_old_read) (. old_read id)))))"
  in
  let result = exec_handler ~ops ast in
  AT.check
    AT.int
    "Deprecated reads from an object with a new write give expected value and right type"
    0
    ( match result with
    | DList [DBool true; a] when Dval.tipe_of a = TID ->
        0
    | _ ->
        1 )


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


let t_db_deprecated_belongs_to_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.CreateDB (dbid2, pos, "SecondDB")
    ; Op.AddDBCol (dbid2, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid2, colnameid2, "y")
    ; Op.SetDBColType (dbid2, coltypeid2, "Int")
    ; Op.AddDBCol (dbid, colnameid3, coltypeid3)
    ; Op.SetDBColName (dbid, colnameid3, "relation")
    ; Op.SetDBColType (dbid, coltypeid3, "SecondDB") ]
  in
  let ast =
    "(let oldin (DB::insert (obj (x 'foo') (relation (obj (y 4)))) MyDB)
               (List::head (DB::fetchAll MyDB)))"
  in
  let result = exec_handler ~ops ast in
  AT.check
    AT.int
    "Deprecated BelongsTo works"
    0
    ( match result with
    | DObj o ->
      ( match (DvalMap.find o "x", DvalMap.find o "relation") with
      | Some (DStr s), Some (DObj inner)
        when Unicode_string.equal s (Unicode_string.of_string_exn "foo") ->
        (match DvalMap.find inner "y" with Some (DInt 4) -> 0 | _ -> 1)
      | _ ->
          1 )
    | _ ->
        1 )


let t_db_deprecated_has_many_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str")
    ; Op.CreateDB (dbid2, pos, "SecondDB")
    ; Op.AddDBCol (dbid2, colnameid2, coltypeid2)
    ; Op.SetDBColName (dbid2, colnameid2, "y")
    ; Op.SetDBColType (dbid2, coltypeid2, "Int")
    ; Op.AddDBCol (dbid, colnameid3, coltypeid3)
    ; Op.SetDBColName (dbid, colnameid3, "relations")
    ; Op.SetDBColType (dbid, coltypeid3, "[SecondDB]") ]
  in
  let ast =
    "(let oldin (DB::insert (obj (x 'foo') (relations ((obj (y 4)) (obj (y 6))))) MyDB)
               (List::head (DB::fetchAll MyDB)))"
  in
  let result = exec_handler ~ops ast in
  AT.check
    AT.int
    "Deprecated HasMany works"
    0
    ( match result with
    | DObj o ->
      ( match (DvalMap.find o "x", DvalMap.find o "relations") with
      | Some (DStr s), Some (DList inners)
        when Unicode_string.equal s (Unicode_string.of_string_exn "foo") ->
        ( match inners with
        | [DObj fst; DObj snd] ->
          ( try
              let sorted_list =
                List.sort
                  ~compare:compare_dval
                  [DvalMap.find_exn fst "y"; DvalMap.find_exn snd "y"]
              in
              match sorted_list with [DInt 4; DInt 6] -> 0 | _ -> 1
            with e -> 1 )
        | _ ->
            1 )
      | _ ->
          1 )
    | _ ->
        1 )


let t_db_deprecated_fetch_by_works () =
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
    "(let one (DB::insert (obj (x 'foo') (sort_by 0)) MyDB)
              (let two (DB::insert (obj (x 'bar') (sort_by 1)) MyDB)
               (let three (DB::insert (obj (x 'bar') (sort_by 2)) MyDB)
                (let fetched (List::sortBy (DB::fetchBy 'bar' 'x' MyDB) (\\x -> (. x sort_by)))
                (== (two three) fetched)))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_deprecated_fetch_by_id_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::insert (obj (x 'foo')) MyDB)
              (let fetched (DB::fetchOneBy (. one id) 'id' MyDB)
                (== one fetched)))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_get_many_works () =
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
               (let fetched (DB::getMany_v1 ('first' 'second') MyDB)
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


let t_db_deprecated_delete_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::insert (obj (x 'foo')) MyDB)
              (let fetched (DB::delete one MyDB)
               (== 0 (List::length (DB::fetchAll MyDB)))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


let t_db_deprecated_update_works () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "MyDB")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "x")
    ; Op.SetDBColType (dbid, coltypeid, "Str") ]
  in
  let ast =
    "(let one (DB::insert (obj (x 'foo')) MyDB)
              (let update (DB::update (assoc one 'x' 'bar') MyDB)
               (== 1 (List::length (DB::fetchAll MyDB)))))"
  in
  check_dval "equal_after_roundtrip" (DBool true) (exec_handler ~ops ast)


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


let t_feature_flags_work () =
  check_dval "flag shows new for true" (DInt 1) (exec_ast "(flag _ true 2 1)") ;
  check_dval
    "flag shows old for false"
    (DInt 2)
    (exec_ast "(flag _ false 2 1)") ;
  check_dval
    "flag shows old for incomplete cond"
    (DInt 2)
    (exec_ast "(flag _ _ 2 1)") ;
  check_dval "flag shows old for null" (DInt 2) (exec_ast "(flag _ null 2 1)") ;
  check_dval
    "flag shows old for error"
    (DInt 2)
    (exec_ast "(flag _ (List::head) 2 1)") ;
  check_dval
    "flag shows old for errorrail"
    (DInt 2)
    (exec_ast "(flag _ (`List::head []) 2 1)") ;
  check_dval
    "flag shows old for object"
    (DInt 2)
    (exec_ast "(flag _ (obj (x true)) 2 1)") ;
  check_dval "flag shows old for list" (DInt 2) (exec_ast "(flag _ [] 2 1)") ;
  ()


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


let t_db_create_with_orblank_name () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDBWithBlankOr (dbid, pos, nameid, "Books")
    ; Op.AddDBCol (dbid, colnameid, coltypeid) ]
  in
  let _, state, _ = test_execution_data ops in
  AT.check AT.bool "database is created" true (state.dbs <> [])


let t_db_rename () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDBWithBlankOr (dbid, pos, nameid, "ElmCode")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.RenameDBname (dbid, "BsCode") ]
  in
  let _, state, _ = test_execution_data ops in
  match List.hd state.dbs with
  | Some db ->
      let newname =
        match db.name with Filled (_, name) -> name | Blank _ -> ""
      in
      AT.check AT.string "database rename success" "BsCode" newname
  | None ->
      AT.check AT.bool "fail to rename database" true false


let t_dark_internal_fns_are_internal () =
  let ast = "(DarkInternal::checkAccess)" in
  let check_access canvas_name =
    match exec_ast ~canvas_name ast with DError _ -> None | dval -> Some dval
  in
  AT.check
    (AT.list (AT.option at_dval))
    "DarkInternal:: functions are internal."
    [check_access "test"; check_access "test_admin"]
    [None; Some DNull]


let t_parsed_request_cookies () =
  let with_headers h =
    Parsed_request.from_request h [] ""
    |> Parsed_request.to_dval
    |> fun v ->
    match v with
    | DObj o ->
        DvalMap.find_exn o "cookies"
    | _ ->
        failwith "didn't end up with 'cookies' in the DObj"
  in
  let with_cookies c = with_headers [("cookie", c)] in
  AT.check
    (AT.list at_dval)
    "Parsed_request.from_request parses cookies correctly."
    [ with_headers []
    ; with_cookies ""
    ; with_cookies "a"
    ; with_cookies "a="
    ; with_cookies "a=b"
    ; with_cookies "a=b;"
    ; with_cookies "a=b; c=d"
    ; with_cookies "a=b; c=d;" ]
    [ Dval.to_dobj_exn []
    ; Dval.to_dobj_exn []
    ; Dval.to_dobj_exn []
    ; Dval.to_dobj_exn [("a", Dval.dstr_of_string_exn "")]
    ; Dval.to_dobj_exn [("a", Dval.dstr_of_string_exn "b")]
    ; Dval.to_dobj_exn [("a", Dval.dstr_of_string_exn "b")]
    ; Dval.to_dobj_exn
        [("a", Dval.dstr_of_string_exn "b"); ("c", Dval.dstr_of_string_exn "d")]
    ; Dval.to_dobj_exn
        [("a", Dval.dstr_of_string_exn "b"); ("c", Dval.dstr_of_string_exn "d")]
    ]


let t_ascii_string_literal_validates_as_utf8 () =
  AT.check
    AT.int
    "ASCII string validates as UTF-8"
    0
    (match Dval.dstr_of_string "foobar" with Some _ -> 0 | _ -> 1)


let t_unicode_replacement_character_utf8_byte_seq_validates_as_utf8 () =
  AT.check
    AT.int
    "Replacement character utf8 multi-byte sequence validates"
    0
    (match Dval.dstr_of_string "\xef\xbf\xbd" with Some _ -> 0 | _ -> 1)


let t_family_emoji_utf8_byte_seq_validates_as_utf8 () =
  AT.check
    AT.int
    "Emoji utf8 multi-byte sequence validates"
    0
    (match Dval.dstr_of_string "\xf0\x9f\x91\xaa" with Some _ -> 0 | _ -> 1)


let t_family_emoji_utf16_byte_seq_fails_validation () =
  AT.check
    AT.int
    "UTF16 representation of family emoji does not validate"
    0
    (match Dval.dstr_of_string "\xd8\x3d\xdc\x6A" with Some _ -> 1 | _ -> 0)


let t_mix_of_ascii_and_utf16_fails_validation () =
  AT.check
    AT.int
    "Mix of valid ASCII followed by a UTF16 byte sequence fails validation"
    0
    ( match Dval.dstr_of_string "hello, \xd8\x3d\xdc\x6A" with
    | Some _ ->
        1
    | _ ->
        0 )


let t_u0000_fails_validation () =
  AT.check
    AT.int
    "String containing U+0000/0x00 fails to validate (due to Postgres quirks)"
    0
    (match Dval.dstr_of_string "hello, \x00" with Some _ -> 1 | _ -> 0)


let t_string_length_v1_works_on_emoji () =
  check_dval
    "stringLength"
    (exec_ast "(String::length_v1 '\xef\xbf\xbd')")
    (DInt 1)


let t_string_uppercase_works_for_ascii_range () =
  check_dval
    "stringUppercaseASCII"
    (exec_ast "(String::toUppercase_v1 'abcdef')")
    (Dval.dstr_of_string_exn "ABCDEF")


let t_string_lowercase_works_for_ascii_range () =
  check_dval
    "stringLowercaseASCII"
    (exec_ast "(String::toLowercase_v1 'ABCDEF')")
    (Dval.dstr_of_string_exn "abcdef")


let t_string_uppercase_v1_works_on_mixed_strings () =
  check_dval
    "stringUpppercaseMixed"
    (exec_ast "(String::toUppercase_v1 'hello\xf0\x9f\x98\x84world')")
    (Dval.dstr_of_string_exn "HELLO\xf0\x9f\x98\x84WORLD")


let t_string_uppercase_v1_works_on_non_ascii_strings () =
  check_dval
    "stringUpppercaseMixed"
    (exec_ast "(String::toUppercase_v1 'żółw')")
    (Dval.dstr_of_string_exn "ŻÓŁW")


let t_string_split_works_for_emoji () =
  check_dval
    "stringSplit"
    (exec_ast "(String::split 'hello\xf0\x9f\x98\x84world' '\xf0\x9f\x98\x84')")
    (DList [Dval.dstr_of_string_exn "hello"; Dval.dstr_of_string_exn "world"])


let t_sanitize_uri_path_with_repeated_slashes () =
  AT.check
    AT.string
    "/foo//bar->/foo/bar"
    (Webserver.sanitize_uri_path "/foo//bar")
    "/foo/bar"


let t_sanitize_uri_path_with_trailing_slash () =
  AT.check AT.string "/foo/->/foo" (Webserver.sanitize_uri_path "/foo/") "/foo"


let t_sanitize_uri_path_with_root_noops () =
  AT.check AT.string "/->/" (Webserver.sanitize_uri_path "/") "/"


let t_sanitize_uri_path_with_repeated_root () =
  AT.check AT.string "//->/" (Webserver.sanitize_uri_path "//") "/"


let t_route_variables_work () =
  AT.check
    (AT.list AT.string)
    "Variables are as expected"
    ["userid"; "cardid"]
    (Http.route_variables "/user/:userid/card/:cardid") ;
  AT.check
    (AT.list (AT.pair AT.string at_dval))
    "Variables are bound as expected"
    [ ("userid", Dval.dstr_of_string_exn "myid")
    ; ("cardid", Dval.dstr_of_string_exn "0") ]
    (Http.bind_route_variables_exn
       "/user/myid/card/0"
       ~route:"/user/:userid/card/:cardid") ;
  AT.check
    AT.bool
    "Path matches the route"
    true
    (Http.request_path_matches_route
       "/user/myid/card/0"
       ~route:"/user/:userid/card/:cardid") ;
  AT.check
    AT.bool
    "Path doesnt match erroneously"
    false
    (Http.request_path_matches_route
       "/api/create-token"
       ~route:"/api/create_token")


let t_route_variables_work_with_stored_events () =
  (* set up test *)
  clear_test_data () ;
  let host = "test-route_variables_works" in
  let oplist = [Op.SetHandler (tlid, pos, http_route_handler ())] in
  let c = Canvas.init host oplist in
  Canvas.serialize_only [tlid] !c ;
  let t1 = Util.create_uuid () in
  let desc = ("HTTP", http_request_path, "GET") in
  let route = ("HTTP", http_route, "GET") in
  (* store an event and check it comes out *)
  ignore
    (SE.store_event
       ~canvas_id:!c.id
       ~trace_id:t1
       desc
       (Dval.dstr_of_string_exn "1")) ;
  (* check we get back the path for a route with a variable in it *)
  let loaded1 = SE.load_events ~canvas_id:!c.id route in
  check_dval_list
    "load GET events"
    [Dval.dstr_of_string_exn "1"]
    (loaded1 |> List.map ~f:t4_get4th) ;
  AT.check
    (AT.list AT.string)
    "path returned correctly"
    (loaded1 |> List.map ~f:t4_get1st)
    [http_request_path] ;
  (* check that the event is not in the 404s *)
  let f404s = Analysis.get_404s ~since:Time.epoch !c in
  AT.check (AT.list (AT.of_pp Stored_event.pp_four_oh_four)) "no 404s" [] f404s ;
  ()


let t_route_variables_work_with_stored_events_and_wildcards () =
  (* set up test *)
  clear_test_data () ;
  let host = "test-route_variables_works_with_wildcards" in
  let route = "/api/create_token" in
  let request_path = "/api/create-token" in
  (* note hyphen vs undeerscore *)
  let oplist = [Op.SetHandler (tlid, pos, http_route_handler ~route ())] in
  let c = Canvas.init host oplist in
  Canvas.serialize_only [tlid] !c ;
  let t1 = Util.create_uuid () in
  let desc = ("HTTP", request_path, "GET") in
  let route = ("HTTP", route, "GET") in
  (* store an event and check it comes out *)
  ignore
    (SE.store_event
       ~canvas_id:!c.id
       ~trace_id:t1
       desc
       (Dval.dstr_of_string_exn "1")) ;
  (* check we get back the path for a route with a variable in it *)
  let loaded1 = SE.load_events ~canvas_id:!c.id route in
  check_dval_list "load GET events" [] (loaded1 |> List.map ~f:t4_get4th) ;
  ()


let unicode_string_tester = AT.testable Unicode_string.pp Unicode_string.equal

let t_unicode_string_reverse_works_with_emojis () =
  let s1 = Unicode_string.of_string_exn "hello\xf0\x9f\x98\x84world" in
  let expected = Unicode_string.of_string_exn "dlrow\xf0\x9f\x98\x84olleh" in
  AT.check
    unicode_string_tester
    "emoji_reverse"
    expected
    (Unicode_string.rev s1)


let t_unicode_string_length_works_with_emojis () =
  let s1 = Unicode_string.of_string_exn "hello\xf0\x9f\x98\x84world" in
  let expected = 11 in
  AT.check AT.int "emoji_length" expected (Unicode_string.length s1)


let t_unicode_string_regex_replace_works_with_emojis () =
  let s1 = Unicode_string.of_string_exn "hello\xf0\x9f\x98\x84world" in
  let pattern = "\xf0\x9f\x98\x84" in
  let replacement = Unicode_string.of_string_exn "FOO" in
  let expected = Unicode_string.of_string_exn "helloFOOworld" in
  AT.check
    unicode_string_tester
    "emoji_regex_replace"
    expected
    (Unicode_string.regexp_replace ~pattern ~replacement s1)


let t_result_to_response_works () =
  let req =
    Req.make
      ~headers:(Header.init ())
      (Uri.of_string "http://test.builtwithdark.com/")
  in
  let req_example_com =
    Req.make
      ~headers:(Header.of_list [("Origin", "https://example.com")])
      (Uri.of_string "http://test.builtwithdark.com/")
  in
  let req_google_com =
    Req.make
      ~headers:(Header.of_list [("Origin", "https://google.com")])
      (Uri.of_string "http://test.builtwithdark.com/")
  in
  let c = Canvas.load_all "test" [] in
  ignore
    (List.map
       ~f:(fun (dval, req, cors_setting, check) ->
         Canvas.update_cors_setting c cors_setting ;
         dval
         |> Webserver.result_to_response ~c ~execution_id ~req
         |> Lwt_main.run
         |> fst
         |> check )
       [ ( exec_ast "(obj)"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "objects get application/json content-type"
               (Some "application/json; charset=utf-8")
               (Header.get (Resp.headers r) "Content-Type") )
       ; ( exec_ast "(1 2)"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "lists get application/json content-type"
               (Some "application/json; charset=utf-8")
               (Header.get (Resp.headers r) "Content-Type") )
       ; ( exec_ast "2"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "other things get text/plain content-type"
               (Some "text/plain; charset=utf-8")
               (Header.get (Resp.headers r) "Content-Type") )
       ; ( exec_ast "(Http::success (obj))"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "Http::success gets application/json"
               (Some "application/json; charset=utf-8")
               (Header.get (Resp.headers r) "Content-Type") )
       ; ( exec_ast "1"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "without any other settings, we get Access-Control-Allow-Origin: *."
               (Some "*")
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
       ; ( exec_ast "1"
         , req
         , Some Canvas.AllOrigins
         , fun r ->
             AT.check
               (AT.option AT.string)
               "with explicit wildcard setting, we get Access-Control-Allow-Origin: *."
               (Some "*")
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
       ; ( exec_ast "1"
         , req
         , Some (Canvas.Origins ["https://example.com"])
         , fun r ->
             AT.check
               (AT.option AT.string)
               "with whitelist setting and no Origin, we get no Access-Control-Allow-Origin"
               None
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
       ; ( exec_ast "1"
         , req_example_com
         , Some (Canvas.Origins ["https://example.com"])
         , fun r ->
             AT.check
               (AT.option AT.string)
               "with whitelist setting and matching Origin, we get good Access-Control-Allow-Origin"
               (Some "https://example.com")
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
       ; ( exec_ast "1"
         , req_google_com
         , Some (Canvas.Origins ["https://example.com"])
         , fun r ->
             AT.check
               (AT.option AT.string)
               "with whitelist setting and mismatched Origin, we get null Access-Control-Allow-Origin"
               (Some "null")
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") ) ]) ;
  ()


let t_old_new_dval_reprs () =
  List.iter sample_dvals ~f:(fun (name, dv) ->
      (* AT.check *)
      (*   AT.string *)
      (*   ("old_new_dval check: " ^ name) *)
      (*   (Dval.old_to_internal_repr dv) *)
      (*   (Dval.to_hashable_repr dv) ; *)
      () ) ;
  ()


let t_trace_data_json_format_redacts_passwords () =
  let id = fid () in
  let trace_data : Analysis_types.trace_data =
    { input = [("event", DPassword (PasswordBytes.of_string "redactme1"))]
    ; timestamp = Time.epoch
    ; function_results =
        [ ( "Password::hash"
          , id
          , "foobar"
          , DPassword (PasswordBytes.of_string "redactme2") ) ] }
  in
  let expected : Analysis_types.trace_data =
    { input = [("event", DPassword (PasswordBytes.of_string "Redacted"))]
    ; timestamp = Time.epoch
    ; function_results =
        [ ( "Password::hash"
          , id
          , "foobar"
          , DPassword (PasswordBytes.of_string "Redacted") ) ] }
  in
  trace_data
  |> Analysis_types.trace_data_to_yojson
  |> Analysis_types.trace_data_of_yojson
  |> Result.ok_or_failwith
  |> AT.check
       (AT.testable
          Analysis_types.pp_trace_data
          Analysis_types.equal_trace_data)
       "trace_data round trip"
       expected


let t_basic_typecheck_works_happy () =
  let args = DvalMap.of_alist_exn [("a", DInt 5); ("b", DInt 4)] in
  let fn = Libs.get_fn_exn ~user_fns:[] "Int::add" in
  let user_tipes = [] in
  AT.check
    AT.bool
    "Basic typecheck succeeds"
    true
    (Type_checker.check_function_call ~user_tipes fn args |> Result.is_ok)


let t_basic_typecheck_works_unhappy () =
  let args = DvalMap.of_alist_exn [("a", DInt 5); ("b", DBool true)] in
  let fn = Libs.get_fn_exn ~user_fns:[] "Int::add" in
  let user_tipes = [] in
  AT.check
    AT.bool
    "Basic typecheck succeeds"
    true
    (Type_checker.check_function_call ~user_tipes fn args |> Result.is_error)


let t_typecheck_any () =
  let args = DvalMap.of_alist_exn [("v", DInt 5)] in
  let fn = Libs.get_fn_exn ~user_fns:[] "toString" in
  let user_tipes = [] in
  AT.check
    AT.bool
    "Typechecking 'Any' succeeds"
    true
    (Type_checker.check_function_call ~user_tipes fn args |> Result.is_ok)


(* ------------------- *)
(* Test setup *)
(* ------------------- *)

let suite =
  [ ("hmac signing works", `Quick, t_hmac_signing)
  ; ("undo", `Quick, t_undo)
  ; ("undo_fns", `Quick, t_undo_fns)
  ; ("int_add_works", `Quick, t_int_add_works)
  ; ("lambda_with_foreach", `Quick, t_lambda_with_foreach)
  ; ("stored_events", `Quick, t_stored_event_roundtrip)
  ; ("event_queue roundtrip", `Quick, t_event_queue_roundtrip)
  ; ("bad ssl cert", `Slow, t_bad_ssl_cert)
  ; ("db binary oplist roundtrip", `Quick, t_db_oplist_roundtrip)
  ; ("http oplist roundtrip", `Quick, t_http_oplist_roundtrip)
  ; ("derror roundtrip", `Quick, t_derror_roundtrip)
  ; ("DB case-insensitive roundtrip", `Quick, t_case_insensitive_db_roundtrip)
  ; ( "Good error when inserting badly"
    , `Quick
    , t_inserting_object_to_missing_col_gives_good_error )
  ; ("Stdlib fns work", `Quick, t_stdlib_works)
  ; ( "Multiple copied of same name don't crash"
    , `Quick
    , t_multiple_copies_of_same_name )
  ; ("Feature flags work", `Quick, t_feature_flags_work)
  ; ("Cron should run sanity", `Quick, t_cron_sanity)
  ; ("Cron just ran", `Quick, t_cron_just_ran)
  ; ( "Roundtrip user_data into jsonb using deprecated funcs"
    , `Quick
    , t_roundtrip_user_data_using_deprecated_functions )
  ; ("Test postgres escaping", `Quick, t_escape_pg_escaping)
  ; ("Nulls allowed in DB", `Quick, t_nulls_allowed_in_db)
  ; ("Nulls for missing column", `Quick, t_nulls_added_to_missing_column)
  ; ( "Parsing JSON to DVals doesn't care about key order"
    , `Quick
    , t_internal_roundtrippable_doesnt_care_about_order )
  ; ( "End-user password hashing and checking works"
    , `Quick
    , t_password_hashing_and_checking_works )
  ; ( "Password hashes can be stored in and retrieved from the DB"
    , `Quick
    , t_password_hash_db_roundtrip )
  ; ( "Passwords serialize correctly and redact (or not) correctly"
    , `Quick
    , t_password_serialization )
  ; ("Incompletes propagate correctly", `Quick, t_incomplete_propagation)
  ; ("HTML escaping works reasonably", `Quick, t_html_escaping)
  ; ("Dark code can't curl file:// urls", `Quick, t_curl_file_urls)
  ; ( "Account.authenticate_user works when it should"
    , `Quick
    , t_authenticate_user )
  ; ("UUIDs round-trip to the DB", `Quick, t_uuid_db_roundtrip)
  ; ("UUIDs round-trip to/from strings", `Quick, t_uuid_string_roundtrip)
  ; ("Webserver.should_use_https works", `Quick, t_should_use_https)
  ; ("Webserver.redirect_to works", `Quick, t_redirect_to)
  ; ("Errorrail simple", `Quick, t_errorrail_simple)
  ; ("Errorrail works in toplevel", `Quick, t_errorrail_toplevel)
  ; ("Errorrail works in user_function", `Quick, t_errorrail_userfn)
  ; ("Handling nothing in code works", `Quick, t_nothing)
  ; ( "authenticate_then_handle sets status codes and cookies correctly "
    , `Quick
    , t_authenticate_then_handle_code_and_cookie )
  ; ( "check_csrf_then_handle checks CSRF authentication correctly  "
    , `Quick
    , t_check_csrf_then_handle )
  ; ("UI routes in admin_handler work ", `Quick, t_admin_handler_ui)
  ; ("/api/ routes in admin_handler work ", `Quick, t_admin_handler_api)
  ; ("New DB code can read old writes", `Quick, t_db_write_deprecated_read_new)
  ; ( "Old DB code can read new writes with UUID key"
    , `Quick
    , t_db_read_deprecated_write_new_duuid )
  ; ("New query function works", `Quick, t_db_new_query_v2_works)
  ; ("DB::set_v1 upserts", `Quick, t_db_set_does_upsert)
  ; ("DB::getAllWithKeys_v1 works", `Quick, t_db_get_all_with_keys_works)
  ; ("Deprecated BelongsTo works", `Quick, t_db_deprecated_belongs_to_works)
  ; ("Deprecated HasMany works", `Quick, t_db_deprecated_has_many_works)
  ; ("Deprecated fetchBy works", `Quick, t_db_deprecated_fetch_by_works)
  ; ( "Deprecated fetchBy works with an id"
    , `Quick
    , t_db_deprecated_fetch_by_id_works )
  ; ("DB::getMany_v1 works", `Quick, t_db_get_many_works)
  ; ( "DB::queryWithKey_v1 works with many items"
    , `Quick
    , t_db_queryWithKey_works_with_many )
  ; ("Deprecated delete works", `Quick, t_db_deprecated_delete_works)
  ; ("Deprecated update works", `Quick, t_db_deprecated_update_works)
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
  ; ("Dvals roundtrip to yojson correctly", `Quick, t_dval_yojson_roundtrips)
  ; ("DB::getAll_v2 works", `Quick, t_db_getAll_v2_works)
  ; ("DB::add works", `Quick, t_db_add_roundtrip)
  ; ( "DarkInternal:: functions are internal"
    , `Quick
    , t_dark_internal_fns_are_internal )
  ; ( "Dval.dstr_of_string validates ASCII as UTF8"
    , `Quick
    , t_ascii_string_literal_validates_as_utf8 )
  ; ( "Dval.dstr_of_string validates replacement character utf8 repr as UTF8"
    , `Quick
    , t_unicode_replacement_character_utf8_byte_seq_validates_as_utf8 )
  ; ( "Dval.dstr_of_string validates utf8 emoji repr as UTF8"
    , `Quick
    , t_family_emoji_utf8_byte_seq_validates_as_utf8 )
  ; ( "Dval.dstr_of_string rejects UTF16 repr of emoji"
    , `Quick
    , t_family_emoji_utf16_byte_seq_fails_validation )
  ; ( "Dval.dstr_of_string rejects mix of ASCII and UTF16"
    , `Quick
    , t_mix_of_ascii_and_utf16_fails_validation )
  ; ("Dval.dstr_of_string rejects 0x00", `Quick, t_u0000_fails_validation)
  ; ( "t_sanitize_uri_path_with_repeated_slashes"
    , `Quick
    , t_sanitize_uri_path_with_repeated_slashes )
  ; ( "t_sanitize_uri_path_with_trailing_slash"
    , `Quick
    , t_sanitize_uri_path_with_trailing_slash )
  ; ( "t_sanitize_uri_path_with_root_noops"
    , `Quick
    , t_sanitize_uri_path_with_root_noops )
  ; ( "t_sanitize_uri_path_with_repeated_root"
    , `Quick
    , t_sanitize_uri_path_with_repeated_root )
  ; ("Route variables work", `Quick, t_route_variables_work)
  ; ( "Route variables work with stored events"
    , `Quick
    , t_route_variables_work_with_stored_events )
  ; ( "Route variables work with stored events and wildcards"
    , `Quick
    , t_route_variables_work_with_stored_events_and_wildcards )
  ; ( "String::length_v2 returns the correct length for a string containing an emoji"
    , `Quick
    , t_string_length_v1_works_on_emoji )
  ; ( "String::toUppercase_v1 works for ASCII range"
    , `Quick
    , t_string_uppercase_works_for_ascii_range )
  ; ( "String::toLowercase_v1 works for ASCII range"
    , `Quick
    , t_string_lowercase_works_for_ascii_range )
  ; ( "String::toUppercase_v1 works on mixed strings"
    , `Quick
    , t_string_uppercase_v1_works_on_mixed_strings )
  ; ( "String::toUppercase_v1 works on non-ascii strings"
    , `Quick
    , t_string_uppercase_v1_works_on_non_ascii_strings )
  ; ( "String split works on strings with emoji + ascii"
    , `Quick
    , t_string_split_works_for_emoji )
  ; ( "Unicode_string.reverse works on strings with emoji + ascii"
    , `Quick
    , t_unicode_string_reverse_works_with_emojis )
  ; ( "Unicode_string.length works for strings with emoji + ascii"
    , `Quick
    , t_unicode_string_length_works_with_emojis )
  ; ( "Unicode_string.regex_replace_works_with_emojis"
    , `Quick
    , t_unicode_string_regex_replace_works_with_emojis )
  ; ( "Can create new DB with Op CreateDBWithBlankOr"
    , `Quick
    , t_db_create_with_orblank_name )
  ; ("Can rename DB with Op RenameDBname", `Quick, t_db_rename)
  ; ( "Dvals get converted to web responses correctly"
    , `Quick
    , t_result_to_response_works )
  ; ( "New dval representations are the same as the old ones"
    , `Quick
    , t_old_new_dval_reprs )
  ; ( "Trace data redacts passwords"
    , `Quick
    , t_trace_data_json_format_redacts_passwords )
  ; ( "Date has correct formats in migration"
    , `Quick
    , date_migration_has_correct_formats )
  ; ( "Basic typechecking works in happy case"
    , `Quick
    , t_basic_typecheck_works_happy )
  ; ( "Basic typechecking works in unhappy case"
    , `Quick
    , t_basic_typecheck_works_unhappy )
  ; ("Type checking supports `Any` in user functions", `Quick, t_typecheck_any)
  ; ( "Loading handler via HTTP router loads user tipes"
    , `Quick
    , t_http_oplist_loads_user_tipes ) ]


let () =
  Libbackend.Init.init ~run_side_effects:true ;
  Log.set_level `All ;
  Account.init_testing () ;
  let wrap f () =
    try f () with e ->
      Exception.reraise_after e (fun bt ->
          print_endline (Exception.to_string e) ;
          print_endline (Exception.backtrace_to_string bt) )
  in
  let wrapped_suite = List.map suite ~f:(fun (n, m, t) -> (n, m, wrap t)) in
  let suite, exit =
    Junit_alcotest.run_and_report "suite" [("tests", wrapped_suite)]
  in
  let report = Junit.make [suite] in
  File.mkdir ~root:Testresults "" ;
  let file =
    File.check_filename ~mode:`Write ~root:Testresults "backend.xml"
  in
  Junit.to_file report file ;
  exit ()
