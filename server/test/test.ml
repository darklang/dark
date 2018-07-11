open Core_kernel
open Libexecution
open Libbackend

open Types
open Types.RuntimeT
open Ast


module C = Canvas
module RT = Runtime
module TL = Toplevel
module Map = Map.Poly
module AT = Alcotest

let fid = Util.create_id
let v str = Filled (fid (), Value str)
let b () = Types.Blank (fid ())
let f a = Types.Filled (fid (), a)
let fncall (a,b) = f (FnCall (a,b))
let tlid = 7
let dbid = 89
let pos = {x=0;y=0}
let execution_id = 6543

let clear_test_data () : unit =
  let owner = Canvas.owner "test" in
  let canvas = Canvas.fetch_canvas_id owner "test" in
  Db.run ~params:[Uuid canvas] ~name:"clear_events_test_data"
    "DELETE FROM events where canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_stored_events_test_data"
    "DELETE FROM stored_events where canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_function_results_test_data"
    "DELETE FROM function_results where canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_user_data_test_data"
    "DELETE FROM user_data WHERE canvas_id = $1";
  Db.run ~params:[Uuid canvas] ~name:"clear_canvases_test_data"
    "DELETE FROM canvases where id = $1";
  ()



let ops2c (host: string) (ops: Op.op list) : C.canvas ref =
  C.init host ops

let execute_ops (ops : Op.op list) : dval =
  let c = ops2c "test" ops in
  let dbs = TL.dbs !c.toplevels in
  let env = User_db.dbs_as_exe_env dbs in
  let h = !c.toplevels
          |> TL.handlers
          |> List.hd_exn in
  let state = Execution.state_for_execution ~c:!c h.tlid
      ~execution_id ~env
  in
  Ast_analysis.execute_handler state h


let at_dval = AT.testable
    (fun fmt dv -> Fmt.pf fmt "%s" (Dval.to_repr dv))
    (fun a b -> compare_dval a b = 0)
let at_dval_list = AT.list at_dval
let check_dval = AT.check at_dval
let check_oplist = AT.check (AT.of_pp Op.pp_oplist)

let handler ast : Handler.handler =
  { tlid = tlid
  ; ast = ast
  ; spec = { module_ = b ()
           ; name = b ()
           ; modifier = b ()
           ; types = { input = b ()
                     ; output = b () }}}

let http_handler ast : Handler.handler =
  { tlid = tlid
  ; ast = ast
  ; spec = { module_ = f "HTTP"
           ; name = f "/test"
           ; modifier = f "GET"
           ; types = { input = b ()
                     ; output = b () }}}



let daily_cron ast : Handler.handler =
  { tlid = tlid
  ; ast = ast
  ; spec = { module_ = f "CRON"
           ; name = f "test"
           ; modifier = f "Daily"
           ; types = { input = b ()
                     ; output = b () }}}

let hop h =
  Op.SetHandler (tlid, pos, h)

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

let is_fn name =
  (* quick hack *)
  name = "+"
  || name = "-"
  || name = "=="
  || name = "dissoc"
  || name = "assoc"
  || String.is_substring ~substring:"::" name

let rec ast_for_ (sexp : Sexp.t) : expr =
  match sexp with
  (* fncall *)
  | Sexp.List (Sexp.Atom fnname :: args) when is_fn fnname ->
    f (FnCall (fnname, (List.map args ~f:ast_for_)))

  (* blocks *)
  | Sexp.List (Sexp.Atom fnname :: Sexp.Atom "->" :: [body])
    when String.is_prefix ~prefix:"\\" fnname ->
    let var = String.lstrip ~drop:((=) '\\') fnname in
    f (Lambda ([f var], (ast_for_ body)))

  (* let *)
  | Sexp.List [Sexp.Atom "let"; Sexp.Atom var; value; body] ->
    f (Let (f var, ast_for_ value, ast_for_ body))

  (* objects *)
  | Sexp.List (Sexp.Atom "obj" :: rest) ->
    (let to_pair pair =
       match pair with
       | Sexp.List [Sexp.Atom key; value] ->
         (f key, ast_for_ value )
       | x ->
         Log.infO "pair" ~data:(Log.dump pair);
         failwith "invalid"
     in
     let args = List.map ~f:to_pair rest in
     f (ObjectLiteral args))

  (* lists *)
  | Sexp.List args ->
    f (ListLiteral (List.map ~f:ast_for_ args))

  (* blanks *)
  | Sexp.Atom "_" -> b()

  (* literals / variables *)
  | Sexp.Atom value ->
    if int_of_string_opt value = None
    && float_of_string_opt value = None
    && value <> "{}"
    && value <> "[]"
    && not (String.Caseless.equal value "null")
    && not (String.is_prefix ~prefix:"\"" value)
    then
      f (Variable value)
    else
      f (Value value)

let ast_for (ast: string) : expr =
  let quotes = (Re2.Regex.create_exn "'(.*)'") in
  ast
  |> (fun s ->
    (* dunno whether it's a bug or just annoying, but we need to specify
     * quotes as \"\\\", so let's "'" instead. (The ocaml parser demands
     * we insert a " here smdh) *)
      Re2.Regex.replace_exn quotes s
        ~f:(fun m ->
            "\"\\\""
            ^ (Re2.Regex.Match.get_exn ~sub:(`Index 1) m)
            ^ "\\\"\""))
  |> Sexp.of_string
  (* |> (fun s -> *)
  (*       let b = Buffer.create 16000 in *)
  (*       Sexp.to_buffer_hum b s; *)
  (*       Log.pP "buf:" (Buffer.contents b); *)
  (*       s *)
     (* ) *)
  |> ast_for_
  (* |> Log.pp ~f:show_expr "expr" *)

let execute (prog: string) : dval =
  prog
  |> ast_for
  (* |> Log.pp ~f:show_expr *)
  |> handler
  |> hop
  |> fun h -> execute_ops [h]



(* ----------------------- *)
(* The tests *)
(* ----------------------- *)

let t_undo_fns () =
  clear_test_data ();
  let n1 = Op.Savepoint [tlid] in
  let n2 = hop (handler (ast_for "(- _ _)")) in
  let n3 = hop (handler (ast_for "(- 3 _)")) in
  let n4 = hop (handler (ast_for "(- 3 4)")) in
  let u = Op.UndoTL tlid in
  let r = Op.RedoTL tlid in

  AT.check AT.int "undocount"
  3 (Undo.undo_count !(ops2c "test" [n1; n1; n1; n1; n2; n3; n4; u; u; u]).ops tlid);

  AT.check AT.bool "redoable" true
    (Undo.is_redoable !(ops2c "test" [n1; n2; n3; n4; u]).ops tlid);
  AT.check AT.bool "undoable" true
    (Undo.is_undoable !(ops2c "test" [n1; n2; n3; n4]).ops tlid);


  AT.check AT.bool "not redoable" false
    (Undo.is_redoable !(ops2c "test" [n1; n2; n3; n4; u; r]).ops tlid);
  AT.check AT.bool "not undoable" false
    (Undo.is_undoable !(ops2c "test" [n1; n2; n3; n4; u]).ops tlid);


  let both = !(ops2c "test" [n1; n1; n2; n3; n4; u; r; u]).ops in
  AT.check AT.bool "both_undo" true (Undo.is_undoable both tlid);
  AT.check AT.bool "both_redo" true (Undo.is_redoable both tlid);

  let neither = !(ops2c "test" [n2; n3; n4]).ops in
  AT.check AT.bool "neither_undo" false (Undo.is_undoable neither tlid);
  AT.check AT.bool "neither_redo" false (Undo.is_redoable neither tlid)

let t_undo () =
  clear_test_data ();
  let ha ast = hop (handler ast) in
  let sp = Op.Savepoint [tlid] in
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
  let createDB = Op.CreateDB (dbid, pos, "TestDB") in
  let insert = ast_for "(DB::insert (obj (col (obj))) TestDB)" in
  let f = fun () -> execute_ops [createDB; hop (handler insert)] in
  let check = fun (de: Exception.exception_data) ->
    de.short = "Found but did not expect: [col]" in
  check_exception "should get good error" ~check ~f

let t_int_add_works () =
  (* Couldn't call Int::add *)
  check_dval "int_add" (DInt 8) (execute "(+ 5 3)")

let t_stdlib_works () =
  check_dval "uniqueBy"
    (execute "(List::uniqueBy (1 2 3 4) (\\x -> (Int::divide x 2)))")
    (DList [DInt 1; DInt 3; DInt 4]);
  check_dval "uniqueBy"
    (execute "(List::uniqueBy (1 2 3 4) (\\x -> x))")
    (DList [DInt 1; DInt 2; DInt 3; DInt 4]);
  ()


let t_derror_roundtrip () =
  let x = DError "test" in
  let converted = x
                |> Dval.dval_to_yojson
                |> Dval.dval_of_yojson
                |> Result.ok_or_failwith in
  check_dval "roundtrip" converted x



let t_db_oplist_roundtrip () =
  clear_test_data ();
  let host = "test_db_oplist_roundtrip" in
  let oplist = [ Op.UndoTL tlid
               ; Op.RedoTL tlid
               ; Op.UndoTL tlid
               ; Op.RedoTL tlid] in
  Serialize.save_binary_to_db host oplist;
  match (Serialize.load_binary_from_db ~digest:Serialize.digest host) with
  | Some ops ->
    check_oplist "db_oplist roundtrip" oplist ops
  | None -> AT.fail "nothing in db"

let t_db_json_oplist_roundtrip () =
  let host = "test_db_json_oplist_roundtrip" in
  let oplist = [ Op.UndoTL tlid
               ; Op.RedoTL tlid
               ; Op.UndoTL tlid
               ; Op.RedoTL tlid] in
  Serialize.save_json_to_db host oplist;
  match (Serialize.load_json_from_db host) with
  | Some ops ->
    check_oplist "db_oplist roundtrip" oplist ops
  | None -> AT.fail "nothing in db"




let t_case_insensitive_db_roundtrip () =
  clear_test_data ();
  let colname = "cOlUmNnAmE" in
  let value = DStr "some value" in
  let ast =
    ast_for
      "(let _
            (DB::insert (obj (cOlUmNnAmE 'some value')) TestUnicode)
            (DB::fetchAll TestUnicode))"
  in
  let oplist = [ Op.CreateDB (dbid, pos, "TestUnicode")
               ; Op.AddDBCol (dbid, 11, 12)
               ; Op.SetDBColName (dbid, 11, colname)
               ; Op.SetDBColType (dbid, 12, "Str")
               ; hop (handler ast) ]
  in
  match execute_ops oplist with
  | DList [DObj v] ->
    AT.(check bool) "matched" true
      (List.mem ~equal:(=) (DvalMap.data v) value)
  | other ->
    Log.erroR "error" ~data:(Dval.to_repr other);
    AT.(check bool) "failed" true false


let t_lambda_with_foreach () =
  let r = execute
      "(String::foreach 'some string'
         (\\var -> (Char::toUppercase var)))"
  in
  check_dval "lambda_wit_foreach" r (DStr "SOME STRING")

module SE = Stored_event
let t_stored_event_roundtrip () =
  clear_test_data ();
  let owner : Uuidm.t = Account.owner ~auth_domain:"test"
                       |> fun x -> Option.value_exn x in
  let id1 = Canvas.fetch_canvas_id owner "host" in
  let id2 = Canvas.fetch_canvas_id owner "host2" in
  SE.clear_events id1;
  SE.clear_events id2;
  let desc1 = ("HTTP", "/path", "GET") in
  let desc2 = ("HTTP", "/path2", "GET") in
  let desc3 = ("HTTP", "/path", "POST") in
  SE.store_event id1 desc1 (DStr "1");
  SE.store_event id1 desc1 (DStr "2");
  SE.store_event id1 desc3 (DStr "3");
  SE.store_event id1 desc2 (DStr "3");
  SE.store_event id2 desc2 (DStr "3");

  let at_desc = AT.of_pp SE.pp_event_desc in
  let at_dval_list = AT.list at_dval in

  let listed = SE.list_events id1 in
  AT.check
    (AT.list at_desc) "list host events"
    (List.sort ~cmp:compare [desc1; desc2; desc3])
    (List.sort ~cmp:compare listed);

  let loaded1 = SE.load_events id1 desc1 in
  AT.check at_dval_list "load GET events" [DStr "1"; DStr "2"] loaded1;

  let loaded2 = SE.load_events id1 desc3 in
  AT.check at_dval_list "load POST events" [DStr "3"] loaded2;

  let loaded3 = SE.load_events id2 desc3 in
  AT.check at_dval_list "load no host2 events" [] loaded3;

  let loaded4 = SE.load_events id2 desc2 in
  AT.check at_dval_list "load host2 events" [DStr "3"] loaded4;

  ()

module EQ = Event_queue
let t_event_queue_roundtrip () =
  clear_test_data ();
  let dval = DInt 345 in
  let exec_id = 147 in
  let space = "TEST_SPACE" in
  let name = "test_name" in
  let c = ops2c "test-event_queue" [] in
  let state = Execution.state_for_execution ~c:!c tlid
      ~execution_id ~env:DvalMap.empty
  in
  EQ.enqueue state space name dval;
  let v =
    EQ.dequeue ~canvas:!c.id ~account:!c.owner exec_id space name
    |> fun x -> Option.value_exn x
  in

  AT.check at_dval "v" v.value dval;

  ()

let t_bad_ssl_cert _ =
  check_exception "should get bad_ssl"
    ~f:(fun () ->
        execute
          "(HttpClient::get 'https://self-signed.badssl.com' {} {} {})")


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
  let handler = !c.toplevels |> TL.handlers |> List.hd_exn in
  let should_run =
    Cron.should_execute !c.id handler
  in
  AT.check AT.bool "should_run should be true" should_run true;
  ()

let t_cron_just_ran () =
  clear_test_data ();
  let h = daily_cron (ast_for "(+ 5 3)") in
  let c = ops2c "test-cron_works" [hop h] in
  let handler = !c.toplevels |> TL.handlers |> List.hd_exn in
  Cron.record_execution !c.id handler;
  let should_run =
    Cron.should_execute !c.id handler
  in
  AT.check AT.bool "should_run should be false" should_run false;
  ()


let t_roundtrip_user_data () =
  clear_test_data ();
  let ast =
    ast_for "(let v 'lasd;04mr'
        (let old (DB::insert (obj (x v)) MyDB)
        (let new (DB::fetchOneBy v 'x' MyDB)
        (== old new))))"
  in
  let oplist = [ Op.CreateDB (dbid, pos, "MyDB")
               ; Op.AddDBCol (dbid, 11, 12)
               ; Op.SetDBColName (dbid, 11, "x")
               ; Op.SetDBColType (dbid, 12, "Str")
               ; hop (handler ast)
               ] in
  check_dval "equal_after_roundtrip"
    (execute_ops oplist)
    (DBool true)

let t_escape_pg_escaping () =
  AT.check AT.string "no quotes" "asdd" (Db.escape_single "asdd");
  AT.check AT.string "single" "as''dd" (Db.escape_single "as'dd");
  AT.check AT.string "double" "as\"dd" (Db.escape_single "as\"dd");
  AT.check AT.string "no quotes" "asdd" (Db.escape_double "asdd");
  AT.check AT.string "single" "as'dd" (Db.escape_double "as'dd");
  AT.check AT.string "double" "as\\\"dd" (Db.escape_double "as\"dd");
  ()

let t_nulls_allowed_in_db () =
  let ast =
    ast_for "(let old (DB::insert (obj (x null)) MyDB)
               (let new (DB::fetchOneBy null 'x' MyDB)
                 (== old new)))"
  in
  let oplist = [ Op.CreateDB (dbid, pos, "MyDB")
               ; Op.AddDBCol (dbid, 11, 12)
               ; Op.SetDBColName (dbid, 11, "x")
               ; Op.SetDBColType (dbid, 12, "Str")
               ; hop (handler ast)
               ] in
  check_dval "equal_after_roundtrip"
    (execute_ops oplist)
    (DBool true)

let t_nulls_added_to_missing_column () =
  (* Test for the hack that columns get null in all rows to start *)
  clear_test_data ();
  let ast = ast_for "(DB::insert (obj (x 'v')) MyDB)"
  in
  let oplist = [ Op.CreateDB (dbid, pos, "MyDB")
               ; Op.AddDBCol (dbid, 11, 12)
               ; Op.SetDBColName (dbid, 11, "x")
               ; Op.SetDBColType (dbid, 12, "Str")
               ; hop (handler ast)
               ] in
  ignore (execute_ops oplist);
  let ast = ast_for "(dissoc (List::head (DB::fetchAll MyDB)) 'id')" in
  let oplist = oplist @ [ Op.AddDBCol (dbid, 13, 14)
                        ; Op.SetDBColName (dbid, 13, "y")
                        ; Op.SetDBColType (dbid, 14, "Str")
                        ; hop (handler ast) ] in

  check_dval "equal_after_fetchall" (execute_ops oplist)
    (DObj (DvalMap.of_alist_exn ["x", DStr "v"; "y", DNull]))

let t_analysis_not_empty () =
  clear_test_data ();
  (* in a filled-in HTTP env, there wasn't a default environment, so we
   * got no variables in the analysis. *)
  let h = http_handler (ast_for "_") in
  let c = ops2c "test" [ hop h ] in
  AT.check AT.int "equal_after_roundtrip" 1
    (Analysis.handler_analysis ~exe_fn_ids:[] ~execution_id !c h
     |> Tuple.T2.get2
     |> List.length)


let suite =
  [ "hmac signing works", `Quick, t_hmac_signing
  ; "undo", `Quick, t_undo
  ; "undo_fns", `Quick, t_undo_fns
  ; "int_add_works", `Quick, t_int_add_works
  ; "lambda_with_foreach", `Quick, t_lambda_with_foreach
  ; "stored_events", `Quick, t_stored_event_roundtrip
  ; "event_queue roundtrip", `Quick, t_event_queue_roundtrip
  ; "bad ssl cert", `Slow, t_bad_ssl_cert
  ; "db binary oplist roundtrip", `Quick, t_db_oplist_roundtrip
  ; "db json oplist roundtrip", `Quick, t_db_json_oplist_roundtrip
  ; "derror roundtrip", `Quick, t_derror_roundtrip
  ; "DB case-insensitive roundtrip", `Quick,
    t_case_insensitive_db_roundtrip
  ; "Good error when inserting badly", `Quick,
    t_inserting_object_to_missing_col_gives_good_error
  ; "Stdlib works", `Quick, t_stdlib_works
  ; "Cron should run sanity", `Quick, t_cron_sanity
  ; "Cron just ran", `Quick, t_cron_just_ran
  ; "Roundtrip user_data into jsonb", `Quick, t_roundtrip_user_data
  ; "Test postgres escaping", `Quick, t_escape_pg_escaping
  ; "Nulls allowed in DB", `Quick, t_nulls_allowed_in_db
  ; "Nulls for missing column", `Quick, t_nulls_added_to_missing_column
  ; "Analysis not empty", `Quick, t_analysis_not_empty
  ]

let () =
  Libbackend.Init.init ();
  Log.set_level `All;
  Account.init_testing ();

  let (suite, exit) =
    Junit_alcotest.run_and_report "suite" ["tests", suite] in
  let report = Junit.make [suite] in
  File.mkdir ~root:Testresults "";
  let file = File.check_filename ~mode:`Write ~root:Testresults "backend.xml" in
  Junit.to_file report file;
  exit ()


