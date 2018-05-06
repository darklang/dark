open Core
open Dark
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
let pos = {x=0;y=0}

let ops2c (name: string) (ops: Op.op list) : C.canvas ref =
  let c = C.create name in
  C.add_ops c [] ops;
  c

let execute_ops (ops : Op.op list) : dval =
  let c = ops2c "testing" ops in

  let h = !c.toplevels
          |> TL.handlers
          |> List.hd_exn in
  let dbs = TL.dbs !c.toplevels in
  let dbs_env = Db.dbs_as_exe_env dbs in
  let env = dbs_env in (* enough env to test for now *)
  let state : exec_state =
    { ff = FromUser !c.name
    ; tlid = h.tlid
    ; hostname = !c.name
    ; user_fns = !c.user_functions
    ; exe_fn_ids = [] (* ctx is real, so unnecessary *)
    ; env = env
    ; dbs = TL.dbs !c.toplevels
    ; id = Util.create_id ()
    } in
  Ast.execute state env h.ast
  |> Log.pp ~f:Types.RuntimeT.show_dval "excute_ops result"


let at_dval = AT.of_pp pp_dval
let at_dval_list = AT.list at_dval
let check_dval = AT.check at_dval
let check_oplist = AT.check (AT.of_pp Op.pp_oplist)

let handler ast =
  Op.SetHandler ( tlid
                , {x=5;y=6}
                , { tlid = tlid
                  ; ast = ast
                  ; spec = { module_ = b ()
                           ; name = b ()
                           ; modifier = b ()
                           ; types = { input = b ()
                                     ; output = b () }}})

let check_exception ?(check=(fun _ -> true)) ~(f:unit -> 'a) msg =
  let e =
    try
      let r = f () in
      Log.erroR "result was" r;
      Some "no exception"
    with
    | Exception.DarkException ed ->
      if check ed
      then None
      else
        (Log.erroR "check failed" ed;
        Some "Check failed")
    | e ->
      let bt = Backtrace.Exn.most_recent () in
      let msg = Exn.to_string e in
      print_endline (Backtrace.to_string bt);
      Log.erroR "different exception" msg;
      Some "different exception"
  in AT.check (AT.option AT.string) msg None e



(* ----------------------- *)
(* The tests *)
(* ----------------------- *)

let t_undo_fns () =
  let n1 = Op.Savepoint [tlid] in
  let n2 = handler (fncall ("-", [b (); b ()])) in
  let n3 = handler (fncall ("-", [v "3"; b ()])) in
  let n4 = handler (fncall ("-", [v "3"; v "4"])) in
  let u = Op.UndoTL tlid in
  let r = Op.RedoTL tlid in

  AT.check AT.int "undocount"
  3 (C.undo_count !(ops2c "g" [n1; n1; n1; n1; n2; n3; n4; u; u; u]) tlid);

  AT.check AT.bool "redoable" true
    (C.is_redoable !(ops2c "g" [n1; n2; n3; n4; u]) tlid);
  AT.check AT.bool "undoable" true
    (C.is_undoable !(ops2c "g" [n1; n2; n3; n4]) tlid);


  AT.check AT.bool "not redoable" false
    (C.is_redoable !(ops2c "g" [n1; n2; n3; n4; u; r]) tlid);
  AT.check AT.bool "not undoable" false
    (C.is_undoable !(ops2c "g" [n1; n2; n3; n4; u]) tlid);


  let both = !(ops2c "g" [n1; n1; n2; n3; n4; u; r; u]) in
  AT.check AT.bool "both_undo" true (C.is_undoable both tlid);
  AT.check AT.bool "both_redo" true (C.is_redoable both tlid);

  let neither = !(ops2c "g" [n2; n3; n4]) in
  AT.check AT.bool "neither_undo" false (C.is_undoable neither tlid);
  AT.check AT.bool "neither_redo" false (C.is_redoable neither tlid)

let t_undo () =
  let ha ast = handler ast in
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


let t_int_add_works () =
  (* Couldn't call Int::add *)
  let add = fncall ("+", [v "5"; v "3"]) in
  let r = execute_ops [handler add] in
  check_dval "int_add" (DInt 8) r

let t_inserting_object_to_missing_col_gives_good_error () =
  let createDB = Op.CreateDB (89, pos, "TestDB") in
  let obj = f (Thread [v "{}"; fncall ("assoc", [v "\"col\""; v "{}"])]) in
  let insert = fncall ("DB::insert", [obj; f (Variable "TestDB")]) in
  let f = fun () -> execute_ops [createDB; handler insert] in
  let check = fun (de: Exception.exception_data) ->
    de.short = "Trying to create a relation that doesn't exist" in
  check_exception "should get good error" ~check ~f



let t_derror_roundtrip () =
  let x = DError "test" in
  let converted = x
                |> Dval.dval_to_yojson
                |> Dval.dval_of_yojson
                |> Result.ok_or_failwith in
  check_dval "roundtrip" converted x



let t_db_oplist_roundtrip () =
  let host = "test_db_oplist_roundtrip" in
  let oplist = [ Op.UndoTL tlid
               ; Op.RedoTL tlid
               ; Op.UndoTL tlid
               ; Op.RedoTL tlid] in
  Serialize.save_in_db host oplist;
  match (Serialize.load_from_db host) with
  | Some ops ->
    check_oplist "db_oplist roundtrip" oplist ops
  | None -> AT.fail "nothing in db"


let t_case_insensitive_db_roundtrip () =
  let colname = "cOlUmNnAmE" in
  let value = DStr "some value" in
  let oplist = [ Op.CreateDB (tlid, pos, "TestUnicode")
               ; Op.AddDBCol (tlid, 11, 12)
               ; Op.SetDBColName (tlid, 11, colname)
               ; Op.SetDBColType (tlid, 12, "Str")
               ] in
  let c = ops2c "test_case_insensitive_db_roundtrip" oplist in
  let dbs = TL.dbs !c.toplevels in
  let db = dbs |> List.hd_exn in
  let dval = DvalMap.singleton colname value in
  let _ = Db.delete_all ~tables:dbs db in
  let _ = Db.insert ~tables:dbs db dval in
  let result = Db.fetch_all ~tables:dbs db in
  match result with
  | DList [DObj v] ->
    AT.(check bool) "matched" true
      (List.mem ~equal:(=) (DvalMap.data v) value)
  | other ->
    Log.pP "error" other;
    AT.(check bool) "failed" true false





let t_lambda_with_foreach () =
  let ast = fncall ( "String::foreach"
                   , [ v "\"some string\""
                     ; f (Lambda ( ["var"]
                              , f (FnCall ( "Char::toUppercase"
                                          , [f (Variable "var")]))))])
  in
  let r = execute_ops [handler ast] in
  check_dval "lambda_wit_foreach" r (DStr "SOME STRING")

module SE = Stored_event
let t_stored_event_roundtrip () =
  SE.clear_events "host";
  SE.clear_events "host2";
  let desc1 = ("HTTP", "/path", "GET") in
  let desc2 = ("HTTP", "/path2", "GET") in
  let desc3 = ("HTTP", "/path", "POST") in
  SE.store_event "host" desc1 (DStr "1");
  SE.store_event "host" desc1 (DStr "2");
  SE.store_event "host" desc3 (DStr "3");
  SE.store_event "host" desc2 (DStr "3");
  SE.store_event "host2" desc2 (DStr "3");

  let at_desc = AT.of_pp SE.pp_event_desc in
  let at_dval_list = AT.list at_dval in

  let listed = SE.list_events "host" in
  AT.check
    (AT.list at_desc) "list host events"
    (List.sort ~cmp:compare [desc1; desc2; desc3])
    (List.sort ~cmp:compare listed);

  let loaded1 = SE.load_events "host" desc1 in
  AT.check at_dval_list "load GET events" [DStr "1"; DStr "2"] loaded1;

  let loaded2 = SE.load_events "host" desc3 in
  AT.check at_dval_list "load POST events" [DStr "3"] loaded2;

  let loaded3 = SE.load_events "host2" desc3 in
  AT.check at_dval_list "load no host2 events" [] loaded3;

  let loaded4 = SE.load_events "host2" desc2 in
  AT.check at_dval_list "load host2 events" [DStr "3"] loaded4;

  ()

let t_bad_ssl_cert _ =
  let ast = f (FnCall ( "HttpClient::get"
                   , [ v "\"https://self-signed.badssl.com\""
                     ; v "{}"
                     ; v "{}"
                     ; v "{}"])) in
  check_exception "should get bad_ssl"
    ~f:(fun () -> execute_ops [handler ast])




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


let suite =
  [ "hmac signing works", `Quick, t_hmac_signing
  ; "undo", `Quick, t_undo
  ; "undo_fns", `Quick, t_undo_fns
  ; "int_add_works", `Quick, t_int_add_works
  ; "lambda_with_foreach", `Quick, t_lambda_with_foreach
  ; "stored_events", `Quick, t_stored_event_roundtrip
  ; "bad ssl cert", `Slow, t_bad_ssl_cert
  ; "db oplist roundtrip", `Quick, t_db_oplist_roundtrip
  ; "derror roundtrip", `Quick, t_derror_roundtrip
  ; "DB case-insensitive roundtrip", `Quick, t_case_insensitive_db_roundtrip
  ; "Good error when inserting badly", `Quick,
    t_inserting_object_to_missing_col_gives_good_error
  ]

let () =
  Exn.initialize_module ();
  Printexc.record_backtrace true;
  let (suite, exit) =
    Junit_alcotest.run_and_report "suite" ["tests", suite] in
  let report = Junit.make [suite] in
  let dir = Sys.getenv "DARK_CONFIG_RUN_DIR"
             |> Option.value ~default:"xxx"
             |> fun x -> x ^ "/test_results" in
  Unix.mkdir_p dir;
  let file = dir ^ "/backend.xml" in
  Junit.to_file report file;
  exit ()


