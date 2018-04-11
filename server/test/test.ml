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

let handle_exception e =
  (* Builtin testing doesnt seem to print exceptions *)
  let bt = Backtrace.Exn.most_recent () in
  let msg = Exn.to_string e in
  print_endline ("Exception: " ^ msg);
  print_endline (Backtrace.to_string bt);
  raise e (* still need to raise so the test doesn't pass *)


let ops2c (name: string) (ops: Op.op list) : C.canvas ref =
  let c = C.create name in
  C.add_ops c [] ops;
  c

let execute_ops (ops : Op.op list) : dval =
  let c = ops2c "test" ops in
  let h = !c.toplevels
          |> TL.handlers
          |> List.hd_exn in
  let state : Ast.exec_state =
    { ff = (FF.todo "test")
    ; tlid = h.tlid
    ; hostname = !c.name
    ; user_fns = !c.user_functions
    ; exe_fn_ids = []
    ; env = DvalMap.empty} in
  Ast.execute state DvalMap.empty h.ast


let at_dval = AT.of_pp pp_dval
let at_dval_list = AT.list at_dval
let check_dval = AT.check at_dval

let handler ast =
  Op.SetHandler ( 7
                , {x=5;y=6}
                , { tlid = 7
                  ; ast = ast
                  ; spec = { module_ = b ()
                           ; name = b ()
                           ; modifier = b ()
                           ; types = { input = b ()
                                     ; output = b () }}})


(* ----------------------- *)
(* The tests *)
(* ----------------------- *)

let t_undo_fns () =
  let n1 = Op.Savepoint in
  let n2 = handler (f (FnCall ("-", [b (); b ()]))) in
  let n3 = handler (f (FnCall ("-", [v "3"; b ()]))) in
  let n4 = handler (f (FnCall ("-", [v "3"; v "4"]))) in
  let u = Op.Undo in
  let r = Op.Redo in

  AT.check AT.int "undocount"
  3 (C.undo_count !(ops2c "g" [n1; n1; n1; n1; n2; n3; n4; u; u; u]));

  AT.check AT.bool "redoable" true
    (C.is_redoable !(ops2c "g" [n1; n2; n3; n4; u]));
  AT.check AT.bool "undoable" true
    (C.is_undoable !(ops2c "g" [n1; n2; n3; n4]));


  AT.check AT.bool "not redoable" false
    (C.is_redoable !(ops2c "g" [n1; n2; n3; n4; u; r]));
  AT.check AT.bool "not undoable" false
    (C.is_undoable !(ops2c "g" [n1; n2; n3; n4; u]));


  let both = !(ops2c "g" [n1; n1; n2; n3; n4; u; r; u]) in
  AT.check AT.bool "both_undo" true (C.is_undoable both);
  AT.check AT.bool "both_redo" true (C.is_redoable both);

  let neither = !(ops2c "g" [n2; n3; n4]) in
  AT.check AT.bool "neither_undo" false (C.is_undoable neither);
  AT.check AT.bool "neither_redo" false (C.is_redoable neither)

let t_undo () =
  let ha ast = handler ast in
  let sp = Op.Savepoint in
  let u = Op.Undo in
  let r = Op.Redo in
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
  let add = f (FnCall ("+", [v "5"; v "3"])) in
  let r = execute_ops [handler add] in
  check_dval "int_add" (DInt 8) r


let t_lambda_with_foreach () =
  let ast = f (FnCall ( "String::foreach"
                   , [ v "\"some string\""
                     ; f (Lambda ( ["var"]
                              , f (FnCall ( "Char::toUppercase"
                                          , [f (Variable "var")]))))]))
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
    (AT.list at_desc) "list host events" [desc1; desc2; desc3] listed;

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
                     ; v "{}"]))
  in
  let v =
    try
      let _ = execute_ops [handler ast] in
      Some "no exception"
    with
    | Exception.DarkException ed -> None
    | _ -> Some "different exception"
  in AT.check (AT.option AT.string) "should get bad_ssl" v None




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
  Exn.initialize_module ();
  Printexc.record_backtrace true;
  [ "hmac signing works", `Slow, t_hmac_signing
  ; "undo", `Slow, t_undo
  ; "undo_fns", `Slow, t_undo_fns
  ; "int_add_works", `Slow, t_int_add_works
  ; "lambda_with_foreach", `Slow, t_lambda_with_foreach
  ; "stored_events", `Slow, t_stored_event_roundtrip
  ; "bad ssl cert", `Slow, t_bad_ssl_cert
  ]

let () =
  Exn.initialize_module ();
  Printexc.record_backtrace true;
  AT.run
    ~argv:[|"--verbose"; "--show-errors"|]
    "suite" [ "tests", suite ]




