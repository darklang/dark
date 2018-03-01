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
let h () = Ast.Hole (fid ())
let v str = Ast.Value (fid (), str)
let b () = Types.Blank (fid ())

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
  let ast = !c.toplevels
            |> TL.handlers
            |> List.hd_exn
            |> fun h -> h.ast in
  Ast.execute DvalMap.empty ast


let check_dval = AT.check (AT.testable pp_dval (=))
let check_canvas = AT.check (AT.testable C.pp_canvas C.equal_canvas)

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
  let n2 = handler (FnCall (fid (), "-", [h (); h ()])) in
  let n3 = handler (FnCall (fid (), "-", [v "3"; h ()])) in
  let n4 = handler (FnCall (fid (), "-", [v "3"; v "4"])) in
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
  let o1 = Value (fid (), "1") in
  let o2 = Value (fid (), "2") in
  let o3 = Value (fid (), "3") in
  let o4 = Value (fid (), "4") in
  let o5 = Value (fid (), "5") in
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
  let add = FnCall (fid (), "+", [v "5"; v "3"]) in
  let r = execute_ops [handler add] in
  check_dval "int_add" (DInt 8) r


let t_lambda_with_foreach () =
  let ast = FnCall ( fid ()
                   , "String::foreach"
                   , [ Value (fid (), "\"some string\"")
                     ; Lambda (fid ()
                              , ["var"]
                              , FnCall (fid ()
                                       , "Char::toUppercase"
                                       , [Variable (fid (), "var")]))])
  in
  let r = execute_ops [handler ast] in
  check_dval "lambda_wit_foreach" r (DStr "SOME STRING")


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
  ]

let () =
  Exn.initialize_module ();
  Printexc.record_backtrace true;
  AT.run ~argv:[|"--verbose"; "--show-errors"|] "suite" [ "tests", suite ]


