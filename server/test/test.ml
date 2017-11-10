open Core
open Types

module C = Canvas
module Map = Map.Poly
module RT = Runtime
module AT = Alcotest

let handle_exception e =
  (* Builtin testing doesnt seem to print exceptions *)
  let bt = Exn.backtrace () in
  let msg = Exn.to_string e in
  print_endline ("Exception: " ^ msg);
  print_endline bt;
  raise e (* still need to raise so the test doesn't pass *)

(* ----------------------- *)
(* The tests *)
(* ----------------------- *)

(* let t_undo_fns () = *)
(*   let n1 = Op.SavePoint in *)
(*   let n2 = Op.Add_fn_call (fid (), Free, "-") in *)
(*   let n3 = Op.Add_value (fid (), Free, "-86") in *)
(*   let n4 = Op.Set_edge (Op.id_of n3, Op.id_of n2, "b") in *)
(*   let u = Op.Undo in *)
(*   let r = Op.Redo in *)
(*  *)
(*   AT.check AT.int "undocount" *)
(*   3 (G.undo_count !(ops2g "g" [n1; n1; n1; n1; n2; n3; n4; u; u; u])); *)
(*  *)
(*   AT.check AT.bool "redoable" true *)
(*     (G.is_redoable !(ops2g "g" [n1; n2; n3; n4; u])); *)
(*   AT.check AT.bool "undoable" true *)
(*     (G.is_undoable !(ops2g "g" [n1; n2; n3; n4])); *)
(*  *)
(*  *)
(*   AT.check AT.bool "not redoable" false *)
(*     (G.is_redoable !(ops2g "g" [n1; n2; n3; n4; u; r])); *)
(*   AT.check AT.bool "not undoable" false *)
(*     (G.is_undoable !(ops2g "g" [n1; n2; n3; n4; u])); *)
(*  *)
(*  *)
(*   let both = !(ops2g "g" [n1; n1; n2; n3; n4; u; r; u]) in *)
(*   AT.check AT.bool "both_undo" true (G.is_undoable both); *)
(*   AT.check AT.bool "both_redo" true (G.is_redoable both); *)
(*  *)
(*   let neither = !(ops2g "g" [n2; n3; n4]) in *)
(*   AT.check AT.bool "neither_undo" false (G.is_undoable neither); *)
(*   AT.check AT.bool "neither_redo" false (G.is_redoable neither) *)
(*  *)
(* let t_undo () = *)
(*   let n1 = Op.Add_fn_call (fid (), Free, "-") in *)
(*   let u1 = Op.SavePoint in *)
(*   let n2 = Op.Add_value (fid (), Free, "5") in *)
(*   let e1 = Op.Set_edge (Op.id_of n2, Op.id_of n1, "a") in *)
(*   let u2 = Op.SavePoint in *)
(*   let n3 = Op.Add_value (fid (), Free, "3") in *)
(*   let e2 = Op.Set_edge (Op.id_of n3, Op.id_of n1, "b") in *)
(*   let ops1 = [n1; u1; n2; e1; u2; n3; e2] in *)
(*  *)
(*   let u3 = Op.SavePoint in *)
(*   let n4 = Op.Add_value (fid (), Free, "6") in *)
(*   let e3 = Op.Set_edge (Op.id_of n4, Op.id_of n1, "b") in *)
(*   let u4 = Op.SavePoint in *)
(*   let n5 = Op.Add_value (fid (), Free, "-86") in *)
(*   let e4 = Op.Set_edge (Op.id_of n5, Op.id_of n1, "b") in *)
(*   let ops2 = [u3; n4; e3; u4; n5; e4] in *)
(*  *)
(*   let u5 = Op.Undo in *)
(*   let u6 = Op.Undo in *)
(*   let u7 = Op.Redo in *)
(*   let u8 = Op.Redo in *)
(*   let u9 = Op.Undo in *)
(*   let u10 = Op.Redo in *)
(*  *)
(*   (* Check assumptions *) *)
(*   let r = execute_ops ops1 n1 in *)
(*   check_dval "t_undo_1" (DInt 2) r; *)
(*   let r2 = execute_ops (List.append ops1 ops2) n1 in *)
(*   check_dval "t_undo_2" (DInt 91) r2; *)
(*  *)
(*   (* First undo *) *)
(*   let r3 = execute_ops (List.concat [ops1; ops2; [u5]]) n1 in *)
(*   check_dval "t_undo_3" (DInt (-1)) r3; *)
(*  *)
(*   (* Second undo *) *)
(*   let r4 = execute_ops (List.concat [ops1; ops2; [u5;u6]]) n1 in *)
(*   check_dval "t_undo_4" (DInt 2) r4; *)
(*  *)
(*   (* First redo *) *)
(*   let r5 = execute_ops (List.concat [ops1; ops2; [u5;u6;u7]]) n1 in *)
(*   check_dval "t_undo_5" (DInt (-1)) r5; *)
(*  *)
(*   (* Second redo *) *)
(*   let r6 = execute_ops (List.concat [ops1; ops2; [u5;u6;u7;u8]]) n1 in *)
(*   check_dval "t_undo_6" (DInt 91) r6; *)
(*  *)
(*   (* Another undo *) *)
(*   let r7 = execute_ops (List.concat [ops1; ops2; [u5;u6;u7;u8;u9]]) n1 in *)
(*   check_dval "t_undo_7" (DInt (-1)) r7; *)
(*  *)
(*   (* Another redo *) *)
(*   let r8 = execute_ops (List.concat [ops1; ops2; [u5;u6;u7;u8;u9;u10]]) n1 in *)
(*   check_dval "t_undo_8" (DInt 91) r8 *)
(*  *)

(*  *)
(* let t_int_add_works () = *)
(*   (* Couldn't call Int::add *) *)
(*   let add = Op.Add_fn_call (fid (), Free, "Int::add") in *)
(*   let v1 = Op.Add_value (fid (), Free, "5") in *)
(*   let v2 = Op.Add_value (fid (), Free, "3") in *)
(*   let e1 = Op.Set_edge (Op.id_of v2, Op.id_of add, "b") in *)
(*   let e2 = Op.Set_edge (Op.id_of v1, Op.id_of add, "a") in *)
(*   let r = execute_ops [add; v1; v2; e2; e1] add in *)
(*   check_dval "int_add" (DInt 8) r *)
(*  *)

let ops2c (name: string) (ops: Op.op list) : C.canvas ref =
  let c = C.create name in
  C.add_ops c ops;
  c

let check_canvas = AT.check (AT.testable C.pp_canvas C.equal_canvas)

let t_load_save _ =
  let n1 = Op.SetAST { id = 1; pos = { x = 0; y = 0 }; ast = Ast.Value DNull }  in
  let name = "test_load_save" in
  let c = ops2c name [n1] in
  let _ = C.save !c in
  let c1 = C.load name [] in
  let _ = C.save !c in
  let c2 = C.load name [] in
  check_canvas "canvas_load_save_1" !c !c1;
  check_canvas "canvas_load_save_2" !c !c2


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
  let args = RT.DvalMap.of_alist_exn [ (k1, RT.DStr v1)
                                     ; (k2, RT.DStr v2)] in

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
  [
    "roundtrip through saving and loading", `Slow, t_load_save
  ; "hmac signing works", `Slow, t_hmac_signing
    (* This test is broken, see comment in Api.json2op *)
  (* ; "functions with edges work too" >:: t_fns_with_edges *)
  (* ; "undos", `Slow, t_undo *)
  (* ; "undo_fns", `Slow, t_undo_fns *)
  ]

let () =
  Exn.initialize_module ();
  Printexc.record_backtrace true;
  AT.run ~argv:[|"--verbose"; "--show-errors"|] "suite" [ "tests", suite ]


