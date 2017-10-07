open Core
open Types

module G = Graph
module Map = Map.Poly
module RT = Runtime
module AT = Alcotest

let check_dval = AT.check (AT.testable RT.pp_dval RT.equal_dval)
let check_graph = AT.check (AT.testable G.pp_graph G.equal_graph)

let handle_exception e =
  (* Builtin testing doesnt seem to print exceptions *)
  let bt = Exn.backtrace () in
  let msg = Exn.to_string e in
  print_endline ("Exception: " ^ msg);
  print_endline bt;
  raise e (* still need to raise so the test doesn't pass *)


let t_param_order () =
  let node = new Node.func 1 None "-" in
  check_dval
    "param_order"
    (node#execute
       ~scope:RT.Scope.empty
       { getf = (fun g -> node)
       ; get_children = (fun _ -> [])
       ; get_deepest = (fun _ -> [])
       }
       (RT.DvalMap.of_alist_exn [ ("a", RT.DInt 1)
                                ; ("b", RT.DInt 1)]))
    (RT.DInt 0)

let fid () = Util.create_id ()

let ops2g (name: string) (ops: Op.op list) : G.graph ref =
  let g = G.create name in
  G.add_ops g ops;
  g

let execute_ops (ops : Op.op list) (result : Op.op) =
  let g = ops2g "test" ops in
  Node.execute ~scope:RT.Scope.empty (Op.id_of result) (G.gfns !g)


let t_undo_fns () =
  let n1 = Op.SavePoint in
  let n2 = Op.Add_fn_call (fid (), None, "-") in
  let n3 = Op.Add_value (fid (), None, "-86") in
  let n4 = Op.Set_edge (Op.id_of n3, Op.id_of n2, "b") in
  let u = Op.Undo in
  let r = Op.Redo in

  AT.check AT.int "undocount"
  (G.undo_count !(ops2g "g" [n1; n1; n1; n1; n2; n3; n4; u; u; u])) 3;

  AT.check AT.bool "redoable"
  (G.is_redoable !(ops2g "g" [n1; n2; n3; n4; u])) true;
  AT.check AT.bool "undoable"
  (G.is_undoable !(ops2g "g" [n1; n2; n3; n4])) true;


  AT.check AT.bool "not redoable"
  (G.is_redoable !(ops2g "g" [n1; n2; n3; n4; u; r])) false;
  AT.check AT.bool "not undoable"
  (G.is_undoable !(ops2g "g" [n1; n2; n3; n4; u])) false;


  let both = !(ops2g "g" [n1; n1; n2; n3; n4; u; r; u]) in
  AT.check AT.bool "both_undo" (G.is_undoable both) true;
  AT.check AT.bool "both_redo" (G.is_redoable both) true;

  let neither = !(ops2g "g" [n2; n3; n4]) in
  AT.check AT.bool "neither_undo" (G.is_undoable neither) false;
  AT.check AT.bool "neither_redo" (G.is_redoable neither) false

let t_undo () =
  try
  let n1 = Op.Add_fn_call (fid (), None, "-") in
  let u1 = Op.SavePoint in
  let n2 = Op.Add_value (fid (), None, "5") in
  let e1 = Op.Set_edge (Op.id_of n2, Op.id_of n1, "a") in
  let u2 = Op.SavePoint in
  let n3 = Op.Add_value (fid (), None, "3") in
  let e2 = Op.Set_edge (Op.id_of n3, Op.id_of n1, "b") in
  let ops1 = [n1; u1; n2; e1; u2; n3; e2] in

  let u3 = Op.SavePoint in
  let n4 = Op.Add_value (fid (), None, "6") in
  let e3 = Op.Set_edge (Op.id_of n4, Op.id_of n1, "b") in
  let u4 = Op.SavePoint in
  let n5 = Op.Add_value (fid (), None, "-86") in
  let e4 = Op.Set_edge (Op.id_of n5, Op.id_of n1, "b") in
  let ops2 = [u3; n4; e3; u4; n5; e4] in

  let u5 = Op.Undo in
  let u6 = Op.Undo in
  let u7 = Op.Redo in
  let u8 = Op.Redo in
  let u9 = Op.Undo in
  let u10 = Op.Redo in

  (* Check assumptions *)
  let r = execute_ops ops1 n1 in
  check_dval "t_undo_1" r (DInt 2);
  let r2 = execute_ops (List.append ops1 ops2) n1 in
  check_dval "t_undo_2" r2 (DInt 91);

  (* First undo *)
  let r3 = execute_ops (List.concat [ops1; ops2; [u5]]) n1 in
  check_dval "t_undo_3" r3 (DInt (-1));

  (* Second undo *)
  let r4 = execute_ops (List.concat [ops1; ops2; [u5;u6]]) n1 in
  check_dval "t_undo_4" r4 (DInt 2);

  (* First redo *)
  let r5 = execute_ops (List.concat [ops1; ops2; [u5;u6;u7]]) n1 in
  check_dval "t_undo_5" r5 (DInt (-1));

  (* Second redo *)
  let r6 = execute_ops (List.concat [ops1; ops2; [u5;u6;u7;u8]]) n1 in
  check_dval "t_undo_6" r6 (DInt 91);

  (* Another undo *)
  let r7 = execute_ops (List.concat [ops1; ops2; [u5;u6;u7;u8;u9]]) n1 in
  check_dval "t_undo_7" r7 (DInt (-1));

  (* Another redo *)
  let r8 = execute_ops (List.concat [ops1; ops2; [u5;u6;u7;u8;u9;u10]]) n1 in
  check_dval "t_undo_8" r8 (DInt 91)

  with
  | e -> handle_exception e


let t_graph_param_order () =
  (* The specific problem here was that we passed the parameters in the order they were added, rather than matching them to param names. *)
  let add = Op.Add_fn_call (fid (), None, "-") in
  let v1 = Op.Add_value (fid (), None, "5") in
  let v2 = Op.Add_value (fid (), None, "3") in
  let e1 = Op.Set_edge (Op.id_of v2, Op.id_of add, "b") in
  let e2 = Op.Set_edge (Op.id_of v1, Op.id_of add, "a") in
  let r1 = execute_ops [add; v1; v2; e1; e2] add in
  let r2 = execute_ops [add; v1; v2; e2; e1] add in
  check_dval "graph_param_order_1" r2 (DInt 2);
  check_dval "graph_param_order_2" r1 (DInt 2)

(* TODO: test with param_Edge *)
let t_fns_with_edges () =
  let str = string_of_int in
  let v1id = fid () in
  let v2id = fid () in
  let v1 = Op.Add_value (v1id, None, "5") in
  let v2 = Op.Add_value (v2id, None, "3") in
  (* Build this as a string and run it through end-to-end *)
  let p1str = "{receiving_edge: {source: " ^ str v1id ^ "}}" in
  let p2str = "{receiving_edge: {source: " ^ str v2id ^ "}}" in
  let edges = "edges: [" ^ p1str ^ ", " ^ p2str ^ "]" in
  let fncall = "{add_function_call: {name: \"-\", pos: {x: 0, y: 0}, " in
  let fncall = "[" ^ fncall ^ edges ^ "}}]" in
  let ops = Api.to_ops fncall in
  let g = ops2g "test" (v1 :: v2 :: ops) in
  let rid = List.nth_exn !g.ops 2 |> Op.id_of in
  let r = Node.execute ~scope:RT.Scope.empty rid (G.gfns !g) in
  check_dval "t_fns_with_edges" r (DInt 2)


let t_int_add_works () =
  (* Couldn't call Int::add *)
  let add = Op.Add_fn_call (fid (), None, "Int::add") in
  let v1 = Op.Add_value (fid (), None, "5") in
  let v2 = Op.Add_value (fid (), None, "3") in
  let e1 = Op.Set_edge (Op.id_of v2, Op.id_of add, "b") in
  let e2 = Op.Set_edge (Op.id_of v1, Op.id_of add, "a") in
  let r = execute_ops [add; v1; v2; e2; e1] add in
  check_dval "int_add" r (DInt 8)

let t_node_deletion _ =
  (* check the argument gets deleted too *)
  let n1 = Op.Add_fn_call (fid (), None, "-") in
  let n2 = Op.Add_value (fid (), None, "5") in
  let e1 = Op.Set_edge (Op.id_of n2, Op.id_of n1, "a") in
  let d1 = Op.Delete_node (Op.id_of n2) in
  let g = ops2g "graph" [n1; n2; e1; d1] in
  let nodes = G.incoming_nodes (Op.id_of n2) !g in
  AT.check
    (AT.list (AT.pair AT.int AT.string))
    "node_deletion" nodes [];
  try
    let _ = G.to_frontend !g in ()
  with
  | _ -> AT.fail "node deletion threw "




let t_load_save _ =
  let n1 = Op.Add_fn_call (fid (), None, "-") in
  let n2 = Op.Add_value (fid (), None, "5") in
  let n3 = Op.Add_value (fid (), None, "3") in
  let n4 = Op.Add_anon (fid (), None, [], []) in
  let e1 = Op.Set_edge (Op.id_of n3, Op.id_of n1, "b") in
  let e2 = Op.Set_edge (Op.id_of n2, Op.id_of n1, "a") in
  let name = "test_load_save" in
  let g = ops2g name [n1; n2; n3; n4; e1; e2] in
  let _ = G.save !g in
  let g1 = G.load name [] in
  let _ = G.save !g in
  let g2 = G.load name [] in
  check_graph "graph_load_save_1" !g !g1;
  check_graph "graph_load_save_2" !g !g2

let t_lambda_with_foreach () =
  let v = Op.Add_value (fid (), None, "\"some string\"") in
  let fe = Op.Add_fn_call (fid (), None, "String::foreach") in
  let upper = Op.Add_fn_call (fid (), None, "Char::toUppercase") in
  let anon_id = fid () in
  let anon_arg = fid () in
  let anon = Op.Add_anon (anon_id, None, [anon_arg], ["item"]) in
  let e1 = Op.Set_edge (Op.id_of v, Op.id_of fe, "s") in
  let e2 = Op.Set_edge (Op.id_of anon, Op.id_of fe, "f") in
  let e3 = Op.Set_edge (anon_arg, Op.id_of upper, "c") in
  let r = execute_ops [v; fe; upper; anon; e1; e2; e3] fe in
  check_dval "lambda_wit_foreach"  r (DStr "SOME STRING")

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
  [ "param args are in the right order", `Quick, t_param_order
  ; "Calling Int::add", `Slow, t_int_add_works
  ; "graph ordering doesnt break param order", `Slow, t_graph_param_order
  ; "roundtrip through saving and loading", `Slow, t_load_save
  ; "hmac signing works", `Slow, t_hmac_signing
    (* This test is broken, see comment in Api.json2op *)
  (* ; "functions with edges work too" >:: t_fns_with_edges *)
  ; "anon functions work", `Slow, t_lambda_with_foreach
  ; "test_node_deletion", `Slow,t_node_deletion
  ; "undos", `Slow, t_undo
  ; "undo_fns", `Slow, t_undo_fns
  ]

let () =
  Exn.initialize_module ();
  Printexc.record_backtrace true;
  AT.run ~argv:[|"--verbose"; "--show-errors"|] "suite" [ "tests", suite ]


