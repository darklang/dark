open OUnit2
open Types

module G = Graph
module Map = Core.Map.Poly
module RT = Runtime

let t_param_order _ =
  let node = new Node.func "-" 1 {x=1; y=1} in
  assert_equal (node#execute
                  (Core.String.Map.of_alist_exn [ ("a", RT.DInt 1)
                                                ; ("b", RT.DInt 1)]))
    (RT.DInt 0)

let fl : loc = {x=0; y=0}

let fid () = Util.create_id ()

let graph_from_ops name ops : G.graph ref =
  let g = G.create name in
  List.iter (fun op -> G.add_op op g) ops;
  g

let execute_ops (ops : Op.op list) (result : Op.op) =
  let g = graph_from_ops "test" ops in
  G.execute (Op.id_of result) !g

let t_graph_param_order _ =
  (* The specific problem here was that we passed the parameters in the order they were added, rather than matching them to param names. *)
  let add = Op.Add_fn_call ("-", fid (), fl) in
  let v1 = Op.Add_value ("5", fid (), fl) in
  let v2 = Op.Add_value ("3", fid (), fl) in
  let e1 = Op.Add_edge (Op.id_of v2, Op.id_of add, "b") in
  let e2 = Op.Add_edge (Op.id_of v1, Op.id_of add, "a") in
  let r1 = execute_ops [add; v1; v2; e1; e2] add in
  let r2 = execute_ops [add; v1; v2; e2; e1] add in
  assert_equal r2 (DInt 2);
  assert_equal r1 (DInt 2)

(* let t_fns_with_edges _ = *)
(*   let v1 = Op.Add_value ("5", fid (), fl) in *)
(*   let v2 = Op.Add_value ("3", fid (), fl) in *)
(*   let add = Op.Add_fn_call ("-", fid (), fl, [Op.id_of v1; Op.id_of v2]) in *)
(*   let r = execute_ops [v1; v2; add] add in *)
(*   assert_equal r (DInt 2) *)



let t_int_add_works _ =
  (* Couldn't call Int::add *)
  let add = Op.Add_fn_call ("Int::add", fid (), fl) in
  let v1 = Op.Add_value ("5", fid (), fl) in
  let v2 = Op.Add_value ("3", fid (), fl) in
  let e1 = Op.Add_edge (Op.id_of v2, Op.id_of add, "b") in
  let e2 = Op.Add_edge (Op.id_of v1, Op.id_of add, "a") in
  let r = execute_ops [add; v1; v2; e2; e1] add in
  assert_equal r (DInt 8)

let t_load_save _ =
  let n1 = Op.Add_fn_call ("-", fid (), fl) in
  let n2 = Op.Add_value ("5", fid (), fl) in
  let n3 = Op.Add_value ("3", fid (), fl) in
  let n4 = Op.Add_anon (fid (), fid (), fl) in
  let e1 = Op.Add_edge (Op.id_of n3, Op.id_of n1, "b") in
  let e2 = Op.Add_edge (Op.id_of n2, Op.id_of n1, "a") in
  let name = "test_load_save" in
  let g = graph_from_ops name [n1; n2; n3; n4; e1; e2] in
  let _ = G.save !g in
  let g1 = G.load name in
  let _ = G.save !g in
  let g2 = G.load name in
  assert (G.equal_graph !g !g1);
  assert (G.equal_graph !g !g2)

let t_lambda_with_foreach _ =
  let v = Op.Add_value ("\"some string\"", fid (), fl) in
  let fe = Op.Add_fn_call ("String::foreach", fid (), fl) in
  let upper = Op.Add_fn_call ("Char::to_uppercase", fid (), fl) in
  let anon_inner = fid () in
  let anon = Op.Add_anon (fid (), anon_inner, fl) in
  let e1 = Op.Add_edge (Op.id_of v, Op.id_of fe, "s") in
  let e2 = Op.Add_edge (Op.id_of anon, Op.id_of fe, "f") in
  let e3 = Op.Add_edge (Op.id_of upper, anon_inner, "return") in
  let e4 = Op.Add_edge (anon_inner, Op.id_of upper, "c") in
  let r = execute_ops [v; fe; upper; anon; e1; e2; e3; e4] fe in
  assert_equal r (DStr "SOME STRING")



let suite =
  "suite" >:::
  [ "param args are in the right order" >:: t_param_order
  ; "Calling Int::add" >:: t_int_add_works
  ; "graph ordering doesnt break param order" >:: t_graph_param_order
  ; "roundtrip through saving and loading" >:: t_load_save
  (* ; "functions with edges work too" >:: t_fns_with_edges *)
  ; "anon functions work" >:: t_lambda_with_foreach
  ]

let () =
  run_test_tt_main suite
