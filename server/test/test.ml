open OUnit2

module G = Graph

let t_param_order _ =
  let node = new Node.func "Int_sub" 1 {x=1; y=1} true in
  assert_equal (node#execute [DInt 1; DInt 1]) (DInt 0)

let fl : Node.loc = {x=0; y=0}

let fid () = Util.create_id ()


let execute_ops (ops : G.op list) (result : G.op) =
  let g = G.create "test" in
  let g = List.fold_left (fun g op -> G.add_op op g ~strict:true) g ops in
  G.execute (G.id_of result) g

let t_graph_param_order _ =
  (* The specific problem here was that we passed the parameters in the order they were added, rather than matching them to param names. *)
  let add = G.Add_fn ("Int_sub", fid (), fl) in
  let v1 = G.Add_value ("5", fid (), fl) in
  let v2 = G.Add_value ("3", fid (), fl) in
  let e1 = G.Add_edge (G.id_of v2, G.id_of add, "b") in
  let e2 = G.Add_edge (G.id_of v1, G.id_of add, "a") in
  let r1 = execute_ops [add; v1; v2; e1; e2] add in
  let r2 = execute_ops [add; v1; v2; e2; e1] add in
  assert_equal r1 (DInt 2);
  assert_equal r2 (DInt 2)


let suite =
  "suite" >:::
  [ "param args are in the right order" >:: t_param_order
  ; "graph ordering doesnt break param order" >:: t_graph_param_order
  ]

let () =
  run_test_tt_main suite
