open OUnit2

module G = Graph
module Map = Core.Map.Poly
module RT = Runtime

let t_param_order _ =
  let node = new Node.func "-" 1 {x=1; y=1} true in
  assert_equal (node#execute
                  (Core.String.Map.of_alist_exn [ ("a", RT.DInt 1)
                                                ; ("b", RT.DInt 1)]))
    (RT.DInt 0)

let fl : Node.loc = {x=0; y=0}

let fid () = Util.create_id ()

let graph_from_ops name ops : G.graph =
  let g = G.create name in
  let g = List.fold_left (fun g op -> G.add_op op g ~strict:true) g ops in
  g

let execute_ops (ops : G.op list) (result : G.op) =
  let g = graph_from_ops "test" ops in
  G.execute (G.id_of result) g

let t_graph_param_order _ =
  (* The specific problem here was that we passed the parameters in the order they were added, rather than matching them to param names. *)
  let add = G.Add_fn_call ("Int::add", fid (), fl) in
  let v1 = G.Add_value ("5", fid (), fl) in
  let v2 = G.Add_value ("3", fid (), fl) in
  let e1 = G.Add_edge (G.id_of v2, G.id_of add, "b") in
  let e2 = G.Add_edge (G.id_of v1, G.id_of add, "a") in
  let r1 = execute_ops [add; v1; v2; e1; e2] add in
  let r2 = execute_ops [add; v1; v2; e2; e1] add in
  assert_equal r1 (DInt 2);
  assert_equal r2 (DInt 2)


let t_int_add_works _ =
  (* Couldn't call Int::add *)
  let add = G.Add_fn_call ("Int::add", fid (), fl) in
  let v1 = G.Add_value ("5", fid (), fl) in
  let v2 = G.Add_value ("3", fid (), fl) in
  let e1 = G.Add_edge (G.id_of v2, G.id_of add, "b") in
  let e2 = G.Add_edge (G.id_of v1, G.id_of add, "a") in
  let r = execute_ops [add; v1; v2; e2; e1] add in
  assert_equal r (DInt 8)

let t_load_save _ =
  let add = G.Add_fn_call ("-", fid (), fl) in
  let v1 = G.Add_value ("5", fid (), fl) in
  let v2 = G.Add_value ("3", fid (), fl) in
  let e1 = G.Add_edge (G.id_of v2, G.id_of add, "b") in
  let e2 = G.Add_edge (G.id_of v1, G.id_of add, "a") in
  let name = "test_load_save" in
  let g = graph_from_ops name [add; v1; v2; e1; e2] in
  let _ = G.save g in
  let g1 = G.load name in
  let _ = G.save g in
  let g2 = G.load name in
  assert (G.equal_graph g g1);
  assert (G.equal_graph g g2)


let suite =
  "suite" >:::
  [ "param args are in the right order" >:: t_param_order
  ; "Calling Int::add" >:: t_int_add_works
  ; "graph ordering doesnt break param order" >:: t_graph_param_order
  ; "roundtrip through saving and loading" >:: t_load_save
  ]

let () =
  run_test_tt_main suite
