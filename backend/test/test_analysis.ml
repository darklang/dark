open Core_kernel
open Libexecution
open Types.RuntimeT
open Utils

let t_on_the_rail () =
  (* When a function which isn't available on the client has analysis data, we need to make sure we process the errorrail functions correctly.   *)
  clear_test_data () ;
  let prog = ast_for "(`fake_test_fn 4 5)" in
  let id = Ast.blank_to_id prog in
  add_test_fn_result
    (tlid, "fake_test_fn", id)
    [DInt 4; DInt 5]
    (DOption (OptJust (DInt 12345)), Time.now ()) ;
  check_dval
    "is on the error rail"
    (DInt 12345)
    (execute_ops [hop (handler prog)])


let suite = [("Missing functions still check the rail", `Quick, t_on_the_rail)]
