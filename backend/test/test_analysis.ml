open Core_kernel
open Libexecution
open Libbackend
open Types
open Types.RuntimeT
open Utils
module AT = Alcotest
module SE = Stored_event

let t_on_the_rail () =
  (* When a function which isn't available on the client has analysis data, we need to make sure we process the errorrail functions correctly.   *)
  clear_test_data () ;
  let prog = ast_for "(`fake_test_fn 4 5)" in
  let id = Ast.blank_to_id prog in
  add_test_fn_result
    (tlid, "fake_test_fn", id)
    [Dval.dint 4; Dval.dint 5]
    (DOption (OptJust (Dval.dint 12345)), Time.now ()) ;
  check_dval
    "is on the error rail"
    (Dval.dint 12345)
    (execute_ops [hop (handler prog)])


let t_test_filter_slash () =
  clear_test_data () ;
  let host = "test-test_filter_slash" in
  let route = "/:rest" in
  let oplist = [Op.SetHandler (tlid, pos, http_route_handler ~route ())] in
  let c = ops2c_exn host oplist in
  Canvas.serialize_only [tlid] !c ;
  let t1 = Util.create_uuid () in
  let desc = ("HTTP", "/", "GET") in
  let at_trace_id = AT.of_pp Uuidm.pp_string in
  ignore
    (SE.store_event
       ~canvas_id:!c.id
       ~trace_id:t1
       desc
       (Dval.dstr_of_string_exn "1")) ;
  let handler = !c.handlers |> Toplevel.handlers |> List.hd_exn in
  let loaded = Analysis.traceids_for_handler !c handler in
  AT.check
    (AT.list at_trace_id)
    "list ids"
    [Uuidm.v5 Uuidm.nil (string_of_id handler.tlid)]
    loaded


let suite =
  [ ("Missing functions still check the rail", `Quick, t_on_the_rail)
  ; ("Filter / from /:rest", `Quick, t_test_filter_slash) ]
