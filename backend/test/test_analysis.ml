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


(* This test is failing because on the backend, the test is actually available *)
let t_other_db_query_functions_have_analysis () =
  let f () =
    (* The SQL compiler inserts analysis results, but I forgot to support DB:queryOne and friends. *)
    let dbTLID = fid () in
    let fieldID = fid () in
    let declID = fid () in
    let faID = fid () in
    let lambdaID = fid () in
    let varID = fid () in
    let dbID = fid () in
    let colNameID = fid () in
    let colTypeID = fid () in
    let ast =
      f
        (FnCall
           ( "DB::queryOne_v3"
           , [ Filled (dbID, Variable "MyDB")
             ; Filled
                 ( lambdaID
                 , Lambda
                     ( [Filled (declID, "value")]
                     , Filled
                         ( faID
                         , FieldAccess
                             ( Filled (varID, Variable "value")
                             , Filled (fieldID, "age") ) ) ) ) ] ))
    in
    let ops =
      [ Op.CreateDB (dbTLID, pos, "MyDB")
      ; Op.AddDBCol (dbTLID, colNameID, colTypeID)
      ; Op.SetDBColName (dbTLID, colNameID, "age")
      ; Op.SetDBColType (dbTLID, colTypeID, "int") ]
    in
    let dvalStore = exec_save_dvals ~ops ast in
    check_condition
      "Find the age field"
      (IDTable.find_exn dvalStore varID)
      ~f:(function
        | DObj v ->
            Option.is_some (DvalMap.get ~key:"age" v)
        | dobj ->
            false)
  in
  Libs.filter_out_non_preview_safe_functions_for_tests ~f ()


let t_list_literals () =
  let blankId = fid () in
  let ast = f (ListLiteral [f (Value "1"); Blank blankId]) in
  let dvalStore = exec_save_dvals ast in
  check_condition
    "Blank in a list evaluates to Incomplete"
    (IDTable.find_exn dvalStore blankId)
    ~f:(function
      | DIncomplete _ ->
          true
      | _ ->
          false)


let suite =
  [ ("Missing functions still check the rail", `Quick, t_on_the_rail)
  ; ("Filter / from /:rest", `Quick, t_test_filter_slash)
  ; ("Analysis on List listerals", `Quick, t_list_literals)
  ; ( "Analysis supports all the DB::query functionns"
    , `Quick
    , t_other_db_query_functions_have_analysis ) ]
