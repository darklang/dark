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
        | ExecutedResult (DObj v) ->
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
      | ExecutedResult (DIncomplete _) ->
          true
      | _ ->
          false)


let t_if_not_executed () =
  let trueid = fid () in
  let falseid = fid () in
  let ifid = fid () in
  let ast =
    Filled
      ( ifid
      , If
          ( f (Value "true")
          , Filled (trueid, Value "5")
          , Filled (falseid, Value "6") ) )
  in
  let dvalStore = exec_save_dvals ast in
  check_execution_result
    "if is ok"
    (IDTable.find_exn dvalStore ifid)
    (ExecutedResult (Dval.dint 5)) ;
  check_execution_result
    "truebody is ok"
    (IDTable.find_exn dvalStore trueid)
    (ExecutedResult (Dval.dint 5)) ;
  check_execution_result
    "elsebody is ok"
    (IDTable.find_exn dvalStore falseid)
    (NonExecutedResult (Dval.dint 6)) ;
  ()


let t_match_evaluation () =
  let gid = Libshared.Shared.gid in
  let open Libshared.FluidShortcuts in
  let mid = gid () in
  let pIntId = gid () in
  let pFloatId = gid () in
  let pBoolId = gid () in
  let pStrId = gid () in
  let pNullId = gid () in
  let pBlankId = gid () in
  let pOkId = gid () in
  let pOkId2 = gid () in
  let pOkVarId = gid () in
  let pOkBlankId = gid () in
  let pNothingId = gid () in
  let pVarId = gid () in
  let intRhsId = gid () in
  let floatRhsId = gid () in
  let boolRhsId = gid () in
  let strRhsId = gid () in
  let nullRhsId = gid () in
  let blankRhsId = gid () in
  let okRhsId = gid () in
  let okRhsId2 = gid () in
  let nothingRhsId = gid () in
  let okRhsVarId = gid () in
  let varRhsId = gid () in
  let astFor (arg : Libshared.FluidExpression.t) =
    match'
      ~id:mid
      arg
      [ (pInt ~mid ~id:pIntId 5, int ~id:intRhsId 17)
      ; (pFloat ~mid ~id:pFloatId "5" "6", str ~id:floatRhsId "float")
      ; (pBool ~mid ~id:pBoolId false, str ~id:boolRhsId "bool")
      ; (pString ~mid ~id:pStrId "myStr", str ~id:strRhsId "str")
      ; (pNull ~mid ~id:pNullId (), str ~id:nullRhsId "null")
      ; (pBlank ~mid ~id:pBlankId (), str ~id:blankRhsId "blank")
      ; ( pConstructor ~mid ~id:pOkId2 "Ok" [pBlank ~mid ~id:pOkBlankId ()]
        , str ~id:okRhsId2 "ok blank" )
      ; ( pConstructor ~mid ~id:pOkId "Ok" [pVar ~mid ~id:pOkVarId "x"]
        , fn ~id:okRhsId "++" [str "ok: "; var ~id:okRhsVarId "x"] )
      ; ( pConstructor ~mid ~id:pNothingId "Nothing" []
        , str ~id:nothingRhsId "constructor nothing" )
      ; (pVar ~mid ~id:pVarId "name", var ~id:varRhsId "name") ]
  in
  let check_match
      (msg : string)
      (arg : Libshared.FluidExpression.t)
      (expected : (id * string * execution_result) list) =
    let ast = astFor arg in
    let dvalStore = exec_save_dvals' ast in
    List.iter expected ~f:(fun (id, name, value) ->
        check_execution_result
          (msg ^ ": " ^ Libshared.FluidExpression.show arg ^ " " ^ name)
          value
          (IDTable.find_exn dvalStore id))
  in
  let er x = ExecutedResult x in
  let ner x = NonExecutedResult x in
  let dstr x = Dval.dstr_of_string_exn x in
  check_match
    "int match"
    (int 5)
    [ (pIntId, "matching pat", er (Dval.dint 5))
    ; (intRhsId, "matching rhs", er (Dval.dint 17))
    ; (pVarId, "2nd matching pat", ner (Dval.dint 5))
    ; (varRhsId, "2nd matching rhs", ner (Dval.dint 5)) ] ;
  check_match
    "non match"
    (int 6)
    [ (pIntId, "non matching pat", ner (Dval.dint 5))
    ; (intRhsId, "non matching rhs", ner (Dval.dint 17))
    ; (pFloatId, "float", ner (DFloat 5.6))
    ; (floatRhsId, "float rhs", ner (dstr "float"))
    ; (pBoolId, "bool", ner (DBool false))
    ; (boolRhsId, "bool rhs", ner (dstr "bool"))
    ; (pNullId, "null", ner DNull)
    ; (nullRhsId, "null rhs", ner (dstr "null"))
    ; (pOkId, "ok pat", ner (DIncomplete (SourceId pOkId)))
      (* TODO: come back to this *)
      (* ; (pOkVarId, "var pat", ner (DIncomplete (SourceId pOkVarId))) *)
    ; (okRhsId, "ok rhs", ner (DIncomplete (SourceId okRhsVarId)))
    ; (okRhsVarId, "ok rhs var", ner (DIncomplete (SourceId okRhsVarId)))
    ; (pNothingId, "ok pat", ner (DOption OptNothing))
    ; (nothingRhsId, "rhs", ner (dstr "constructor nothing"))
    ; (pOkId2, "ok pat", ner (DIncomplete (SourceId pOkId2)))
      (* TODO  *)
      (* ; (pOkBlankId, "blank pat", ner (DIncomplete (SourceId pOkBlankId))) *)
    ; (okRhsId2, "rhs", ner (dstr "ok blank"))
    ; (pVarId, "catch all pat", er (Dval.dint 6))
    ; (varRhsId, "catch all rhs", er (Dval.dint 6)) ] ;
  check_match
    "float"
    (float' 5 6)
    [(pFloatId, "pat", er (DFloat 5.6)); (floatRhsId, "rhs", er (dstr "float"))] ;
  check_match
    "bool"
    (bool false)
    [(pBoolId, "pat", er (DBool false)); (boolRhsId, "rhs", er (dstr "bool"))] ;
  check_match
    "null"
    null
    [(pNullId, "pat", er DNull); (nullRhsId, "rhs", er (dstr "null"))] ;
  check_match
    "ok: x"
    (constructor "Ok" [str "y"])
    [ (pOkId, "ok pat", er (DResult (ResOk (dstr "y"))))
    ; (pOkVarId, "var pat", er (dstr "y"))
    ; (okRhsId, "rhs", er (dstr "ok: y"))
    ; (okRhsVarId, "rhs", er (dstr "y")) ] ;
  check_match
    "nothing"
    (constructor "Nothing" [])
    [ (pNothingId, "ok pat", er (DOption OptNothing))
    ; (nothingRhsId, "rhs", er (dstr "constructor nothing")) ] ;
  check_match
    "ok: blank"
    (constructor "Ok" [str "y"])
    [ (pOkId2, "ok pat", ner (DIncomplete (SourceId pOkId2)))
    ; (pOkBlankId, "blank pat", ner (DIncomplete (SourceId pOkBlankId)))
    ; (okRhsId2, "rhs", ner (dstr "ok blank")) ] ;
  (* constructor around a blank, still pat/expr still has a traxe *)
  (* constructor around a literal, still pat/expr still has a traxe *)
  (* constructor around a variable, still pat/expr still has a traxe *)
  (* constructor around a value, nested twice, pat/expr still has a trace *)
  ()


let suite =
  [ ("Missing functions still check the rail", `Quick, t_on_the_rail)
  ; ("Filter / from /:rest", `Quick, t_test_filter_slash)
  ; ("Analysis on List listerals", `Quick, t_list_literals)
  ; ( "Analysis supports all the DB::query functionns"
    , `Quick
    , t_other_db_query_functions_have_analysis )
  ; ("If branches are evaluated correctly", `Quick, t_if_not_executed)
  ; ("Matches are evaluated correctly", `Quick, t_match_evaluation) ]
