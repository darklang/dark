open Core_kernel
open Libexecution
open Libbackend
open Libshared
open Libshared.FluidShortcuts
open Libcommon
open Types
open Types.RuntimeT
open Utils
module AT = Alcotest
module SE = Stored_event

let t_trace_tlids_exec_fn () =
  clear_test_data () ;
  let value, tlids = exec_userfn_trace_tlids (int 5) in
  check_dval "sanity check we executed the body" (Dval.dint 5) value ;
  AT.check (AT.list testable_id) "tlid of function is traced" [tlid] tlids


let t_on_the_rail () =
  (* When a function which isn't available on the client has analysis data, we need to make sure we process the errorrail functions correctly.   *)
  clear_test_data () ;
  let prog = fn "fake_test_fn" ~ster:Rail [int 4; int 5] in
  let id = FluidExpression.toID prog in
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
    let declID = fid () in
    let faID = fid () in
    let lambdaID = fid () in
    let varID = fid () in
    let dbID = fid () in
    let colNameID = fid () in
    let colTypeID = fid () in
    let ast =
      fn
        "DB::queryOne_v4"
        [ var ~id:dbID "MyDB"
        ; ELambda
            ( lambdaID
            , [(declID, "value")]
            , fieldAccess ~id:faID (var ~id:varID "value") "age" ) ]
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
  let ast = list [int 1; EBlank blankId] in
  let dvalStore = exec_save_dvals ast in
  check_condition
    "Blank in a list evaluates to Incomplete"
    (IDTable.find_exn dvalStore blankId)
    ~f:(function
      | ExecutedResult (DIncomplete _) ->
          true
      | _ ->
          false)


let t_recursion_in_editor () =
  let caller_id = fid () in
  let skipped_caller_id = fid () in
  let open FluidShortcuts in
  let recurse =
    user_fn
      "recurse"
      ["i"]
      (* if it goes down all paths, it'll recurse forever *)
      (if'
         (binop "<" (var "i") (int 1))
         (int 0)
         (fn ~id:skipped_caller_id "recurse" [int 2]))
  in
  let ast = fn ~id:caller_id "recurse" [int 0] in
  let ops = [Op.SetFunction recurse] in
  let dvalStore = exec_save_dvals' ~ops ast in
  check_execution_result
    "result is there as expected"
    (IDTable.find_exn dvalStore caller_id)
    (ExecutedResult (Dval.dint 0)) ;
  check_execution_result
    "result is incomplete for other path"
    (IDTable.find_exn dvalStore skipped_caller_id)
    (NonExecutedResult (DIncomplete (SourceId (id_of_int 7, skipped_caller_id)))) ;
  ()


let t_if_not_executed () =
  let trueid = fid () in
  let falseid = fid () in
  let ifid = fid () in
  let ast = if' ~id:ifid (bool true) (int ~id:trueid 5) (int ~id:falseid 6) in
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
  let gid = Shared.gid in
  let open FluidShortcuts in
  let module E = FluidExpression in
  let mid = gid () in
  let pIntId = gid () in
  let pFloatId = gid () in
  let pBoolId = gid () in
  let pStrId = gid () in
  let pNullId = gid () in
  let pBlankId = gid () in
  let pOkVarOkId = gid () in
  let pOkVarVarId = gid () in
  let pOkBlankOkId = gid () in
  let pOkBlankBlankId = gid () in
  let pNothingId = gid () in
  let pVarId = gid () in
  let intRhsId = gid () in
  let floatRhsId = gid () in
  let boolRhsId = gid () in
  let strRhsId = gid () in
  let nullRhsId = gid () in
  let blankRhsId = gid () in
  let okVarRhsId = gid () in
  let okBlankRhsId = gid () in
  let nothingRhsId = gid () in
  let okVarRhsVarId = gid () in
  let okVarRhsStrId = gid () in
  let varRhsId = gid () in
  let astFor (arg : E.t) =
    match'
      ~id:mid
      arg
      [ (pInt ~mid ~id:pIntId 5, int ~id:intRhsId 17)
      ; (pFloat ~mid ~id:pFloatId "5" "6", str ~id:floatRhsId "float")
      ; (pBool ~mid ~id:pBoolId false, str ~id:boolRhsId "bool")
      ; (pString ~mid ~id:pStrId "myStr", str ~id:strRhsId "str")
      ; (pNull ~mid ~id:pNullId (), str ~id:nullRhsId "null")
      ; (pBlank ~mid ~id:pBlankId (), str ~id:blankRhsId "blank")
      ; ( pConstructor
            ~mid
            ~id:pOkBlankOkId
            "Ok"
            [pBlank ~mid ~id:pOkBlankBlankId ()]
        , str ~id:okBlankRhsId "ok blank" )
      ; ( pConstructor ~mid ~id:pOkVarOkId "Ok" [pVar ~mid ~id:pOkVarVarId "x"]
        , fn
            ~id:okVarRhsId
            "++"
            [str ~id:okVarRhsStrId "ok: "; var ~id:okVarRhsVarId "x"] )
      ; ( pConstructor ~mid ~id:pNothingId "Nothing" []
        , str ~id:nothingRhsId "constructor nothing" )
      ; (pVar ~mid ~id:pVarId "name", var ~id:varRhsId "name") ]
  in
  let check_match
      (msg : string)
      (arg : E.t)
      (expected : (id * string * execution_result) list) =
    let ast = astFor arg in
    Log.inspecT "ast" ~f:E.show ast ;
    let dvalStore = exec_save_dvals' ast in
    (* check expected values *)
    List.iter expected ~f:(fun (id, name, value) ->
        check_execution_result
          (msg ^ "(" ^ string_of_id id ^ "): " ^ E.show arg ^ " " ^ name)
          value
          (IDTable.find_exn dvalStore id)) ;
    let argIDs = E.filterMap arg ~f:(fun e -> Some (E.toID e)) in
    let ids = (mid :: argIDs) @ List.map expected ~f:Tc.Tuple3.first in
    (* Check all the other values are NotExecutedResults *)
    IDTable.iteri dvalStore ~f:(fun ~key ~data ->
        if Tc.List.member ~value:key ids
        then ()
        else
          match data with
          | ExecutedResult dval ->
              AT.fail
                ( msg
                ^ ": found unexected executed result ("
                ^ string_of_id key
                ^ "): "
                ^ Dval.show dval )
          | NonExecutedResult _ ->
              ()) ;
    ()
  in
  let er x = ExecutedResult x in
  let ner x = NonExecutedResult x in
  let dstr x = Dval.dstr_of_string_exn x in
  let inc id = DIncomplete (SourceId (id_of_int 7, id)) in
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
    ; (pOkVarOkId, "ok pat", ner (inc pOkVarOkId))
    ; (pOkVarVarId, "var pat", ner (inc pOkVarVarId))
    ; (okVarRhsId, "ok rhs", ner (inc okVarRhsVarId))
    ; (okVarRhsVarId, "ok rhs var", ner (inc okVarRhsVarId))
    ; (okVarRhsStrId, "ok rhs str", ner (dstr "ok: "))
    ; (pNothingId, "ok pat", ner (DOption OptNothing))
    ; (nothingRhsId, "rhs", ner (dstr "constructor nothing"))
    ; (pOkBlankOkId, "ok pat", ner (inc pOkBlankOkId))
    ; (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
    ; (okBlankRhsId, "rhs", ner (dstr "ok blank"))
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
    "ok: y"
    (constructor "Ok" [str "y"])
    [ (pOkBlankOkId, "ok pat", ner (inc pOkBlankOkId))
    ; (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
    ; (okBlankRhsId, "rhs", ner (dstr "ok blank"))
    ; (pOkVarOkId, "ok pat", er (DResult (ResOk (dstr "y"))))
    ; (pOkVarVarId, "var pat", er (dstr "y"))
    ; (okVarRhsId, "rhs", er (dstr "ok: y"))
    ; (okVarRhsVarId, "rhs", er (dstr "y"))
    ; (okVarRhsStrId, "str", er (dstr "ok: ")) ] ;
  check_match
    "ok: blank"
    (constructor "Ok" [EBlank (gid ())])
    [ (pOkBlankOkId, "blank pat", ner (inc pOkBlankOkId))
    ; (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
    ; (okBlankRhsId, "blank rhs", ner (dstr "ok blank"))
    ; (pOkVarOkId, "ok pat", ner (inc pOkVarOkId))
    ; (pOkVarVarId, "var pat", ner (inc pOkVarVarId))
    ; (okVarRhsId, "rhs", ner (inc okVarRhsVarId))
    ; (okVarRhsVarId, "rhs var", ner (inc okVarRhsVarId))
    ; (okVarRhsStrId, "str", ner (dstr "ok: ")) ] ;
  check_match
    "nothing"
    (constructor "Nothing" [])
    [ (pNothingId, "ok pat", er (DOption OptNothing))
    ; (nothingRhsId, "rhs", er (dstr "constructor nothing")) ] ;
  (* TODO: test constructor around a literal *)
  (* TODO: constructor around a variable *)
  (* TODO: constructor around a constructor around a value *)
  ()


let suite =
  [ ( "Executing user function traces touched tlids"
    , `Quick
    , t_trace_tlids_exec_fn )
  ; ("Recursion only works in editor", `Quick, t_recursion_in_editor)
  ; ("Missing functions still check the rail", `Quick, t_on_the_rail)
  ; ("Filter / from /:rest", `Quick, t_test_filter_slash)
  ; ("Analysis on List listerals", `Quick, t_list_literals)
  ; ( "Analysis supports all the DB::query functions"
    , `Quick
    , t_other_db_query_functions_have_analysis )
  ; ("If branches are evaluated correctly", `Quick, t_if_not_executed)
  ; ("Matches are evaluated correctly", `Quick, t_match_evaluation) ]
