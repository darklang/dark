module Tests.Analysis

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open TestUtils

open LibExecution.RuntimeTypes
open LibExecution.Shortcuts

module Exe = LibExecution.Execution

module AT = LibExecution.AnalysisTypes

let parse = FSharpToExpr.parseRTExpr

let execSaveDvals
  (userFns : List<UserFunction.T>)
  (ast : Expr)
  : Task<AT.AnalysisResults> =

  task {
    let fns = userFns |> List.map (fun fn -> fn.name, fn) |> Map.ofList
    let! state = executionStateFor "test" Map.empty fns
    let inputVars = Map.empty

    return Exe.analyseExpr state Exe.loadNoResults Exe.loadNoArguments inputVars ast
  }


// let t_trace_tlids_exec_fn () =
//   clear_test_data () ;
//   let value, tlids = exec_userfn_trace_tlids (int 5) in
//   check_dval "sanity check we executed the body" (Dval.dint 5) value ;
//   AT.check (AT.list testable_id) "tlid of function is traced" [tlid] tlids
//
//
// let t_on_the_rail () =
//   (* When a function which isn't available on the client has analysis data, we need to make sure we process the errorrail functions correctly.   *)
//   clear_test_data () ;
//   let prog = fn "fake_test_fn" ~ster:Rail [int 4; int 5] in
//   let id = FluidExpression.toID prog in
//   add_test_fn_result
//     (tlid, "fake_test_fn", id)
//     [Dval.dint 4; Dval.dint 5]
//     (DOption (OptJust (Dval.dint 12345)), Time.now ()) ;
//   check_dval
//     "is on the error rail"
//     (Dval.dint 12345)
//     (execute_ops [hop (handler prog)])
//
//
// let t_test_filter_slash () =
//   clear_test_data () ;
//   let host = "test-test_filter_slash" in
//   let route = "/:rest" in
//   let oplist = [SetHandler (tlid, pos, http_route_handler ~route ())] in
//   let c = ops2c_exn host oplist in
//   Canvas.serialize_only [tlid] !c ;
//   let t1 = Util.create_uuid () in
//   let desc = ("HTTP", "/", "GET") in
//   let at_trace_id = AT.of_pp Uuidm.pp_string in
//   ignore
//     (SE.store_event
//        ~canvas_id:!c.id
//        ~trace_id:t1
//        desc
//        (Dval.dstr_of_string_exn "1")) ;
//   let handler = !c.handlers |> Toplevel.handlers |> List.hd_exn in
//   let loaded = Analysis.traceids_for_handler !c handler in
//   AT.check
//     (AT.list at_trace_id)
//     "list ids"
//     [Uuidm.v5 Uuidm.nil (string_of_id handler.tlid)]
//     loaded
//
//
// (* This test is failing because on the backend, the test is actually available *)
// let t_other_db_query_functions_have_analysis () =
//   let f () =
//     (* The SQL compiler inserts analysis results, but I forgot to support DB:queryOne and friends. *)
//     let dbTLID = fid () in
//     let declID = fid () in
//     let faID = fid () in
//     let lambdaID = fid () in
//     let varID = fid () in
//     let dbID = fid () in
//     let colNameID = fid () in
//     let colTypeID = fid () in
//     let ast =
//       fn
//         "DB::queryOne_v4"
//         [ var ~id:dbID "MyDB"
//         ; ELambda
//             ( lambdaID
//             , [(declID, "value")]
//             , fieldAccess ~id:faID (var ~id:varID "value") "age" ) ]
//     in
//     let ops =
//       [ CreateDB (dbTLID, pos, "MyDB")
//       ; AddDBCol (dbTLID, colNameID, colTypeID)
//       ; SetDBColName (dbTLID, colNameID, "age")
//       ; SetDBColType (dbTLID, colTypeID, "int") ]
//     in
//     let dvalStore = exec_save_dvals ~ops ast in
//     check_condition
//       "Find the age field"
//       (IDTable.find_exn dvalStore varID)
//       ~f:(function
//         | ExecutedResult (DObj v) ->
//             Option.is_some (DvalMap.get ~key:"age" v)
//         | dobj ->
//             false)
//   in
//   Libs.filter_out_non_preview_safe_functions_for_tests ~f ()


let testListLiterals : Test =
  testTask "Blank in a list evaluates to Incomplete" {
    let id = gid ()
    let ast = eList [ eInt 1; EBlank id ]
    let! (results : AT.AnalysisResults) = execSaveDvals [] ast

    return
      match Dictionary.tryGetValue id results with
      | Some (AT.ExecutedResult (DIncomplete _)) -> Expect.isTrue true ""
      | _ -> Expect.isTrue false ""
  }


let testRecursionInEditor : Test =
  testTask "results in recursion" {
    let callerID = gid ()
    let skippedCallerID = gid ()

    let fnExpr =
      (eIf
        (eApply (eStdFnVal "" "<" 0) [ eVar "i"; eInt 1 ])
        (eInt 0)
        (EApply(skippedCallerID, eUserFnVal "recurse", [ eInt 2 ], NotInPipe, NoRail)))

    let recurse = testUserFn "recurse" [ "i" ] fnExpr
    let ast = EApply(callerID, eUserFnVal "recurse", [ eInt 0 ], NotInPipe, NoRail)
    let! results = execSaveDvals [ recurse ] ast

    Expect.equal
      (Dictionary.tryGetValue callerID results)
      (Some(AT.ExecutedResult(DInt 0I)))
      "result is there as expected"

    Expect.equal
      (Dictionary.tryGetValue skippedCallerID results)
      (Some(
        AT.NonExecutedResult(DIncomplete(SourceID(recurse.tlid, skippedCallerID)))
      ))
      "result is incomplete for other path"
  }


let testIfNotIsEvaluated : Test =
  testTask "test if else case is evaluated" {
    let falseID = gid ()
    let trueID = gid ()
    let ifID = gid ()
    let ast = EIf(ifID, eBool true, EInteger(trueID, 5I), EInteger(falseID, 6I))
    let! results = execSaveDvals [] ast

    let check id expected msg =
      Expect.equal (Dictionary.tryGetValue id results) (Some expected) msg

    check ifID (AT.ExecutedResult(DInt 5I)) "if is ok"
    check trueID (AT.ExecutedResult(DInt 5I)) "truebody is ok"
    check falseID (AT.NonExecutedResult(DInt 6I)) "falsebody is ok"
  }


let testMatchEvaluation : Test =
  testTask "test match evaluation" {
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

    let astFor (arg : Expr) =
      EMatch(
        mid,
        arg,
        [ (PInteger(pIntId, 5I), EInteger(intRhsId, 17I))
          (PFloat(pFloatId, 5.6), EString(floatRhsId, "float"))
          (PBool(pBoolId, false), EString(boolRhsId, "bool"))
          (PString(pStrId, "myStr"), EString(strRhsId, "str"))
          (PNull(pNullId), EString(nullRhsId, "null"))
          (PBlank(pBlankId), EString(blankRhsId, "blank"))
          (PConstructor(pOkBlankOkId, "Ok", [ PBlank pOkBlankBlankId ]),
           EString(okBlankRhsId, "ok blank"))
          (PConstructor(pOkVarOkId, "Ok", [ PVariable(pOkVarVarId, "x") ]),
           EApply(
             okVarRhsId,
             eBinopFnVal "++",
             [ EString(okVarRhsStrId, "ok: "); EVariable(okVarRhsVarId, "x") ],
             NotInPipe,
             NoRail
           ))
          (PConstructor(pNothingId, "Nothing", []),
           EString(nothingRhsId, "constructor nothing"))
          (PVariable(pVarId, "name"), EVariable(varRhsId, "name")) ]
      )

    let check
      (msg : string)
      (arg : Expr)
      (expected : List<id * string * AT.ExecutionResult>)
      =
      task {
        let ast = astFor arg
        let! results = execSaveDvals [] ast
        // check expected values are there
        List.iter
          (fun (id, name, value) ->
            Expect.equal
              (Dictionary.tryGetValue id results)
              (Some value)
              $"{msg}: {id}, {name}")
          expected
      }
    // Check all the other values are there and are NotExecutedResults
//     let argIDs = E.filterMap arg ~f:(fun e -> Some (E.toID e)) in
//     let ids = (mid :: argIDs) @ List.map expected ~f:Tc.Tuple3.first in
//     IDTable.iteri dvalStore ~f:(fun ~key ~data ->
//         if Tc.List.member ~value:key ids
//         then ()
//         else
//           match data with
//           | ExecutedResult dval ->
//               AT.fail
//                 ( msg
//                 ^ ": found unexected executed result ("
//                 ^ string_of_id key
//                 ^ "): "
//                 ^ Dval.show dval )
//           | NonExecutedResult _ ->
//               ()) ;
//     ()
//   in
    let er x = AT.ExecutedResult x in

    let ner x = AT.NonExecutedResult x in
    let inc iid = DIncomplete(SourceID(id 7, iid)) in

    do!
      check
        "int match"
        (eInt 5)
        [ (pIntId, "matching pat", er (DInt 5I))
          (intRhsId, "matching rhs", er (DInt 17I))
          (pVarId, "2nd matching pat", ner (DInt 5I))
          (varRhsId, "2nd matching rhs", ner (DInt 5I)) ]
  }
//   check_match
//     "non match"
//     (int 6)
//     [ (pIntId, "non matching pat", ner (Dval.dint 5))
//     ; (intRhsId, "non matching rhs", ner (Dval.dint 17))
//     ; (pFloatId, "float", ner (DFloat 5.6))
//     ; (floatRhsId, "float rhs", ner (dstr "float"))
//     ; (pBoolId, "bool", ner (DBool false))
//     ; (boolRhsId, "bool rhs", ner (dstr "bool"))
//     ; (pNullId, "null", ner DNull)
//     ; (nullRhsId, "null rhs", ner (dstr "null"))
//     ; (pOkVarOkId, "ok pat", ner (inc pOkVarOkId))
//     ; (pOkVarVarId, "var pat", ner (inc pOkVarVarId))
//     ; (okVarRhsId, "ok rhs", ner (inc okVarRhsVarId))
//     ; (okVarRhsVarId, "ok rhs var", ner (inc okVarRhsVarId))
//     ; (okVarRhsStrId, "ok rhs str", ner (dstr "ok: "))
//     ; (pNothingId, "ok pat", ner (DOption OptNothing))
//     ; (nothingRhsId, "rhs", ner (dstr "constructor nothing"))
//     ; (pOkBlankOkId, "ok pat", ner (inc pOkBlankOkId))
//     ; (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
//     ; (okBlankRhsId, "rhs", ner (dstr "ok blank"))
//     ; (pVarId, "catch all pat", er (Dval.dint 6))
//     ; (varRhsId, "catch all rhs", er (Dval.dint 6)) ] ;
//   check_match
//     "float"
//     (float' 5 6)
//     [(pFloatId, "pat", er (DFloat 5.6)); (floatRhsId, "rhs", er (dstr "float"))] ;
//   check_match
//     "bool"
//     (bool false)
//     [(pBoolId, "pat", er (DBool false)); (boolRhsId, "rhs", er (dstr "bool"))] ;
//   check_match
//     "null"
//     null
//     [(pNullId, "pat", er DNull); (nullRhsId, "rhs", er (dstr "null"))] ;
//   check_match
//     "ok: y"
//     (constructor "Ok" [str "y"])
//     [ (pOkBlankOkId, "ok pat", ner (inc pOkBlankOkId))
//     ; (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
//     ; (okBlankRhsId, "rhs", ner (dstr "ok blank"))
//     ; (pOkVarOkId, "ok pat", er (DResult (ResOk (dstr "y"))))
//     ; (pOkVarVarId, "var pat", er (dstr "y"))
//     ; (okVarRhsId, "rhs", er (dstr "ok: y"))
//     ; (okVarRhsVarId, "rhs", er (dstr "y"))
//     ; (okVarRhsStrId, "str", er (dstr "ok: ")) ] ;
//   check_match
//     "ok: blank"
//     (constructor "Ok" [EBlank (gid ())])
//     [ (pOkBlankOkId, "blank pat", ner (inc pOkBlankOkId))
//     ; (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
//     ; (okBlankRhsId, "blank rhs", ner (dstr "ok blank"))
//     ; (pOkVarOkId, "ok pat", ner (inc pOkVarOkId))
//     ; (pOkVarVarId, "var pat", ner (inc pOkVarVarId))
//     ; (okVarRhsId, "rhs", ner (inc okVarRhsVarId))
//     ; (okVarRhsVarId, "rhs var", ner (inc okVarRhsVarId))
//     ; (okVarRhsStrId, "str", ner (dstr "ok: ")) ] ;
//   check_match
//     "nothing"
//     (constructor "Nothing" [])
//     [ (pNothingId, "ok pat", er (DOption OptNothing))
//     ; (nothingRhsId, "rhs", er (dstr "constructor nothing")) ] ;
//   (* TODO: test constructor around a literal *)
//   (* TODO: constructor around a variable *)
//   (* TODO: constructor around a constructor around a value *)
//   ()

let tests =
  testList
    "Analysis"
    [ testListLiterals
      testRecursionInEditor
      testIfNotIsEvaluated
      testMatchEvaluation ]
