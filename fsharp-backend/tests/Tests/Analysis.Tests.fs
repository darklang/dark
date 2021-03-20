module Tests.Analysis

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils

open LibExecution.RuntimeTypes
open LibExecution.Shortcuts

module Exe = LibExecution.Execution
module Ast = LibExecution.Ast

module AT = LibExecution.AnalysisTypes

let parse = FSharpToExpr.parseRTExpr

let execSaveDvals
  (dbs : List<DB.T>)
  (userFns : List<UserFunction.T>)
  (ast : Expr)
  : Task<AT.AnalysisResults> =
  task {
    let fns = userFns |> List.map (fun fn -> fn.name, fn) |> Map.ofList
    let dbs = dbs |> List.map (fun db -> db.name, db) |> Map.ofList
    let! state = executionStateFor "test" dbs fns
    let inputVars = Map.empty

    return! Exe.analyseExpr state Exe.loadNoResults Exe.loadNoArguments inputVars ast
  }


let testExecFunctionTLIDs : Test =
  testTask "test that exec function returns the right tlids in the trace" {

    let name = "testFunction"
    let fn = testUserFn name [] (eInt 5)
    let fns = Map.ofList [ (name, fn) ]
    let! state = executionStateFor "test" Map.empty fns

    let! (value, tlids) =
      Exe.executeFunction
        state
        Exe.storeNoResults
        Exe.storeNoArguments
        (gid ())
        []
        name

    Expect.equal tlids [ fn.tlid ] "tlid of function is traced"
    Expect.equal value (DInt 5I) "sanity check"

  }


let testErrorRailUsedInAnalysis : Test =
  testTask "When a function which isn't available on the client has analysis data, we need to make sure we process the errorrail functions correctly" {

    let! state = executionStateFor "test" Map.empty Map.empty
    let loadTraceResults _ _ = Some(DOption(Some(DInt 12345I)), System.DateTime.Now)
    let state = { state with loadFnResult = loadTraceResults }
    let inputVars = Map.empty
    let ast = eFnRail "" "fake_test_fn" 0 [ eInt 4; eInt 5 ]

    let! result = Exe.run state inputVars ast

    Expect.equal result (DInt 12345I) "is on the error rail"
  }
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


let testOtherDbQueryFunctionsHaveAnalysis : Test =
  testTask "The SQL compiler inserts analysis results, but I forgot to support DB:queryOne and friends." {
    let varID = gid ()

    let (db : DB.T) =
      { tlid = gid (); name = "MyDB"; version = 0; cols = [ "age", TInt ] }

    let ast =
      eFn
        "DB"
        "queryOne"
        4
        [ eVar "MyDB"
          eLambda [ "value" ] (eFieldAccess (EVariable(varID, "value")) "age") ]

    let! state = executionStateFor "test" (Map [ "MyDB", db ]) Map.empty
    let state = { state with functions = Map.empty }

    let! results =
      Exe.analyseExpr state Exe.loadNoResults Exe.loadNoArguments Map.empty ast

    Expect.equal
      (Dictionary.tryGetValue varID results)
      (Some(AT.ExecutedResult(DObj(Map.ofList [ "age", DIncomplete SourceNone ]))))
      "Has an age field"
  }


let testListLiterals : Test =
  testTask "Blank in a list evaluates to Incomplete" {
    let id = gid ()
    let ast = eList [ eInt 1; EBlank id ]
    let! (results : AT.AnalysisResults) = execSaveDvals [] [] ast

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
    let! results = execSaveDvals [] [ recurse ] ast

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
    let! results = execSaveDvals [] [] ast

    let check id expected msg =
      Expect.equal (Dictionary.tryGetValue id results) (Some expected) msg

    check ifID (AT.ExecutedResult(DInt 5I)) "if is ok"
    check trueID (AT.ExecutedResult(DInt 5I)) "truebody is ok"
    check falseID (AT.NonExecutedResult(DInt 6I)) "falsebody is ok"
  }


let testMatchEvaluation : Test =
  testTask "test match evaluation" {
    let mid = gid ()
    let pIntId = gid ()
    let pFloatId = gid ()
    let pBoolId = gid ()
    let pStrId = gid ()
    let pNullId = gid ()
    let pBlankId = gid ()
    let pOkVarOkId = gid ()
    let pOkVarVarId = gid ()
    let pOkBlankOkId = gid ()
    let pOkBlankBlankId = gid ()
    let pNothingId = gid ()
    let pVarId = gid ()
    let binopFnValId = gid ()
    let intRhsId = gid ()
    let floatRhsId = gid ()
    let boolRhsId = gid ()
    let strRhsId = gid ()
    let nullRhsId = gid ()
    let blankRhsId = gid ()
    let okVarRhsId = gid ()
    let okBlankRhsId = gid ()
    let nothingRhsId = gid ()
    let okVarRhsVarId = gid ()
    let okVarRhsStrId = gid ()
    let varRhsId = gid ()

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
             EFQFnValue(binopFnValId, FQFnName.stdlibFqName "" "++" 0),
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
        let! results = execSaveDvals [] [] ast
        // check expected values are there
        List.iter
          (fun (id, name, value) ->
            Expect.equal
              (Dictionary.tryGetValue id results)
              (Some value)
              $"{msg}: {id}, {name}")
          expected


        // Check all the other values are there and are NotExecutedResults
        let argIDs = ref []

        arg
        |> Ast.postTraversal
             (fun e ->
               argIDs := (Expr.toID e) :: !argIDs
               e)
        |> ignore

        let expectedIDs = (mid :: !argIDs) @ List.map Tuple3.first expected |> Set
        Expect.isGreaterThan results.Count (Set.count expectedIDs) "sanity check"

        results
        |> Dictionary.keys
        |> Seq.toList
        |> List.iter
             (fun id ->
               if not (Set.contains id expectedIDs) then
                 match Dictionary.tryGetValue id results with
                 | Some (AT.ExecutedResult dv) ->
                     Expect.isTrue
                       false
                       $"{msg}: found unexpected execution result ({id}: {dv})"
                 | None -> Expect.isTrue false "missing value"
                 | Some (AT.NonExecutedResult _) -> ())
      }

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

    do!
      check
        "non match"
        (eInt 6)
        [ (pIntId, "non matching pat", ner (DInt 5I))
          (intRhsId, "non matching rhs", ner (DInt 17I))
          (pFloatId, "float", ner (DFloat 5.6))
          (floatRhsId, "float rhs", ner (DStr "float"))
          (pBoolId, "bool", ner (DBool false))
          (boolRhsId, "bool rhs", ner (DStr "bool"))
          (pNullId, "null", ner DNull)
          (nullRhsId, "null rhs", ner (DStr "null"))
          (pOkVarOkId, "ok pat", ner (inc pOkVarOkId))
          (pOkVarVarId, "var pat", ner (inc pOkVarVarId))
          (okVarRhsId, "ok rhs", ner (inc okVarRhsVarId))
          (okVarRhsVarId, "ok rhs var", ner (inc okVarRhsVarId))
          (okVarRhsStrId, "ok rhs str", ner (DStr "ok: "))
          (pNothingId, "ok pat", ner (DOption None))
          (nothingRhsId, "rhs", ner (DStr "constructor nothing"))
          (pOkBlankOkId, "ok pat", ner (inc pOkBlankOkId))
          (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
          (okBlankRhsId, "rhs", ner (DStr "ok blank"))
          (pVarId, "catch all pat", er (DInt 6I))
          (varRhsId, "catch all rhs", er (DInt 6I)) ]

    do!
      check
        "float"
        (eFloat Positive 5I 6I)
        [ (pFloatId, "pat", er (DFloat 5.6))
          (floatRhsId, "rhs", er (DStr "float")) ]

    do!
      check
        "bool"
        (eBool false)
        [ (pBoolId, "pat", er (DBool false)); (boolRhsId, "rhs", er (DStr "bool")) ]

    do!
      check
        "null"
        (eNull ())
        [ (pNullId, "pat", er DNull); (nullRhsId, "rhs", er (DStr "null")) ]

    do!
      check
        "ok: y"
        (eConstructor "Ok" [ eStr "y" ])
        [ (pOkBlankOkId, "ok pat", ner (inc pOkBlankOkId))
          (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
          (okBlankRhsId, "rhs", ner (DStr "ok blank"))
          (pOkVarOkId, "ok pat", er (DResult(Ok(DStr "y"))))
          (binopFnValId,
           "fnval",
           er (DFnVal(FnName(FQFnName.stdlibFqName "" "++" 0))))
          (pOkVarVarId, "var pat", er (DStr "y"))
          (okVarRhsId, "rhs", er (DStr "ok: y"))
          (okVarRhsVarId, "rhs", er (DStr "y"))
          (okVarRhsStrId, "str", er (DStr "ok: ")) ]

    do!
      check
        "ok: blank"
        (eConstructor "Ok" [ EBlank(gid ()) ])
        [ (pOkBlankOkId, "blank pat", ner (inc pOkBlankOkId))
          (pOkBlankBlankId, "blank pat", ner (inc pOkBlankBlankId))
          (okBlankRhsId, "blank rhs", ner (DStr "ok blank"))
          (pOkVarOkId, "ok pat", ner (inc pOkVarOkId))
          (pOkVarVarId, "var pat", ner (inc pOkVarVarId))
          (okVarRhsId, "rhs", ner (inc okVarRhsVarId))
          (okVarRhsVarId, "rhs var", ner (inc okVarRhsVarId))
          (okVarRhsStrId, "str", ner (DStr "ok: ")) ]

    do!
      check
        "nothing"
        (eConstructor "Nothing" [])
        [ (pNothingId, "ok pat", er (DOption None))
          (nothingRhsId, "rhs", er (DStr "constructor nothing")) ]
  // TODO: test constructor around a literal
  // TODO: constructor around a variable
  // TODO: constructor around a constructor around a value
  }

let tests =
  testList
    "Analysis"
    [ testListLiterals
      testRecursionInEditor
      testIfNotIsEvaluated
      testMatchEvaluation
      testExecFunctionTLIDs
      testErrorRailUsedInAnalysis
      testOtherDbQueryFunctionsHaveAnalysis ]
