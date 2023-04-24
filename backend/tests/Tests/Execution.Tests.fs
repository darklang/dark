module Tests.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth

open TestUtils.TestUtils
open LibExecution.RuntimeTypes
open TestUtils.RTShortcuts

module Exe = LibExecution.Execution
module RuntimeTypesAst = LibExecution.RuntimeTypesAst

module AT = LibExecution.AnalysisTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

let parse = Parser.RuntimeTypes.parseExpr

let executionStateForPreview
  (name : string)
  (dbs : Map<string, DB.T>)
  (types : Map<FQTypeName.UserTypeName, UserType.T>)
  (fns : Map<FQFnName.UserFnName, UserFunction.T>)
  : Task<AT.AnalysisResults * ExecutionState> =
  task {
    let canvasID = System.Guid.NewGuid()
    let! state = executionStateFor canvasID false dbs types fns
    let results, traceFn = Exe.traceDvals ()

    let state =
      { state with
          tracing =
            { state.tracing with traceDval = traceFn; realOrPreview = Preview } }
    return (results, state)
  }

let execSaveDvals
  (canvasName : string)
  (dbs : List<DB.T>)
  (userTypes : List<UserType.T>)
  (userFns : List<UserFunction.T>)
  (ast : Expr)
  : Task<AT.AnalysisResults> =
  task {
    let types = userTypes |> List.map (fun typ -> typ.name, typ) |> Map.ofList
    let fns = userFns |> List.map (fun fn -> fn.name, fn) |> Map.ofList
    let dbs = dbs |> List.map (fun db -> db.name, db) |> Map.ofList
    let! (results, state) = executionStateForPreview canvasName dbs types fns

    let inputVars = Map.empty
    let! _result = Exe.executeExpr state inputVars ast

    return results
  }


let testExecFunctionTLIDs : Test =
  testTask "test that exec function returns the right tlids in the trace" {
    let! meta = initializeTestCanvas "exec-function-tlids"
    let name = "testFunction"
    let fn =
      testUserFn name [] [] (PT.TVariable "a") (PT.EInt(gid (), 5))
      |> PT2RT.UserFunction.toRT
    let fns = Map.ofList [ (fn.name, fn) ]
    let! state = executionStateFor meta false Map.empty Map.empty fns

    let tlids, traceFn = Exe.traceTLIDs ()

    let state =
      { state with
          tracing =
            { state.tracing with traceTLID = traceFn; realOrPreview = Preview } }

    let! value = Exe.executeFunction state (gid ()) (FQFnName.User fn.name) [] []

    Expect.equal (HashSet.toList tlids) [ fn.tlid ] "tlid of function is traced"
    Expect.equal value (DInt 5L) "sanity check"
  }

// TYPESCLEANUP add tests for non-record-shaped types


let testRecursionInEditor : Test =
  testTask "execution avoids recursion in editor" {
    let callerID = gid ()
    let skippedCallerID = gid ()

    let fnExpr =
      PT.EIf(
        gid (),

        // condition
        PT.EInfix(
          gid (),
          PT.InfixFnCall PT.ComparisonLessThan,
          PT.EVariable(gid (), "i"),
          PT.EInt(gid (), 1)
        ),

        // 'then' expression
        PT.EInt(gid (), 0),

        // 'else' expression
        // calls self ("recurse") resulting in recursion
        PT.EFnCall(
          skippedCallerID,
          PT.FQFnName.userFqName [] "recurse" 0,
          [],
          [ PT.EInt(gid (), 2) ]
        )
      )

    let recurse =
      testUserFn "recurse" [] [ "i" ] (PT.TVariable "a") fnExpr
      |> PT2RT.UserFunction.toRT
    let ast = EApply(callerID, eUserFnName "recurse", [], [ eInt 0 ])
    let! results = execSaveDvals "recursion in editor" [] [] [ recurse ] ast

    Expect.equal
      (Dictionary.get callerID results)
      (Some(AT.ExecutedResult(DInt 0L)))
      "result is there as expected"

    Expect.equal
      (Dictionary.get skippedCallerID results)
      (Some(
        AT.NonExecutedResult(DIncomplete(SourceID(recurse.tlid, skippedCallerID)))
      ))
      "result is incomplete for other path"
  }

let testIfPreview : Test =
  let ifID = gid ()
  let thenID = gid ()
  let elseID = gid ()
  let f cond =
    task {
      let ast =
        EIf(
          ifID,
          cond,
          EString(thenID, [ StringText "then" ]),
          EString(elseID, [ StringText "else" ])
        )
      let! results = execSaveDvals "if-preview" [] [] [] ast

      return
        (Dictionary.get ifID results
         |> Exception.unwrapOptionInternal "cannot find ifID" [],
         Dictionary.get thenID results
         |> Exception.unwrapOptionInternal "cannot find thenID" [],
         Dictionary.get elseID results
         |> Exception.unwrapOptionInternal "cannot find elseID" [])
    }

  // Using the first test below for illustration,
  //
  // First we pass in a condition to be evaluated:
  // - `eBool false`
  //
  // The 3-tuple that follows is used to check three things:
  //
  // - the first part is "what does the if/then expression evaluate to?"
  //   If the condition is 'truthy', then the expression will return "then"
  //   Otherwise it will turn "else"
  //
  // - the other two parts correspond to the `then` and `else` branches of the if condition.
  //   if the first is an `ExecutedResult` and the second is a `NonExecutedResult`,
  //   then the 'then' condition was evaluated but not the 'else' condition.

  testManyTask
    "if-then expression previews correctly"
    f
    [ (eBool false,
       (AT.ExecutedResult(DString "else"),
        AT.NonExecutedResult(DString "then"),
        AT.ExecutedResult(DString "else")))
      (eUnit (),
       (AT.ExecutedResult(DError(SourceID(7UL, ifID), "If only supports Booleans")),
        AT.NonExecutedResult(DString "then"),
        AT.NonExecutedResult(DString "else")))
      // fakevals
      (eFn [ "Test" ] "typeError" 0 [] [ eStr "test" ],
       (AT.ExecutedResult(DError(SourceNone, "test")),
        AT.NonExecutedResult(DString "then"),
        AT.NonExecutedResult(DString "else")))
      // others are true
      (eBool true,
       (AT.ExecutedResult(DString "then"),
        AT.ExecutedResult(DString "then"),
        AT.NonExecutedResult(DString "else")))
      (eInt 5,
       (AT.ExecutedResult(DError(SourceID(7UL, ifID), "If only supports Booleans")),
        AT.NonExecutedResult(DString "then"),
        AT.NonExecutedResult(DString "else")))
      (eStr "test",
       (AT.ExecutedResult(DError(SourceID(7UL, ifID), "If only supports Booleans")),
        AT.NonExecutedResult(DString "then"),
        AT.NonExecutedResult(DString "else"))) ]

let testOrPreview : Test =
  let orID = gid ()
  let f (arg1, arg2) =
    task {
      let ast = EOr(orID, arg1, arg2)
      let! results = execSaveDvals "or-preview" [] [] [] ast

      return
        (Dictionary.get (Expr.toID (arg1)) results
         |> Exception.unwrapOptionInternal "cannot find arg1 id" [],
         Dictionary.get (Expr.toID (arg2)) results
         |> Exception.unwrapOptionInternal "cannot find arg2 id" [],
         Dictionary.get orID results
         |> Exception.unwrapOptionInternal "cannot find overall id" [])
    }

  testManyTask
    "or expression previews correctly"
    f
    // bools
    [ ((eBool false, eBool false),
       (AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DBool false)))
      ((eBool false, eBool true),
       (AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DBool true)))
      ((eBool true, eBool false),
       (AT.ExecutedResult(DBool true),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DBool true)))
      ((eBool true, eBool true),
       (AT.ExecutedResult(DBool true),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DBool true)))
      // strings
      ((eBool false, eStr "test"),
       (AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DString "test"),
        AT.ExecutedResult(DError(SourceID(7UL, orID), "|| only supports Booleans"))))
      ((eBool true, eStr "test"),
       (AT.ExecutedResult(DBool true),
        AT.NonExecutedResult(DString "test"),
        AT.ExecutedResult(DBool true)))
      ((EString(999UL, [ StringText "test" ]), eBool false),
       (AT.ExecutedResult(DString "test"),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DError(SourceID(7UL, orID), "|| only supports Booleans"))))
      ((EString(999UL, [ StringText "test" ]), eBool true),
       (AT.ExecutedResult(DString "test"),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DError(SourceID(7UL, orID), "|| only supports Booleans")))) ]

let testAndPreview : Test =
  let andID = gid ()
  let f (arg1, arg2) =
    task {
      let ast = EAnd(andID, arg1, arg2)
      let! results = execSaveDvals "and-preview" [] [] [] ast

      return
        (Dictionary.get (Expr.toID arg1) results
         |> Exception.unwrapOptionInternal "cannot find arg1 id" [],
         Dictionary.get (Expr.toID arg2) results
         |> Exception.unwrapOptionInternal "cannot find arg2 id" [],
         Dictionary.get andID results
         |> Exception.unwrapOptionInternal "cannot find overall id" [])
    }

  testManyTask
    "and expression previews correctly"
    f
    // bools
    [ ((eBool false, eBool false),
       (AT.ExecutedResult(DBool false),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DBool false)))
      ((eBool false, eBool true),
       (AT.ExecutedResult(DBool false),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DBool false)))
      ((eBool true, eBool false),
       (AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DBool false),
        AT.ExecutedResult(DBool false)))
      ((eBool true, eBool true),
       (AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DBool true)))
      // strings
      ((eBool false, eStr "test"),
       (AT.ExecutedResult(DBool false),
        AT.NonExecutedResult(DString "test"),
        AT.ExecutedResult(DBool false)))
      ((eBool true, eStr "test"),
       (AT.ExecutedResult(DBool true),
        AT.ExecutedResult(DString "test"),
        AT.ExecutedResult(DError(SourceID(7UL, andID), "&& only supports Booleans"))))
      ((EString(999UL, [ StringText "test" ]), eBool false),
       (AT.ExecutedResult(DString "test"),
        AT.NonExecutedResult(DBool false),
        AT.ExecutedResult(DError(SourceID(7UL, andID), "&& only supports Booleans"))))
      ((EString(999UL, [ StringText "test" ]), eBool true),
       (AT.ExecutedResult(DString "test"),
        AT.NonExecutedResult(DBool true),
        AT.ExecutedResult(DError(SourceID(7UL, andID), "&& only supports Booleans")))) ]


let testLambdaPreview : Test =
  let lID = gid ()
  let p1ID = gid ()
  let p2ID = gid ()
  let f body =
    task {
      let ast = ELambda(lID, [ (p1ID, ""); (p2ID, "var") ], body)
      let! results = execSaveDvals "lambda-preview" [] [] [] ast
      return results |> Dictionary.toList |> Map
    }
  testManyTask
    "lambda preview"
    f
    [ (EString(65UL, [ StringText "body" ])),
      (Map.fromList [ (lID,
                       AT.ExecutedResult(
                         DFnVal(
                           Lambda(
                             { parameters = [ (p2ID, "var") ]
                               symtable = Map.empty
                               body = EString(65UL, [ StringText "body" ]) }
                           )
                         )
                       ))
                      (p1ID, AT.NonExecutedResult(DIncomplete(SourceID(7UL, p1ID))))
                      (p2ID, AT.NonExecutedResult(DIncomplete(SourceID(7UL, p2ID))))
                      (65UL, AT.NonExecutedResult(DString "body")) ]) ]


/// Test the results that are returned when we're "previewing" (i.e. Analysis)
///
/// "Preview" evals involve more tracing, including of unmatched patterns. In
/// this test, we set up a `match` expr with many patterns and RHSs. Each test
/// supplies an `expr` to match against, and a list of expected traces to look
/// for - up to one of the traces will be an Executed result, and the others
/// should all be NonExecutedResults.
let testMatchPreview : Test =
  let matchId = gid ()
  let pIntId, intRhsId = gid (), gid ()
  let pFloatId, floatRhsId = gid (), gid ()
  let pBoolId, boolRhsId = gid (), gid ()
  let pStrId, strRhsId = gid (), gid ()
  let pUnitId, unitRhsId = gid (), gid ()
  let pTupleId, tupleRhsId = gid (), gid ()

  let (pOkVarOkId,
       pOkVarVarId,
       okVarRhsId,
       binopFnValId,
       okVarRhsVarId,
       okVarRhsStrId,
       pTupleIdX,
       pTupleIdY) =
    gid (), gid (), gid (), gid (), gid (), gid (), gid (), gid ()

  let pNothingId, nothingRhsId = gid (), gid ()
  let pVarId, varRhsId = gid (), gid ()

  let patternsToMatchAgainst =
    [ // | 5 -> 17
      (MPInt(pIntId, 5L), EInt(intRhsId, 17L))

      // | 5.6 -> "float"
      (MPFloat(pFloatId, 5.6), EString(floatRhsId, [ StringText "float" ]))

      // | false -> "bool"
      (MPBool(pBoolId, false), EString(boolRhsId, [ StringText "bool" ]))

      // | "myStr" -> "str"
      (MPString(pStrId, "myStr"), EString(strRhsId, [ StringText "str" ]))

      // | () -> "unit"
      (MPUnit(pUnitId), EString(unitRhsId, [ StringText "unit" ]))

      // | (2, y) -> "tuple"
      (MPTuple(pTupleId, MPInt(pTupleIdX, 2L), MPVariable(pTupleIdY, "y"), []),
       EString(tupleRhsId, [ StringText "tuple" ]))

      // | Ok x -> "ok: " ++ x
      (MPEnum(pOkVarOkId, "Ok", [ MPVariable(pOkVarVarId, "x") ]),
       EApply(
         okVarRhsId,
         PT.FQFnName.stdlibFqName [ "String" ] "append" 1
         |> PT2RT.FQFnName.toRT
         |> FnName,
         [],
         [ EString(okVarRhsStrId, [ StringText "ok: " ])
           EVariable(okVarRhsVarId, "x") ]
       ))

      // | None -> "enum nothing"
      (MPEnum(pNothingId, "Nothing", []),
       EString(nothingRhsId, [ StringText "enum nothing" ]))

      // | name -> name
      // (everything should match this, except for 'fake' dvals such as errors)
      (MPVariable(pVarId, "name"), EVariable(varRhsId, "name")) ]

  let getSubExprIds (arg : Expr) =
    let mutable argIDs = []
    arg
    |> RuntimeTypesAst.postTraversal (fun e ->
      argIDs <- (Expr.toID e) :: argIDs
      e)
    |> ignore<Expr>
    argIDs

  // Checks that the 'expected' list of results are all as expected.
  // Then, checks that any patterns not explicitly called out are present in
  // the results, as NotExecutedResults, ensuring that all patterns are
  // accounted for.
  // - `arg` is the expr we're matching against
  // - `expected` is a list of explicitly called-out results to verify,
  //   including at most one 'matched' pattern (ExecutedResult)
  //   - the `string` in `expected` is the 'msg' to report in case of failure
  let t
    (msg : string)
    (arg : Expr)
    (expected : List<id * string * AT.ExecutionResult>)
    =
    testTask msg {
      let ast = EMatch(matchId, arg, patternsToMatchAgainst)

      let! results = execSaveDvals "match-preview" [] [] [] ast

      // check expected values are there
      List.iter
        (fun (id, name, value) ->
          Expect.equal
            (Dictionary.get id results)
            (Some value)
            $"{msg}: {id}, {name}")
        expected

      // Check that all patterns not included in 'expected' were evaluated,
      // and are NotExecutedResults

      // we expect _some_ result for all of these exprs
      let expectedIDs =
        (matchId :: getSubExprIds arg) @ List.map Tuple3.first expected |> Set

      // ensure we don't have more expected results than actual results
      Expect.isGreaterThan results.Count (Set.count expectedIDs) "sanity check"

      let resultsUnaccountedFor =
        Set.ofSeq (Dictionary.keys results)
        |> Set.difference expectedIDs
        |> Seq.map (fun id -> id, Dictionary.get id results)

      resultsUnaccountedFor
      |> Seq.iter (fun (id, result) ->
        match result with
        | Some (AT.ExecutedResult dv) ->
          Expect.isTrue
            false
            $"{msg}: found unexpected execution result ({id}: {dv})"
        | None -> Expect.isTrue false "missing value"
        | Some (AT.NonExecutedResult _) -> ())
    }

  // helpers
  let er x = AT.ExecutedResult x
  let ner x = AT.NonExecutedResult x
  let inc iid = DIncomplete(SourceID(id 7, iid))

  testList
    "test match evaluation"
    [ t
        "int match"
        (eInt 5)
        [ (pIntId, "matching pat", er (DInt 5L))
          (intRhsId, "matching rhs", er (DInt 17L))

          (pVarId, "2nd matching pat", ner (DInt 5L))
          (varRhsId, "2nd matching rhs", ner (DInt 5L)) ]

      t
        "non match"
        (eInt 6)
        [ (pIntId, "non matching pat", ner (DInt 5L))
          (intRhsId, "non matching rhs", ner (DInt 17L))

          (pFloatId, "float pat", ner (DFloat 5.6))
          (floatRhsId, "float rhs", ner (DString "float"))

          (pBoolId, "bool pat", ner (DBool false))
          (boolRhsId, "bool rhs", ner (DString "bool"))

          (pUnitId, "unit pat", ner DUnit)
          (unitRhsId, "unit rhs", ner (DString "unit"))

          (pOkVarOkId, "ok var pat ok", ner (inc pOkVarOkId))
          (pOkVarVarId, "ok var pat var", ner (inc pOkVarVarId))
          (okVarRhsId, "ok var pat rhs", ner (inc pOkVarVarId))
          (okVarRhsVarId, "ok var rhs var", ner (inc pOkVarVarId))
          (okVarRhsStrId, "ok var rhs str", ner (DString "ok: "))

          (pNothingId, "nothing pat", ner (DOption None))
          (nothingRhsId, "nothing pat rhs", ner (DString "enum nothing"))

          (pVarId, "catch all pat", er (DInt 6L))
          (varRhsId, "catch all rhs", er (DInt 6L)) ]

      t
        "float"
        (eFloat Positive "5" "6")
        [ (pFloatId, "pat", er (DFloat 5.6))
          (floatRhsId, "rhs", er (DString "float")) ]

      t
        "bool"
        (eBool false)
        [ (pBoolId, "pat", er (DBool false))
          (boolRhsId, "rhs", er (DString "bool")) ]

      t
        "unit"
        (eUnit ())
        [ (pUnitId, "pat", er DUnit); (unitRhsId, "rhs", er (DString "unit")) ]

      t
        "ok: y"
        (EEnum None "Ok" [ eStr "y" ])
        [ (pOkVarOkId, "ok pat 2", er (DResult(Ok(DString "y"))))
          (pOkVarVarId, "var pat", er (DString "y"))
          (okVarRhsId, "rhs", er (DString "ok: y"))
          (okVarRhsVarId, "rhs", er (DString "y"))
          (okVarRhsStrId, "str", er (DString "ok: ")) ]

      t
        "tuple match"
        (eTuple (eInt 2) (eStr "sample") [])
        [ (pTupleId, "matching pat", er (DTuple(DInt 2L, DString "sample", [])))
          (tupleRhsId, "matching rhs", er (DString "tuple"))

          (pVarId, "2nd matching pat", ner (DTuple(DInt 2L, DString "sample", [])))
          (varRhsId, "2nd matching rhs", ner (DTuple(DInt 2L, DString "sample", []))) ]

      t
        "nothing"
        (EEnum None "Nothing" [])
        [ (pNothingId, "ok pat", er (DOption None))
          (nothingRhsId, "rhs", er (DString "enum nothing")) ]

      // TODO: test enum around a literal
      // TODO: enum around a variable
      // TODO: enum around a enum around a value
      ]


let testLetPreview : Test =
  let letID = gid ()

  let createLetPattern
    (patternMatch : LetPattern)
    (expr : Expr)
    (body : Expr)
    : Task<Map<id, AT.ExecutionResult>> =
    task {
      let ast = ELet(letID, patternMatch, expr, body)

      let! results = execSaveDvals "let-preview" [] [] [] ast
      return results |> Dictionary.toList |> Map
    }

  testList
    "test let preview"
    [ testTask "let hello = 1 in hello" {
        let lpID = gid ()

        let letPattern = LPVariable(lpID, "hello")
        let assignExpr = eInt 1
        let retExpr = eVar "hello"

        let! result = createLetPattern letPattern assignExpr retExpr

        Expect.equal
          (Map.get lpID result)
          (Some(AT.ExecutedResult(DInt 1L)))
          "hello assigned correctly"
      }

      testTask "let (x, y) = (1, 2) in y" {
        let lpID = gid ()
        let xID = gid ()
        let yID = gid ()

        let letPattern =
          LPTuple(lpID, LPVariable(xID, "x"), LPVariable(yID, "y"), [])
        let assignExpr = eTuple (eInt 1) (eInt 2) []
        let retExpr = eVar "y"

        let! result = createLetPattern letPattern assignExpr retExpr

        Expect.equal
          (Map.get xID result)
          (Some(AT.ExecutedResult(DInt 1L)))
          "x assigned correctly"

        Expect.equal
          (Map.get yID result)
          (Some(AT.ExecutedResult(DInt 2L)))
          "y assigned correctly"
      }

      testTask "let (x, y) = (1, 1/0) in y" {
        let lpID = gid ()
        let xID = gid ()
        let yID = gid ()
        let divID = gid ()

        let letPattern =
          LPTuple(lpID, LPVariable(xID, "x"), LPVariable(yID, "y"), [])

        // Should result in type error
        let divisionExpr =
          EApply(
            divID,
            PT.FQFnName.stdlibFqName [ "Int" ] "divide" 0
            |> PT2RT.FQFnName.toRT
            |> FnName,
            [],
            [ eInt 1; eInt 0 ]
          )

        let assignExpr = eTuple (eInt 1) divisionExpr []
        let retExpr = eVar "y"

        let! result = createLetPattern letPattern assignExpr retExpr


        Expect.equal
          (Map.get (Expr.toID assignExpr) result)
          (Some(AT.ExecutedResult(DError(SourceNone, "Division by zero"))))
          "the whole tuple is a type error due to division by zero in y"

        Expect.equal
          (Map.get lpID result)
          (Some(AT.NonExecutedResult(DIncomplete(SourceID(7UL, lpID)))))
          "let pattern is incomplete due to error in RHS"
      }


      testTask "let (a, b, ((c, d), e)) = (1, 2, ((3, 4), 5)) in c" {
        let lpID = gid ()
        let aID = gid ()
        let bID = gid ()
        let cID = gid ()
        let dID = gid ()
        let eID = gid ()
        let innerTupleID = gid ()
        let outerTupleID = gid ()

        let letPattern =
          LPTuple(
            lpID,
            LPVariable(aID, "a"),
            LPVariable(bID, "b"),
            [ LPTuple(
                outerTupleID,
                LPTuple(innerTupleID, LPVariable(cID, "c"), LPVariable(dID, "d"), []),
                LPVariable(eID, "e"),
                []
              ) ]
          )

        let assignExpr =
          eTuple
            (eInt 1)
            (eInt 2)
            [ eTuple (eTuple (eInt 3) (eInt 4) []) (eInt 5) [] ]
        let retExpr = eVar "c"

        let! result = createLetPattern letPattern assignExpr retExpr

        Expect.equal
          (Map.get aID result)
          (Some(AT.ExecutedResult(DInt 1L)))
          "a assigned correctly"

        Expect.equal
          (Map.get bID result)
          (Some(AT.ExecutedResult(DInt 2L)))
          "b assigned correctly"

        Expect.equal
          (Map.get cID result)
          (Some(AT.ExecutedResult(DInt 3L)))
          "c assigned correctly"

        Expect.equal
          (Map.get dID result)
          (Some(AT.ExecutedResult(DInt 4L)))
          "d assigned correctly"

        Expect.equal
          (Map.get eID result)
          (Some(AT.ExecutedResult(DInt 5L)))
          "e assigned correctly"
      } ]





let tests =
  testList
    "ExecutionUnitTests"
    [ testRecursionInEditor
      testIfPreview
      testOrPreview
      testAndPreview
      testLambdaPreview
      testMatchPreview
      testExecFunctionTLIDs
      testLetPreview ]
