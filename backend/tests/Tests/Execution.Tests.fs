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

let executionStateForPreview
  (name : string)
  (dbs : Map<string, DB.T>)
  (types : Map<TypeName.UserProgram, UserType.T>)
  (fns : Map<FnName.UserProgram, UserFunction.T>)
  : Task<AT.AnalysisResults * ExecutionState> =
  task {
    let canvasID = System.Guid.NewGuid()
    let! state = executionStateFor canvasID false false dbs types fns
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
    let! state = executionStateFor meta false false Map.empty Map.empty fns

    let tlids, traceFn = Exe.traceTLIDs ()

    let state =
      { state with
          tracing =
            { state.tracing with traceTLID = traceFn; realOrPreview = Preview } }

    let! value =
      Exe.executeFunction state (gid ()) (FQName.UserProgram fn.name) [] []

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
        PT.EApply(
          skippedCallerID,
          PT.FnTargetName(Ok(PT.FnName.fqUserProgram [] "recurse" 0)),
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
      (Map.fromList
        [ (lID,
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
  let pCharId, charRhsId = gid (), gid ()
  let pUnitId, unitRhsId = gid (), gid ()
  let pTupleId, tupleRhsId = gid (), gid ()
  let pTuple2Id, tuple2RhsId = gid (), gid ()
  let pListConsId, listConsRhsId = gid (), gid ()
  let pListConsIntId7, pListConsIntId3, pListConsVarId, pListConsId2 =
    gid (), gid (), gid (), gid ()
  let pList2Id, list2RhsId = gid (), gid ()
  let pList2IntId3, pList2IntId7 = gid (), gid ()


  let (pOkVarOkId,
       pOkVarVarId,
       okVarRhsId,
       okVarRhsVarId,
       okVarRhsStrId,
       pTupleIdX,
       pTupleIdY,
       pTuple2IdX,
       pTuple2IdY) =
    gid (), gid (), gid (), gid (), gid (), gid (), gid (), gid (), gid ()

  let pNothingId, nothingRhsId = gid (), gid ()
  let pVarId, varRhsId = gid (), gid ()

  let patternsToMatchAgainst =
    [ // | 5 -> 17
      (MPInt(pIntId, 5L), EInt(intRhsId, 17L))

      // | false -> "bool"
      (MPBool(pBoolId, false), EString(boolRhsId, [ StringText "bool" ]))

      // | 'c' -> "char"
      (MPChar(pCharId, "c"), EString(charRhsId, [ StringText "char" ]))

      // | "myStr" -> "str"
      (MPString(pStrId, "myStr"), EString(strRhsId, [ StringText "str" ]))

      // | 5.6 -> "float"
      (MPFloat(pFloatId, 5.6), EString(floatRhsId, [ StringText "float" ]))

      // | () -> "unit"
      (MPUnit(pUnitId), EString(unitRhsId, [ StringText "unit" ]))

      // | Ok x -> "ok: " ++ x
      (MPEnum(pOkVarOkId, "Ok", [ MPVariable(pOkVarVarId, "x") ]),
       EApply(
         okVarRhsId,
         PT.FnName.fqBuiltIn [ "String" ] "append" 0
         |> PT2RT.FnName.toRT
         |> FnTargetName,
         [],
         [ EString(okVarRhsStrId, [ StringText "ok: " ])
           EVariable(okVarRhsVarId, "x") ]
       ))

      // | Nothing -> "enum nothing"
      (MPEnum(pNothingId, "Nothing", []),
       EString(nothingRhsId, [ StringText "enum nothing" ]))

      // | (2, y) -> "tuple"
      (MPTuple(pTupleId, MPInt(pTupleIdX, 2L), MPVariable(pTupleIdY, "y"), []),
       EString(tupleRhsId, [ StringText "tuple" ]))

      // | (2, 5) -> "tuple 2"
      (MPTuple(pTuple2Id, MPInt(pTuple2IdX, 2L), MPInt(pTuple2IdY, 5), []),
       EString(tuple2RhsId, [ StringText "tuple2" ]))

      // | 7 :: 3 :: z -> "list"
      (MPListCons(
        pListConsId,
        MPInt(pListConsIntId7, 7L),
        MPListCons(
          pListConsId2,
          MPInt(pListConsIntId3, 3L),
          MPVariable(pListConsVarId, "z")
        )
       ),
       EString(listConsRhsId, [ StringText "list" ]))

      // | [7, 3] -> "list 2"
      (MPList(pList2Id, [ MPInt(pList2IntId7, 7L); MPInt(pList2IntId3, 3L) ]),
       EString(list2RhsId, [ StringText "list2" ]))

      // | name -> name
      // (everything should match this, except for 'fake' dvals such as errors)
      (MPVariable(pVarId, "name"), EVariable(varRhsId, "name")) ]

  let getSubExprIds (arg : Expr) : List<id * string> =
    let mutable argIDs = []
    arg
    |> RuntimeTypesAst.postTraversal
      (fun e ->
        argIDs <- (Expr.toID e, string e) :: argIDs
        e)
      identity
      identity
      identity
      (fun lp ->
        argIDs <- (LetPattern.toID lp, string lp) :: argIDs
        lp)
      (fun mp ->
        argIDs <- (MatchPattern.toID mp, string mp) :: argIDs
        mp)
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

      // we expect _some_ result for all of these exprs and patterns
      let expected =
        getSubExprIds ast @ List.map (fun (id, name, _) -> (id, name)) expected
        |> Map

      let resultKeys = Set.ofSeq (Dictionary.keys results)
      let expectedKeys = Map.keys expected |> Seq.toList |> Set
      let unexpectedResultIDs = // keys in the results that we didn't expect
        Set.difference resultKeys expectedKeys
        |> Seq.map (fun id -> id, Dictionary.get id results)


      unexpectedResultIDs
      |> Seq.iter (fun (id, result) ->
        match result with
        | Some(AT.ExecutedResult dv) ->
          Expect.isTrue
            false
            $"{msg}: found unexpected execution result ({id}: {dv}\nin {ast})"
        | None ->
          // This should never be missing as these are supposedly the IDs in results not present elsewhere
          Expect.isTrue false $"missing value: {id}\n({ast})"
        | Some(AT.NonExecutedResult _) -> ())


      let missingIDs =
        Map.filterWithIndex
          (fun id _ -> not <| Dictionary.containsKey id results)
          expected
      missingIDs
      |> Map.iter (fun id v ->
        // TODO: Some results are currently missing (esp around MPList/MPListCons)
        // so don't assert this yet
        //   Expect.isTrue false $"{msg}: missing expected result ({id}: {v})"
        ())

    // ensure we don't have more expected results than actual results
    // TODO: also not correct at this time
    // Expect.equal results.Count (Map.count expected) "expected counts"
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
        [ (pIntId, "int pat", ner (DInt 5L))
          (intRhsId, "int rhs", ner (DInt 17L))

          (pBoolId, "bool pat", ner (DBool false))
          (boolRhsId, "bool rhs", ner (DString "bool"))

          (pCharId, "char pat", ner (DChar "c"))
          (charRhsId, "char rhs", ner (DString "char"))

          (pStrId, "string pat", ner (DString "myStr"))
          (strRhsId, "string rhs", ner (DString "str"))

          (pFloatId, "float pat", ner (DFloat 5.6))
          (floatRhsId, "float rhs", ner (DString "float"))

          (pUnitId, "unit pat", ner DUnit)
          (unitRhsId, "unit rhs", ner (DString "unit"))

          (pOkVarOkId, "ok var pat ok", ner (inc pOkVarOkId))
          (pOkVarVarId, "ok var pat var", ner (inc pOkVarVarId))
          (okVarRhsId, "ok var pat rhs", ner (inc pOkVarVarId))
          (okVarRhsVarId, "ok var rhs var", ner (inc pOkVarVarId))
          (okVarRhsStrId, "ok var rhs str", ner (DString "ok: "))

          // An Enum pattern in just a name, not a type ref, so we can't know
          // what DEnum was supposed to be here at runtime
          (pNothingId, "nothing pat", ner (inc pNothingId)) // TODO: provide this value in the trace
          (nothingRhsId, "nothing pat rhs", ner (DString "enum nothing"))

          (pTupleId, "tuple pat", ner (inc pTupleId))
          (pTupleIdX, "tuple pat x", ner (inc pTupleIdX)) // TODO: provide this value in the trace
          (pTupleIdY, "tuple pat y", ner (inc pTupleIdY))
          (tupleRhsId, "tuple rhs", ner (DString "tuple"))

          (pTuple2Id, "tuple 2 pat", ner (inc pTuple2Id)) // TODO: provide this value in the trace
          (pTuple2IdX, "tuple 2 pat x", ner (inc pTuple2IdX)) // TODO: provide this value in the trace
          (pTuple2IdY, "tuple 2 pat y", ner (inc pTuple2IdY)) // TODO: provide this value in the trace
          (tuple2RhsId, "tuple 2 rhs", ner (DString "tuple2"))

          (pListConsId, "list cons pat", ner (inc pListConsId))
          (pListConsIntId7, "list cons pat 7", ner (inc pListConsIntId7)) // TODO: provide this value in the trace
          // (pListConsIntId3, "list cons pat 3", ner (inc pListConsIntId3) ) // TODO: this is missing, provide this value in the trace
          // (pListConsVarId, "list cons pat var", ner (inc pListConsVarId)) // TODO: this is missing, provide this value in the trace
          (pListConsId2, "list cons pat 2", ner (inc pListConsId2))
          (listConsRhsId, "list cons rhs", ner (DString "list"))

          (pList2Id, "list 2 pat", ner (inc pList2Id)) // TODO: can we provide something here
          (pList2IntId7, "list 2 pat 7", ner (inc pList2IntId7)) // TODO: provide this value in the trace
          (pList2IntId3, "list 2 pat 3", ner (inc pList2IntId3)) // TODO: provide this value in the trace
          (list2RhsId, "list 2 rhs", ner (DString "list2"))


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
        (let typeName = Dval.resultType
         eEnum typeName "Ok" [ eStr "y" ])
        [ (pOkVarOkId, "ok pat 2", er (Dval.resultOk (DString "y")))
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
        (let typeName = Dval.optionType
         eEnum typeName "Nothing" [])
        [ (pNothingId, "nothing pat", er (Dval.optionNothing))
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
            PT.FnName.fqBuiltIn [ "Int" ] "divide" 0
            |> PT2RT.FnName.toRT
            |> FnTargetName,
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
