module Tests.Interpreter

open Expecto
open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

module E = TestValues.Expressions

let t name ptExpr expectedInsts =
  testTask name {
    let vmState = ptExpr |> PT2RT.Expr.toRT 0 |> RT.VMState.fromInstructions

    let! exeState =
      executionStateFor PT.PackageManager.empty (System.Guid.NewGuid()) false false

    let! actual = LibExecution.Interpreter.eval exeState vmState |> Ply.toTask
    return Expect.equal actual expectedInsts ""
  }

let tFail name ptExpr expectedRte =
  testTask name {
    let instructionsWithContext = ptExpr |> PT2RT.Expr.toRT 0

    let! exeState =
      executionStateFor PT.PackageManager.empty (System.Guid.NewGuid()) false false

    let! actual =
      LibExecution.Execution.executeExpr exeState Map.empty instructionsWithContext

    match actual with
    | Ok _ -> return Expect.equal 1 2 "Expected an RTE, but got a successful result"
    | Error(_cs, actualRte) -> return Expect.equal actualRte expectedRte ""
  }


module Basic =
  // CLEANUP back fill with more simple stuff

  let onePlusTwo = t "1+2" E.Basic.onePlusTwo (RT.DInt64 3L)

  let tests = testList "Basic" [ onePlusTwo ]


module List =
  let simple =
    t
      "[true; false; true]"
      E.List.simple
      (RT.DList(VT.bool, [ RT.DBool true; RT.DBool false; RT.DBool true ]))

  let nested =
    t
      "[[true; false]; [false; true]]"
      E.List.nested
      (RT.DList(
        VT.list VT.bool,
        [ RT.DList(VT.bool, [ RT.DBool true; RT.DBool false ])
          RT.DList(VT.bool, [ RT.DBool false; RT.DBool true ]) ]
      ))

  let mixed =
    tFail
      "[1; true]"
      E.List.mixed
      (RT.RuntimeError.fromDT (
        RT.DString "Could not merge types List<Bool> and List<Int64>"
      ))

  let tests = testList "Lists" [ simple; nested; mixed ]


module Let =
  let simple = t "let x = true\nx" E.Let.simple (RT.DBool true)

  let tuple = t "let (x, y) = (1, 2)\nx" E.Let.tuple (RT.DInt64 1L)

  /// `let (a, b) = 1 in a`
  let tupleNotTuple =
    tFail
      "let (a, b) = 1 in a"
      E.Let.tupleNotTuple
      (RT.RuntimeError.fromDT (RT.DString "Let Pattern did not match"))

  /// `let (a, b) = (1, 2, 3) in a`
  let tupleIncorrectLen =
    tFail
      "let (a, b) = (1, 2, 3) in a"
      E.Let.tupleIncorrectLen
      (RT.RuntimeError.fromDT (RT.DString "Let Pattern did not match"))

  let tupleNested =
    t "let (a, (b, c)) = (1, (2, 3))\nb" E.Let.tupleNested (RT.DInt64 2L)

  /// `a`
  let undefinedVar =
    tFail
      "a"
      E.Let.undefinedVar
      (RT.RuntimeError.fromDT (RT.DString "Variable not found: a"))

  let tests =
    testList
      "Let"
      [ simple; tuple; tupleNotTuple; tupleIncorrectLen; tupleNested; undefinedVar ]


module String =
  let simple = t "[\"hello\"]" E.String.simple (RT.DString "hello")

  let withInterpolation =
    t
      "[let x = \"world\" in $\"hello {x}\"]"
      E.String.withInterpolation
      (RT.DString "hello, world")

  let tests = testList "Strings" [ simple; withInterpolation ]


module Dict =
  let empty = t "Dict {}" E.Dict.empty (RT.DDict(VT.unknown, Map.empty))

  let simple =
    t
      "Dict { t: true}"
      E.Dict.simple
      (RT.DDict(VT.unknown, Map [ "key", RT.DBool true ]))

  let multEntries =
    t
      "Dict {t: true; f: false}"
      E.Dict.multEntries
      (RT.DDict(VT.unknown, Map [ "t", RT.DBool true; "f", RT.DBool false ]))

  let dupeKey =
    t
      "Dict {t: true; f: false; t: false}"
      E.Dict.dupeKey
      (RT.DDict(VT.unknown, Map [ "t", RT.DBool false; "f", RT.DBool false ]))

  let tests = testList "Dict" [ empty; simple; multEntries; dupeKey ]


module If =
  let gotoThenBranch = t "if true then 1 else 2" E.If.gotoThenBranch (RT.DInt64 1L)
  let gotoElseBranch = t "if false then 1 else 2" E.If.gotoElseBranch (RT.DInt64 2L)
  let elseMissing = t "if false then 1" E.If.elseMissing RT.DUnit

  let tests = testList "If" [ gotoThenBranch; gotoElseBranch; elseMissing ]


module Tuples =
  let two =
    t "(false, true)" E.Tuples.two (RT.DTuple(RT.DBool false, RT.DBool true, []))

  let three =
    t
      "(false, true, false)"
      E.Tuples.three
      (RT.DTuple(RT.DBool false, RT.DBool true, [ RT.DBool false ]))

  let nested =
    t
      "((false, true), true, (true, false)))"
      E.Tuples.nested
      (RT.DTuple(
        RT.DTuple(RT.DBool false, RT.DBool true, []),
        RT.DBool true,
        [ RT.DTuple(RT.DBool true, RT.DBool false, []) ]
      ))

  let tests = testList "Tuples" [ two; three; nested ]


module Match =
  let simple =
    t
      "match true with\n| false -> \"first branch\"\n| true -> \"second branch\""
      E.Match.simple
      (RT.DString "second branch")

  let notMatched =
    tFail
      "match true with\n| false -> \"first branch\""
      E.Match.notMatched
      (RT.RuntimeError.fromDT (RT.DString "match not matched"))

  let withVar = t "match true with\n| x -> x" E.Match.withVar (RT.DBool true)

  let withVarAndWhenCondition =
    t
      "match 4 with\n| 1 -> \"first branch\"\n| x when x % 2 == 0 -> \"second branch\""
      E.Match.withVarAndWhenCondition
      (RT.DString "second branch")

  let list =
    t
      "match [1, 2] with\n| [1, 2] -> \"first branch\""
      E.Match.list
      (RT.DString "first branch")

  let listCons =
    t
      "match [1, 2] with\n| 1 :: tail -> tail"
      E.Match.listCons
      (RT.DList(VT.int64, [ RT.DInt64 2L ]))

  let tuple =
    t
      "match (1, 2) with\n| (1, 2) -> \"first branch\""
      E.Match.tuple
      (RT.DString "first branch")

  let tests =
    testList
      "Match"
      [ simple
        notMatched
        withVar
        //withVarAndWhenCondition
        list
        listCons
        tuple ]


let tests =
  testList
    "Interpreter"
    [ Basic.tests
      List.tests
      Let.tests
      String.tests
      Dict.tests
      If.tests
      Tuples.tests
      Match.tests ]
