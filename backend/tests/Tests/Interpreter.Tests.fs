module Tests.Interpreter

open Expecto
open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

module E = Tests.ProgramTypesToRuntimeTypes.Expressions

let eval pt =
  uply {
    let vmState = pt |> PT2RT.Expr.toRT 0 |> RT.VMState.fromInstructions

    let! executionState =
      executionStateFor PT.PackageManager.empty (System.Guid.NewGuid()) false false

    return! LibExecution.Interpreter.eval executionState vmState
  }

let t name expr expected =
  testTask name {
    let! actual = eval expr |> Ply.toTask
    return Expect.equal actual expected ""
  }


let onePlusTwo = t "1+2" E.onePlusTwo (RT.DInt64 3L)

let boolList =
  t
    "[true; false; true]"
    E.boolList
    (RT.DList(VT.bool, [ RT.DBool true; RT.DBool false; RT.DBool true ]))

let boolListList =
  t
    "[[true; false]; [false; true]]"
    E.boolListList
    (RT.DList(
      VT.list VT.bool,
      [ RT.DList(VT.bool, [ RT.DBool true; RT.DBool false ])
        RT.DList(VT.bool, [ RT.DBool false; RT.DBool true ]) ]
    ))
let letSimple = t "let x = true\nx" E.letSimple (RT.DBool true)
let letTuple = t "let (x, y) = (1, 2)\nx" E.letTuple (RT.DInt64 1L)
let letTupleNested =
  t "let (a, (b, c)) = (1, (2, 3))\nb" E.letTupleNested (RT.DInt64 2L)

let simpleString = t "[\"hello\"]" E.simpleString (RT.DString "hello")

let stringWithInterpolation =
  t
    "[let x = \"world\" in $\"hello {x}\"]"
    E.stringWithInterpolation
    (RT.DString "hello, world")

let dictEmpty = t "Dict {}" E.dictEmpty (RT.DDict(VT.unknown, Map.empty))
let dictSimple =
  t
    "Dict { t: true}"
    E.dictSimple
    (RT.DDict(VT.unknown, Map [ "key", RT.DBool true ]))
let dictMultEntries =
  t
    "Dict {t: true; f: false}"
    E.dictMultEntries
    (RT.DDict(VT.unknown, Map [ "t", RT.DBool true; "f", RT.DBool false ]))
let dictDupeKey =
  t
    "Dict {t: true; f: false; t: false}"
    E.dictDupeKey
    (RT.DDict(VT.unknown, Map [ "t", RT.DBool false; "f", RT.DBool false ]))


let ifGotoThenBranch = t "if true then 1 else 2" E.ifGotoThenBranch (RT.DInt64 1L)

let ifGotoElseBranch = t "if false then 1 else 2" E.ifGotoElseBranch (RT.DInt64 2L)
let ifElseMissing = t "if false then 1" E.ifElseMissing RT.DUnit

let tuple2 =
  t "(false, true)" E.tuple2 (RT.DTuple(RT.DBool false, RT.DBool true, []))

let tuple3 =
  t
    "(false, true, false)"
    E.tuple3
    (RT.DTuple(RT.DBool false, RT.DBool true, [ RT.DBool false ]))
let tupleNested =
  t
    "((false, true), true, (true, false)))"
    E.tupleNested
    (RT.DTuple(
      RT.DTuple(RT.DBool false, RT.DBool true, []),
      RT.DBool true,
      [ RT.DTuple(RT.DBool true, RT.DBool false, []) ]
    ))

let matchSimple =
  t
    "match true with\n| false -> \"first branch\"\n| true -> \"second branch\""
    E.matchSimple
    (RT.DString "second branch")

let matchNotMatched =
  t
    "match true with\n| false -> \"first branch\""
    E.matchNotMatched
    (RT.DString "match not matched")

let matchWithVar = t "match true with\n| x -> x" E.matchWithVar (RT.DBool true)

let matchWithVarAndWhenCondition =
  t
    "match 4 with\n| 1 -> \"first branch\"\n| x when x % 2 == 0 -> \"second branch\""
    E.matchWithVarAndWhenCondition
    (RT.DString "second branch")

let matchList =
  t
    "match [1, 2] with\n| [1, 2] -> \"first branch\""
    E.matchList
    (RT.DString "first branch")

let matchListCons =
  t
    "match [1, 2] with\n| 1 :: tail -> tail"
    E.matchListCons
    (RT.DList(VT.int64, [ RT.DInt64 2L ]))

let matchTuple =
  t
    "match (1, 2) with\n| (1, 2) -> \"first branch\""
    E.matchTuple
    (RT.DString "first branch")

let tests =
  testList
    "Interpreter"
    [ onePlusTwo
      boolList
      boolListList
      letSimple
      letTuple
      letTupleNested
      simpleString
      stringWithInterpolation
      dictEmpty
      dictSimple
      dictMultEntries
      dictDupeKey
      ifGotoThenBranch
      ifGotoElseBranch
      ifElseMissing
      tuple2
      tuple3
      tupleNested
      matchSimple
      matchNotMatched
      matchWithVar
      //matchWithVarAndWhenCondition
      matchList
      matchListCons
      matchTuple ]
