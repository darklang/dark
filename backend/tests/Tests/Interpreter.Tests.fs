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
    let vmState = PT2RT.Expr.toRT 0 pt |> RT.VMState.fromInstructions

    let! executionState =
      executionStateFor PT.PackageManager.empty (System.Guid.NewGuid()) false false

    return! LibExecution.Interpreter.eval executionState vmState
  }


let onePlusTwo =
  testTask "1+2" {
    let! actual = eval E.onePlusTwo |> Ply.toTask
    let expected = RT.DInt64 3L
    return Expect.equal actual expected ""
  }

let boolList =
  testTask "[true; false; true]" {
    let! actual = eval E.boolList |> Ply.toTask
    let expected =
      RT.DList(VT.unknown, [ RT.DBool true; RT.DBool false; RT.DBool true ])
    return Expect.equal actual expected ""
  }

let boolListList =
  testTask "[[true; false]; [false; true]]" {
    let! actual = eval E.boolListList |> Ply.toTask
    let expected =
      RT.DList(
        VT.unknown,
        [ RT.DList(VT.unknown, [ RT.DBool true; RT.DBool false ])
          RT.DList(VT.unknown, [ RT.DBool false; RT.DBool true ]) ]
      )
    return Expect.equal actual expected ""
  }

let simpleString =
  testTask "[\"hello\"]" {
    let! actual = eval E.simpleString |> Ply.toTask
    let expected = RT.DString "hello"
    return Expect.equal actual expected ""
  }

let stringWithInterpolation =
  testTask "[let x = \"world\" in $\"hello {x}\"]" {
    let! actual = eval E.stringWithInterpolation |> Ply.toTask
    let expected = RT.DString "hello, world"
    return Expect.equal actual expected ""
  }

let dictEmpty =
  testTask "Dict {}" {
    let! actual = eval E.dictEmpty |> Ply.toTask
    let expected = RT.DDict(VT.unknown, Map.empty)
    return Expect.equal actual expected ""
  }
let dictSimple =
  testTask "Dict { t: true}" {
    let! actual = eval E.dictSimple |> Ply.toTask
    let expected = RT.DDict(VT.unknown, Map [ "key", RT.DBool true ])
    return Expect.equal actual expected ""
  }
let dictMultEntries =
  testTask "Dict {t: true; f: false}" {
    let! actual = eval E.dictMultEntries |> Ply.toTask
    let expected =
      RT.DDict(VT.unknown, Map [ "t", RT.DBool true; "f", RT.DBool false ])
    return Expect.equal actual expected ""
  }
let dictDupeKey =
  testTask "Dict {t: true; f: false; t: false}" {
    let! actual = eval E.dictDupeKey |> Ply.toTask
    let expected =
      RT.DDict(VT.unknown, Map [ "t", RT.DBool false; "f", RT.DBool false ])
    return Expect.equal actual expected ""
  }


let ifGotoThenBranch =
  testTask "if true then 1 else 2" {
    let! actual = eval E.ifGotoThenBranch |> Ply.toTask
    let expected = RT.DInt64 1L
    return Expect.equal actual expected ""
  }

let ifGotoElseBranch =
  testTask "if false then 1 else 2" {
    let! actual = eval E.ifGotoElseBranch |> Ply.toTask
    let expected = RT.DInt64 2L
    return Expect.equal actual expected ""
  }
let ifElseMissing =
  testTask "if false then 1" {
    let! actual = eval E.ifElseMissing |> Ply.toTask
    let expected = RT.DUnit
    return Expect.equal actual expected ""
  }

let tuple2 =
  testTask "(false, true)" {
    let! actual = eval E.tuple2 |> Ply.toTask
    let expected = RT.DTuple(RT.DBool false, RT.DBool true, [])
    return Expect.equal actual expected ""
  }

let tuple3 =
  testTask "(false, true, false)" {
    let! actual = eval E.tuple3 |> Ply.toTask
    let expected = RT.DTuple(RT.DBool false, RT.DBool true, [ RT.DBool false ])
    return Expect.equal actual expected ""
  }

let tupleNested =
  testTask "((false, true), true, (true, false)))" {
    let! actual = eval E.tupleNested |> Ply.toTask
    let expected =
      RT.DTuple(
        RT.DTuple(RT.DBool false, RT.DBool true, []),
        RT.DBool true,
        [ RT.DTuple(RT.DBool true, RT.DBool false, []) ]
      )
    return Expect.equal actual expected ""
  }

// let TODO =
//   testTask "TODO" {
//     let! actual = eval E.TODO |> Ply.toTask
//     let expected = RT.DUnit
//     return Expect.equal actual expected ""
//   }


let tests =
  testList
    "Interpreter"
    [ onePlusTwo
      boolList
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
      tupleNested ]
