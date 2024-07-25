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
    let _registersNeeded, instructions, resultReg = PT2RT.Expr.toRT 0 pt

    let! executionState =
      executionStateFor PT.PackageManager.empty (System.Guid.NewGuid()) false false

    return! LibExecution.Interpreter.eval executionState instructions resultReg
  }


let onePlusTwo =
  testTask "1+2" {
    let! actual = eval E.onePlusTwo |> Ply.toTask
    return Expect.equal actual (RT.DInt64 3L) ""
  }

let boolList =
  testTask "[true; false; true]" {
    let! actual = eval E.boolList |> Ply.toTask

    return
      Expect.equal
        actual
        (RT.DList(VT.unknown, [ RT.DBool true; RT.DBool false; RT.DBool true ]))
        ""
  }

let boolListList =
  testTask "[[true; false]; [false; true]]" {
    let! actual = eval E.boolListList |> Ply.toTask

    return
      Expect.equal
        actual
        (RT.DList(
          VT.unknown,
          [ RT.DList(VT.unknown, [ RT.DBool true; RT.DBool false ])
            RT.DList(VT.unknown, [ RT.DBool false; RT.DBool true ]) ]
        ))
        ""
  }


let tests = testList "Interpreter" [ onePlusTwo; boolList ]
