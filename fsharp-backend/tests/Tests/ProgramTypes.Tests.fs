module Tests.ProgramTypes

open Expecto
open Prelude
open TestUtils

module PT = LibBackend.ProgramTypes
module S = LibExecution.Shortcuts

let fuzzedTests =
  let p = PT.FQFnName.parse

  testList
    "Tests found by fuzzing"
    [ testListUsingProperty
        "FQFnName parse tests"
        FuzzTests.All.FQFnName.ptRoundtrip
        [ ("", p "d6x3an030gugdr7t74k6k/s/F::pIi4tOCQujxl_v3")
          ("", p "uawmdntve/dolxb/X4Im::nsgKJGO_v1")
          ("", p "gqs/ekupo0/AmOCq7bpK9xBftJX1F4s::nFTxmaoJ8wAeshW0E_v1") ] ]

let testPipesToRuntimeTypes =
  test "pipes to runtime types" {
    let actual =
      FSharpToExpr.parseRTExpr "value.age |> (-) 2 |> (+) value.age |> (<) 3"

    let expected =
      S.ePipeApply
        (S.eStdFnVal "" "<" 0)
        [ S.ePipeApply
            (S.eStdFnVal "" "+" 0)
            [ S.ePipeApply
                (S.eStdFnVal "" "-" 0)
                [ S.eFieldAccess (S.eVar "value") "age"; S.eInt 2 ]
              S.eFieldAccess (S.eVar "value") "age" ]
          S.eInt 3 ]

    Expect.equalExprIgnoringIDs actual expected
  }

let tests = testList "ProgramTypes" [ fuzzedTests; testPipesToRuntimeTypes ]
