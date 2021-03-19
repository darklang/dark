module Tests.ProgramTypes

open Expecto
open Prelude
open TestUtils

module RT = LibExecution.RuntimeTypes
module PT = LibBackend.ProgramTypes
module S = LibExecution.Shortcuts

let parseTests =
  let p = PT.FQFnName.parse
  let User = RT.FQFnName.User
  let Stdlib = RT.FQFnName.Stdlib

  testList
    "Parsing fn names"
    [ testListUsingProperty
        "FQFnName parse tests"
        FuzzTests.All.FQFnName.ptRoundtrip
        [ ("", p "d6x3an030gugdr7t74k6k/s/F::pIi4tOCQujxl_v3")
          ("", p "uawmdntve/dolxb/X4Im::nsgKJGO_v1")
          ("", p "gqs/ekupo0/AmOCq7bpK9xBftJX1F4s::nFTxmaoJ8wAeshW0E_v1")
          ("", Stdlib { module_ = ""; function_ = "toString"; version = 0 })
          ("", User "someUserFn")
          ("", p "String::toInt_v1")
          ("", Stdlib { module_ = ""; function_ = "++"; version = 0 })
          ("", Stdlib { module_ = ""; function_ = "+"; version = 0 })
          ("", p "-")
          ("", p "^") ] ]

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

let tests = testList "ProgramTypes" [ parseTests; testPipesToRuntimeTypes ]
