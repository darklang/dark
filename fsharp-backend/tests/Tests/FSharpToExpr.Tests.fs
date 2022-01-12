module Tests.FSharpToExpr

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

open PT.Shortcuts

let parserTests =
  let t name testStr expectedExpr =
    testTask name {
      let source = FSharpToExpr.parse testStr
      let actualProg = FSharpToExpr.convertToExpr source

      return
        (Expect.isTrue
          (actualProg.testEqualIgnoringIDs expectedExpr)
          $"{actualProg}\n\n=\n\n{expectedExpr}")
    }

  testList
    "Parser tests"
    [ t
        "pipe without expr"
        "(let x = 5\nx |> List.map_v0 5)"
        (eLet
          "x"
          (eInt 5)
          (ePipe (eVar "x") (eFn "List" "map" 0 [ (ePipeTarget ()); eInt 5 ]) []))
      t
        "simple expr"
        "(5 + 3) == 8"
        (eBinOp "" "==" 0 (eBinOp "" "+" 0 (eInt 5) (eInt 3)) (eInt 8))
      t "lambdas with 2 args" "fun x y -> 8" (eLambda [ "x"; "y" ] (eInt 8))
      t "lambdas with 3 args" "fun x y z -> 8" (eLambda [ "x"; "y"; "z" ] (eInt 8))
      t
        "lambdas with 4 args"
        "fun a b c d -> 8"
        (eLambda [ "a"; "b"; "c"; "d" ] (eInt 8))
      t "negative zero" "(-0.0)" (eFloat Negative "0" "0")
      t
        "10 cents"
        "82.10"
        (eFloat Positive "82" "099999999999994315658113919198513031005859375")
      t "zero" "0.0" (eFloat Positive "0" "0")
      t "negative 180" "-180.0" (eFloat Negative "180" "0") ]

let tests = testList "FSharpToExpr" [ parserTests ]
