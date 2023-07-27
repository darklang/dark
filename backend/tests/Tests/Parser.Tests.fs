module Tests.Parser

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


let parserTests =
  let t name testStr expectedExpr =
    testTask name {
      let actual =
        Parser.Parser.parseRTExpr builtinResolver "parser.tests.fs" testStr
      return Expect.equalExprIgnoringIDs actual (PT2RT.Expr.toRT expectedExpr)
    }
  let id = 0UL // since we're ignoring IDs, just use the same one everywhere
  testList
    "Parser tests"
    [ t
        "pipe without expr"
        "(let x = 5\nx |> List.map_v0 5)"
        (PT.ELet(
          id,
          PT.LPVariable(id, "x"),
          PT.EInt(id, 5),
          PT.EPipe(
            id,
            PT.EVariable(id, "x"),
            PT.EPipeFnCall(
              id,
              Ok(PT.FnName.fqBuiltIn [ "List" ] "map" 0),
              [],
              [ PT.EInt(id, 5) ]
            ),
            []
          )
        ))
      t
        "simple expr"
        "(5 + 3) == 8"
        (PT.EInfix(
          id,
          PT.InfixFnCall(PT.ComparisonEquals),
          PT.EInfix(
            id,
            (PT.InfixFnCall(PT.ArithmeticPlus)),
            PT.EInt(id, 5),
            PT.EInt(id, 3)
          ),
          PT.EInt(id, 8)
        ))
      t
        "lambdas with 2 args"
        "fun x y -> 8"
        (PT.ELambda(id, [ id, "x"; id, "y" ], PT.EInt(id, 8)))
      t
        "lambdas with 3 args"
        "fun x y z -> 8"
        (PT.ELambda(id, [ id, "x"; id, "y"; id, "z" ], PT.EInt(id, 8)))
      t
        "lambdas with 4 args"
        "fun a b c d -> 8"
        (PT.ELambda(id, [ id, "a"; id, "b"; id, "c"; id, "d" ], PT.EInt(id, 8)))
      t "negative zero" "(-0.0)" (PT.EFloat(id, Negative, "0", "0"))
      t
        "10 cents"
        "82.10"
        (PT.EFloat(
          id,
          Positive,
          "82",
          "099999999999994315658113919198513031005859375"
        ))
      t "zero" "0.0" (PT.EFloat(id, Positive, "0", "0"))
      t "negative 180" "-180.0" (PT.EFloat(id, Negative, "180", "0")) ]

let tests = testList "Parser" [ parserTests ]
