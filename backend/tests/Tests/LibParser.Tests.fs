/// Tests the old, F#-based parser.
module Tests.LibParser

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PackageRefs = LibExecution.PackageRefs
module NR = LibParser.NameResolver

let id = 0UL // since we're ignoring IDs, just use the same one everywhere

let exprRTs =
  let t name testStr expectedExpr =
    testTask name {
      let! actual =
        LibParser.Parser.parsePTExpr
          (localBuiltIns pmPT)
          pmPT
          NR.OnMissing.Allow
          "libparser.tests.fs"
          testStr
        |> Ply.toTask
      return Expect.PT.equalExprIgnoringIDs actual expectedExpr
    }


  testList
    "Exprs"
    // TODO: order these by simplicity, and add more tests
    [
      // First, let's start with some simple ones
      // that don't have any dependencies.
      t "zero" "0.0" (PT.EFloat(id, Positive, "0", "0"))

      t "negative zero" "(-0.0)" (PT.EFloat(id, Negative, "0", "0"))

      t "negative 180" "-180.0" (PT.EFloat(id, Negative, "180", "0"))

      t
        "10 cents"
        "82.10"
        (PT.EFloat(
          id,
          Positive,
          "82",
          "099999999999994315658113919198513031005859375"
        ))

      t
        "simple expr"
        "(5L + 3L) == 8L"
        (PT.EInfix(
          id,
          PT.InfixFnCall(PT.ComparisonEquals),
          PT.EInfix(
            id,
            (PT.InfixFnCall(PT.ArithmeticPlus)),
            PT.EInt64(id, 5L),
            PT.EInt64(id, 3L)
          ),
          PT.EInt64(id, 8L)
        ))

      t
        "lambdas with 2 args"
        "fun x y -> 8L"
        (PT.ELambda(
          id,
          NEList.doubleton (PT.LPVariable(id, "x")) (PT.LPVariable(id, "y")),
          PT.EInt64(id, 8L)
        ))

      t
        "lambdas with 3 args"
        "fun x y z -> 8L"
        (PT.ELambda(
          id,
          NEList.ofList
            (PT.LPVariable(id, "x"))
            [ PT.LPVariable(id, "y"); PT.LPVariable(id, "z") ],
          PT.EInt64(id, 8L)
        ))

      t
        "lambdas with 4 args"
        "fun a b c d -> 8L"
        (PT.ELambda(
          id,
          NEList.ofList
            (PT.LPVariable(id, "a"))
            [ PT.LPVariable(id, "b")
              PT.LPVariable(id, "c")
              PT.LPVariable(id, "d") ],
          PT.EInt64(id, 8L)
        ))

      t
        "lambda with nested tuple arg"
        "fun ((a, b), c) -> a"
        (PT.ELambda(
          id,
          NEList.singleton (
            PT.LPTuple(
              id,
              PT.LPTuple(id, PT.LPVariable(id, "a"), PT.LPVariable(id, "b"), []),
              PT.LPVariable(id, "c"),
              []
            )
          ),
          PT.EVariable(id, "a")
        ))

      t
        "lambda with mixed arg and nested tuple arg"
        "fun x ((a, b), c) -> (x, a, b, c)"
        (PT.ELambda(
          id,
          NEList.doubleton
            (PT.LPVariable(id, "x"))
            (PT.LPTuple(
              id,
              PT.LPTuple(id, PT.LPVariable(id, "a"), PT.LPVariable(id, "b"), []),
              PT.LPVariable(id, "c"),
              []
            )),
          PT.ETuple(
            id,
            PT.EVariable(id, "x"),
            PT.EVariable(id, "a"),
            [ PT.EVariable(id, "b"); PT.EVariable(id, "c") ]
          )
        ))

      // Now let's test some more complex expressions
      t
        "pipe without expr"
        "(let x = 5L\nx |> Darklang.Stdlib.List.map 5L)"
        (PT.ELet(
          id,
          PT.LPVariable(id, "x"),
          PT.EInt64(id, 5L),
          PT.EPipe(
            id,
            PT.EVariable(id, "x"),
            [ PT.EPipeFnCall(
                id,
                { originalName = [ "Darklang"; "Stdlib"; "List"; "map" ]
                  resolved =
                    Ok(
                      PT.FQFnName.fqPackage
                        "6f980e08225c0f345c4887485a2785e7df0690293b1f2061bd4a5e048a441089"
                    ) },
                [],
                [ PT.EInt64(id, 5L) ]
              ) ]
          )
        )) ]

let tests = testList "LibParser" [ exprRTs ]
