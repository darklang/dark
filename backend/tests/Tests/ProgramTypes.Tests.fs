module Tests.ProgramTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module ST = LibBinarySerialization.SerializedTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2ST = LibBinarySerialization.ProgramTypesToSerializedTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module S = TestUtils.RTShortcuts

let ptFQFnName =
  testMany
    "ProgramTypes.FnName.ToString"
    (fun name -> name |> PT2RT.FnName.toRT |> RT.FnName.toString)
    [ (PT.FnName.fqBuiltIn [ "String" ] "append" 1), "String.append_v1" ]


let testPipesToRuntimeTypes =
  testTask "pipes to runtime types" {
    let! actual =
      "value.age |> (-) 2L |> (+) value.age |> (<) 3L"
      |> LibParser.Parser.parseRTExpr nameResolver "programTypes.tests.fs"
      |> Ply.toTask

    let expected =
      S.eFn
        [ "Int64" ]
        "lessThan"
        0
        []
        [ S.eFn
            [ "Int64" ]
            "add"
            0
            []
            [ S.eFn
                [ "Int64" ]
                "subtract"
                0
                []
                [ S.eFieldAccess (S.eVar "value") "age"; S.eInt64 2 ]
              S.eFieldAccess (S.eVar "value") "age" ]
          S.eInt64 3 ]

    return Expect.equalExprIgnoringIDs actual expected
  }

let testProgramTypesToRuntimeTypes =
  let u = PT.EUnit 8UL
  let ru = RT.EUnit 8UL

  testMany
    "program types to runtime types"
    PT2RT.Expr.toRT
    [ PT.EFloat(7UL, Positive, "", "0"), RT.EFloat(7UL, 0.0)
      PT.EFloat(7UL, Positive, "0", ""), RT.EFloat(7UL, 0.0)
      PT.EFloat(7UL, Positive, "", ""), RT.EFloat(7UL, 0.0)
      (PT.EMatch(
        9UL,
        u,
        [ { pat = PT.MPFloat(5UL, Positive, "", ""); whenCondition = None; rhs = u } ]
       ),
       RT.EMatch(
         9UL,
         ru,
         NEList.singleton
           { pat = RT.MPFloat(5UL, 0.0); whenCondition = None; rhs = ru }
       ))
      (PT.EMatch(
        9UL,
        u,
        [ { pat = PT.MPFloat(5UL, Positive, "0", ""); whenCondition = None; rhs = u } ]
       ),
       RT.EMatch(
         9UL,
         ru,
         NEList.singleton
           { pat = RT.MPFloat(5UL, 0.0); whenCondition = None; rhs = ru }
       ))
      (PT.EMatch(
        9UL,
        u,
        [ { pat = PT.MPFloat(5UL, Positive, "", "0"); whenCondition = None; rhs = u } ]
       ),
       RT.EMatch(
         9UL,
         ru,
         NEList.singleton
           { pat = RT.MPFloat(5UL, 0.0); whenCondition = None; rhs = ru }
       ))
      (PT.EMatch(
        9UL,
        u,
        [ { pat = PT.MPFloat(5UL, Positive, "0", "0")
            whenCondition = Some u
            rhs = u } ]
       ),
       RT.EMatch(
         9UL,
         ru,
         NEList.singleton
           { pat = RT.MPFloat(5UL, 0.0); whenCondition = Some ru; rhs = ru }
       )) ]

let testInfixProgramTypesToSerializedTypes =
  testMany
    "infix program types to serialized types"
    PT2ST.Expr.toST
    [ (PT.EInfix(
        8UL,
        PT.InfixFnCall(PT.ArithmeticPlus),
        PT.EInt64(9UL, 6L),
        PT.EInt64(10UL, 6L)
       ),
       ST.EInfix(
         8UL,
         ST.InfixFnCall(ST.ArithmeticPlus),
         ST.EInt64(9UL, 6L),
         ST.EInt64(10UL, 6L)
       )) ]


let tests =
  testList
    "ProgramTypes"
    [ testPipesToRuntimeTypes
      testProgramTypesToRuntimeTypes
      ptFQFnName
      testInfixProgramTypesToSerializedTypes ]
