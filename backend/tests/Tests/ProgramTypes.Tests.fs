module Tests.ProgramTypes

open Expecto
open Prelude
open TestUtils.TestUtils

//module ST = LibBinarySerialization.SerializedTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
//module PT2ST = LibBinarySerialization.ProgramTypesToSerializedTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module S = TestUtils.RTShortcuts
module PackageIDs = LibExecution.PackageIDs
// module PT2DT = LibExecution.ProgramTypesToDarkTypes
// module C2DT = LibExecution.CommonToDarkTypes
// let pm = LibCloud.PackageManager.pt

// let p (code : string) : Ply<PT.Expr> =
//   uply {
//     let! (state : RT.ExecutionState) =
//       let canvasID = System.Guid.NewGuid()
//       executionStateFor pm canvasID false false Map.empty

//     let name =
//       RT.FQFnName.FQFnName.Package PackageIDs.Fn.LanguageTools.Parser.parsePTExpr

//     let args = NEList.singleton (RT.DString code)
//     let! execResult = LibExecution.Execution.executeFunction state name [] args

//     match execResult with
//     | Ok dval ->
//       match C2DT.Result.fromDT PT2DT.Expr.fromDT dval identity with
//       | Ok expr -> return expr
//       | Error _ ->
//         return Exception.raiseInternal "Error converting Dval to PT.Expr" []
//     | _ -> return Exception.raiseInternal "Error executing parsePTExpr function" []
//   }

// let ptFQFnName =
//   testMany
//     "ProgramTypes.FnName.ToString"
//     (fun name -> name |> PT2RT.FQFnName.toRT |> RT.FQFnName.toString)
//     [ (PT.FQFnName.fqBuiltIn "stringAppend" 1), "stringAppend_v1" ]

let pmPT = PT.PackageManager.empty

// let testPipesToRuntimeTypes =
//   testTask "pipes to runtime types" {
//     let parsed = p "value.age |> (-) 2L |> (+) value.age |> (<) 3L"
//     let! actual = Ply.map PT2RT.Expr.toRT parsed |> Ply.toTask

//     let expected =
//       S.eFn
//         "int64LessThan"
//         0
//         []
//         [ S.eFn
//             "int64Add"
//             0
//             []
//             [ S.eFn
//                 "int64Subtract"
//                 0
//                 []
//                 [ S.eFieldAccess (S.eVar "value") "age"; S.eInt64 2 ]
//               S.eFieldAccess (S.eVar "value") "age" ]
//           S.eInt64 3 ]

//     return Expect.equalExprIgnoringIDs actual expected
//   }

// let testProgramTypesToRuntimeTypes =
//   let u = PT.EUnit 8UL
//   let ru = RT.EUnit 8UL

//   testMany
//     "program types to runtime types"
//     PT2RT.Expr.toRT
//     [ PT.EFloat(7UL, Positive, "", "0"), RT.EFloat(7UL, 0.0)
//       PT.EFloat(7UL, Positive, "0", ""), RT.EFloat(7UL, 0.0)
//       PT.EFloat(7UL, Positive, "", ""), RT.EFloat(7UL, 0.0)
//       (PT.EMatch(
//         9UL,
//         u,
//         [ { pat = PT.MPFloat(5UL, Positive, "", ""); whenCondition = None; rhs = u } ]
//        ),
//        RT.EMatch(
//          9UL,
//          ru,
//          NEList.singleton
//            { pat = RT.MPFloat(5UL, 0.0); whenCondition = None; rhs = ru }
//        ))
//       (PT.EMatch(
//         9UL,
//         u,
//         [ { pat = PT.MPFloat(5UL, Positive, "0", ""); whenCondition = None; rhs = u } ]
//        ),
//        RT.EMatch(
//          9UL,
//          ru,
//          NEList.singleton
//            { pat = RT.MPFloat(5UL, 0.0); whenCondition = None; rhs = ru }
//        ))
//       (PT.EMatch(
//         9UL,
//         u,
//         [ { pat = PT.MPFloat(5UL, Positive, "", "0"); whenCondition = None; rhs = u } ]
//        ),
//        RT.EMatch(
//          9UL,
//          ru,
//          NEList.singleton
//            { pat = RT.MPFloat(5UL, 0.0); whenCondition = None; rhs = ru }
//        ))
//       (PT.EMatch(
//         9UL,
//         u,
//         [ { pat = PT.MPFloat(5UL, Positive, "0", "0")
//             whenCondition = Some u
//             rhs = u } ]
//        ),
//        RT.EMatch(
//          9UL,
//          ru,
//          NEList.singleton
//            { pat = RT.MPFloat(5UL, 0.0); whenCondition = Some ru; rhs = ru }
//        )) ]

// let testInfixProgramTypesToSerializedTypes =
//   testMany
//     "infix program types to serialized types"
//     PT2ST.Expr.toST
//     [ (PT.EInfix(
//         8UL,
//         PT.InfixFnCall(PT.ArithmeticPlus),
//         PT.EInt64(9UL, 6L),
//         PT.EInt64(10UL, 6L)
//        ),
//        ST.EInfix(
//          8UL,
//          ST.InfixFnCall(ST.ArithmeticPlus),
//          ST.EInt64(9UL, 6L),
//          ST.EInt64(10UL, 6L)
//        )) ]


let tests =
  testList
    "ProgramTypes"
    [ //testPipesToRuntimeTypes
    //testProgramTypesToRuntimeTypes
    //ptFQFnName
    //testInfixProgramTypesToSerializedTypes
    ]
