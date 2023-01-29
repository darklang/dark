module Tests.ProgramTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module ST = LibBinarySerialization.SerializedTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module ST2PT = LibBinarySerialization.SerializedTypesToProgramTypes
module PT2ST = LibBinarySerialization.ProgramTypesToSerializedTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module S = TestUtils.RTShortcuts
module FQFnNameParser = TestUtils.FQFnNameParser

let ptFQFnName =
  testMany
    "ProgramTypes.FQFnName.ToString"
    (fun name -> name |> PT2RT.FQFnName.toRT |> RT.FQFnName.toString)
    [ (FQFnNameParser.stdlibFqName "" "++" 0), "++"
      (FQFnNameParser.stdlibFqName "" "!=" 0), "!="
      (FQFnNameParser.stdlibFqName "" "&&" 0), "&&"
      (FQFnNameParser.stdlibFqName "" "toString" 0), "toString"
      (FQFnNameParser.stdlibFqName "String" "append" 1), "String::append_v1" ]


let testPipesToRuntimeTypes =
  test "pipes to runtime types" {
    let actual =
      TestUtils.FSharpToExpr.parseRTExpr
        "value.age |> (-) 2 |> (+) value.age |> (<) 3"

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

let testProgramTypesToRuntimeTypes =
  let b = PT.EBlank(8UL)
  let rb = RT.EBlank(8UL)
  testMany
    "program types to runtime types"
    PT2RT.Expr.toRT
    [ PT.EFloat(7UL, Positive, "", "0"), RT.EFloat(7UL, 0.0)
      PT.EFloat(7UL, Positive, "0", ""), RT.EFloat(7UL, 0.0)
      PT.EFloat(7UL, Positive, "", ""), RT.EFloat(7UL, 0.0)
      (PT.EMatch(9UL, b, [ PT.MPFloat(5UL, Positive, "", ""), b ]),
       RT.EMatch(9UL, rb, [ RT.MPFloat(5UL, 0.0), rb ]))
      (PT.EMatch(9UL, b, [ PT.MPFloat(5UL, Positive, "0", ""), b ]),
       RT.EMatch(9UL, rb, [ RT.MPFloat(5UL, 0.0), rb ]))
      (PT.EMatch(9UL, b, [ PT.MPFloat(5UL, Positive, "", "0"), b ]),
       RT.EMatch(9UL, rb, [ RT.MPFloat(5UL, 0.0), rb ])) ]

// We didn't use a special infix type in serialized types, so check it converts OK
let testInfixSerializedTypesToProgramTypes =
  testMany
    "serialized infix types to program types"
    ST2PT.Expr.toPT
    [ (ST.EDeprecatedBinOp(
        8UL,
        ST.FQFnName.Stdlib { module_ = ""; function_ = "+"; version = 0 },
        ST.EInteger(9UL, 6),
        ST.EInteger(10UL, 6),
        ST.NoRail
       ),
       PT.EInfix(
         8UL,
         PT.InfixFnCall({ module_ = None; function_ = "+" }, PT.NoRail),
         PT.EInteger(9UL, 6),
         PT.EInteger(10UL, 6)
       ))
      (ST.EDeprecatedBinOp(
        8UL,
        ST.FQFnName.Stdlib { module_ = "Date"; function_ = "<"; version = 0 },
        ST.EInteger(9UL, 6),
        ST.EInteger(10UL, 6),
        ST.NoRail
       ),
       PT.EInfix(
         8UL,
         PT.InfixFnCall({ module_ = Some "Date"; function_ = "<" }, PT.NoRail),
         PT.EInteger(9UL, 6),
         PT.EInteger(10UL, 6)
       )) ]

let testInfixProgramTypesToSerializedTypes =
  testMany
    "infix program types to serialized types"
    PT2ST.Expr.toST
    [ (PT.EInfix(
        8UL,
        PT.InfixFnCall({ module_ = None; function_ = "+" }, PT.NoRail),
        PT.EInteger(9UL, 6),
        PT.EInteger(10UL, 6)
       ),
       ST.EInfix(
         8UL,
         ST.InfixFnCall({ module_ = None; function_ = "+" }, ST.NoRail),
         ST.EInteger(9UL, 6),
         ST.EInteger(10UL, 6)
       ))
      (PT.EInfix(
        8UL,
        PT.InfixFnCall({ module_ = Some "Date"; function_ = "<" }, PT.NoRail),
        PT.EInteger(9UL, 6),
        PT.EInteger(10UL, 6)
       ),
       ST.EInfix(
         8UL,
         ST.InfixFnCall({ module_ = Some("Date"); function_ = "<" }, ST.NoRail),
         ST.EInteger(9UL, 6),
         ST.EInteger(10UL, 6)
       )) ]

/// We have functions that were written as user functions, but accidentally
/// converted to StdLibFns before being saved to the DB
let testVersionedSerializedTypesToProgramTypes =
  testMany
    "versioned user functions migrated"
    ST2PT.FQFnName.toPT
    [ ST.FQFnName.Stdlib { module_ = ""; function_ = "myFunction"; version = 2 },
      (PT.FQFnName.User "myFunction_v2")
      ST.FQFnName.Stdlib { module_ = ""; function_ = "myFunction"; version = 0 },
      (PT.FQFnName.User "myFunction_v0") ]


let tests =
  testList
    "ProgramTypes"
    [ testPipesToRuntimeTypes
      testProgramTypesToRuntimeTypes
      ptFQFnName
      testInfixSerializedTypesToProgramTypes
      testInfixProgramTypesToSerializedTypes
      testVersionedSerializedTypesToProgramTypes ]
