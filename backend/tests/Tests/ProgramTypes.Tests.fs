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

let ptFQFnName =
  testMany
    "ProgramTypes.FQFnName.ToString"
    (fun name -> name |> PT2RT.FQFnName.toRT |> RT.FQFnName.toString)
    [ (PTParser.FQFnName.stdlibFqName "" "++" 0), "++"
      (PTParser.FQFnName.stdlibFqName "" "!=" 0), "!="
      (PTParser.FQFnName.stdlibFqName "" "&&" 0), "&&"
      (PTParser.FQFnName.stdlibFqName "String" "append" 1), "String::append_v1" ]


let parseTests =
  let p = PTParser.FQFnName.parse

  testList
    "Parsing fn names"
    [ testListUsingProperty
        "FQFnName roundtrip tests"
        FuzzTests.FQFnName.ptRoundtrip
        [ ("", p "d6x3an030gugdr7t74k6k/s/F::pIi4tOCQujxl_v3")
          ("", p "uawmdntve/dolxb/X4Im::nsgKJGO_v1")
          ("", p "gqs/ekupo0/AmOCq7bpK9xBftJX1F4s::nFTxmaoJ8wAeshW0E_v1")
          ("no v0", p "gqs/ekupo0/AmOCq7bpK9xBftJX1F4s::nFTxmaoJ8wAeshW0E")
          ("", PT.FQFnName.User "someUserFn")
          ("capital letter", PT.FQFnName.User "SomeUserFn") // CLEANUP shouldn't be needed
          ("has _v2 in it", PT.FQFnName.User "myfunction_v2")
          ("has _v0 in it", PT.FQFnName.User "myfunction_v0")
          ("", p "String::toInt_v1")
          ("", PT.FQFnName.Stdlib { module_ = ""; function_ = "++"; version = 0 })
          ("", PT.FQFnName.Stdlib { module_ = ""; function_ = "+"; version = 0 })
          ("", p "-")
          ("", p "^") ]
      testMany
        "FQFnName parse tests"
        (fun name ->
          try
            Some(PTParser.FQFnName.parse name)
          with
          | _ -> None)
        [ ("equals",
           Some(
             PT.FQFnName.Stdlib { module_ = ""; function_ = "equals"; version = 0 }
           ))
          ("notEquals",
           Some(
             PT.FQFnName.Stdlib
               { module_ = ""; function_ = "notEquals"; version = 0 }
           ))
          ("emit_v1",
           Some(PT.FQFnName.Stdlib { module_ = ""; function_ = "emit"; version = 1 }))
          ("myFunction_v2", Some(PT.FQFnName.User "myFunction_v2"))
          ("myFunction_v0", Some(PT.FQFnName.User "myFunction_v0"))
          ("++",
           Some(PT.FQFnName.Stdlib { module_ = ""; function_ = "++"; version = 0 }))
          ("+",
           Some(PT.FQFnName.Stdlib { module_ = ""; function_ = "+"; version = 0 }))
          ("dark/stdlib/Twitter::sendText_v0",
           Some(
             PT.FQFnName.Package
               { owner = "dark"
                 package = "stdlib"
                 module_ = "Twitter"
                 function_ = "sendText"
                 version = 0 }
           ))
          ("dark/stdlib/Twitter::sendText",
           Some(
             PT.FQFnName.Package
               { owner = "dark"
                 package = "stdlib"
                 module_ = "Twitter"
                 function_ = "sendText"
                 version = 0 }
           ))
          ("paul56/random/Rand56om::string20_v57",
           Some(
             PT.FQFnName.Package
               { owner = "paul56"
                 package = "random"
                 module_ = "Rand56om"
                 function_ = "string20"
                 version = 57 }
           ))
          // CLEANUP these are all parsed as user functions now. We should get rid of
          // parsing and this will be much better
          // ("twitter::sendText_v0", None)
          // ("Twitter::send/Text_v0", None)
          // ("Dark/stdlib/Twitter::send/text_v0", None)
          // ("dark/stDlib/Twitter::send/text_v0", None)
          // ("dark/stDlib/Twitter::send/text_v0", None)
          // ("d+ark/stDlib/Twitter::send/text_v0", None)
          // ("paul56/ra*ndom/Rand56om::string20_v57", None)
          ] ]



let testPipesToRuntimeTypes =
  test "pipes to runtime types" {
    let actual = Parser.parseRTExpr "value.age |> (-) 2 |> (+) value.age |> (<) 3"

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
  let u = PT.EUnit(8UL)
  let ru = RT.EUnit(8UL)
  testMany
    "program types to runtime types"
    PT2RT.Expr.toRT
    [ PT.EFloat(7UL, Positive, "", "0"), RT.EFloat(7UL, 0.0)
      PT.EFloat(7UL, Positive, "0", ""), RT.EFloat(7UL, 0.0)
      PT.EFloat(7UL, Positive, "", ""), RT.EFloat(7UL, 0.0)
      (PT.EMatch(9UL, u, [ PT.MPFloat(5UL, Positive, "", ""), u ]),
       RT.EMatch(9UL, ru, [ RT.MPFloat(5UL, 0.0), ru ]))
      (PT.EMatch(9UL, u, [ PT.MPFloat(5UL, Positive, "0", ""), u ]),
       RT.EMatch(9UL, ru, [ RT.MPFloat(5UL, 0.0), ru ]))
      (PT.EMatch(9UL, u, [ PT.MPFloat(5UL, Positive, "", "0"), u ]),
       RT.EMatch(9UL, ru, [ RT.MPFloat(5UL, 0.0), ru ])) ]

let testInfixProgramTypesToSerializedTypes =
  testMany
    "infix program types to serialized types"
    PT2ST.Expr.toST
    [ (PT.EInfix(
        8UL,
        PT.InfixFnCall({ module_ = None; function_ = "+" }),
        PT.EInteger(9UL, 6),
        PT.EInteger(10UL, 6)
       ),
       ST.EInfix(
         8UL,
         ST.InfixFnCall({ module_ = None; function_ = "+" }),
         ST.EInteger(9UL, 6),
         ST.EInteger(10UL, 6)
       ))
      (PT.EInfix(
        8UL,
        PT.InfixFnCall({ module_ = Some "DateTime"; function_ = "<" }),
        PT.EInteger(9UL, 6),
        PT.EInteger(10UL, 6)
       ),
       ST.EInfix(
         8UL,
         ST.InfixFnCall({ module_ = Some("DateTime"); function_ = "<" }),
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
    [ parseTests
      testPipesToRuntimeTypes
      testProgramTypesToRuntimeTypes
      ptFQFnName
      testInfixProgramTypesToSerializedTypes
      testVersionedSerializedTypesToProgramTypes ]
