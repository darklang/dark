module Tests.ProgramTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
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
      (PTParser.FQFnName.stdlibFqName "" "toString" 0), "toString"
      (PTParser.FQFnName.stdlibFqName "String" "append" 1), "String::append_v1" ]


let parseTests =
  let p = PTParser.FQFnName.parse
  let User = RT.FQFnName.User
  let Stdlib = RT.FQFnName.Stdlib
  let Package = RT.FQFnName.Package

  testList
    "Parsing fn names"
    [ testListUsingProperty
        "FQFnName roundtrip tests"
        FuzzTests.FQFnName.ptRoundtrip
        [ ("", p "d6x3an030gugdr7t74k6k/s/F::pIi4tOCQujxl_v3")
          ("", p "uawmdntve/dolxb/X4Im::nsgKJGO_v1")
          ("", p "gqs/ekupo0/AmOCq7bpK9xBftJX1F4s::nFTxmaoJ8wAeshW0E_v1")
          ("no v0", p "gqs/ekupo0/AmOCq7bpK9xBftJX1F4s::nFTxmaoJ8wAeshW0E")
          ("",
           PT.FQFnName.Stdlib { module_ = ""; function_ = "toString"; version = 0 })
          ("", PT.FQFnName.User "someUserFn")
          ("capital letter", PT.FQFnName.User "SomeUserFn") // CLEANUP shouldn't be needed
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
        [ ("toString",
           Some(
             PT.FQFnName.Stdlib { module_ = ""; function_ = "toString"; version = 0 }
           ))
          ("toRepr",
           Some(
             PT.FQFnName.Stdlib { module_ = ""; function_ = "toRepr"; version = 0 }
           ))
          ("equals",
           Some(
             PT.FQFnName.Stdlib { module_ = ""; function_ = "equals"; version = 0 }
           ))
          ("notEquals",
           Some(
             PT.FQFnName.Stdlib
               { module_ = ""; function_ = "notEquals"; version = 0 }
           ))
          ("assoc",
           Some(
             PT.FQFnName.Stdlib { module_ = ""; function_ = "assoc"; version = 0 }
           ))
          ("dissoc",
           Some(
             PT.FQFnName.Stdlib { module_ = ""; function_ = "dissoc"; version = 0 }
           ))
          ("toForm",
           Some(
             PT.FQFnName.Stdlib { module_ = ""; function_ = "toForm"; version = 0 }
           ))
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

let tests =
  testList "ProgramTypes" [ parseTests; testPipesToRuntimeTypes; ptFQFnName ]
