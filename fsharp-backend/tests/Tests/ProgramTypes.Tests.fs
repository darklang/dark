module Tests.ProgramTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module S = LibExecution.Shortcuts

let rtFQFnName =
  testMany
    "FQFnName.ToString"
    string
    [ (RT.FQFnName.stdlibFqName "" "++" 0), "++"
      (RT.FQFnName.stdlibFqName "" "!=" 0), "!="
      (RT.FQFnName.stdlibFqName "" "&&" 0), "&&"
      (RT.FQFnName.stdlibFqName "" "toString" 0), "toString"
      (RT.FQFnName.stdlibFqName "String" "append" 1), "String::append_v1" ]

// TODO parsing function names from OCaml

let ptFQFnName =
  testMany
    "ProgramTypes.FQFnName.ToString"
    string
    [ (PT.FQFnName.stdlibFqName "" "++" 0), "++"
      (PT.FQFnName.stdlibFqName "" "!=" 0), "!="
      (PT.FQFnName.stdlibFqName "" "&&" 0), "&&"
      (PT.FQFnName.stdlibFqName "" "toString" 0), "toString"
      (PT.FQFnName.stdlibFqName "String" "append" 1), "String::append_v1" ]


let parseTests =
  let p = PT.FQFnName.parse
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
          ("", Stdlib { module_ = ""; function_ = "toString"; version = 0 })
          ("", User "someUserFn")
          ("capital letter", User "SomeUserFn") // CLEANUP shouldn't be needed
          ("", p "String::toInt_v1")
          ("", Stdlib { module_ = ""; function_ = "++"; version = 0 })
          ("", Stdlib { module_ = ""; function_ = "+"; version = 0 })
          ("", p "-")
          ("", p "^") ]
      testMany
        "FQFnName parse tests"
        (fun name ->
          try
            Some(PT.FQFnName.parse name)
          with
          | _ -> None)
        [ ("toString",
           Some(Stdlib { module_ = ""; function_ = "toString"; version = 0 }))
          ("toRepr", Some(Stdlib { module_ = ""; function_ = "toRepr"; version = 0 }))
          ("equals", Some(Stdlib { module_ = ""; function_ = "equals"; version = 0 }))
          ("notEquals",
           Some(Stdlib { module_ = ""; function_ = "notEquals"; version = 0 }))
          ("assoc", Some(Stdlib { module_ = ""; function_ = "assoc"; version = 0 }))
          ("dissoc", Some(Stdlib { module_ = ""; function_ = "dissoc"; version = 0 }))
          ("toForm", Some(Stdlib { module_ = ""; function_ = "toForm"; version = 0 }))
          ("++", Some(Stdlib { module_ = ""; function_ = "++"; version = 0 }))
          ("+", Some(Stdlib { module_ = ""; function_ = "+"; version = 0 }))
          ("dark/stdlib/Twitter::sendText_v0",
           Some(
             Package
               { owner = "dark"
                 package = "stdlib"
                 module_ = "Twitter"
                 function_ = "sendText"
                 version = 0 }
           ))
          ("dark/stdlib/Twitter::sendText",
           Some(
             Package
               { owner = "dark"
                 package = "stdlib"
                 module_ = "Twitter"
                 function_ = "sendText"
                 version = 0 }
           ))
          ("paul56/random/Rand56om::string20_v57",
           Some(
             Package
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
  testList
    "ProgramTypes"
    [ parseTests; testPipesToRuntimeTypes; rtFQFnName; ptFQFnName ]
