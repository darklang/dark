module Tests.OCamlInterop

open Expecto
open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

open LibExecution.ProgramTypes

let fuzzedTests =
  [ testListUsingProperty
      "OCamlInterop expr tests"
      FuzzTests.OCamlInterop.yojsonExprRoundtrip
      [ ("norail was copied wrong",
         EFnCall(0UL, FQFnName.parse "b/k/C::r_v1", [], NoRail))
        ("",
         EBinOp(
           0UL,
           FQFnName.parse "b/k/C::r_v1",
           ERecord(0UL, []),
           EVariable(0UL, ""),
           NoRail
         ))
        ("constructors were compared wrong",
         EMatch(
           0UL,
           EBlank 0UL,
           [ (PConstructor(0UL, "", [ PBool(0UL, true) ]), ENull 0UL) ]
         )) ]
    testListUsingProperty
      "OCamlInterop Yojson handler tests"
      FuzzTests.OCamlInterop.yojsonHandlerRoundtrip
      [ ("",
         { tlid = 0UL
           ast = EFnCall(0UL, FQFnName.parse "o/t/F::e_v1", [], NoRail)
           pos = { x = 0; y = 0 }
           spec =
             Handler.Worker("", { moduleID = 0UL; nameID = 0UL; modifierID = 0UL }) })
        ("",
         { tlid = 0UL
           pos = { x = 0; y = 0 }
           ast = EBool(0UL, false)
           spec =
             Handler.Cron(
               "",
               None,
               { moduleID = 0UL; nameID = 0UL; modifierID = 0UL }
             ) }) ]
    testListUsingProperty
      "queryStringToParams"
      FuzzTests.HttpClient.queryStringToParams
      [ "empty", ""
        "newline", "\n"
        "newline with value", "\n=6"
        "just equals", "="
        "equals and", "c=&"
        // "and equals and", "&=&" // OCAMLONLY
        "hash", "#"
        "hash with value", "#=6"
        "question mark", "?"
        "question mark with value", "?=6"
        "question mark later", "x=5&?"
        "question mark later with value", "x=5&?=6"
        "question mark surrounded", "x?x"
        "question mark surrounded with value", "x?x=6"
        "question mark surrounded later", "x=5&y?y"
        "question mark surrounded later with value", "x=5&y?y=6" ]
    testListUsingProperty
      "toQueryString"
      FuzzTests.HttpClient.queryToEncodedString
      [ "empty", [ "", [] ]
        "empty string value", [ "", [ "" ] ]
        "empty value", [ "a", [] ]
        "empty value and empty string value", [ "a", [ "" ] ] ]
    testListUsingProperty
      "queryToDval"
      FuzzTests.HttpClient.queryToDval
      [ "empty", [ "", [] ] ] ]

let tests = testList "ocamlInterop" fuzzedTests
