module Tests.OCamlInterop

open Expecto
open Prelude
open TestUtils.TestUtils
open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module Convert = LibExecution.OCamlTypes.Convert

// These are test that we've written fuzzers to test,
// but would also like to cover these specific cases.
//
// Most likely, we've encountered problems with these before,
// and want to ensure the issues don't come back up.
let fuzzedTests =
  [ testListUsingProperty
      "OCamlInterop expr tests"
      FuzzTests.OCamlInterop.yojsonExprRoundtrip
      [ ("norail was copied wrong",
         PT.EFnCall(0UL, PTParser.FQFnName.parse "b/k/C::r_v1", [], PT.NoRail))
        ("constructors were compared wrong",
         PT.EMatch(
           0UL,
           PT.EBlank 0UL,
           [ (PT.PConstructor(0UL, "", [ PT.PBool(0UL, true) ]), PT.ENull 0UL) ]
         )) ]

    testListUsingProperty
      "OCamlInterop Yojson handler tests"
      FuzzTests.OCamlInterop.yojsonHandlerRoundtrip
      [ ("",
         { tlid = 0UL
           ast =
             PT.EFnCall(0UL, PTParser.FQFnName.parse "o/t/F::e_v1", [], PT.NoRail)
           pos = { x = 0; y = 0 }
           spec =
             PT.Handler.Worker(
               "",
               { moduleID = 0UL; nameID = 0UL; modifierID = 0UL }
             ) })
        ("",
         { tlid = 0UL
           pos = { x = 0; y = 0 }
           ast = PT.EBool(0UL, false)
           spec =
             PT.Handler.Cron(
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
      [ "empty", [ "", [] ] ]

    testMany
      "dval2rt"
      (fun (name, dv) ->
        let converted = dv |> Convert.rt2ocamlDval |> Convert.ocamlDval2rt
        Expect.equalDval converted dv $"{name} dval2rt")
      (List.map (fun v -> v, ()) sampleDvals) ]


let tests = testList "ocamlInterop" fuzzedTests
