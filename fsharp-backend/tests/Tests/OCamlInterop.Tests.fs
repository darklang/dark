module Tests.OCamlInterop

open Expecto
open Prelude
open TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

open LibExecution.ProgramTypes

let fuzzedTests =
  [ testListUsingProperty
      "OCamlInterop expr tests"
      FuzzTests.All.OCamlInterop.yojsonExprRoundtrip
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
      FuzzTests.All.OCamlInterop.yojsonHandlerRoundtrip
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
      "OCamlInterop Binary handler tests"
      FuzzTests.All.OCamlInterop.binaryHandlerRoundtrip
      [] ]

let tests = testList "ocamlInterop" fuzzedTests
