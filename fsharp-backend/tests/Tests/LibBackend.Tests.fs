module Tests.LibBackend

open Expecto
open Prelude

module PT = LibBackend.ProgramSerialization.ProgramTypes

open LibBackend.ProgramSerialization.ProgramTypes.Shortcuts

let parserTests =
  let t name testStr expectedExpr =
    testTask name {
      let source = FSharpToExpr.parse testStr
      let actualProg = FSharpToExpr.convertToExpr source

      return
        (Expect.isTrue
          (actualProg.testEqualIgnoringIDs (expectedExpr))
          $"{actualProg}\n\n=\n\n{expectedExpr}")
    }

  testList
    "Parser tests"
    [ t
        "pipe without expr"
        "(let x = 5\nx |> List.map_v0 5)"
        (eLet
          "x"
          (eInt 5)
          (ePipe (eVar "x") (eFn "List" "map" 0 [ (ePipeTarget ()); eInt 5 ]) []))
      t
        "simple expr"
        "(5 + 3) == 8"
        (eBinOp "" "==" 0 (eBinOp "" "+" 0 (eInt 5) (eInt 3)) (eInt 8))
      t "lambdas with 2 args" "fun x y -> 8" (eLambda [ "x"; "y" ] (eInt 8))
      t "lambdas with 3 args" "fun x y z -> 8" (eLambda [ "x"; "y"; "z" ] (eInt 8))
      t
        "lambdas with 4 args"
        "fun a b c d -> 8"
        (eLambda [ "a"; "b"; "c"; "d" ] (eInt 8)) ]

// Allow reusing property-test definitions with test cases found by fuzzing
let testListUsingProperty (name : string) (prop : 'a -> bool) (list : 'a list) =
  testList
    name
    (List.map
      (fun testCase ->
        testTask $"{name} {testCase}" { return (Expect.isTrue (prop testCase) "") })
      list)

module FuzzTests =
  open PT

  let fuzzedTests =
    testList
      "Tests found by fuzzing"
      [ testListUsingProperty
          "FQFnName parse tests"
          FuzzTests.All.fqFnNameRoundtrip

          (List.map
            FQFnName.parse
            [ "d6x3an030gugdr7t74k6k/s/F::pIi4tOCQujxl_v3"
              "uawmdntve/dolxb/X4Im::nsgKJGO_v1"
              "gqs/ekupo0/AmOCq7bpK9xBftJX1F4s::nFTxmaoJ8wAeshW0E_v1" ])
        testListUsingProperty
          "OCamlInterop expr tests"
          FuzzTests.All.ocamlInteropYojsonExprRoundtrip
          [ EFnCall(0UL, FQFnName.parse "b/k/C::r_v1", [], NoRail) // norail was copied wrong
            EBinOp(
              0UL,
              FQFnName.parse "b/k/C::r_v1",
              ERecord(0UL, []),
              EVariable(0UL, ""),
              NoRail
            )
            EMatch(
              0UL,
              EBlank 0UL,
              // constructors were compared wrong
              [ (PConstructor(0UL, "", [ PBool(0UL, true) ]), ENull 0UL) ]
            ) ]
        testListUsingProperty
          "OCamlInterop Yojson handler tests"
          FuzzTests.All.ocamlInteropYojsonHandlerRoundtrip
          [ { tlid = 0UL
              ast = EFnCall(0UL, FQFnName.parse "o/t/F::e_v1", [], NoRail)
              pos = { x = 0; y = 0 }
              spec =
                Handler.Worker(
                  "",
                  { moduleID = 0UL; nameID = 0UL; modifierID = 0UL }
                ) }

            { tlid = 0UL
              pos = { x = 0; y = 0 }
              ast = EBool(0UL, false)
              spec =
                Handler.Cron(
                  "",
                  "",
                  { moduleID = 0UL; nameID = 0UL; modifierID = 0UL }
                ) } ]
        testListUsingProperty
          "OCamlInterop Binary handler tests"
          FuzzTests.All.ocamlInteropBinaryHandlerRoundtrip
          [ { tlid = 0UL
              ast = PT.EPipeTarget 0UL
              pos = { x = 0; y = 0 }
              spec =
                PT.Handler.OldWorker(
                  "",
                  "",
                  { moduleID = 0UL; nameID = 0UL; modifierID = 0UL }
                ) } ] ]


let tests = testList "LibBackend" [ parserTests; FuzzTests.fuzzedTests ]
