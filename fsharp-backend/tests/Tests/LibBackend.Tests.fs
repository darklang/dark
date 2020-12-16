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

module PropertyTests =
  open PT

  let fuzzedTests =
    testList
      "Tests found by fuzzing"
      [ testListUsingProperty
          "FQFnName parse tests"
          PropertyTests.All.fqFnNameRoundtrip

          (List.map
            FQFnName.parse
            [ "d6x3an030gugdr7t74k6k/s/F::pIi4tOCQujxl_v3"
              "uawmdntve/dolxb/X4Im::nsgKJGO_v1"
              "gqs/ekupo0/AmOCq7bpK9xBftJX1F4s::nFTxmaoJ8wAeshW0E_v1" ])
        testListUsingProperty
          "OCamlInterop expr tests"
          PropertyTests.All.ocamlInteropYojsonExprRoundtrip
          [ EFnCall(0L, FQFnName.parse "b/k/C::r_v1", [], NoRail) // norail was copied wrong
            EBinOp(
              0L,
              FQFnName.parse "b/k/C::r_v1",
              ERecord(0L, []),
              EVariable(0L, ""),
              NoRail
            )
            EMatch(
              0L,
              EBlank 0L,
              // constructors were compared wrong
              [ (PConstructor(0L, "", [ PBool(0L, true) ]), ENull 0L) ]
            ) ]
        testListUsingProperty
          "OCamlInterop handler tests"
          PropertyTests.All.ocamlInteropYojsonHandlerRoundtrip
          [ { tlid = 0L
              ast = EFnCall(0L, FQFnName.parse "o/t/F::e_v1", [], NoRail)
              spec =
                Handler.Worker("", { moduleID = 0L; nameID = 0L; modifierID = 0L }) }

            { tlid = 0L
              ast = EBool(0L, false)
              spec =
                Handler.Cron("", "", { moduleID = 0L; nameID = 0L; modifierID = 0L }) } ] ]


let tests = testList "LibBackend" [ parserTests; PropertyTests.fuzzedTests ]
