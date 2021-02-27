module Tests.ProgramTypes

open Expecto
open Prelude
open TestUtils

module PT = LibBackend.ProgramTypes

let fuzzedTests =
  testList
    "Tests found by fuzzing"
    [ testListUsingProperty
        "FQFnName parse tests"
        FuzzTests.All.FQFnName.roundtrip

        (List.map
          PT.FQFnName.parse
          [ "d6x3an030gugdr7t74k6k/s/F::pIi4tOCQujxl_v3"
            "uawmdntve/dolxb/X4Im::nsgKJGO_v1"
            "gqs/ekupo0/AmOCq7bpK9xBftJX1F4s::nFTxmaoJ8wAeshW0E_v1" ]) ]

let tests = testList "ProgramTypes" [ fuzzedTests ]
