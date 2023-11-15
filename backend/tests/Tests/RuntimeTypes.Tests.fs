module Tests.RuntimeTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes

let dvalTypeMatches =
  testList
    "dvalTypeMatches"
    [ test "matching tuple" {
        let v =
          RT.Dval.DTuple(
            RT.Dval.DInt64 1,
            RT.Dval.DString "two",
            [ RT.Dval.DFloat 3.14 ]
          )
        let tipe =
          RT.TypeReference.TTuple(
            RT.TypeReference.TInt64,
            RT.TypeReference.TString,
            [ RT.TypeReference.TFloat ]
          )

        Expect.isTrue (RT.Dval.typeMatches tipe v) ""
      }

      test "non-matching tuple" {
        let v =
          RT.Dval.DTuple(
            RT.Dval.DInt64 1,
            RT.Dval.DString "two",
            [ RT.Dval.DFloat 3.14 ]
          )
        let tipe =
          RT.TypeReference.TTuple(
            RT.TypeReference.TInt64,
            RT.TypeReference.TString,
            [ RT.TypeReference.TChar ]
          )

        Expect.isFalse (RT.Dval.typeMatches tipe v) ""
      } ]

let tests = testList "RuntimeTypes" [ dvalTypeMatches ]
