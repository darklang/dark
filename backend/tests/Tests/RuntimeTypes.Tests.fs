module Tests.RuntimeTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes

let dvalToType =
  testList
    "dvalToType"
    [ test "simple tuple" {
        let tpl =
          RT.Dval.DTuple(
            RT.Dval.DInt 1,
            RT.Dval.DString "two",
            [ RT.Dval.DFloat 3.14 ]
          )
        let actual = RT.Dval.toType tpl
        let expected =
          RT.DType.TTuple(RT.DType.TInt, RT.DType.TString, [ RT.DType.TFloat ])
        Expect.equal actual expected ""
      } ]

let dvalTypeMatches =
  testList
    "dvalTypeMatches"
    [ test "matching tuple" {
        let v =
          RT.Dval.DTuple(
            RT.Dval.DInt 1,
            RT.Dval.DString "two",
            [ RT.Dval.DFloat 3.14 ]
          )
        let tipe =
          RT.DType.TTuple(RT.DType.TInt, RT.DType.TString, [ RT.DType.TFloat ])

        Expect.isTrue (RT.Dval.typeMatches tipe v) ""
      }

      test "non-matching tuple" {
        let v =
          RT.Dval.DTuple(
            RT.Dval.DInt 1,
            RT.Dval.DString "two",
            [ RT.Dval.DFloat 3.14 ]
          )
        let tipe =
          RT.DType.TTuple(RT.DType.TInt, RT.DType.TString, [ RT.DType.TChar ])

        Expect.isFalse (RT.Dval.typeMatches tipe v) ""
      } ]

let tests = testList "RuntimeTypes" [ dvalToType ]
