module Tests.OCamlTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module OT = LibExecution.OCamlTypes
module ORT = OT.RuntimeT

let testSomething =
  test "EApply(ELambda .. ) converts to ORT correctly" {
    // This test was inspired by a rollbar caused by us being unprepared for
    // such a conversion during Analysis.

    let expected = ORT.ELambda(0UL, [ (0UL, "f") ], ORT.ENull(0UL))

    let actual =
      RT.EApply(
        0UL,
        RT.ELambda(0UL, [ (0UL, "f") ], RT.ENull(0UL)),
        [],
        RT.InPipe 0UL,
        RT.NoRail
      )
      |> OT.Convert.rt2ocamlExpr

    Expect.equal actual expected "Could not convert, or converted incorrectly"
  }

let tests = testList "ocamlTypes" [ testSomething ]
