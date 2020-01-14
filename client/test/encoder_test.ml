open Tester

let run () =
  describe "fluidExpr" (fun () ->
      test "complexExpr" (fun () ->
          expect
            ( Fluid_test_data.complexExpr
            |> Encoders.fluidExpr
            |> Decoders.fluidExpr )
          |> toBe Fluid_test_data.complexExpr) ;
      ()) ;
  ()
