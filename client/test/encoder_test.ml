open Tester

let run () =
  describe "fluidExpr" (fun () ->
      test "complexExpr" (fun () ->
          expect
            ( Fluid_test_data.complexExpr
            |> Encoders.fluidExpr
            |> Decoders.fluidExpr )
          |> toEqual Fluid_test_data.complexExpr) ;
      ()) ;
  ()
