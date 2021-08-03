open Tester

let run = () => {
  describe("fluidExpr", () => {
    test("complexExpr", () =>
      expect(Fluid_test_data.complexExpr |> Encoders.fluidExpr |> Decoders.fluidExpr) |> toEqual(
        Fluid_test_data.complexExpr,
      )
    )
    ()
  })
  ()
}
