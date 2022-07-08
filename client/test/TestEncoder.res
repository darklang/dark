open Tester

let run = () => {
  describe("fluidExpr", () => {
    test("complexExpr", () =>
      expect(FluidTestData.complexExpr |> Encoders.fluidExpr |> Decoders.fluidExpr) |> toEqual(
        FluidTestData.complexExpr,
      )
    )
  })
}
