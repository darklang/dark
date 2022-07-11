open Prelude
open FluidShortcuts

open Tester


let run = () => {
  describe("dval", () => {
    let roundtrip = d => d |> Encoders.dval |> Decoders.dval

    test("tuple dval roundtrips", () => {
      let dval = DTuple(DInt(56), DIncomplete(SourceNone), list{DInt(78)})

      expect(dval |> roundtrip) |> toEqual(dval)
    })
  })

  describe("fluidExpr", () => {
    let roundtrip = expr => expr |> Encoders.fluidExpr |> Decoders.fluidExpr

    test("complexExpr roundtrips", () =>
      expect(FluidTestData.complexExpr |> roundtrip) |> toEqual(FluidTestData.complexExpr)
    )
    test("tuple expr roundtrips", () => {
      let expr = tuple(int(56), FluidExpression.newB(), list{int(78)})

      expect(expr |> roundtrip) |> toEqual(expr)
    })
  })

  describe("tipe", () => {
    let roundtrip = tipe => tipe |> Encoders.tipe |> Decoders.tipe

    test("tuple tipe roundtrips", () => {
      let tipe = TTuple(TInt, TFloat, list{TIncomplete})

      expect(tipe |> roundtrip) |> toEqual(tipe)
    })
  })
}
