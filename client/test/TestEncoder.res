open Prelude
open FluidShortcuts

open Tester

let testRoundtrip = (decoder, encoder, name: string, v: 'a) =>
  test("roundtrip " ++ name, () => expect(v) |> toEqual(v |> encoder |> decoder))

let run = () => {
  describe("dval", () => {
    let roundtrip = d => d |> Encoders.ocamlDval |> Decoders.ocamlDval

    test("tuple dval roundtrips", () => {
      let dval = DTuple(DInt(56L), DIncomplete(SourceNone), list{DInt(78L)})

      expect(dval |> roundtrip) |> toEqual(dval)
    })
  })

  describe("tlid roundtrips", () => {
    let t = testRoundtrip(TLID.decode, TLID.encode)
    t("zero", TLID.fromInt(0))
    t("one", TLID.fromInt(1))
    t("two", TLID.fromInt(2))
    t("int_max_31_bits", TLID.fromInt(1073741823)) // 2^30-1
    t("int_max_32_bits", TLID.fromInt(2147483647)) // 2^31-1
    t("int_max_63_bits", TLID.fromString("4611686018427387903")->Option.unwrapUnsafe) // 2^62-1
    t("int_max_64_bits", TLID.fromString("9223372036854775807")->Option.unwrapUnsafe) // 2^63-1
    t("int_max_double", TLID.fromString("9007199254740992")->Option.unwrapUnsafe) // 2^53

    t("above uint63max", TLID.fromString("15223423459603010931")->Option.unwrapUnsafe)
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
