open Prelude

open Tester

let testRoundtrip = (decoder, encoder, name: string, v: 'a) =>
  test("roundtrip " ++ name, () => expect(v) |> toEqual(v |> encoder |> decoder))

let run = () => {
  describe("decoding", () => {
    module Decode = Json_decode_extended
    test("infinity", () =>
      expect(Decode.decodeString(RT.Dval.decode, `[ "DFloat", "Infinity" ]`)) |> toEqual(
        Belt.Result.Ok(RT.Dval.DFloat(Float.infinity)),
      )
    )
    test("negativeInfinity", () =>
      expect(Decode.decodeString(RT.Dval.decode, `[ "DFloat", "-Infinity" ]`)) |> toEqual(
        Belt.Result.Ok(RT.Dval.DFloat(Float.negativeInfinity)),
      )
    )
    test("notANumber", () => {
      switch Decode.decodeString(RT.Dval.decode, `[ "DFloat", "NaN" ]`) {
      | Ok(DFloat(flt)) => expect(Float.isNaN(flt)) |> toEqual(true)
      | _ => expect("A valid dfloat dval") |> toEqual("something invalid, or not a dfloat")
      }
    })
  })
  describe("dval misc tests", () => {
    describe("compatible with server JSON encoding", () => {
      test("obj uses list", () =>
        expect("[\"DObj\",{\"foo\":[\"DInt\",5]}]") |> toEqual(
          RT.Dval.obj(list{("foo", DInt(5L))}) |> RT.Dval.encode |> Js.Json.stringify,
        )
      )
      test("DHttpResponse shape", () =>
        expect("[\"DHttpResponse\",[\"Response\",401,[],[\"DNull\"]]]") |> toEqual(
          RT.Dval.DHttpResponse(Response(401L, list{}, DNull))
          |> RT.Dval.encode
          |> Js.Json.stringify,
        )
      )
    })
    describe("dval roundtrips", () => {
      open RT.Dval
      let rtDval = testRoundtrip(decode, encode)
      let id = UInt64.fromString("15223423459603010931")->Tc.Option.unwrapUnsafe
      rtDval("int", DInt(5L))
      rtDval("int_max_31_bits", DInt(1073741823L)) // 2^30-1
      rtDval("int_min_31_bits", DInt(-1073741824L)) // -2^30
      rtDval("int_max_32_bits", DInt(2147483647L)) // 2^31-1
      rtDval("int_min_32_bits", DInt(-2147483648L)) // 2^31-1
      rtDval("int_max_63_bits", DInt(4611686018427387903L)) // 2^62-1
      rtDval("int_min_63_bits", DInt(-4611686018427387904L)) // -2^62
      rtDval("int_max_64_bits", DInt(9223372036854775807L)) // 2^63-1
      rtDval("int_min_64_bits", DInt(-9223372036854775808L)) // -2^63
      rtDval("int_max_double", DInt(9007199254740992L)) // 2^53
      rtDval("int_min_double", DInt(-9007199254740992L)) // -2^53
      rtDval("obj", DObj(Belt.Map.String.fromArray([("foo", DInt(5L))])))
      rtDval("date", DDate("can be anything atm"))
      rtDval("incomplete", DIncomplete(SourceNone))
      rtDval("incomplete2", DIncomplete(SourceID(TLID.fromUInt64(id), ID.fromUInt64(id))))
      rtDval("float", DFloat(7.2))
      rtDval("true", DBool(true))
      rtDval("false", DBool(false))
      rtDval("tuple", DTuple(DInt(56L), DIncomplete(SourceNone), list{DInt(78L)}))
      rtDval("string", DStr("incredibly this was broken"))
      rtDval("null", DNull)
      rtDval("errorrail", DErrorRail(DInt(5L)))
      rtDval("db", DDB("Visitors"))
      rtDval("list", DList(list{DDB("Visitors"), DInt(4L)}))
      rtDval("redirect", DHttpResponse(Redirect("/home")))
      rtDval("httpresponse", DHttpResponse(Response(200L, list{}, DStr("success"))))
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
    let t = testRoundtrip(PT.Expr.decode, PT.Expr.encode)
    t("complex", FluidTestData.complexExpr)
  })

  describe("tipe", () => {
    let roundtrip = typ => typ |> DType.encode |> DType.decode

    test("tuple tipe roundtrips", () => {
      let tipe = DType.TTuple(TInt, TFloat, list{TIncomplete})

      expect(tipe |> roundtrip) |> toEqual(tipe)
    })
  })
}
