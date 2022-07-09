open Prelude
open Tester

let testRoundtrip = (decoder, encoder, name: string, v: 'a) =>
  test("roundtrip " ++ name, () => expect(v) |> toEqual(v |> encoder |> decoder))

let rtDval = testRoundtrip(Decoders.ocamlDval, Encoders.ocamlDval)

let run = () => {
  describe("compatible with server JSON encoding", () => {
    test("obj uses list", () =>
      expect("[\"DObj\",{\"foo\":[\"DInt\",5]}]") |> toEqual(
        DObj(Belt.Map.String.fromArray([("foo", DInt(5L))]))
        |> Encoders.ocamlDval
        |> Js.Json.stringify,
      )
    )
    test("dresp shape", () =>
      expect("[\"DResp\",[[\"Response\",401,[]],[\"DNull\"]]]") |> toEqual(
        DResp(Response(401, list{}), DNull) |> Encoders.ocamlDval |> Js.Json.stringify,
      )
    )
    describe("roundtrips", () => {
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
      rtDval("incomplete2", DIncomplete(SourceId(TLID.parse("14219007199254740993"), ID.parse("14219007199254740993"))))
      rtDval("float", DFloat(7.2))
      rtDval("true", DBool(true))
      rtDval("false", DBool(false))
      rtDval("string", DStr("incredibly this was broken"))
      rtDval("null", DNull)
      rtDval("errorrail", DErrorRail(DInt(5L)))
      rtDval("db", DDB("Visitors"))
      rtDval("list", DList([DDB("Visitors"), DInt(4L)]))
      rtDval("redirect", DResp(Redirect("/home"), DNull))
      rtDval("httpresponse", DResp(Response(200, list{}), DStr("success")))
      ()
    })
    ()
  })
  ()
}
