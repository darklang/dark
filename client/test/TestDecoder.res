open Tester
open Prelude

module Decode = Json_decode_extended

include Json.Decode

let run = () => {
  describe("decoding", () => {
    test("infinity", () =>
      expect(Decode.decodeString(Decoders.dval, `[ "DFloat", "Infinity" ]`)) |> toEqual(
        Belt.Result.Ok(DFloat(Float.infinity)),
      )
    )
    test("negativeInfinity", () =>
      expect(Decode.decodeString(Decoders.dval, `[ "DFloat", "-Infinity" ]`)) |> toEqual(
        Belt.Result.Ok(DFloat(Float.negativeInfinity)),
      )
    )
    test("notANumber", () => {
      switch Decode.decodeString(Decoders.dval, `[ "DFloat", "NaN" ]`) {
      | Ok(DFloat(flt)) => expect(Float.isNaN(flt)) |> toEqual(true)
      | _ => expect("A valid dfloat dval") |> toEqual("something invalid, or not a dfloat")
      }
    })
  })
}
