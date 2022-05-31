open Tester
open Prelude

module Decode = Json_decode_extended

include Json.Decode

let run = () => {
  describe("decoding", () => {
    test("infinity", () =>
      expect(Decode.decodeString(Decoders.dval,`[ "DFloat", "Infinity" ]`))
      |> toEqual(Belt.Result.Ok(DFloat(Float.infinity)))
    )
    test("negativeInfinity", () =>
      expect(Decode.decodeString(Decoders.dval,`[ "DFloat", "-Infinity" ]`))
      |> toEqual(Belt.Result.Ok(DFloat(Float.negativeInfinity)))
    )
    // TODO: Test NaN
    ()
  })
  ()
}
