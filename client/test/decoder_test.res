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

    // Committing broken - NaN != NaN, so we'll have to use some other strategy
    test("notANumber", () => {
      let decoded = Decode.decodeString(Decoders.dval, `[ "DFloat", "NaN" ]`)

      // If we can extract the float from `extractedDval` somehow, we can call
      // `Float.isNan` on `extractedFloat`, and `|> toEqual(true)`
      //let extractedDval = Belt.Result.getWithDefault(decoded, DNull)
      // let (DFloat extractedFloat) = extractedDval

      expect(decoded)
      |> toEqual(Belt.Result.Ok(DFloat(Float.nan)))
      }
    )
    ()
  })
  ()
}
