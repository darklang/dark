open Tester
open Prelude

module Decode = Json_decode_extended

include Json.Decode

let run = () => {
  describe("decoding", () => {
    test("infinityAndNaN", () => {
      let expr = `["Ok", {
          "1818290858": [
              "ExecutedResult",
              [
                  "DObj",
                  {
                      "a": [ "DFloat", "NaN" ],
                      "b": [ "DFloat", "Infinity" ],
                      "c": [ "DFloat", 5.6 ]
                  }
              ]
          ]
      }]`

      let expectedResult = Belt.Map.String.fromArray([("1818290858", ExecutedResult(Dval.obj(list{
        ("a", DFloat(Float.nan)),
        ("b", DFloat(Float.infinity)),
        ("c", DFloat(5.6)),
      })))])

      expect(Decode.result(Decoders.intermediateResultStore, Decode.string, Json.parseOrRaise(expr)))
      |> toEqual(Belt.Result.Ok(expectedResult))
      }
    )
    ()
  })
  ()
}
