open Prelude
open Json.Decode

// Dark

@val external stringify: Js.Json.t => string = "JSON.stringify"

let exception_ = (j): exception_ => {
  short: field("short", string, j),
  long: field("long", optional(string), j),
  exceptionType: field("tipe", string, j),
  actual: field("actual", optional(string), j),
  actualType: field("actual_tipe", optional(string), j),
  expected: field("expected", optional(string), j),
  result: field("result", optional(string), j),
  resultType: field("result_tipe", optional(string), j),
  info: field("info", strDict(string), j),
  workarounds: field("workarounds", list(string), j),
}

// Wrap JSON decoders using bs-json's format, into TEA's HTTP expectation format
let wrapExpect = (fn: Js.Json.t => 'a): (string => Tea.Result.t<'ok, string>) =>
  j =>
    try Ok(fn(Json.parseOrRaise(j))) catch {
    | e =>
      reportError("unexpected json", j)
      switch e {
      | DecodeError(e) | Json.ParseError(e) => Error(e)
      | e => Error(Exception.toString(e)->Option.unwrap(~default="wrapExpect error"))
      }
    }

// Wrap JSON decoders using bs-json's format, into TEA's JSON decoder format
let wrapDecoder = (fn: Js.Json.t => 'a): Tea.Json.Decoder.t<Js.Json.t, 'a> => Decoder(
  value =>
    try Tea_result.Ok(fn(value)) catch {
    | e =>
      reportError("undecodable json", value)
      switch e {
      | DecodeError(e) | Json.ParseError(e) => Tea_result.Error(e)
      | e =>
        Tea_result.Error(
          "Json error: " ++ Exception.toString(e)->Option.unwrap(~default="wrapDecoder error"),
        )
      }
    },
)
