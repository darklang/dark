open Prelude
open Json.Decode

// Dark

@val external stringify: Js.Json.t => string = "JSON.stringify"

let loadable = (decoder: Js.Json.t => 'a, j: Js.Json.t): loadable<'a> =>
  variants(
    list{
      ("LoadableSuccess", variant1(a => LoadableSuccess(a), decoder)),
      ("LoadableNotInitialized", variant0(LoadableNotInitialized)),
      ("LoadableLoading", variant1(a => LoadableLoading(a), optional(decoder))),
      ("LoadableError", variant1(a => LoadableError(a), string)),
    },
    j,
  )

let exception_ = (j): exception_ => {
  short: field("short", string, j),
  long: field("long", optional(string), j),
  exceptionTipe: field("tipe", string, j),
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
      | e => Error(Printexc.to_string(e))
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
      | e => Tea_result.Error("Json error: " ++ Printexc.to_string(e))
      }
    },
)
