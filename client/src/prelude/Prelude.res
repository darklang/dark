include Tc
include Belt_extended
include Types
include Recover

type jsonType = Js.Json.t

let debugger = (): unit =>
  %raw(`
  (function() {
    debugger;
  })()
`)

let reportError = ErrorReporting.reportError

// Every other module should have `open Prelude` as its first statement.
// You don't need to open/include Tc or Types, Prelude includes them.

module Exception = {
  let toString = (e: exn): option<string> => {
    switch e {
    | Js.Exn.Error(e) => Js.Exn.message(e)
    | e => e->Js.Exn.asJsExn->Option.andThen(~f=Js.Exn.message)
    }
  }
}

module Json = {
  exception ParseError = Json.ParseError

  let parseOrRaise = Json.parseOrRaise

  let parse = Json.parse

  let stringify = Json.stringify

  let stringifyAlways = (v: 'a) =>
    v
    ->Js.Json.stringifyAny
    ->Option.map(~f=Js.Json.parseExn)
    // Since this is used for debugging, the strings will sometimes be very long. If
    // there is no whitespace, then these strings can break the rendering, making it
    // scroll weirdly. So we go through this drama to ensure that doesn't happen
    ->Option.map(~f=v => Js.Json.stringifyWithSpace(v, 2))
    ->Option.unwrap(~default="Bad Json serialization")

  module Decode = Json_decode_extended
  module Encode = Json_encode_extended
}

// When we move to support larger numbers here, do not allow UInt64.max, as we use that as FluidToken.fakeid
let gid = ID.generate

let gtlid = TLID.generate

module Debug = {
  let log = (~f: 'a => 'b=x => Obj.magic(x), msg: string, data: 'a): 'a => {
    Js.log2(msg, f(data))
    data
  }

  let loG = (~f: 'a => 'b=x => Obj.magic(x), msg: string, data: 'a): unit => Js.log2(msg, f(data))
}
