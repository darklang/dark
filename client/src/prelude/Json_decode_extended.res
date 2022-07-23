open Tc
include Json.Decode

@val external _stringify: Js.Json.t => string = "JSON.stringify"

let succeed = (any, _) => any

let index = (i, decode, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    decode(Caml.Array.unsafe_get(source, i))
  } else {
    raise(DecodeError("Expected array, got " ++ _stringify(json)))
  }

let map2 = (f, d1, d2, json) => f(d1(json), d2(json))

let map3 = (f, d1, d2, d3, json) => f(d1(json), d2(json), d3(json))

let map4 = (f, d1, d2, d3, d4, json) => f(d1(json), d2(json), d3(json), d4(json))

let map5 = (f, d1, d2, d3, d4, d5, json) => f(d1(json), d2(json), d3(json), d4(json), d5(json))

let variant0 = constructor => succeed(constructor)

let variant1 = (constructor, d1) => map(constructor, index(1, d1))

let variant2 = (constructor, d1, d2) => map2(constructor, index(1, d1), index(2, d2))

let variant3 = (constructor, d1, d2, d3) =>
  map3(constructor, index(1, d1), index(2, d2), index(3, d3))

let variant4 = (constructor, d1, d2, d3, d4) =>
  map4(constructor, index(1, d1), index(2, d2), index(3, d3), index(4, d4))

let variant5 = (constructor, d1, d2, d3, d4, d5) =>
  map5(constructor, index(1, d1), index(2, d2), index(3, d3), index(4, d4), index(5, d5))

let recordVariant3 = (constructor, (k1, d1), (k2, d2), (k3, d3)) =>
  map3(constructor, index(1, field(k1, d1)), index(1, field(k2, d2)), index(1, field(k3, d3)))

let variants = (decoders: list<(string, decoder<'a>)>): decoder<'a> =>
  index(0, string) |> andThen(str_constructor =>
    switch Belt.List.getAssoc(decoders, str_constructor, \"=") {
    | Some(decode) => decode
    | None =>
      let constructors = decoders |> List.map(~f=Tuple2.first) |> String.join(~sep=", ")

      \"@@"(
        raise,
        DecodeError("Got " ++ (str_constructor ++ (", expected one of " ++ constructors))),
      )
    }
  )

let result = (decoderOk: decoder<'ok>, decoderError: decoder<'error>): decoder<
  result<'ok, 'error>,
> =>
  variants(list{
    ("Ok", variant1(j => Ok(j), decoderOk)),
    ("Error", variant1(j => Error(j), decoderError)),
  })

let tryDecode2 = (try1, try2, json) =>
  try try1(json) catch {
  | DecodeError(_) => try2(json)
  }

let strDict = (decoder: Js.Json.t => 'a, json: Js.Json.t): Map.String.t<'a> =>
  dict(decoder, json) |> Js.Dict.entries |> Map.String.fromArray

let beltStrDict = (decoder: Js.Json.t => 'a, json: Js.Json.t): Belt.Map.String.t<'a> =>
  dict(decoder, json) |> Js.Dict.entries |> Belt.Map.String.fromArray

let strSet = json => json |> array(string) |> Set.String.fromArray

let decodeString = (decoder: decoder<'a>, str: string): Tc.Result.t<'a, string> =>
  try Ok(decoder(Json.parseOrRaise(str))) catch {
  | DecodeError(e) =>
    // Debug.loG ("json decoding error: '" ^ e ^ "'") str;
    Error(e)
  | Json.ParseError(e) =>
    // Debug.loG ("json parse error: '" ^ e ^ "'") str;
    Error(e)
  | _ =>
    // let errStr = Printexc.to_string e in
    // Debug.loG ("unknown json parsing error: '" ^ errStr ^ "'") str;
    Error(str)
  }

let int64 = (j: Js.Json.t) =>
  if Js.typeof(j) == "string" {
    Int64.of_string(string(j))
  } else {
    // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
    // 2^53. `float` here can be considered to be `int53`, since we know we're
    // getting whole integers as anything that doesn't fit in the integer portion of
    // a float is expected to be encoded as a string
    Int64.of_float(Json.Decode.float(j))
  }

let uint64 = (j: Js.Json.t): UInt64.t =>
  if Js.typeof(j) == "string" {
    j->string->UInt64.fromString->Option.unwrapUnsafe
  } else {
    // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
    // 2^53. `float` here can be considered to be `int53`, since we know we're
    // getting whole integers as anything that doesn't fit in the integer portion of
    // a float is expected to be encoded as a string
    j->Json.Decode.float->UInt64.fromFloat
  }

let tuple5 = (decodeA, decodeB, decodeC, decodeD, decodeE, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 5 {
      try (
        decodeA(Caml.Array.unsafe_get(source, 0)),
        decodeB(Caml.Array.unsafe_get(source, 1)),
        decodeC(Caml.Array.unsafe_get(source, 2)),
        decodeD(Caml.Array.unsafe_get(source, 3)),
        decodeE(Caml.Array.unsafe_get(source, 4)),
      ) catch {
      | DecodeError(msg) => \"@@"(raise, DecodeError(msg ++ "\n\tin tuple5"))
      }
    } else {
      \"@@"(raise, DecodeError(j`Expected array of length 5, got array of length $length`))
    }
  } else {
    \"@@"(raise, DecodeError("Expected array, got not-an-array"))
  }

module Base64 = {
  @deriving(abstract) type jsArrayBuffer = {byteLength: int}

  @deriving(abstract) type jsUint8Array

  @new external createUint8Array: jsArrayBuffer => jsUint8Array = "Uint8Array"

  @get_index external getUint8ArrayIdx: (jsUint8Array, int) => int = ""

  // @set_index external setUint8ArrayIdx: (jsUint8Array, int, int) => unit = ""

  // Note: unsafe. Wrap in bytes_from_base64url, which validates the input
  let dark_arrayBuffer_from_b64url = %raw(`
  function (base64) {
    // Modified version of https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js
    // Note that this version uses the url and filename safe alphabet instead of the standard b64 alphabet.
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

    // Use a lookup table to find the index.
    var lookup = new Uint8Array(256);
    for (var i = 0; i < chars.length; i++) {
      lookup[chars.charCodeAt(i)] = i;
    }


    var bufferLength = base64.length * 0.75, len = base64.length, i, p = 0, encoded1, encoded2, encoded3, encoded4;

    if (base64[base64.length - 1] === "=") {
      bufferLength--;
      if (base64[base64.length - 2] === "=") {
        bufferLength--;
      }
    }

    var arraybuffer = new ArrayBuffer(bufferLength),
    bytes = new Uint8Array(arraybuffer);

    for (i = 0; i < len; i+=4) {
      encoded1 = lookup[base64.charCodeAt(i)];
      encoded2 = lookup[base64.charCodeAt(i+1)];
      encoded3 = lookup[base64.charCodeAt(i+2)];
      encoded4 = lookup[base64.charCodeAt(i+3)];

      bytes[p++] = (encoded1 << 2) | (encoded2 >> 4);
      bytes[p++] = ((encoded2 & 15) << 4) | (encoded3 >> 2);
      bytes[p++] = ((encoded3 & 3) << 6) | (encoded4 & 63);
    }

    return arraybuffer;
  }
`)

  let _bytes_from_uint8Array = (input: jsArrayBuffer): Bytes.t => {
    let len = byteLengthGet(input)
    let bytes = Bytes.create(len)
    let reader = createUint8Array(input)
    for i in 0 to len - 1 {
      let char = getUint8ArrayIdx(reader, i)
      Bytes.unsafe_set(bytes, i, char_of_int(char))
    }
    bytes
  }

  exception Invalid_B64(string)

  let valid_rfc4648_b64_or_exn = (str: string) => {
    let rfc4648_section5_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789\\-_"

    // '=' isn't in the alphabet, but we allow it as padding
    if Regex.exactly(~re="[" ++ (rfc4648_section5_alphabet ++ ("=" ++ "]*")), str) {
      str
    } else {
      raise(Invalid_B64("Expected B64 input matching RFC4648 alphabet."))
    }
  }

  let bytes_from_base64url = (b64: string): Bytes.t =>
    b64 |> valid_rfc4648_b64_or_exn |> dark_arrayBuffer_from_b64url |> _bytes_from_uint8Array
}
let base64EncodedBytes: decoder<Bytes.t> = Json.Decode.map(
  Base64.bytes_from_base64url,
  Json.Decode.string,
)
