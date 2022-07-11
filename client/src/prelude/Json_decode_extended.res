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

let uint64 = (j: Js.Json.t) =>
  if Js.typeof(j) == "string" {
    j->string->U.UInt64.ofString->Option.unwrapUnsafe
  } else {
    // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
    // 2^53. `float` here can be considered to be `int53`, since we know we're
    // getting whole integers as anything that doesn't fit in the integer portion of
    // a float is expected to be encoded as a string
    j->Json.Decode.float->Int64.of_float->U.UInt64.ofInt64
  }


