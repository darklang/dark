module T = {
  module Nested = {
    @ppx.deriving(show({with_path: false})) type rec t = TLID(UInt64.t)
    let compare = (TLID(id1): t, TLID(id2): t) => UInt64.compare(id1, id2)
  }

  include Nested
  include Tablecloth.Comparator.Make(Nested)

  let fromInt = (i: int) => TLID(UInt64.fromInt(i))
  let fromUInt64 = (i: UInt64.t) => TLID(i)
  let toString = (TLID(id): t) => UInt64.toString(id)
  let fromString = (s: string) => UInt64.fromString(s)->Tc.Option.map(~f=fromUInt64)

  let encode = (TLID(tlid)) => Json_encode_extended.uint64(tlid)
  let decode = j => TLID(Json_decode_extended.uint64(j))
  let generate = (): t => fromInt(Js_math.random_int(0, 2147483647))
}

include T

module Set = {
  include Tc.Set.Of(T)

  let fromArray = a => Tablecloth.Set.Poly.fromArray(a)->Obj.magic

  let empty = fromArray([])

  let singleton = value => fromArray([value])

  let fromList = l => fromArray(Array.of_list(l))

  let decode = (j: Js.Json.t): t => {
    j |> Json.Decode.array(T.decode) |> fromArray
  }

  let encode = (t: t): Js.Json.t => {
    open Json.Encode
    t |> Tc.Set.toList |> list(T.encode)
  }
}

// CLEANUP: rename to map
module Dict = {
  include Tc.Map.Of(T)

  let fromArray = a => Tablecloth.Map.Poly.fromArray(a)->Obj.magic

  let empty = fromArray([])

  let singleton = (~key, ~value) => fromArray([(key, value)])

  let fromList = l => fromArray(Array.of_list(l))

  let decode = (decoder: Js.Json.t => 'value, j: Js.Json.t) => {
    let parse = (s: string): T.t => T.fromString(s)->Tablecloth.Option.unwrap(~default=T.fromInt(0))
    Json.Decode.dict(decoder, j)
    |> Js.Dict.entries
    |> Array.map(((k, v)) => (parse(k), v))
    |> fromArray
  }

  let encode = (f: 'value => Js.Json.t, t: t<'value>): Js.Json.t => {
    open Json.Encode
    t |> Tc.Map.toList |> List.map(((k, v)) => (T.toString(k), f(v))) |> object_
  }
}
