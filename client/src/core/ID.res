module T = {
  module Nested = {
    @ppx.deriving(show({with_path: false})) type rec t = ID(int64)
    let compare = (ID(id1): t, ID(id2): t) => compare(id1, id2)
    let cmp = compare
  }

  include Nested
  include Tablecloth.Comparator.Make(Nested)

  let fromInt = (i : int) => ID(UInt64.fromInt(i))
  let fromInt64 = (i : int64) => ID(UInt64.fromInt64(i))

  let toString = (ID(id): t) => UInt64.toString(id)
  let fromString = (s : string) =>
    switch UInt64.fromString(s) {
      | None => None
      | Some(i) => Some(ID(i))
    }


  let encode = (ID(id)) => Json_encode_extended.uint64(id)
  let decode = (j) => ID(Json_decode_extended.uint64(j))
}

include T

module Set = {
  include Tc.Set.Of(T)

  let pp = Tc.Set.pp(T.pp)

  let fromArray = a => Tc.Set.fromArray(module(T), a)

  let empty = Tc.Set.empty(module(T))

  let singleton = value => Tc.Set.singleton(module(T), value)

  let fromList = l => Tc.Set.fromList(module(T), l)
}

module Map = {
  include Tc.Map.Of(T)

  let pp = (
    valueFormatter: (Format.formatter, 'value) => unit,
    fmt: Format.formatter,
    map: t<'value>,
  ): unit => Tc.Map.pp(T.pp, valueFormatter, fmt, map)

  let fromArray = a => Tablecloth.Map.Poly.fromArray(a)->Obj.magic

  let empty = fromArray([])

  let singleton = (~key, ~value) => Tc.Map.singleton(module(T), ~key, ~value)

  let fromList = l => Tc.Map.fromList(module(T), l)

  let decode = (decoder: Js.Json.t => 'value, j: Js.Json.t) => {
    let parse = (s:string) : T.t => T.fromString(s)->Tablecloth.Option.unwrap(~default=T.fromInt(0))
    Json.Decode.dict(decoder, j)
    |> Js.Dict.entries
    |> Array.map(((k, v)) => (parse(k), v))
    |> fromArray
  }
}
