module T = {
  module Nested = {
    @ppx.deriving(show({with_path: false})) type rec t = TLID(int64)
    let compare = (TLID(id1): t, TLID(id2): t) => compare(id1, id2)
  }

  include Nested
  include Tablecloth.Comparator.Make(Nested)

  let fromInt = (i : int) => TLID(UInt64.fromInt(i))
  let fromInt64 = (i : int64) => TLID(UInt64.fromInt64(i))

  let toString = (TLID(id): t) => UInt64.toString(id)
  let fromString = (s : string) =>
    switch UInt64.fromString(s) {
      | None => None
      | Some(i) => Some(TLID(i))
    }

  let encode = (TLID(tlid)) => Json_encode_extended.uint64(tlid)
  let decode = (j) => TLID(Json_decode_extended.uint64(j))

}

include T

module Set = {
  include Tc.Set.Of(T)

  let pp = Tc.Set.pp(T.pp)

  let fromArray = a => Tablecloth.Set.Poly.fromArray(a)->Obj.magic

  let empty = fromArray([])

  let singleton = value => fromArray([value])

  let fromList = l => fromArray(Array.of_list(l))
}

// CLEANUP: rename to map
module Dict = {
  include Tc.Map.Of(T)

  let pp = (
    valueFormatter: (Format.formatter, 'value) => unit,
    fmt: Format.formatter,
    map: t<'value>,
  ): unit => Tc.Map.pp(T.pp, valueFormatter, fmt, map)

  let fromArray = a => Tablecloth.Map.Poly.fromArray(a)->Obj.magic

  let empty = fromArray([])

  let singleton = (~key, ~value) => fromArray([(key, value)])

  let fromList = l => fromArray(Array.of_list(l))

  let decode = (decoder: Js.Json.t => 'value, j: Js.Json.t) => {
    let parse = (s:string) : T.t => T.fromString(s)->Tablecloth.Option.unwrap(~default=T.fromInt(0))
    Json.Decode.dict(decoder, j)
    |> Js.Dict.entries
    |> Array.map(((k, v)) => (parse(k), v))
    |> fromArray
  }
}

