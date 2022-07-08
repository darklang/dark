module T = {
  module Nested = {
    @ppx.deriving(show({with_path: false})) type rec t = TLID(string)
    let compare = (TLID(id1): t, TLID(id2): t) => compare(id1, id2)
  }

  include Nested
  include Tablecloth.Comparator.Make(Nested)

  let fromString = (tlid: string) => TLID(tlid)

  let toString = (TLID(tlid): t) => tlid

  let encode = (tlid) => Json.Encode.string(toString(tlid))

  let decode = (j) => fromString(Json.Decode.string(j))
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
}

