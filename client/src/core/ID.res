module T = {
  module Nested = {
    @ppx.deriving(show({with_path: false})) type rec t = ID(string)

    let compare = (ID(id1): t, ID(id2): t) => compare(id1, id2)
  }

  include Nested

  let fromString = (id: string) => ID(id)

  let toString = (ID(id): t) => id

  include Tablecloth.Comparator.Make(Nested)
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
}
