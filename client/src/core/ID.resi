module T: {
  @ppx.deriving(show({with_path: false})) type rec t = ID(int64)

  type identity

  let comparator: Tablecloth.Comparator.t<t, identity>
  let cmp: (t, t) => int

  let toString: t => string
  let fromString: string => option<t>
  let fromInt: int => t
  let fromInt64: int64 => t

  let encode: t => Js.Json.t
  let decode: Js.Json.t => t
}

include module type of {
  include T
}

module Set: {
  include module type of Tc.Set.Of(T)

  let pp: (Format.formatter, t) => unit

  let empty: t

  let singleton: T.t => t

  let fromArray: array<T.t> => t

  let fromList: list<T.t> => t
}

module Map: {
  include module type of Tc.Map.Of(T)

  let pp: ((Format.formatter, 'value) => unit, Format.formatter, t<'value>) => unit

  let empty: t<'value>

  let singleton: (~key: T.t, ~value: 'value) => t<'value>

  let fromArray: array<(T.t, 'value)> => t<'value>

  let fromList: list<(T.t, 'value)> => t<'value>

  let decode: (Js.Json.t => 'value, Js.Json.t) => t<'value>
}
