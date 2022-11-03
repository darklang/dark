module T: {
  @unboxed type rec t = TLID(UInt64.t)

  type identity

  let comparator: Tablecloth.Comparator.t<t, identity>

  let toString: t => string
  let fromString: string => option<t>
  let fromInt: int => t
  let fromUInt64: UInt64.t => t

  let encode: t => Js.Json.t
  let decode: Js.Json.t => t
  let generate: unit => t
}

include module type of {
  include T
}

module Set: {
  include module type of Tc.Set.Of(T)

  let empty: t

  let singleton: T.t => t

  let fromArray: array<T.t> => t

  let fromList: list<T.t> => t

  let decode: Js.Json.t => t

  let encode: t => Js.Json.t
}

module Dict: {
  include module type of Tc.Map.Of(T)

  let empty: t<'value>

  let singleton: (~key: T.t, ~value: 'value) => t<'value>

  let fromArray: array<(T.t, 'value)> => t<'value>

  let fromList: list<(T.t, 'value)> => t<'value>

  let decode: (Js.Json.t => 'value, Js.Json.t) => t<'value>

  let encode: ('value => Js.Json.t, t<'value>) => Js.Json.t
}
