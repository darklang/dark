module T: {
  /* FIXME it would be nice to make this an opaque type, but things in
   * libshared are tangled up a bit and this needs to unify with
   * libshared/Shared.id, which is also aliased to UnsharedTypes.id */
  @ppx.deriving(show) type t = UnsharedTypes.id

  type identity

  let comparator: Tablecloth.Comparator.t<t, identity>

  let fromString: string => t

  let toString: t => string
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
}
