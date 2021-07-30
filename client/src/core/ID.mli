module T : sig
  (* FIXME it would be nice to make this an opaque type, but things in
   * libshared are tangled up a bit and this needs to unify with
   * libshared/Shared.id, which is also aliased to UnsharedTypes.id *)
  type t = UnsharedTypes.id [@@ppx.deriving show, ord]

  type identity

  val comparator : (t, identity) Tablecloth.Comparator.t

  val fromString : string -> t

  val toString : t -> string
end

include module type of struct
  include T
end

module Set : sig
  include module type of Tc.Set.Of (T)

  val pp : Format.formatter -> t -> unit

  val empty : t

  val singleton : T.t -> t

  val fromArray : T.t array -> t

  val fromList : T.t list -> t
end

module Map : sig
  include module type of Tc.Map.Of (T)

  val pp :
    (Format.formatter -> 'value -> unit) -> Format.formatter -> 'value t -> unit

  val empty : 'value t

  val singleton : key:T.t -> value:'value -> 'value t

  val fromArray : (T.t * 'value) array -> 'value t

  val fromList : (T.t * 'value) list -> 'value t
end
