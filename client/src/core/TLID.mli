module T : sig
  type t [@@ppx.deriving show, ord]

  type identity

  val fromString : string -> t

  val toString : t -> string

  val comparator : (t, identity) Tablecloth.Comparator.t
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

module Dict : sig
  include module type of Tc.Map.Of (T)

  val pp :
    (Format.formatter -> 'value -> unit) -> Format.formatter -> 'value t -> unit

  val empty : 'value t

  val singleton : key:T.t -> value:'value -> 'value t

  val fromArray : (T.t * 'value) array -> 'value t

  val fromList : (T.t * 'value) list -> 'value t
end
