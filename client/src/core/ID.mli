module T : sig
  (* FIXME it would be nice to make this an opaque type, but things in
   * libshared are tangled up a bit and this needs to unify with
   * libshared/Shared.id, which is also aliased to UnsharedTypes.id *)
  type t = UnsharedTypes.id [@@ppx.deriving show]

  val toString : t -> string

  val fromString : string -> t

  val empty : t
end

include module type of struct
  include T
end

module Set : sig
  include module type of Tc.Set (T)
end
