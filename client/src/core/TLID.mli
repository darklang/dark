module T : sig
  type t [@@ppx.deriving show]

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

module Dict : sig
  include module type of Tc.Dict (T)

  (* TODO: convert the tlid key back to being called key *)
  val get : tlid:T.t -> 'a t -> 'a option

  val insert : tlid:T.t -> value:'a -> 'a t -> 'a t

  val tlids : 'a t -> T.t list

  val updateIfPresent : tlid:T.t -> f:('a -> 'a) -> 'a t -> 'a t

  val update : tlid:T.t -> f:('a option -> 'a option) -> 'a t -> 'a t

  val remove : tlid:T.t -> 'a t -> 'a t

  val removeMany : tlids:T.t list -> 'a t -> 'a t
end
