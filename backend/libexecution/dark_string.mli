open Core_kernel

type t

module Character : sig
  type t

  val uppercase : t -> t

  val lowercase : t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool
end

val of_utf8 : string -> t option

val to_utf8 : t -> string

val uppercase : t -> t

val lowercase : t -> t

val graphemes : t -> Character.t list

val length : t -> int

val compare : t -> t -> int

val equal : t -> t -> bool
