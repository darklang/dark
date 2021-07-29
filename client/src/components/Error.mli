type t [@@ppx.deriving show]

val clear : t -> t

val default : t

val set : string -> t -> t

val asOption : t -> string option
