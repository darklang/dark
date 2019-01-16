open Core_kernel

type t

module Character : sig
  type t

  val uppercase : t -> t

  val lowercase : t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val to_string : t -> string

  val to_yojson : t -> Yojson.Safe.json

  val of_yojson : Yojson.Safe.json -> (t, string) Result.t
end

val of_utf8 : string -> t option

val of_utf8_exn : ?message:string -> string -> t

val to_utf8 : t -> string

val uppercase : t -> t

val lowercase : t -> t

val append : t -> t -> t

val map_graphemes : f:(Character.t -> 'a) -> t -> 'a list

val graphemes : t -> Character.t list

val of_grapheme : Character.t -> t

val of_graphemes : Character.t list -> t

val is_substring : substring:t -> t -> bool

val replace : search:t -> replace:t -> t -> t

val regexp_replace : pattern:string -> replacement:t -> t -> t

val split : sep:t -> t -> t list

val concat : sep:t -> t list -> t

val rev : t -> t

val length : t -> int

val compare : t -> t -> int

val equal : t -> t -> bool

val to_yojson : t -> Yojson.Safe.json

val of_yojson : Yojson.Safe.json -> (t, string) Result.t
