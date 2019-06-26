open Core_kernel

(** Represents a string of user-perceivable Unicode characters.
 ** The internal representation is deliberately opaque, and no guarantee
 ** is given regarding the representation or encoding. **)
type t

val pp : Format.formatter -> t -> unit

module Character : sig
  (** Represents a single user-perceivable Unicode character, more specifically
   ** an 'extended grapheme cluster' in Unicoder terminology **)
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit

  val unsafe_of_string : string -> t

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

val of_utf8_encoded_string : string -> t option
(** Creates a `t` from an OCaml `string`, which is required to be encoded in UTF-8.
 ** Returns `None` if the passed string is not valid UTF-8 **)

val of_string : string -> t option
(** alias of `of_utf8_encoded_string` **)

val of_utf8_encoded_string_exn : ?message:string -> string -> t
(** Creates a `t` from an OCaml `string`, which is required to be encoded in UTF-8
 ** Throws an exception with `?message` as the message if the passed string
 ** is not valid UTF-8. The exception has a default message if no message
 ** is provided**)

val of_string_exn : ?message:string -> string -> t
(** alias of `of_utf8_encoded_string_exn` **)

val to_utf8_encoded_string : t -> string

val to_string : t -> string
(** alias of `to_utf8_encoded_string` **)

val uppercase : t -> t
(** Maps the casing of the characters of the String to their defined
 ** uppercase equivalent.
 ** Note: `length a != length (uppercase a)` in the general case as
 ** some characters become multiple characters when their case is changed **)

val lowercase : t -> t
(** Maps the casing of the characters of the String to their defined
 ** lowercase equivalent.
 ** Note: `length a != length (lowercase a)` in the general case as
 ** some characters become multiple characters ** when their case is changed **)

val append : t -> t -> t
(** `append a b` returns the concatenation of `a` and `b` **)

val characters : t -> Character.t list
(** Returns a list of the user-perceived Characters in the Unicode_string.t **)

val map_characters : f:(Character.t -> 'a) -> t -> 'a list

val of_character : Character.t -> t

val of_characters : Character.t list -> t

val is_substring : substring:t -> t -> bool

val replace : search:t -> replace:t -> t -> t

val regexp_replace : pattern:string -> replacement:t -> t -> t

val split : sep:t -> t -> t list

val concat : sep:t -> t list -> t

val rev : t -> t
(** Returns the `t`, with the characters reversed **)

val length : t -> int
(** Returns the number of user-perceived Characters in `t` **)

val compare : t -> t -> int

val equal : t -> t -> bool

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) Result.t
