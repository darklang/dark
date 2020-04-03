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

(** Creates a `t` from an OCaml `string`, which is required to be encoded in UTF-8.
 ** Returns `None` if the passed string is not valid UTF-8 **)
val of_utf8_encoded_string : string -> t option

(** alias of `of_utf8_encoded_string` **)
val of_string : string -> t option

(** Creates a `t` from an OCaml `string`, which is required to be encoded in UTF-8
 ** Throws an exception with `?message` as the message if the passed string
 ** is not valid UTF-8. The exception has a default message if no message
 ** is provided**)
val of_utf8_encoded_string_exn : ?message:string -> string -> t

(** alias of `of_utf8_encoded_string_exn` **)
val of_string_exn : ?message:string -> string -> t

val to_utf8_encoded_string : t -> string

(** alias of `to_utf8_encoded_string` **)
val to_string : t -> string

(** get the bytes of the string, using utf8 encoding **)
val to_utf8_bytes : t -> Bytes.t

(** Maps the casing of the characters of the String to their defined
 ** uppercase equivalent.
 ** Note: `length a != length (uppercase a)` in the general case as
 ** some characters become multiple characters when their case is changed **)
val uppercase : t -> t

(** Maps the casing of the characters of the String to their defined
 ** lowercase equivalent.
 ** Note: `length a != length (lowercase a)` in the general case as
 ** some characters become multiple characters ** when their case is changed **)
val lowercase : t -> t

(** `append a b` returns the concatenation of `a` and `b` **)
val append : t -> t -> t

(** Returns a list of the user-perceived Characters in the Unicode_string.t **)
val characters : t -> Character.t list

val map_characters : f:(Character.t -> 'a) -> t -> 'a list

val of_character : Character.t -> t

val of_characters : Character.t list -> t

val is_substring : substring:t -> t -> bool

val starts_with : prefix:t -> t -> bool

val ends_with : suffix:t -> t -> bool

val replace : search:t -> replace:t -> t -> t

val regexp_replace : pattern:string -> replacement:t -> t -> t

val split : sep:t -> t -> t list

val concat : sep:t -> t list -> t

(** Returns the `t`, with the characters reversed **)
val rev : t -> t

(** Returns the number of user-perceived Characters in `t` **)
val length : t -> int

(** Removes whitespace from the front and end of a string.
 * 'Whitespace' is defined according to the terms of the Unicode Derived Core Property White_Space
 * https://en.wikipedia.org/wiki/Unicode_character_property#Whitespace
 * *)
val trim : t -> t

(** [slice str first last] returns a substring between [first] (inclusive) and [last] (exclusive) indices.
 * Indices represent EGCs. Negative indices start counting from the end of the [str].
 *)
val slice : t -> first:int -> last:int -> t

(** [first_n str num_egcs] returns a substring formed of the first [num_egcs] EGCs of [str]. *)
val first_n : t -> int -> t

(** [drop_first_n str num_egcs] returns a substring formed of all but the first [num_egcs] EGCs of [str]. *)
val drop_first_n : t -> int -> t

(** [last_n str num_egcs] returns a substring formed of the last [num_egcs] EGCs of [str]. *)
val last_n : t -> int -> t

(** [drop_last_n str num_egcs] returns a substring formed of all but the last [num_egcs] EGCs of [str]. *)
val drop_last_n : t -> int -> t

(** [pad_start str pad_with target_egcs] pads the start of [str] with repeated copies of [pad_with] while
 * the number of EGCs in the result is less than [target_egcs].
 *)
val pad_start : t -> pad_with:t -> int -> t

(** [pad_end str pad_with target_egcs] pads the end of [str] with repeated copies of [pad_with] while
 * the number of EGCs in the result is less than [target_egcs].
 *)
val pad_end : t -> pad_with:t -> int -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) Result.t
