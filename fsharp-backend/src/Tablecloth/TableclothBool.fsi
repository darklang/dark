module Tablecloth.Bool

(** Functions for working with boolean values.

    Booleans in OCaml / Reason are represented by the [true] and [false] literals.

    Whilst a bool isnt a variant, you will get warnings if you haven't
    exhaustively pattern match on them:

    {[
      let bool = false
      let string =
        match bool with
        | false -> "false"
      (*
        Warning 8: this pattern-matching is not exhaustive.
        Here is an example of a case that is not matched:
        true
      *)
    ]}
*)

type t = bool

(** {1 Create} *)

val fromInt: int -> bool option
(** Convert an {!Int} into a {!Bool}.

    {2 Examples}

    {[Bool.fromInt 0 = Some false]}

    {[Bool.fromInt 1 = Some true]}

    {[Bool.fromInt 8 = None]}

    {[Bool.fromInt (-3) = None]}
*)

val from_int: int -> bool option

val fromString: string -> bool option
(** Convert a {!String} into a {!Bool}.

    {2 Examples}

    {[Bool.fromString "true" = Some true]}

    {[Bool.fromString "false" = Some false]}

    {[Bool.fromString "True" = None]}

    {[Bool.fromString "False" = None]}

    {[Bool.fromString "0" = None]}

    {[Bool.fromString "1" = None]}

    {[Bool.fromString "Not even close" = None]}
*)

val from_string: string -> bool option

(** {1 Basic operations} *)

val and_: bool -> bool -> bool
(** The lazy logical AND operator.

    Returns [true] if both of its operands evaluate to [true].

    If the 'left' operand evaluates to [false], the 'right' operand is not evaluated.

    {2 Examples}

    {[Bool.(true && true) = true]}

    {[Bool.(true && false) = false]}

    {[Bool.(false && true) = false]}

    {[Bool.(false && false) = false]}
*)

val or_: bool -> bool -> bool
(** The lazy logical OR operator.

    Returns [true] if one of its operands evaluates to [true].

    If the 'left' operand evaluates to [true], the 'right' operand is not evaluated.

    {2 Examples}

    {[Bool.(true || true) = true]}

    {[Bool.(true || false) = true]}

    {[Bool.(false || true) = true]}

    {[Bool.(false || false) = false]}
*)

val xor: bool -> bool -> bool
(** The exclusive or operator.

    Returns [true] if {b exactly one} of its operands is [true].

    {2 Examples}

    {[Bool.xor true true  = false]}

    {[Bool.xor true false = true]}

    {[Bool.xor false true  = true]}

    {[Bool.xor false false = false]}
*)

val not: t -> bool
(** Negate a [bool].

    {2 Examples}

    {[Bool.not false = true]}

    {[Bool.not true = false]}
*)

(** {1 Convert} *)

val toString: bool -> string
(** Convert a [bool] to a {!String}

    {2 Examples}

    {[Bool.toString true = "true"]}

    {[Bool.toString false = "false"]}
*)

val to_string: bool -> string

val toInt: bool -> int
(** Convert a [bool] to an {!Int}.

    {2 Examples}

    {[Bool.toInt true = 1]}

    {[Bool.toInt false = 0]}
*)

val to_int: bool -> int

(** {1 Compare} *)

val equal: bool -> bool -> bool
(** Test for the equality of two [bool] values.

    {2 Examples}

    {[Bool.equal true true = true]}

    {[Bool.equal false false = true]}

    {[Bool.equal false true = false]}
*)

val compare: bool -> bool -> int
(** Compare two boolean values

    {2 Examples}

    {[Bool.compare true false = 1]}

    {[Bool.compare false true = -1]}

    {[Bool.compare true true = 0]}

    {[Bool.compare false false = 0]}
*)