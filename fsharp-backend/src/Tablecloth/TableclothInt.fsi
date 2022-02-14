module Tablecloth.Int

(** The platform-dependant {{: https://en.wikipedia.org/wiki/Signed_number_representations } signed } {{: https://en.wikipedia.org/wiki/Integer } integer} type.

    An [int] is a whole number.

    [int]s are subject to {{: https://en.wikipedia.org/wiki/Integer_overflow } overflow }, meaning that [Int.maximumValue + 1 = Int.minimumValue].

    If you need to work with integers larger than {!maximumValue} (or smaller than {!minimumValue} you can use the {!Integer} module.

    Valid syntax for [int]s includes:
    {[
      0
      42
      9000
      1_000_000
      1_000_000
      0xFF (* 255 in hexadecimal *)
      0x000A (* 10 in hexadecimal *)
    ]}

    {b Note:} The number of bits used for an [int] is platform dependent.

    When targeting Bucklescript {{: Ints are 32 bits} https://bucklescript.github.io/docs/en/common-data-types.html#int }.

    When targeting native OCaml uses 31-bits on 32-bit platforms and 63-bits on 64-bit platforms
    which means that [int] math is well-defined in the range [-2 ** 30] to [2 ** 30 - 1] for 32bit platforms [-2 ** 62] to [2 ** 62 - 1] for 64bit platforms.

    Outside of that range, the behavior is determined by the compilation target.

    You can read about the reasons for OCaml's unusual integer sizes {{: https://v1.realworldocaml.org/v1/en/html/memory-representation-of-values.html} here }.

    When targeting F#, uses 32-bits on all platforms which means that [int] math is well-defined in the range [-2 ** 31] to [2 ** 31 - 1].

    {e Historical Note: } The name [int] comes from the term {{: https://en.wikipedia.org/wiki/Integer } integer}). It appears
    that the [int] abbreviation was introduced in the programming language ALGOL 68.

    Today, almost all programming languages use this abbreviation.
*)

type t = int

(** {1 Constants } *)

val zero: t
(** The literal [0] as a named value *)

val one: t
(** The literal [1] as a named value *)

val maximumValue: t
(** The maximum representable [int] on the current platform *)

val maximum_value: t

val minimumValue: t
(** The minimum representable [int] on the current platform *)

val minimum_value: t

(** {1 Create} *)

val fromString: string -> t option
(** Attempt to parse a [string] into a [int].

    {2 Examples}

    {[Int.fromString "0" = Some 0.]}

    {[Int.fromString "42" = Some 42.]}

    {[Int.fromString "-3" = Some (-3)]}

    {[Int.fromString "123_456" = Some 123_456]}

    {[Int.fromString "0xFF" = Some 255]}

    {[Int.fromString "0x00A" = Some 10]}

    {[Int.fromString "Infinity" = None]}

    {[Int.fromString "NaN" = None]}
*)

val from_string: string -> t option

(** {1 Operators}

    {b Note } You do not need to open the {!Int} module to use the
    {!( + )}, {!( - )}, {!( * )}, {!( ** )}, {! (mod)} or {!( / )} operators, these are
    available as soon as you [open Tablecloth]
*)

val add: t -> t -> t
(** Add two {!Int} numbers.

  {[Int.add 3002 4004 = 7006]}

  Or using the globally available operator:

  {[3002 + 4004 = 7006]}

  You {e cannot } add an [int] and a [float] directly though.

  See {!Float.add} for why, and how to overcome this limitation.
*)

val (+): t -> t -> t
(** See {!Int.add} *)

val subtract: t -> t -> t
(** Subtract numbers

    {[Int.subtract 4 3 = 1]}

    Alternatively the operator can be used:

    {[4 - 3 = 1]}
*)

val (-): t -> t -> t
(** See {!Int.subtract} *)

val multiply: t -> t -> t
(** Multiply [int]s like

    {[Int.multiply 2 7 = 14]}

    Alternatively the operator can be used:

    {[(2 * 7) = 14]}
*)

val (*): t -> t -> t
(** See {!Int.multiply} *)

val divide: by: t -> t -> t
(** Integer division

    Notice that the remainder is discarded.

    {3 Exceptions}

    Throws [Division_by_zero] when the divisor is [0].

    {2 Examples}

    {[Int.divide 3 ~by:2 = 1]}

    {[27 / 5 = 5]}
*)

val (/): t -> t -> t
(** See {!Int.divide} *)

val (/.): t -> t -> float
(** Floating point division

    {2 Examples}

    {[Int.(3 /. 2) = 1.5]}

    {[Int.(27 /. 5) = 5.25]}

    {[Int.(8 /. 4) = 2.0]}
*)

val power: exponent: t -> ``base``: t -> t
(** Exponentiation, takes the base first, then the exponent.

    {2 Examples}

    {[Int.power ~base:7 ~exponent:3 = 343]}

    Alternatively the [**] operator can be used:

    {[7 ** 3 = 343]}
*)

val ( ** ): t -> t -> t
(** See {!Int.power} *)

val negate: t -> t
(** Flips the 'sign' of an integer so that positive integers become negative and negative integers become positive. Zero stays as it is.

    {2 Examples}

    {[Int.negate 8 = (-8)]}

    {[Int.negate (-7) = 7]}

    {[Int.negate 0 = 0]}

    Alternatively the [~-] operator can be used:

    {[~-(7) = (-7)]}
*)

val (~-): t -> t
(** See {!Int.negate} *)

val absolute: t -> t
(** Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value } of a number.

    {2 Examples}

    {[Int.absolute 8 = 8]}

    {[Int.absolute (-7) = 7]}

    {[Int.absolute 0 = 0]}
*)

val modulo: by: t -> t -> t
(** Perform {{: https://en.wikipedia.org/wiki/Modular_arithmetic } modular arithmetic }.

    If you intend to use [modulo] to detect even and odd numbers consider using {!Int.isEven} or {!Int.isOdd}.

    The [modulo] function works in the typical mathematical way when you run into negative numbers

    Use {!Int.remainder} for a different treatment of negative numbers.

    {2 Examples}

    {[Int.modulo 3 (-4) = 1]}

    {[Int.modulo 3 (-3 )= 0]}

    {[Int.modulo 3 (-2) = 2]}

    {[Int.modulo 3 (-1) = 1]}

    {[Int.modulo 3 0 = 0]}

    {[Int.modulo 3 1 = 1]}

    {[Int.modulo 3 2 = 2]}

    {[Int.modulo 3 3 = 0]}

    {[Int.modulo 3 4 = 1]}
*)

val ``mod``: t -> t -> t
(** See {!Int.modulo} *)

val remainder: by: t -> t -> t
(** Get the remainder after division. Here are bunch of examples of dividing by four:

    Use {!Int.modulo} for a different treatment of negative numbers.

    {2 Examples}

    {[
      List.map
        (Int.remainder 4)
        [(-5); (-4); (-3); (-2); (-1); 0; 1; 2; 3; 4; 5] =
          [(-1); 0; (-3); (-2); (-1); 0; 1; 2; 3; 0; 1]
    ]}
*)

val maximum: t -> t -> t
(** Returns the larger of two [int]s

    {2 Examples}

    {[Int.maximum 7 9 = 9]}

    {[Int.maximum (-4) (-1) = (-1)]}
*)

val minimum: t -> t -> t
(** Returns the smaller of two [int]s

    {2 Examples}

    {[Int.minimum 7 9 = 7]}

    {[Int.minimum (-4) (-1) = (-4)]}
*)

(** {1 Query} *)

val isEven: t -> bool
(** Check if an [int] is even

    {2 Examples}

    {[Int.isEven 8 = true]}

    {[Int.isEven 7 = false]}

    {[Int.isEven 0 = true]}
*)

val is_even: t -> bool

val isOdd: t -> bool
(** Check if an [int] is odd

  {2 Examples}

  {[Int.isOdd 7 = true]}

  {[Int.isOdd 8 = false]}

  {[Int.isOdd 0 = false]}
*)

val is_odd: t -> bool

val clamp: lower: t -> upper: t -> t -> t
(** Clamps [n] within the inclusive [lower] and [upper] bounds.

  {3 Exceptions}

  Throws a [System.ArgumentOutOfRangeException] exception if [lower > upper]

  {2 Examples}

  {[Int.clamp 8 5 0 = 5]}

  {[Int.clamp 8 9 0 = 8]}

  {[Int.clamp (-5) 5 (-10) = (-5)]}
*)

val inRange: lower: t -> upper: t -> t -> bool
(** Checks if [n] is between [lower] and up to, but not including, [upper].

    {3 Exceptions}

    Throws an [ArgumentOutOfRangeException] exception if [lower > upper]

    {2 Examples}

    {[Int.inRange 2 4 3 = true]}

    {[Int.inRange 5 8 4 = false]}

    {[Int.inRange (-6) (-2) (-3) = true]}

*)

val in_range: lower: t -> upper: t -> t -> bool

(** {1 Convert} *)

val toFloat: t -> float
(** Convert an integer into a float. Useful when mixing {!Int} and {!Float} values like this:

    {2 Examples}

    {[
      let halfOf (number : int) : float =
        Float.((Int.toFloat number) / 2)
        (* Note that locally opening the {!Float} module here allows us to use the floating point division operator *)
      in
      halfOf 7 = 3.5
    ]}
*)

val to_float: t -> float

val toString: t -> string
(** Convert an [int] into a [string] representation.

    Guarantees that

    {[Int.(fromString (toString n)) = Some n ]}

    {2 Examples}

    {[Int.toString 3 = "3"]}

    {[Int.toString (-3) = "-3"]}

    {[Int.to_sString 0 = "0"]}
*)

val to_string: t -> string

(** {1 Compare} *)

val equal: t -> t -> bool
(** Test two [int]s for equality *)

val compare: t -> t -> int
(** Compare two [int]s *)