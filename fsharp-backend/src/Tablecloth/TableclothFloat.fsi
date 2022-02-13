module Tablecloth.Float

(** A module for working with {{: https://en.wikipedia.org/wiki/Floating-point_arithmetic } 64-bit floating-point numbers}.

    Valid syntax for [float]s includes:
    {[
      0.
      42.
      42.0
      3.14
      -0.1234
      123_456.123_456
      6.022e23   (* = (6.022 * 10^23) *)
      6.022e+23  (* = (6.022 * 10^23) *)
      1.602e-19  (* = (1.602 * 10^-19) *)
      1e3        (* = (1 * 10 ** 3) = 1000. *)
    ]}

    Without opening this module you can use the [.] suffixed operators e.g

    {[ 1. +. 2. /. 0.25 *. 2. = 17. ]}

    But by opening this module locally you can use the un-suffixed operators

    {[Float.((10.0 - 1.5 / 0.5) ** 3.0) = 2401.0]}

    {b Historical Note: } The particular details of floats (e.g. [NaN]) are
    specified by {{: https://en.wikipedia.org/wiki/IEEE_754 } IEEE 754 } which is literally hard-coded into almost all
    CPUs in the world.
*)

type t = float

(** {1 Constants} *)

val zero: t
(** The literal [0.0] as a named value *)

val one: t
(** The literal [1.0] as a named value *)

val nan: t
(** [NaN] as a named value. NaN stands for {{: https://en.wikipedia.org/wiki/NaN } not a number}.

    {b Note } comparing values with {!Float.nan} will {b always return } [false] even if the value you are comparing against is also [NaN].

    e.g

    {[
      let isNotANumber x = Float.(x = nan) in

      isNotANumber nan = false
    ]}

    For detecting [Nan] you should use {!Float.isNaN}
*)

val infinity: t
(** Positive {{: https://en.wikipedia.org/wiki/IEEE_754-1985#Positive_and_negative_infinity } infinity }

    {[Float.log ~base:10.0 0.0 = Float.infinity]}
*)

val negativeInfinity: t
(** Negative infinity, see {!Float.infinity} *)

val negative_infinity: t

val e: t
(** An approximation of {{: https://en.wikipedia.org/wiki/E_(mathematical_constant) } Euler's number }. *)

val pi: t
(** An approximation of {{: https://en.wikipedia.org/wiki/Pi } pi }. *)

val epsilon: t
(** The smallest interval between two representable numbers, calculated by subtracting 1.0 from the smallest number larger than 1.0. This uses the same algorithm as in javascript/rescript and OCaml. However, it is a different value than dotnet's System.Double.Epsilon, which is discussed here: https://github.com/dotnet/runtime/issues/28054 *)

val largestValue: t
(** The largest (furthest from zero) representable positive [float] *)

val largest_value: t

val smallestValue: t
(** The smallest representable positive [float]. The closest to zero without actually being zero. *)

val smallest_value: t

val maximumSafeInteger: t
(** For floats greater than [maximumSafeInteger], it no longer holds that [Float.(n + 1.) > n]  *)

val maximum_safe_integer: t

val minimumSafeInteger: t
(** For floats less than [minimumSafeInteger], it no longer holds that [Float.(n - 1.) < n]  *)

val minimum_safe_integer: t

(** {1 Create} *)

val fromInt: int -> t
(** Convert an {!Int} to a [float]

    {2 Examples}

    {[
      Float.fromInt 5 = 5.0
      Float.fromInt 0 = 0.0
      Float.fromInt -7 = -7.0
    ]}
*)

val from_int: int -> t

val fromString: string -> t option
(** Convert a {!String} to a [float].

    Parses [nan] and [infinity] case-insensitive.

    {2 Examples}

    {[Float.fromString "4.667" = Some 4.667]}

    {[Float.fromString "-4.667" = Some (-4.667)]}

    {[Float.fromString "Hamster" = None]}

    {[Float.fromString "NaN" = Some Float.nan]}

    {[Float.fromString "nan" = Some Float.nan]}

    {[Float.fromString "Infinity" = Some Float.infinity]}
*)

val from_string: string -> t option

(** {1 Basic arithmetic and operators} *)

val add: t -> t -> t
(** Addition for floating point numbers.

    Although [int]s and [float]s support many of the same basic operations such as
    addition and subtraction you {b cannot} [add] an [int] and a [float] directly which
    means you need to use functions like {!Int.toFloat} to convert both values to the same type.

    So if you needed to add a {!List.length} to a [float] for some reason, you
    could:

    {[Float.add 3.14 (Int.toFloat (List.length [1,2,3])) = 6.14]}

    or

    {[Float.roundToInt 3.14 + List.length [1,2,3] = 6]}

    Languages like Java and JavaScript automatically convert [int] values
    to [float] values when you mix and match. This can make it difficult to be sure
    exactly what type of number you are dealing with and cause unexpected behavior.

    OCaml has opted for a design that makes all conversions explicit.

    {2 Examples}

    {[
      Float.add 3.14 3.14 = 6.28
      Float.(3.14 + 3.14 = 6.28)
    ]}
*)

val (+): t -> t -> t
(** See {!Float.add} *)

val subtract: t -> t -> t
(** Subtract numbers

    Alternatively the [-] operator can be used

    {2 Examples}

    {[Float.subtract 4.0 3.0 = 1.0]}

    {[Float.(4.0 - 3.0) = 1.0]}
*)

val (-): t -> t -> t
(** See {!Float.subtract} *)

val multiply: t -> t -> t
(** Multiply numbers

    Alternatively the [*] operator can be used

    {2 Examples}

    {[Float.multiply 2.0 7.0 = 14.0]}

    {[Float.(2.0 * 7.0) = 14.0]}
*)

val (*): t -> t -> t
(** See {!Float.multiply} *)

val divide: t -> by: t -> t
(** Floating-point division:

    Alternatively the [/] operator can be used

    {2 Examples}

    {[Float.divide 3.14 2.0 = 1.57]}

    {[Float.(3.14 / 2.0) = 1.57]}
*)

val (/): t -> t -> t
(** See {!Float.divide} *)

val power: exponent: t -> ``base``: t -> t
(** Exponentiation, takes the base first, then the exponent.

    Alternatively the [**] operator can be used

    {2 Examples}

    {[Float.power 3.0 7.0 = 343.0]}

    {[Float.(7.0 ** 3.0) = 343.0]}
*)

val ( ** ): t -> t -> t
(** See {!Float.power} *)

val negate: t -> t
(** Flips the 'sign' of a [float] so that positive floats become negative and negative integers become positive. Zero stays as it is.

    Alternatively an operator is available

    {2 Examples}

    {[Float.(~- 4.0) = (-4.0)]}

    {[
      Float.negate 8 = (-8)
      Float.negate (-7) = 7
      Float.negate 0 = 0
    ]}
*)

val (~-): t -> t
(** See {!Float.negate} *)

val absolute: t -> t
(** Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value} of a number.

    {2 Examples}

    {[
      Float.absolute 8. = 8.
      Float.absolute (-7) = 7
      Float.absolute 0 = 0
    ]}
*)

val maximum: t -> t -> t
(** Returns the larger of two [float]s, if both arguments are equal, returns the first argument

    If either (or both) of the arguments are [NaN], returns [NaN]

    {2 Examples}

    {[Float.maximum 7. 9. = 9.]}

    {[Float.maximum (-4.) (-1.) = (-1.)]}

    {[Float.(isNaN (maximum 7. nan)) = true]}
*)

val minimum: t -> t -> t
(** Returns the smaller of two [float]s, if both arguments are equal, returns the first argument

    If either (or both) of the arguments are [NaN], returns [NaN]

    {2 Examples}

    {[Float.minimum 7.0 9.0 = 7.0]}

    {[Float.minimum (-4.0) (-1.0) = (-4.0)]}

    {[Float.(isNaN (minimum 7. nan)) = true]}
*)

val clamp: lower: t -> upper: t -> t -> t
(** Clamps [n] within the inclusive [lower] and [upper] bounds.

    {3 Exceptions}

    Throws an [ArgumentOutOfBoundsException] exception if [lower > upper]

    {2 Examples}

    {[Float.clamp 0. 8. 5. = 5.]}

    {[Float.clamp 0. 8. 9. = 8.]}

    {[Float.clamp (-10.) (-5.) 5. = -5.]}
*)

(** {1 Fancier math} *)

val squareRoot: t -> t
(** Take the square root of a number.

    [squareRoot] returns [NaN] when its argument is negative. See {!Float.nan} for more.

    {2 Examples}

    {[Float.squareRoot 4.0 = 2.0]}

    {[Float.squareRoot 9.0 = 3.0]}
*)

val square_root: t -> t

val log: ``base``: t -> t -> t
(** Calculate the logarithm of a number with a given base.

    {2 Examples}

    {[Float.log 10. 100. = 2.]}

    {[Float.log 2. 256. = 8.]}
*)

(** {1 Query} *)

val isNaN: t -> bool
(** Determine whether a float is an undefined or unrepresentable number.

    {b Note } this function is more useful than it might seem since [NaN] {b does not } equal [Nan]:

    {[Float.(nan = nan) = false]}

    {2 Examples}

    {[Float.isNaN (0.0 / 0.0) = true]}

    {[Float.(isNaN (squareRoot (-1.0))) = true]}

    {[Float.isNaN (1.0 / 0.0) = false  (* Float.infinity {b is} a number *)]}

    {[Float.isNaN 1. = false]}
*)

val is_nan: t -> bool

val isFinite: t -> bool
(** Determine whether a float is finite number. True for any float except [Infinity], [-Infinity] or [NaN]

    Notice that [NaN] is not finite!

    {2 Examples}

    {[Float.isFinite (0. / 0.) = false]}

    {[Float.(isFinite (squareRoot (-1.))) = false]}

    {[Float.isFinite (1. / 0.) = false]}

    {[Float.isFinite 1. = true]}

    {[Float.(isFinite nan) = false]}
*)

val is_finite: t -> bool

val isInfinite: t -> bool
(** Determine whether a float is positive or negative infinity.

    {2 Examples}

    {[Float.isInfinite (0. / 0.) = false]}

    {[Float.(isInfinite (squareRoot (-1.))) = false]}

    {[Float.isInfinite (1. / 0.) = true]}

    {[Float.isInfinite 1. = false]}

    {[Float.(isInfinite nan) = false]}
*)

val is_infinite: t -> bool

val isInteger: t -> bool
(** Determine whether the passed value is an integer.

    {2 Examples}

    {[Float.isInteger 4.0 = true]}

    {[Float.isInteger Float.pi = false]}
*)

val is_integer: t -> bool

val isSafeInteger: t -> bool
(** Determine whether the passed value is a safe integer (number between -(2**53 - 1) and 2**53 - 1).

    {2 Examples}

    {[Float.isSafeInteger 4.0 = true]}

    {[Float.isSafeInteger Float.pi = false]}

    {[Float.(isSafeInteger (maximumSafeInteger + 1.)) = false]}
*)

val is_safe_integer: t -> bool

val inRange: lower: t -> upper: t -> t -> bool
(** Checks if a float is between [lower] and up to, but not including, [upper].

    If [lower] is not specified, it's set to to [0.0].

    {3 Exceptions}

    Throws an [ArgumentOutOfBoundsException] exception if [lower > upper]

    {2 Examples}

    {[Float.inRange 2. 4. 3. = true]}

    {[Float.inRange 1. 2. 2. = false]}

    {[Float.inRange 5.2 7.9 9.6 = false]}
*)

val in_range: lower: t -> upper: t -> t -> bool

(** {1 Angles} *)

(** This type is just an alias for [float].

    Its purpose is to make understanding the signatures of the following
    functions a little easier.
*)
type radians = float

val hypotenuse: t -> t -> t
(** [hypotenuse x y] returns the length of the hypotenuse of a right-angled triangle with sides of length [x] and [y], or, equivalently, the distance of the point [(x, y)] to [(0, 0)].

    {2 Examples}

    {[Float.hypotenuse 3. 4. = 5.]}
*)

val degrees: t -> radians
(** Converts an angle in {{: https://en.wikipedia.org/wiki/Degree_(angle) } degrees} to {!Float.radians}.

    {2 Examples}

    {[Float.degrees 180. = Float.pi]}

    {[Float.degrees 360. = Float.pi * 2.]}

    {[Float.degrees 90. = Float.pi /. 2.]}
*)

val radians: t -> radians
(** Convert a {!Float.t} to {{: https://en.wikipedia.org/wiki/Radian } radians }.

    {b Note } This function doesn't actually do anything to its argument, but can be useful to indicate intent when inter-mixing angles of different units within the same function.

    {2 Examples}

    {[Float.(radians pi) = 3.141592653589793]}
*)

val turns: t -> radians
(** Convert an angle in {{: https://en.wikipedia.org/wiki/Turn_(geometry) } turns} into {!Float.radians}.

    One turn is equal to 360 degrees.

    {2 Examples}

    {[Float.(turns (1. / 2.)) = pi]}

    {[Float.(turns 1. = degrees 360.)]}
*)

(** {1 Polar coordinates} *)

val fromPolar: float * radians -> float * float
(** Convert {{: https://en.wikipedia.org/wiki/Polar_coordinate_system } polar coordinates } (radius, radians) to {{: https://en.wikipedia.org/wiki/Cartesian_coordinate_system } Cartesian coordinates } (x,y).

    {2 Examples}

    {[Float.(fromPolar (squareRoot 2., degrees 45.)) = (1., 1.)]}
*)

val from_polar: float * radians -> float * float

val toPolar: float * float -> float * radians
(** Convert {{: https://en.wikipedia.org/wiki/Cartesian_coordinate_system } Cartesian coordinates } [(x, y)] to {{: https://en.wikipedia.org/wiki/Polar_coordinate_system } polar coordinates } [(radius, radians)].

    {2 Examples}

    {[Float.toPolar (-1.0, 0.0) = (1.0, Float.pi)]}

    {[Float.toPolar (3.0, 4.0) = (5.0, 0.9272952180016122)]}

    {[Float.toPolar (5.0, 12.0) = (13.0, 1.1760052070951352)]}
*)

val to_polar: float * float -> float * radians

val cos: radians -> t
(** Figure out the cosine given an angle in {{: https://en.wikipedia.org/wiki/Radian } radians }.

    {2 Examples}

    {[Float.(cos (degrees 60.)) = 0.5000000000000001]}

    {[Float.(cos (radians (pi / 3.))) = 0.5000000000000001]}
*)

val acos: radians -> t
(** Figure out the arccosine for [adjacent / hypotenuse] in {{: https://en.wikipedia.org/wiki/Radian } radians }:

    {2 Examples}

    {[Float.(acos (radians 1.0 / 2.0)) = Float.radians 1.0471975511965979 (* 60 degrees or pi/3 radians *)]}
*)

val sin: radians -> t
(** Figure out the sine given an angle in {{: https://en.wikipedia.org/wiki/Radian } radians }.

    {2 Examples}

    {[Float.(sin (degrees 30.)) = 0.49999999999999994]}

    {[Float.(sin (radians (pi / 6.))) = 0.49999999999999994]}
*)

val asin: radians -> t
(** Figure out the arcsine for [opposite / hypotenuse] in {{: https://en.wikipedia.org/wiki/Radian } radians }:

    {2 Examples}

    {[Float.(asin (1.0 / 2.0)) = 0.5235987755982989 (* 30 degrees or pi / 6 radians *)]}
*)

val tan: radians -> t
(** Figure out the tangent given an angle in radians.

    {2 Examples}

    {[Float.(tan (degrees 45.)) = 0.9999999999999999]}

    {[Float.(tan (radians (pi / 4.))) = 0.9999999999999999]}

    {[Float.(tan (pi / 4.)) = 0.9999999999999999]}
*)

val atan: t -> radians
(** This helps you find the angle (in radians) to an [(x, y)] coordinate, but
    in a way that is rarely useful in programming.

    {b You probably want} {!atan2} instead!

    This version takes [y / x] as its argument, so there is no way to know whether
    the negative signs comes from the [y] or [x] value. So as we go counter-clockwise
    around the origin from point [(1, 1)] to [(1, -1)] to [(-1,-1)] to [(-1,1)] we do
    not get angles that go in the full circle:

    Notice that everything is between [pi / 2] and [-pi/2]. That is pretty useless
    for figuring out angles in any sort of visualization, so again, check out
    {!Float.atan2} instead!

    {2 Examples}

    {[Float.atan (1. /. 1.) = 0.7853981633974483  (* 45 degrees or pi/4 radians *)]}

    {[Float.atan (1. /. -1.) = -0.7853981633974483  (* 315 degrees or 7 * pi / 4 radians *)]}

    {[Float.atan (-1. /. -1.) = 0.7853981633974483 (* 45 degrees or pi/4 radians *)]}

    {[Float.atan (-1. /.  1.) = -0.7853981633974483 (* 315 degrees or 7 * pi/4 radians *)]}
*)

val atan2: y: t -> x: t -> radians
(** This helps you find the angle (in radians) to an [(x, y)] coordinate.

    So rather than [Float.(atan (y / x))] you can [Float.atan2 ~y ~x] and you can get a full range of angles:

    {2 Examples}

    {[Float.atan2 ~y:1. ~x:1. = 0.7853981633974483  (* 45 degrees or pi/4 radians *)]}

    {[Float.atan2 ~y:1. ~x:(-1.) = 2.3561944901923449  (* 135 degrees or 3 * pi/4 radians *)]}

    {[Float.atan2 ~y:(-1.) ~x:(-1.) = -(2.3561944901923449) (* 225 degrees or 5 * pi/4 radians *)]}

    {[Float.atan2 ~y:(-1.) ~x:1. = -(0.7853981633974483) (* 315 degrees or 7 * pi/4 radians *)]}
*)

(** {1 Rounding} *)

(** The possible [direction]s availible when doing {!Float.round}.

    See {!Float.round} for what each variant represents.
 *)
type Direction =
  | Zero
  | AwayFromZero
  | Up
  | Down
  | Closest of Direction
  | ClosestToEven


val round: direction: Direction -> t -> t
(** Round a number, by default to the to the closest [int] with halves rounded [`Up] (towards positive infinity)

    Other rounding strategies are available by using the optional [~direction] labelelled.

    {2 Examples}

    {[
      Float.round 1.2 = 1.0
      Float.round 1.5 = 2.0
      Float.round 1.8 = 2.0
      Float.round -1.2 = -1.0
      Float.round -1.5 = -1.0
      Float.round -1.8 = -2.0
    ]}

    {3 Towards zero}

    {[
      Float.round Zero 1.2 = 1.0
      Float.round Zero 1.5 = 1.0
      Float.round Zero 1.8 = 1.0
      Float.round Zero (-1.2) = -1.0
      Float.round Zero (-1.5) = -1.0
      Float.round Zero (-1.8) = -1.0
    ]}

    {3 Away from zero}

    {[
      Float.round AwayFromZero 1.2 = 1.0
      Float.round AwayFromZero 1.5 = 1.0
      Float.round AwayFromZero 1.8 = 1.0
      Float.round AwayFromZero (-1.2) = -1.0
      Float.round AwayFromZero (-1.5) = -1.0
      Float.round AwayFromZero (-1.8) = -1.0
    ]}

    {3 Towards infinity}

    This is also known as {!Float.ceiling}

    {[
      Float.round Up 1.2 = 1.0
      Float.round Up 1.5 = 1.0
      Float.round Up 1.8 = 1.0
      Float.round Up (-1.2) = -1.0
      Float.round Up (-1.5) = -1.0
      Float.round Up (-1.8) = -1.0
    ]}

    {3 Towards negative infinity}

    This is also known as {!Float.floor}

    {[List.map (Float.round Down) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -2.0; -2.0; 1.0 1.0 1.0]]}

    {3 To the closest integer}

    Rounding a number [x] to the closest integer requires some tie-breaking for when the [fraction] part of [x] is exactly [0.5].

    {4 Halves rounded towards zero}

    {[List.map (Float.round (Closest AwayFromZero)) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -1.0; -1.0; 1.0 1.0 2.0]]}

    {4 Halves rounded away from zero}

    This method is often known as {b commercial rounding }

    {[List.map (Float.round (Closest AwayFromZero)) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -2.0; -1.0; 1.0 2.0 2.0]]}

    {4 Halves rounded down}

    {[List.map (Float.round (Closest Down)) [-1.8; -1.5; -1.2; 1.2; 1.5; 1.8] = [-2.0; -2.0; -1.0; 1.0 1.0 2.0]]}

    {4 Halves rounded up}

    This is the default.

    [Float.round 1.5] is the same as [Float.round (Closest Up) 1.5]

    {4 Halves rounded towards the closest even number}

    {[
      Float.round ClosestToEven -1.5 = -2.0
      Float.round ClosestToEven -2.5 = -2.0
    ]}
*)

val floor: t -> t
(** Floor function, equivalent to [Float.round ~direction:`Down].

    {2 Examples}

    {[
      Float.floor 1.2 = 1.0
      Float.floor 1.5 = 1.0
      Float.floor 1.8 = 1.0
      Float.floor -1.2 = -2.0
      Float.floor -1.5 = -2.0
      Float.floor -1.8 = -2.0
    ]}
*)

val ceiling: t -> t
(** Ceiling function, equivalent to [Float.round ~direction:`Up].

    {2 Examples}

    {[
      Float.ceiling 1.2 = 2.0
      Float.ceiling 1.5 = 2.0
      Float.ceiling 1.8 = 2.0
      Float.ceiling -1.2 = (-1.0)
      Float.ceiling -1.5 = (-1.0)
      Float.ceiling -1.8 = (-1.0)
    ]}
*)

val truncate: t -> t
(** Ceiling function, equivalent to [Float.round ~direction:`Zero].

    {2 Examples}

    {[
      Float.truncate 1.0 = 1.
      Float.truncate 1.2 = 1.
      Float.truncate 1.5 = 1.
      Float.truncate 1.8 = 1.
      Float.truncate (-1.2) = -1.
      Float.truncate (-1.5) = -1.
      Float.truncate (-1.8) = -1.
    ]}
*)

(** {1 Convert} *)

val toInt: t -> int option
(** Converts a [float] to an {!Int} by {b ignoring the decimal portion}. See {!Float.truncate} for examples.

    Returns [None] when trying to round a [float] which can't be represented as an [int] such as {!Float.nan} or {!Float.infinity} or numbers which are too large or small.

    You probably want to use some form of {!Float.round} prior to using this function.

    {2 Examples}

    {[Float.(toInt 1.6) = (Some 1)]}

    {[Float.(toInt 2.0) = (Some 2)]}

    {[Float.(toInt 5.683) = (Some 5)]}

    {[Float.(toInt nan) = None]}

    {[Float.(toInt infinity) = None]}

    {[Float.(round 1.6 |> toInt) = Some 2]}
*)

val to_int: t -> int option

val toString: t -> string
(** Convert a [float] to a {!String}

    The behaviour of this function is platform specific
*)

val to_string: t -> string

(** {1 Compare} *)

val equal: t -> t -> bool
(** Test two floats for equality *)

val compare: t -> t -> int
(** Compare two floats *)