module Tablecloth.Float

// Functions for working with floating point numbers.

type t = float

type radians = t

let fromInt (i : Tablecloth.Int.t) = float i

let from_int i = fromInt i

let fromString string : t option =
  try
    Some(System.Double.Parse string)
  with _ -> None


let from_string s = fromString s

let zero = 0.0

let one = 1.0

let nan = System.Double.NaN

let infinity = System.Double.PositiveInfinity

let negativeInfinity = System.Double.NegativeInfinity

let negative_infinity = negativeInfinity

let e = System.Math.E

let pi = System.Math.PI

let epsilon = (System.Math.BitIncrement 1.0) - 1.0

let maximumSafeInteger = (2. ** 52.) - 1.

let maximum_safe_integer = maximumSafeInteger

let minimumSafeInteger = (-2. ** 52.) - 1.

let minimum_safe_integer = minimumSafeInteger

let largestValue = System.Double.MaxValue

let largest_value = largestValue

let smallestValue = epsilon

let smallest_value = smallestValue

let add (a : t) (b : t) = (+) a b

let (+) (a : t) (b : t) = (+) a b

let subtract (a : t) (b : t) = (-) a b

let (-) (a : t) (b : t) = (-) a b

let multiply (a : t) (b : t) = a * b

let (*) (a : t) (b : t) = a * b

let divide (by : t) (n : t) = n / by

let (/) (a : t) (b : t) = a / b

let power (exponent : t) (``base`` : t) : t = ``base`` ** exponent

let ( ** ) (a : t) (b : t) = a ** b

let negate (a : t) = (~-) a

let (~-) (a : t) = negate a

let absolute (a : t) = abs a

let isInteger (f : t) = f = System.Math.Floor f

let is_integer f = isInteger f

let isSafeInteger t = isInteger t && t <= maximumSafeInteger

let is_safe_integer f = isSafeInteger f

let clamp lower upper n =
  let isNaN = System.Double.IsNaN

  if upper < lower then
    raise (
      System.ArgumentOutOfRangeException(
        "upper",
        $"lower ({lower}) must be less than or equal to upper: ({upper})"
      )
    )
  else if isNaN lower || isNaN upper || isNaN n then
    System.Double.NaN
  else
    max lower (min upper n)


let inRange (lower : t) (upper : t) (n : t) : bool =
  let isNaN = System.Double.IsNaN

  if upper < lower then
    raise (
      System.ArgumentOutOfRangeException(
        "upper",
        $"lower ({lower}) must be less than or equal to upper: ({upper})"
      )
    )
  else
    n >= lower && n < upper


let in_range l u f = inRange l u f

let squareRoot (f : t) = sqrt f

let square_root (f : t) = squareRoot f

let log ``base`` n = log10 n / log10 ``base``

let isNaN (f : t) = System.Double.IsNaN f

let is_nan (f : t) = isNaN f

let isInfinite f = System.Double.IsInfinity f

let is_infinite f = isInfinite f

let isFinite n = (not (isInfinite n)) && not (isNaN n)

let is_finite n = isFinite n

let maximum x y =
  if isNaN x || isNaN y then nan
  else if y > x then y
  else x

let minimum x y =
  if isNaN x || isNaN y then nan
  else if y < x then y
  else x

let hypotenuse x y = squareRoot ((x * x) + (y * y))

let degrees n = n * (pi / 180.0)

let radians (f : t) = f

let turns n = n * 2. * pi

let cos n = System.Math.Cos n

let acos n = System.Math.Acos n

let sin n = System.Math.Sin n

let asin n = System.Math.Asin n

let tan n = System.Math.Tan n

let atan n = System.Math.Atan n

let atan2 y x = System.Math.Atan2(y, x)

type Direction =
  | Zero
  | AwayFromZero
  | Up
  | Down
  | Closest of Direction
  | ClosestToEven

let round (direction : Direction) (n : t) : t =
  match direction with
  | Up -> ceil n
  | Down -> floor n
  | Zero -> truncate n
  | AwayFromZero -> if n > 0. then ceil n else floor n
  | Closest Zero -> if n > 0. then ceil (n - 0.5) else floor (n + 0.5)
  | Closest AwayFromZero -> if n > 0. then floor (n + 0.5) else ceil (n - 0.5)
  | Closest Down -> ceil (n - 0.5)
  | Closest Up -> round n
  | Closest ClosestToEven
  | ClosestToEven ->
      let roundNearestLowerBound = -(2. ** 52.) in
      let roundNearestUpperBound = 2. ** 52. in

      if n <= roundNearestLowerBound || n >= roundNearestUpperBound then
        n + 0.
      else
        let floor = floor n in
        let ceil_or_succ = floor + 1. in
        let diff_floor = n - floor in
        let diff_ceil = ceil_or_succ - n in

        if diff_floor < diff_ceil then floor
        else if diff_floor > diff_ceil then ceil_or_succ
        else if floor % 2. = 0. then floor
        else ceil_or_succ


let floor n = floor n

let ceiling n = ceil n

let truncate n = truncate n

let fromPolar (r, theta) = (r * cos theta, r * sin theta)

let from_polar (r, theta) = fromPolar (r, theta)

let toPolar (x, y) = (hypotenuse x y, atan2 x y)

let to_polar (x, y) = toPolar (x, y)

let toInt (f : t) : int option =
  if isNaN f then None
  else if isInfinite f then None
  else Some(round Zero f |> int)

let to_int f = toInt f

let toString (f : t) = f.ToString()

let to_string f = toString f

let equal (f1 : t) (f2 : t) = (=) f1 f2

let compare (f1 : t) (f2 : t) = f1.CompareTo f2
