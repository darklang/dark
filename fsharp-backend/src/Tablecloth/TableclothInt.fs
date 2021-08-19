module Tablecloth.Int

// Fixed precision integers

type t = int

let minimumValue = System.Int32.MinValue

let minimum_value = minimumValue

let maximumValue = System.Int32.MaxValue

let maximum_value = maximumValue

let zero = 0

let one = 1

let add a b = (+) a b

let (+) a b = (+) a b

let subtract a b = (-) a b

let (-) a b = (-) a b

let multiply a b = (*) a b

let (*) a b = multiply a b

let divide by n = n / by

let (/.) (a : int) (b : int) = (/) (float a) (float b)

let (/) (a : int) (b : int) = (/) a b

let power (exponent : int) (``base`` : int) : int =
  int (float ``base`` ** float exponent)

let ( ** ) a b = power a b

let negate a = (~-) a

let (~-) a = (~-) a

let modulo by n = (if n < 0 then 2 * abs n else n) % by

let ``mod`` by n = modulo n by

let remainder by n = n % by

let maximum (a : int) (b : int) = max a b

let minimum (a : int) (b : int) = min a b

let absolute n = if n < 0 then n * -1 else n

let isEven (n : int) : bool = modulo 2 n = 0

let is_even n : bool = isEven n

let isOdd n : bool = modulo 2 n <> 0

let is_odd n : bool = isOdd n

let clamp (lower : int) (upper : int) (n : int) =
  if upper < lower then
    raise (
      System.ArgumentOutOfRangeException(
        "upper",
        $"lower ({lower}) must be less than or equal to upper: ({upper})"
      )
    )
  else
    max lower (min upper n)


let inRange (lower : int) (upper : int) (n : int) =
  if upper < lower then
    raise (
      System.ArgumentOutOfRangeException(
        "upper",
        $"lower ({lower}) must be less than or equal to upper: ({upper})"
      )
    )
  else
    n >= lower && n < upper


let in_range l u n = inRange l u n

let toFloat i = float i

let to_float i = toFloat i

let toString (i : int) = i.ToString()

let to_string i = toString i

let fromString (str : string) : int option =
  let mutable result = 0
  if System.Int32.TryParse(str, &result) then Some result else None

let from_string str = fromString str

let equal (a : int) (b : int) = (=) a b

let compare (a : int) (b : int) = a.CompareTo b
