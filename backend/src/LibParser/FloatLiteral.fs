/// Exact decimal precision validation and exponent-free float normalization.
module LibParser.FloatLiteral

open System.Numerics

type Error =
  | Overflow
  | Underflow
  | Precision
  | Invalid

let diagnostic (error : Error) : string * string =
  match error with
  | Overflow -> "PARSE-FLOAT-OVERFLOW", "Float literal overflows binary64"
  | Underflow -> "PARSE-FLOAT-UNDERFLOW", "Nonzero float literal underflows to zero"
  | Precision ->
    "PARSE-FLOAT-PRECISION", "Float literal loses precision specified in the source"
  | Invalid -> "PARSE-FLOAT-INVALID", "Invalid float literal"

let private digits (text : string) : bool =
  text |> Seq.forall (fun c -> c >= '0' && c <= '9')

let private trimLeadingZeros (text : string) : string =
  let trimmed = text.TrimStart '0'
  if trimmed = "" then "0" else trimmed

/// A finite nonzero binary64 bounds the effective decimal exponent by the
/// mantissa's source length plus 324. Saturation avoids enormous powers of ten
/// even when the source exponent itself contains arbitrarily many digits.
let private exponent (limit : int) (text : string) : Option<int> =
  let start, sign =
    if text.StartsWith '-' then 1, -1
    elif text.StartsWith '+' then 1, 1
    else 0, 1
  let rec loop index value =
    if index = text.Length then
      Some(sign * value)
    elif text[index] < '0' || text[index] > '9' then
      None
    else
      let digit = int text[index] - int '0'
      let value = if value > (limit - digit) / 10 then limit else value * 10 + digit
      loop (index + 1) value
  if start = text.Length then None else loop start 0

let private binaryRatio (value : float) : bigint * bigint =
  let bits = System.BitConverter.DoubleToUInt64Bits(abs value)
  let encodedExponent = int ((bits >>> 52) &&& 0x7ffUL)
  let fraction = bits &&& 0x000fffffffffffffUL
  let significand, power =
    if encodedExponent = 0 then
      fraction, -1074
    else
      fraction ||| 0x0010000000000000UL, encodedExponent - 1075
  let significand = bigint significand
  if power >= 0 then significand <<< power, 1I else significand, 1I <<< -power

let private preservesPrecision
  (value : float)
  (coefficient : bigint)
  (power : int)
  : bool =
  let numerator, denominator = binaryRatio value
  let numerator, denominator =
    if power >= 0 then
      numerator, denominator * BigInteger.Pow(10I, power)
    else
      numerator * BigInteger.Pow(10I, -power), denominator
  // Half a unit at the last specified decimal place is a tie. The source
  // coefficient must be even to win that tie under round-to-nearest-even.
  let distance = abs (numerator - coefficient * denominator)
  let comparison = compare (distance <<< 1) denominator
  comparison < 0 || (comparison = 0 && coefficient.IsEven)

let private normalized
  (whole : string)
  (fraction : string)
  (power : int)
  : string * string =
  let allDigits = whole + fraction
  let point = whole.Length + power
  let whole, fraction =
    if point <= 0 then
      "0", System.String('0', -point) + allDigits
    elif point >= allDigits.Length then
      allDigits + System.String('0', point - allDigits.Length), "0"
    else
      allDigits.Substring(0, point), allDigits.Substring(point)
  trimLeadingZeros whole, (if fraction = "" then "0" else fraction)

let parse (source : string) (value : float) : Result<string * string, Error> =
  let source = if source.StartsWith '-' then source.Substring 1 else source
  let decimal =
    match source.Split([| 'e'; 'E' |]) with
    | [| mantissa |] -> Some(mantissa, "0")
    | [| mantissa; power |] -> Some(mantissa, power)
    | _ -> None
  match decimal with
  | None -> Error Invalid
  | Some(mantissa, powerText) ->
    let parts =
      match mantissa.Split '.' with
      | [| whole |] -> Some(whole, "")
      | [| whole; fraction |] -> Some(whole, fraction)
      | _ -> None
    match parts with
    | None -> Error Invalid
    | Some(whole, fraction) when
      whole + fraction = "" || not (digits whole && digits fraction)
      ->
      Error Invalid
    | Some(whole, fraction) ->
      let coefficientText = trimLeadingZeros (whole + fraction)
      if System.Double.IsNaN value then
        Error Invalid
      elif System.Double.IsInfinity value then
        Error Overflow
      elif value = 0.0 && coefficientText <> "0" then
        Error Underflow
      elif value = 0.0 then
        Ok("0", "0")
      else
        match exponent (source.Length + 400) powerText with
        | None -> Error Invalid
        | Some power ->
          let coefficient = BigInteger.Parse coefficientText
          if preservesPrecision value coefficient (power - fraction.Length) then
            Ok(normalized whole fraction power)
          else
            Error Precision
