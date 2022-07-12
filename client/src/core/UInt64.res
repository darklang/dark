/* Functions to represent uint64, encoded as an int64 due to other options being
 * extremely slow (bs-zarith) or challenging to use (ReScriptJs.BigInt does seem to
 * work with polymorphic comparators).
 *
 * The main thing this does is encode the range 0 to MAX_UINT63 as the positive side
 * of Int64, and the remainder (MAX_UINT63+1 to MAX_UINT64) on the negative side.
 *
 * It only even exposes the id as a positive UInt64 bit numbers - so toInt or toFloat
 * have option types, and toString and fromString always treat it exactly like a
 * UInt64 number, and don't work on negative numbers.
 */

module BigInt = ReScriptJs.Js.BigInt

// It's important that this be an int64 as we use polymorphic comparison in a lot of
// places, rescript builtin equality and comparison has significant issues.
type rec t = int64

let isValidFloat = (t: t) => t > 0L && t < 9007199254740992L
// let isValidInt = (t: t) => t > 0L && t <= 2147483647L

let fromInt = (i: int) => Int64.of_int(i)
let fromFloat = (f: float) => Int64.of_float(f)

let toFloat = (i: t) =>
  if isValidFloat(i) {
    Some(Int64.to_float(i))
  } else {
    None
  }

module BI = {
  let maxInt64 = BigInt.fromString("9223372036854775807")
}

// module I64 = {
//   let maxUInt32 = Int64.of_float(4294967295.0)
// }

/* This is the very important section. It turns the int64 into JS BigInt types that
 * can be used for actual UInt64 operations, like toString */
let toString = (i: t) =>
  if i >= 0L {
    Int64.to_string(i)
  } else {
    Int64.neg(i)->Int64.to_string->BigInt.fromString->BigInt.add(BI.maxInt64)->BigInt.toString
  }

/* This is the very important section. It turns the int64 into JS BigInt types that
 * can be used for actual UInt64 operations, like toString */
let fromString = (str: string) =>
  try {
    let bi = str->BigInt.fromString
    if bi > BI.maxInt64 {
      let bi = BigInt.sub(bi, BI.maxInt64)
      let i64 = bi->BigInt.toString->Int64.of_string
      Some(Int64.neg(i64))
    } else {
      Some(bi->BigInt.toString->Int64.of_string)
    }
  } catch {
  | _ => None
  }

let max = -1L

let pp = (f, i) => Format.pp_print_string(f, toString(i))

let compare = (i1, i2) =>
  if i1 >= 0L {
    if i2 >= 0L {
      Int64.compare(i1, i2)
    } else {
      Int64.compare(i1, -1L)
    }
  } else if (
    // i1 is negative
    i2 >= 0L
  ) {
    Int64.compare(-1L, i2)
  } else {
    Int64.compare(Int64.neg(i1), Int64.neg(i2))
  }
