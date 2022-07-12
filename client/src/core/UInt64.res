/* Functions to represent uint64, encoded as an int64 due to other options (bs-zarith)
 * being extremely slow. Uses bs-zarith where it has to and nowhere else.
 *
 * We represent the range 0 to MAX_INT63 as the positive side of Int64, and the
 * remainder (MAX_INT63+1 to MAX_INT64) on the negative side.
 */

@ppx.deriving(show({with_path: false})) type rec t = int64

let toString = (id: t) => {
  if id >= 0L {
    Int64.to_string(id)
  } else {
    let x = Int64.abs(id)->Int64.sub(1L)->U.UInt64.ofInt64
    let m = Int64.max_int->U.UInt64.ofInt64
    (U.UInt64.add(x, m))->U.UInt64.toString
  }
}

let fromInt = (i : int) => Int64.of_int(i)
let fromInt64 = (i : int64) => i
let fromString = (s : string) =>

  switch U.UInt64.ofString(s) {
    | None => None
    | Some(i) => {
        if i > U.UInt64.ofInt64(Int64.max_int) {
          let max = U.UInt64.ofInt64(Int64.max_int)
          let actual = U.UInt64.sub(i, max)
          // Convert it to a negative int64
          Some(U.UInt64.toInt64(actual))
        } else {
          Some(U.UInt64.toInt64(i))
        }
    }
  }