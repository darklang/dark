type rec t

let compare: (t, t) => int

let fromString: string => option<t>
let toString: t => string

let fromInt: int => t

/* Returns a float only if the value is within the range 0 to 2^53, which is the range
 * in which the ID can be accurately represented as a float. */
let toFloat: t => option<float>
let fromFloat: float => t

let max: t

let pp: (Format.formatter, t) => unit
