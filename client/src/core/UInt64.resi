@ppx.deriving(show({with_path: false})) type rec t = int64

let toString: t => string
let fromString: string => option<t>
let fromInt: int => t
let fromInt64: int64 => t