@ppx.deriving(show) type t = option<string>

let clear = _ => None

let default = None

let set = (str, _) => Some(str)

let asOption = t => t
