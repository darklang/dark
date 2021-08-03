@ppx.deriving(show) type rec t = option<string>

let clear = _ => None

let default = None

let set = (str, _) => Some(str)

let asOption = t => t
