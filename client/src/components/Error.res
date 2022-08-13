// Defines an Error message type and related functions
// Errors are currently represented as a simple option<string>

@ppx.deriving(show) type rec t = option<string>

let clear = _ => None

let default = None

let set = (str, _) => Some(str)

let asOption = t => t
