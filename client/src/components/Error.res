// Supports the error bar (shown at the bottom of the editor when errors occur)

@ppx.deriving(show) type rec t = option<string>

let clear = _ => None

let default = None

let set = (str, _) => Some(str)

let asOption = t => t
