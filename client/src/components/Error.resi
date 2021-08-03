@ppx.deriving(show) type t

let clear: t => t

let default: t

let set: (string, t) => t

let asOption: t => option<string>
