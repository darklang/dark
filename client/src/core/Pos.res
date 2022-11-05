@ppx.deriving(show({with_path: false}))
type rec t = {
  // The backend uses int (which is 64bit) for this, but it's a hassle to deal with
  // on the client, and no one should be scrolling 2B pixels anyway.
  x: int,
  y: int,
}
let encode = (p: t): Js.Json.t => {
  open Json_encode_extended
  object_(list{("x", int(p.x)), ("y", int(p.y))})
}
let decode = (j: Js.Json.t): t => {
  open Json.Decode
  {x: field("x", int, j), y: field("y", int, j)}
}

let toString = (p: t): string => {
  "(" ++ string_of_int(p.x) ++ ", " ++ string_of_int(p.y) ++ ")"
}

let center: t = {x: 475, y: 200}

let origin: t = {x: 0, y: 0}
