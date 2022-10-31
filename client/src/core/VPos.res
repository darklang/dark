// There are two coordinate systems. Pos is an absolute position in the
// canvas. Nodes and Edges have Pos'. VPos is the viewport: clicks occur
// within the viewport and we map Absolute positions back to the
// viewport to display in the browser.
@ppx.deriving(show({with_path: false}))
type rec t = {
  vx: int,
  vy: int,
}

let encode = (vp: t) => {
  open Json_encode_extended
  object_(list{("vx", int(vp.vx)), ("vy", int(vp.vy))})
}

let decode = (j): t => {
  open Json_decode_extended
  {vx: field("vx", int, j), vy: field("vy", int, j)}
}

let toString = (vp: t) => {
  let x = string_of_int(vp.vx)
  let y = string_of_int(vp.vy)
  "VPos(" ++ x ++ ", " ++ y ++ ")"
}

let default: t = {vx: 475, vy: 200}
