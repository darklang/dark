// There are two coordinate systems. Pos is an absolute position in the canvas.
// Nodes and Edges have Pos'. VPos is the viewport: clicks occur within the
// viewport and we map Absolute positions back to the viewport to display in
// the browser.

@ppx.deriving(show({with_path: false}))
type rec t = {
  // The backend uses int (which is 64bit) for this, but it's a hassle to deal
  // with on the client, and no one should be scrolling 2B pixels anyway.
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

let center: t = {x: 475, y: 200}

let origin: t = {x: 0, y: 0}
