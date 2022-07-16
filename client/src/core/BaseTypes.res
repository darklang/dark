/// Basic types

@ppx.deriving(show({with_path: false}))
type rec pos = {
  // The backend uses int (which is 64bit) for this, but it's a hassle to deal with
  // on the client, and no one should be scrolling 2B pixels anyway.
  x: int,
  y: int,
}
let encodePos = (p: pos) => {
  open Json_encode_extended
  object_(list{("x", int(p.x)), ("y", int(p.y))})
}
let decodePos = (j): pos => {
  open Json.Decode
  {x: field("x", int, j), y: field("y", int, j)}
}

// CLEANUP: Move BlankOr to own module and file. Right now there's already a BlankOr
// files with functions in it.
@ppx.deriving(show({with_path: false}))
type rec blankOr<'a> =
  | Blank(ID.t)
  | F(ID.t, 'a)

let encodeBlankOr = (encoder: 'a => Js.Json.t, v: blankOr<'a>) => {
  open Json_encode_extended
  switch v {
  | F(i, s) => variant("Filled", list{ID.encode(i), encoder(s)})
  | Blank(i) => variant("Blank", list{ID.encode(i)})
  }
}

let decodeBlankOr = d => {
  open Json_decode_extended
  variants(list{
    ("Filled", variant2((id, v) => F(id, v), ID.decode, d)),
    ("Blank", variant1(id => Blank(id), ID.decode)),
  })
}
