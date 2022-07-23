/// Basic types

module Belt = {
  include (Belt: module type of Belt with module Map := Belt.Map and module Result := Belt.Result)

  module Result = {
    include Belt.Result

    let pp = (
      okValueFormatter: (Format.formatter, 'okValue) => unit,
      errValueFormatter: (Format.formatter, 'errValue) => unit,
      fmt: Format.formatter,
      value: t<'okValue, 'errValue>,
    ) => {
      switch value {
      | Ok(value) =>
        Format.pp_print_string(fmt, "Ok")
        okValueFormatter(fmt, value)
      | Error(value) =>
        Format.pp_print_string(fmt, "Error")
        errValueFormatter(fmt, value)
      }
    }
  }

  module Map = {
    include (Belt.Map: module type of Belt.Map with module String := Belt.Map.String)

    module String = {
      include Belt.Map.String

      let pp = (
        valueFormatter: (Format.formatter, 'value) => unit,
        fmt: Format.formatter,
        map: t<'value>,
      ) => {
        Format.pp_print_string(fmt, "{ ")
        Belt.Map.String.forEach(map, (key, value) => {
          Format.pp_print_string(fmt, key)
          Format.pp_print_string(fmt, ": ")
          valueFormatter(fmt, value)
          Format.pp_print_string(fmt, ",  ")
        })
        Format.pp_print_string(fmt, "}")
        ()
      }
    }
  }
}

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
