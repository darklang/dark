// TODO: can this be replaced with a type from a library?
@ppx.deriving(show({with_path: false}))
type rec t = {
  mePos: VPos.t,
  button: int,
  altKey: bool,
  ctrlKey: bool,
  shiftKey: bool,
  detail: int,
}
let decode = (j: Js.Json.t): t => {
  open Json.Decode
  {
    // We decode floats b/c newer Chromes may use floats instead of ints; we
    // then truncate rather than moving to floats everywhere due to concerns
    // about sending data back to browsers whose DOMs don't support float
    // positions - see https://github.com/darklang/dark/pull/2016 for
    // discussion, and
    // https://drafts.csswg.org/cssom-view/#extensions-to-the-window-interface
    // for the spec
    mePos: {
      vx: field("pageX", Json.Decode.float, j) |> truncate,
      vy: field("pageY", Json.Decode.float, j) |> truncate,
    },
    button: field("button", int, j),
    ctrlKey: field("ctrlKey", bool, j),
    shiftKey: field("shiftKey", bool, j),
    altKey: field("altKey", bool, j),
    detail: field("detail", int, j),
  }
}
