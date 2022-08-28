open Tc

type rect = {
  id: string,
  top: int,
  left: int,
  right: int,
  bottom: int,
}

exception NativeCodeError(string)

module Ext = {
  let window: Dom.window = %raw("(typeof window === undefined) ? window : {}")

  @val @scope("document")
  external _querySelector: string => Js.Nullable.t<Dom.element> = "querySelector"

  let querySelector = (s: string): option<Dom.element> => Js.Nullable.toOption(_querySelector(s))

  @get external clientWidth: Dom.element => int = "clientWidth"

  @get external clientHeight: Dom.element => int = "clientHeight"

  @bs.send external getBoundingClientRect: Dom.element => Dom.domRect = "getBoundingClientRect"

  @get external rectTop: Dom.domRect => float = "top"

  @get external rectBottom: Dom.domRect => float = "bottom"

  @get external rectRight: Dom.domRect => float = "right"

  @get external rectLeft: Dom.domRect => float = "left"

  @get external rectHeight: Dom.domRect => float = "height"

  @get external rectWidth: Dom.domRect => float = "width"

  let staticHost: unit => string = %raw("function(){ return staticUrl; }")

  @get external offsetTop: Dom.element => int = "offsetTop"

  let getBoundingClient = (e: Dom.element, s: string): rect => {
    let client = getBoundingClientRect(e)
    {
      id: s,
      top: rectTop(client) |> int_of_float,
      left: rectLeft(client) |> int_of_float,
      right: rectRight(client) |> int_of_float,
      bottom: rectBottom(client) |> int_of_float,
    }
  }

  @val @scope(("window", "location")) external redirect: string => unit = "replace"
}

module OffsetEstimator = {
  /* Takes a mouse event, ostensibly a `click` inside an BlankOr with id `elementID`
   * and produces an 0-based integer offset from the beginning of the BlankOrs content where
   * the click occurred on the DOM.
   *
   * ie. if the DOM element has "foobar" and the user clicks between the `o` and the `b`
   * the return value should be `4`.
   *
   * TODO: It's a super hacky estimate based on our common screen size at Dark and the default
   * font size and should be replaced with a proper implementation. But it's done us
   * okay so far. */
  let estimateClickOffset = (elementID: string, event: AppTypes.MouseEvent.t): option<int> =>
    switch Js.Nullable.toOption(Web_document.getElementById(elementID)) {
    | Some(elem) =>
      let rect = elem["getBoundingClientRect"]()
      if (
        event.mePos.vy >= int_of_float(rect["top"]) &&
          (event.mePos.vy <= int_of_float(rect["bottom"]) &&
          (event.mePos.vx >= int_of_float(rect["left"]) &&
            event.mePos.vx <= int_of_float(rect["right"])))
      ) {
        Some((event.mePos.vx - int_of_float(rect["left"])) / 8)
      } else {
        None
      }
    | None => None
    }
}

module Random = {
  let random = (): int => Js_math.random_int(0, 2147483647)

  let range = (min: int, max: int): int => Js_math.random_int(min, max)
}

module Location = {
  @val @scope(("window", "location")) external queryString: string = "search"

  @val @scope(("window", "location")) external hashString: string = "hash"

  @val @scope(("window", "location")) external reload: bool => unit = "reload"

  // TODO write string query parser
}

module Clipboard = {
  @module external copyToClipboard: string => unit = "clipboard-copy"
}

module Decoder = {
  let tuple2 = (decodeA, decodeB) => {
    open Tea.Json.Decoder
    Decoder(
      j =>
        switch Web.Json.classify(j) {
        | JSONArray(arr) =>
          if Js_array.length(arr) === 2 {
            switch (
              decodeValue(decodeA, Caml.Array.unsafe_get(arr, 0)),
              decodeValue(decodeB, Caml.Array.unsafe_get(arr, 1)),
            ) {
            | (Ok(a), Ok(b)) => Ok(a, b)
            | (Error(e1), _) => Error("tuple2[0] -> " ++ e1)
            | (_, Error(e2)) => Error("tuple2[1] -> " ++ e2)
            }
          } else {
            Error("tuple2 expected array with 2 elements")
          }
        | _ => Error("tuple2 expected array")
        },
    )
  }
}
