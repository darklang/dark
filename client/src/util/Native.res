type rect = {
  id: string,
  top: int,
  left: int,
  right: int,
  bottom: int,
}

exception NativeCodeError(string)

// TODO: remove all this, and use Rescript-webapi instead
module Ext = {
  let window: Dom.window = %raw("(typeof window === undefined) ? window : {}")

  @val @scope("document")
  external _querySelector: string => Js.Nullable.t<Dom.element> = "querySelector"

  let querySelector = (s: string): option<Dom.element> => Js.Nullable.toOption(_querySelector(s))

  @get external clientWidth: Dom.element => int = "clientWidth"

  @get external clientHeight: Dom.element => int = "clientHeight"

  let staticHost: unit => string = %raw("function(){ return staticUrl; }")

  @get external offsetTop: Dom.element => int = "offsetTop"

  let getBoundingClient = (e: Dom.element, s: string): rect => {
    let client = Webapi.Dom.Element.getBoundingClientRect(e)
    {
      id: s,
      top: Webapi.Dom.DomRect.top(client) |> int_of_float,
      left: Webapi.Dom.DomRect.left(client) |> int_of_float,
      right: Webapi.Dom.DomRect.right(client) |> int_of_float,
      bottom: Webapi.Dom.DomRect.bottom(client) |> int_of_float,
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
  let estimateClickOffset = (elementID: string, event: MouseEvent.t): option<int> => {
    open Webapi.Dom
    let elem = document->Document.getElementById(elementID)
    switch elem {
    | Some(elem) =>
      let rect = elem->Element.getBoundingClientRect
      if (
        event.mePos.vy >= int_of_float(rect->DomRect.top) &&
          (event.mePos.vy <= int_of_float(rect->DomRect.bottom) &&
          (event.mePos.vx >= int_of_float(rect->DomRect.left) &&
            event.mePos.vx <= int_of_float(rect->DomRect.right)))
      ) {
        Some((event.mePos.vx - int_of_float(rect->DomRect.left)) / 8)
      } else {
        None
      }
    | None => None
    }
  }
}

module Random = {
  let random = (): int => Js_math.random_int(0, 2147483647)

  let range = (min: int, max: int): int => Js_math.random_int(min, max)
}

// TODO: remove all this, and use Rescript-webapi instead
module Location = {
  @val @scope(("window", "location")) external queryString: string = "search"

  @val @scope(("window", "location")) external hashString: string = "hash"

  @val @scope(("window", "location")) external reload: bool => unit = "reload"
}

// TODO: remove all this, and use Rescript-webapi instead
module Clipboard = {
  @module external copyToClipboard: string => unit = "clipboard-copy"
}
