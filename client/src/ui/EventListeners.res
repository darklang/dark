let decodeTransEvent = (fn: string => 'a, j): 'a => {
  open Json.Decode
  fn(field("propertyName", string, j))
}

let decodeAnimEvent = (fn: string => 'a, j): 'a => {
  open Json.Decode
  fn(field("animationName", string, j))
}

// Generic event, the the listener handle and do what it wants with the event object
let onEvent = (
  ~event: string,
  ~key: string,
  ~preventDefault=true,
  listener: Dom.event => 'msg,
): Vdom.property<'msg> =>
  Tea.Html.Events.onCB(event, key, evt => {
    if preventDefault {
      Webapi.Dom.Event.preventDefault(Obj.magic(evt))
    }
    Some(listener(Obj.magic(evt)))
  })

let eventBoth = (~key: string, event: string, constructor: MouseEvent.t => 'msg): Vdom.property<
  'msg,
> =>
  Tea.Html.Events.onWithOptions(
    ~key,
    event,
    {stopPropagation: false, preventDefault: false},
    Decoders.wrapDecoder(Json.Decode.map(constructor, MouseEvent.decode)),
  )

let eventPreventDefault = (
  ~key: string,
  event: string,
  constructor: MouseEvent.t => 'msg,
): Vdom.property<'msg> =>
  Tea.Html.Events.onWithOptions(
    ~key,
    event,
    {stopPropagation: false, preventDefault: true},
    Decoders.wrapDecoder(Json.Decode.map(constructor, MouseEvent.decode)),
  )

let eventNeither = (~key: string, event: string, constructor: MouseEvent.t => 'msg): Vdom.property<
  'msg,
> =>
  Tea.Html.Events.onWithOptions(
    ~key,
    event,
    {stopPropagation: true, preventDefault: true},
    Decoders.wrapDecoder(Json.Decode.map(constructor, MouseEvent.decode)),
  )

let scrollEventNeither = (
  ~key: string,
  event: string,
  constructor: ScrollEvent.t => 'msg,
): Vdom.property<'msg> =>
  Tea.Html.Events.onWithOptions(
    ~key,
    event,
    {stopPropagation: true, preventDefault: true},
    Decoders.wrapDecoder(Json.Decode.map(constructor, ScrollEvent.decode)),
  )

let eventNoPropagation = (
  ~key: string,
  event: string,
  constructor: MouseEvent.t => 'msg,
): Vdom.property<'msg> =>
  Tea.Html.Events.onWithOptions(
    ~key,
    event,
    {stopPropagation: true, preventDefault: false},
    Decoders.wrapDecoder(Json.Decode.map(constructor, MouseEvent.decode)),
  )

let onTransitionEnd = (~key: string, ~listener: string => 'msg): Vdom.property<'msg> =>
  Tea.Html.Events.onWithOptions(
    ~key,
    "transitionend",
    {stopPropagation: false, preventDefault: true},
    Decoders.wrapDecoder(decodeTransEvent(listener)),
  )

let onAnimationEnd = (~key: string, ~listener: string => 'msg): Vdom.property<'msg> =>
  Tea.Html.Events.onWithOptions(
    ~key,
    "animationend",
    {stopPropagation: false, preventDefault: true},
    Decoders.wrapDecoder(decodeAnimEvent(listener)),
  )

let nothingMouseEvent = (name: string): Vdom.property<AppTypes.msg> =>
  eventNoPropagation(~key="", name, _ =>
    // For fluid, we need to know about most (all?) mouseups
    if name == "mouseup" {
      AppTypes.Msg.IgnoreMouseUp
    } else {
      AppTypes.Msg.IgnoreMsg(name)
    }
  )
