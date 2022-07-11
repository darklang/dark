open Prelude

// Dark
module B = BlankOr
module TL = Toplevel

// Create a Html.div for this ID, incorporating all ID-related data,
// such as whether it's selected, appropriate events, mouseover, etc.
let div = (
  ~id: id,
  ~enterable: bool,
  ~classes: list<string>,
  vp: ViewUtils.viewProps,
  content: list<Html.html<msg>>,
): Html.html<msg> => {
  let selected = switch vp.cursorState {
  | Selecting(_, Some(selectingID)) => id == selectingID
  | _ => false
  }

  let mouseoverClass = {
    let targetted = enterable && Some(id) == Option.map(~f=Tuple2.second, vp.hovering)

    if targetted {
      list{"mouseovered-selectable"}
    } else {
      list{}
    }
  }

  let idClasses = list{"blankOr", "id-" ++ ID.toString(id)}
  let allClasses = Belt.List.concatMany([
    classes,
    idClasses,
    if selected {
      list{"selected"}
    } else {
      list{}
    },
    mouseoverClass,
  ])

  let classAttr = Html.class'(String.join(~sep=" ", allClasses))
  let events = if enterable {
    let tlid = TL.id(vp.tl)
    let keyStr = TLID.toString(tlid) ++ ("-" ++ ID.toString(id))
    let event = ViewUtils.eventNoPropagation
    list{
      event("click", ~key="bcc-" ++ keyStr, x => BlankOrClick(tlid, id, x)),
      event("dblclick", ~key="bcdc-" ++ keyStr, x => BlankOrDoubleClick(tlid, id, x)),
      event("mouseenter", ~key="me-" ++ keyStr, x => BlankOrMouseEnter(tlid, id, x)),
      event("mouseleave", ~key="ml-" ++ keyStr, x => BlankOrMouseLeave(tlid, id, x)),
    }
  } else {
    /* Rather than relying on property lengths changing, we should use
     * noProp to indicate that the property at idx N has changed. */
    list{Vdom.noProp, Vdom.noProp, Vdom.noProp, Vdom.noProp}
  }

  let idAttr = Html.id(ID.toString(id))
  let attrs = list{idAttr, classAttr, ...events}
  Html.div(
    /* if the id of the blank_or changes, this whole node should be redrawn
     * without any further diffing. there's no good reason for the Vdom/Dom node
     * to be re-used for a different blank_or */
    ~unique=ID.toString(id),
    attrs,
    content,
  )
}

let placeHolderFor = (vp: ViewUtils.viewProps, pt: blankOrType): string =>
  switch pt {
  | EventName =>
    switch TL.spaceOf(vp.tl) |> Option.unwrap(~default=HSDeprecatedOther) {
    | HSHTTP => "route"
    | HSWorker | HSDeprecatedOther | HSRepl | HSCron => "name"
    }
  | EventModifier =>
    switch TL.spaceOf(vp.tl) |> Option.unwrap(~default=HSDeprecatedOther) {
    | HSHTTP => "verb"
    | HSCron => "interval"
    | HSWorker | HSDeprecatedOther | HSRepl => "modifier"
    }
  | EventSpace => "event space"
  | DBName => "db name"
  | DBColName => "db field name"
  | DBColType => "db type"
  | FnName => "function name"
  | FnReturnTipe => "return type"
  | ParamName => "param name"
  | ParamTipe => "param type"
  | TypeName => "type name"
  | TypeFieldName => "field name"
  | TypeFieldTipe => "field type"
  }

let viewBlankOr = (
  ~enterable: bool,
  ~classes: list<string>,
  htmlFn: 'a => Html.html<msg>,
  pt: blankOrType,
  vp: ViewUtils.viewProps,
  bo: blankOr<'a>,
): Html.html<msg> => {
  let id = B.toID(bo)
  let thisText = switch bo {
  | F(_, fill) => div(~id, ~enterable, ~classes, vp, list{htmlFn(fill)})
  | Blank(_) =>
    div(
      ~id,
      ~enterable,
      ~classes=list{"blank", ...classes},
      vp,
      list{Html.div(list{Html.class'("blank-entry")}, list{Html.text(placeHolderFor(vp, pt))})},
    )
  }

  switch vp.cursorState {
  | Entering(_, thisID) =>
    let id = B.toID(bo)
    if id == thisID {
      if vp.showEntry {
        let placeholder = placeHolderFor(vp, pt)
        div(~id, ~enterable, ~classes, vp, list{ViewEntry.normalEntryHtml(placeholder, vp.ac)})
      } else {
        Html.text(vp.ac.value)
      }
    } else {
      thisText
    }
  | _ => thisText
  }
}

let viewText = (
  ~enterable: bool,
  ~classes: list<string>,
  pt: blankOrType,
  vp: ViewUtils.viewProps,
  str: blankOr<string>,
): Html.html<msg> => viewBlankOr(~enterable, ~classes, Html.text, pt, vp, str)

let viewTipe = (
  ~enterable: bool,
  ~classes: list<string>,
  pt: blankOrType,
  vp: ViewUtils.viewProps,
  str: blankOr<tipe>,
): Html.html<msg> => {
  let fn = t => Html.text(Runtime.tipe2str(t))
  viewBlankOr(~enterable, ~classes, fn, pt, vp, str)
}
