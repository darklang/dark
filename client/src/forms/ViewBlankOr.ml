open Prelude

(* Dark *)
module B = BlankOr
module TL = Toplevel
module Attributes = Tea.Html2.Attributes

(* Create a Html.div for this ID, incorporating all ID-related data, *)
(* such as whether it's selected, appropriate events, mouseover, etc. *)
let div
    ~(id : ID.t)
    ~(enterable : bool)
    ~(classes : string list)
    (vp : ViewUtils.viewProps)
    (content : msg Html.html list) : msg Html.html =
  let selected =
    match vp.cursorState with
    | Selecting (_, Some selectingID) ->
        id = selectingID
    | _ ->
        false
  in
  let mouseoverClass =
    let targetted =
      enterable && Some id = Option.map ~f:Tuple2.second vp.hovering
    in
    if targetted then ["mouseovered-selectable"] else []
  in
  let idClasses = ["blankOr"; "id-" ^ ID.toString id] in
  let allClasses =
    classes
    @ idClasses
    @ (if selected then ["selected"] else [])
    @ mouseoverClass
  in
  let classAttr = Html.class' (String.join ~sep:" " allClasses) in
  let events =
    if enterable
    then
      let tlid = TL.id vp.tl in
      let keyStr = TLID.toString tlid ^ "-" ^ ID.toString id in
      let event = ViewUtils.eventNoPropagation in
      [ event "click" ~key:("bcc-" ^ keyStr) (fun x ->
            BlankOrClick (tlid, id, x))
      ; event "dblclick" ~key:("bcdc-" ^ keyStr) (fun x ->
            BlankOrDoubleClick (tlid, id, x))
      ; event "mouseenter" ~key:("me-" ^ keyStr) (fun x ->
            BlankOrMouseEnter (tlid, id, x))
      ; event "mouseleave" ~key:("ml-" ^ keyStr) (fun x ->
            BlankOrMouseLeave (tlid, id, x)) ]
    else
      (* Rather than relying on property lengths changing, we should use
       * noProp to indicate that the property at idx N has changed. *)
      [Vdom.noProp; Vdom.noProp; Vdom.noProp; Vdom.noProp]
  in
  let idAttr = Html.id (ID.toString id) in
  let attrs = idAttr :: classAttr :: events in
  Html.div
  (* if the id of the blank_or changes, this whole node should be redrawn
     * without any further diffing. there's no good reason for the Vdom/Dom node
     * to be re-used for a different blank_or *)
    ~unique:(ID.toString id)
    attrs
    content


let placeHolderFor (vp : ViewUtils.viewProps) (pt : blankOrType) : string =
  match pt with
  | EventName ->
    ( match
        TL.spaceOf vp.tl |> Option.withDefault ~default:HSDeprecatedOther
      with
    | HSHTTP ->
        "route"
    | HSWorker | HSDeprecatedOther | HSRepl | HSCron ->
        "name" )
  | EventModifier ->
    ( match
        TL.spaceOf vp.tl |> Option.withDefault ~default:HSDeprecatedOther
      with
    | HSHTTP ->
        "verb"
    | HSCron ->
        "interval"
    | HSWorker | HSDeprecatedOther | HSRepl ->
        "modifier" )
  | EventSpace ->
      "event space"
  | DBName ->
      "db name"
  | DBColName ->
      "db field name"
  | DBColType ->
      "db type"
  | FnName ->
      "function name"
  | FnDescription ->
      "What does this function do?"
  | FnReturnTipe ->
      "return type"
  | ParamName ->
      "param name"
  | ParamTipe ->
      "param type"
  | TypeName ->
      "type name"
  | TypeFieldName ->
      "field name"
  | TypeFieldTipe ->
      "field type"
  | GroupName ->
      "group name"


let viewBlankOr
    ~(enterable : bool)
    ~(classes : string list)
    (htmlFn : 'a -> msg Html.html)
    (pt : blankOrType)
    (vp : ViewUtils.viewProps)
    (bo : 'a blankOr) : msg Html.html =
  let id = B.toID bo in
  let thisText =
    match bo with
    | F (_, fill) ->
        div ~id ~enterable ~classes vp [htmlFn fill]
    | Blank _ ->
        div
          ~id
          ~enterable
          ~classes:("blank" :: classes)
          vp
          [ Html.div
              [Html.class' "blank-entry"]
              [Html.text (placeHolderFor vp pt)] ]
  in
  match vp.cursorState with
  | Entering (Filling (_, thisID)) ->
      let id = B.toID bo in
      if id = thisID
      then
        if vp.showEntry
        then
          let placeholder = placeHolderFor vp pt in
          div
            ~id
            ~enterable
            ~classes
            vp
            [ViewEntry.normalEntryHtml placeholder vp.ac]
        else Html.text vp.ac.value
      else thisText
  | _ ->
      thisText


let viewMultilineText
    ~(enterable : bool)
    ~(classes : string list)
    (_pt : blankOrType)
    (vp : ViewUtils.viewProps)
    (str : string blankOr) : msg Html.html =
  let id = B.toID str in
  let unique = ID.toString id in
  let classAttr = Html.class' (String.join ~sep:" " classes) in
  let events =
    if enterable
    then
      let tlid = TL.id vp.tl in
      let keyStr = TLID.toString tlid ^ "-" ^ ID.toString id in
      let event = ViewUtils.eventNoPropagation in
      [ event "click" ~key:("bcc-" ^ keyStr) (fun x ->
            BlankOrClick (tlid, id, x))
      ; event "dblclick" ~key:("bcdc-" ^ keyStr) (fun x ->
            BlankOrDoubleClick (tlid, id, x))
      ; event "mouseenter" ~key:("me-" ^ keyStr) (fun x ->
            BlankOrMouseEnter (tlid, id, x))
      ; event "mouseleave" ~key:("ml-" ^ keyStr) (fun x ->
            BlankOrMouseLeave (tlid, id, x)) ]
    else
      (* Rather than relying on property lengths changing, we should use
       * noProp to indicate that the property at idx N has changed. *)
      [Vdom.noProp; Vdom.noProp; Vdom.noProp; Vdom.noProp]
  in
  let idAttr =
    match vp.cursorState with
    | Entering (Filling (_, thisID)) when thisID = id ->
        (* Other blankors entirely change the HTML structure when being edited.
         * Use the same id as other being-edited blankors for consistency. *)
        Html.id Defaults.entryID
    | _ ->
        Html.id (ID.toString id)
  in
  let readonly =
    (* Attribute.readonly is fixed in bstea 0.15.0 *)
    if enterable then Vdom.noProp else Vdom.attribute "" "readonly" "readonly"
  in
  let preventPanningOnScroll = ViewUtils.nothingMouseEvent "wheel" in
  let attrs =
    idAttr
    :: readonly
    :: Html.placeholder "What does this function do?"
    :: Attributes.rows 3
    :: preventPanningOnScroll
    :: classAttr
    :: events
  in
  let txt = match str with F (_, str) -> str | Blank _ -> "" in
  Html.textarea ~unique attrs [Html.text txt]


let viewText
    ~(enterable : bool)
    ~(classes : string list)
    (pt : blankOrType)
    (vp : ViewUtils.viewProps)
    (str : string blankOr) : msg Html.html =
  viewBlankOr ~enterable ~classes Html.text pt vp str


let viewTipe
    ~(enterable : bool)
    ~(classes : string list)
    (pt : blankOrType)
    (vp : ViewUtils.viewProps)
    (str : tipe blankOr) : msg Html.html =
  let fn t = Html.text (Runtime.tipe2str t) in
  viewBlankOr ~enterable ~classes fn pt vp str
