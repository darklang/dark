open Prelude

(* Dark *)
module B = BlankOr
module TL = Toplevel

type htmlConfig =
  (* Add this class (can be done multiple times) *)
  | WithClass of string
  (* Adds param name to the left *)
  | WithParamName of string

let wc (s : string) : htmlConfig = WithClass s

let viewParamName (name : string) : msg Html.html =
  let leftOffset = String.length name + 1 in
  let styles = [("margin-left", "-" ^ string_of_int leftOffset ^ "ch")] in
  Html.div [Html.class' "param-name"; Vdom.styles styles] [Html.text name]


(* Create a Html.div for this ID, incorporating all ID-related data, *)
(* such as whether it's selected, appropriate events, mouseover, etc. *)
let div
    ~(id : ID.t)
    ~(enterable : bool)
    (vs : ViewUtils.viewState)
    (configs : htmlConfig list)
    (content : msg Html.html list) : msg Html.html =
  let classes =
    configs
    |> List.filterMap ~f:(fun a ->
           match a with WithClass c -> Some c | _ -> None)
  in
  let selected =
    match vs.cursorState with
    | Selecting (_, Some selectingID) ->
        id = selectingID
    | _ ->
        false
  in
  let showParamName =
    configs
    |> List.filterMap ~f:(fun a ->
           match a with WithParamName v -> Some (viewParamName v) | _ -> None)
  in
  let mouseoverClass =
    let mouseoverAs = if enterable then Some id else None in
    let targetted =
      mouseoverAs = Option.map ~f:Tuple2.second vs.hovering
      && Option.isSome mouseoverAs
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
  let tlid = TL.id vs.tl in
  let events =
    if enterable
    then
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
  let leftSideHtml = showParamName in
  let idAttr = Html.id (ID.toString id) in
  let attrs = idAttr :: classAttr :: events in
  Html.div
  (* if the id of the blank_or changes, this whole node should be redrawn
     * without any further diffing. there's no good reason for the Vdom/Dom node
     * to be re-used for a different blank_or *)
    ~unique:(ID.toString id)
    attrs
    (leftSideHtml @ content)


let placeHolderFor (vs : ViewUtils.viewState) (pt : blankOrType) : string =
  match pt with
  | EventName ->
    ( match
        TL.spaceOf vs.tl |> Option.withDefault ~default:HSDeprecatedOther
      with
    | HSHTTP ->
        "route"
    | HSWorker | HSDeprecatedOther | HSRepl | HSCron ->
        "name" )
  | EventModifier ->
    ( match
        TL.spaceOf vs.tl |> Option.withDefault ~default:HSDeprecatedOther
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
    (htmlFn : 'a -> msg Html.html)
    (pt : blankOrType)
    (vs : ViewUtils.viewState)
    (configs : htmlConfig list)
    (bo : 'a blankOr) : msg Html.html =
  let id = B.toID bo in
  let thisText =
    match bo with
    | F (_, fill) ->
        div ~id ~enterable vs configs [htmlFn fill]
    | Blank _ ->
        div
          ~id
          ~enterable
          vs
          ([WithClass "blank"] @ configs)
          [ Html.div
              [Html.class' "blank-entry"]
              [Html.text (placeHolderFor vs pt)] ]
  in
  match vs.cursorState with
  | Entering (Filling (_, thisID)) ->
      let id = B.toID bo in
      if id = thisID
      then
        if vs.showEntry
        then
          let placeholder = placeHolderFor vs pt in
          div
            ~id
            ~enterable
            vs
            configs
            [ViewEntry.normalEntryHtml placeholder vs.ac]
        else Html.text vs.ac.value
      else thisText
  | _ ->
      thisText


let viewText
    ~(enterable : bool)
    (pt : blankOrType)
    (vs : ViewUtils.viewState)
    (c : htmlConfig list)
    (str : string blankOr) : msg Html.html =
  viewBlankOr ~enterable Html.text pt vs c str


let viewTipe
    ~(enterable : bool)
    (pt : blankOrType)
    (vs : ViewUtils.viewState)
    (c : htmlConfig list)
    (str : tipe blankOr) : msg Html.html =
  let fn t = Html.text (Runtime.tipe2str t) in
  viewBlankOr ~enterable fn pt vs c str
