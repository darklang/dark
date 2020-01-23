open Prelude

(* Dark *)
module B = BlankOr
module TL = Toplevel

type htmlConfig =
  (* Add this class (can be done multiple times) *)
  | WithClass of string
  (* when you click this node, select this pointer *)
  | ClickSelectAs of id
  | ClickSelect
  (* highlight this node as if it were ID *)
  | MouseoverAs of id
  | Mouseover
  (* use this as ID for Mouseover, ClickSelect *)
  | WithID of id
  (* editable *)
  | Enterable
  (* Adds param name to the left *)
  | WithParamName of string

let wc (s : string) : htmlConfig = WithClass s

let idConfigs : htmlConfig list = [ClickSelect; Mouseover]

let viewParamName (name : string) : msg Html.html =
  let leftOffset = String.length name + 1 in
  let styles = [("margin-left", "-" ^ string_of_int leftOffset ^ "ch")] in
  Html.div [Html.class' "param-name"; Vdom.styles styles] [Html.text name]


(* Create a Html.div for this ID, incorporating all ID-related data, *)
(* such as whether it's selected, appropriate events, mouseover, etc. *)
let div
    (vs : ViewUtils.viewState)
    (configs : htmlConfig list)
    (content : msg Html.html list) : msg Html.html =
  let getFirst fn = configs |> List.filterMap ~f:fn |> List.head in
  (* Extract config *)
  let thisID =
    getFirst (fun a -> match a with WithID id -> Some id | _ -> None)
  in
  let clickAs =
    getFirst (fun a ->
        match a with
        | ClickSelectAs id ->
            Some id
        | ClickSelect ->
            thisID
        | _ ->
            None)
  in
  let mouseoverAs =
    getFirst (fun a ->
        match a with
        | MouseoverAs id ->
            Some id
        | Mouseover ->
            thisID
        | _ ->
            None)
  in
  let classes =
    configs
    |> List.filterMap ~f:(fun a ->
           match a with WithClass c -> Some c | _ -> None)
  in
  let selectedID =
    match vs.cursorState with Selecting (_, Some id) -> Some id | _ -> None
  in
  let selected = thisID = selectedID && Option.isSome thisID in
  let showParamName =
    configs
    |> List.filterMap ~f:(fun a ->
           match a with WithParamName v -> Some (viewParamName v) | _ -> None)
  in
  let mouseoverClass =
    let targetted =
      mouseoverAs = Option.map ~f:Tuple2.second vs.hovering
      && Option.isSome mouseoverAs
    in
    if targetted then ["mouseovered-selectable"] else []
  in
  let idClasses =
    match thisID with Some id -> ["blankOr"; "id-" ^ deID id] | _ -> []
  in
  let allClasses =
    classes
    @ idClasses
    @ (if selected then ["selected"] else [])
    @ mouseoverClass
  in
  let classAttr = Html.class' (String.join ~sep:" " allClasses) in
  let tlid = TL.id vs.tl in
  let events =
    match clickAs with
    | Some id ->
        [ ViewUtils.eventNoPropagation
            "click"
            ~key:("bcc-" ^ showTLID tlid ^ "-" ^ showID id)
            (fun x -> BlankOrClick (tlid, id, x))
        ; ViewUtils.eventNoPropagation
            "dblclick"
            ~key:("bcdc-" ^ showTLID tlid ^ "-" ^ showID id)
            (fun x -> BlankOrDoubleClick (tlid, id, x))
        ; ViewUtils.eventNoPropagation
            "mouseenter"
            ~key:("me-" ^ showTLID tlid ^ "-" ^ showID id)
            (fun x -> BlankOrMouseEnter (tlid, id, x))
        ; ViewUtils.eventNoPropagation
            "mouseleave"
            ~key:("ml-" ^ showTLID tlid ^ "-" ^ showID id)
            (fun x -> BlankOrMouseLeave (tlid, id, x)) ]
    | _ ->
        (* Rather than relying on property lengths changing, we should use
         * noProp to indicate that the property at idx N has changed. *)
        [Vdom.noProp; Vdom.noProp; Vdom.noProp; Vdom.noProp]
  in
  let leftSideHtml = showParamName in
  let idAttr =
    match thisID with Some i -> Html.id (showID i) | _ -> Vdom.noProp
  in
  let attrs = idAttr :: classAttr :: events in
  Html.div
  (* if the id of the blank_or changes, this whole node should be redrawn
     * without any further diffing. there's no good reason for the Vdom/Dom node
     * to be re-used for a different blank_or *)
    ~unique:(thisID |> Option.map ~f:showID |> Option.withDefault ~default:"")
    attrs
    (leftSideHtml @ content)


let text (vs : ViewUtils.viewState) (c : htmlConfig list) (str : string) :
    msg Html.html =
  div vs c [Html.text str]


let tipe (vs : ViewUtils.viewState) (c : htmlConfig list) (t : tipe) :
    msg Html.html =
  text vs c (Runtime.tipe2str t)


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
    (htmlFn : ViewUtils.viewState -> htmlConfig list -> 'a -> msg Html.html)
    (pt : blankOrType)
    (vs : ViewUtils.viewState)
    (c : htmlConfig list)
    (bo : 'a blankOr) : msg Html.html =
  let wID id = [WithID id] in
  let drawBlank id =
    div
      vs
      ([WithClass "blank"] @ c @ wID id)
      [Html.div [Html.class' "blank-entry"] [Html.text (placeHolderFor vs pt)]]
  in
  let drawFilled id fill =
    let configs = wID id @ c in
    htmlFn vs configs fill
  in
  let thisText =
    match bo with
    | F (id, fill) ->
        drawFilled id fill
    | Blank id ->
        drawBlank id
  in
  match vs.cursorState with
  | Entering (Filling (_, thisID)) ->
      let id = B.toID bo in
      if id = thisID
      then
        if vs.showEntry
        then
          let placeholder = placeHolderFor vs pt in
          div vs (c @ wID id) [ViewEntry.normalEntryHtml placeholder vs.ac]
        else Html.text vs.ac.value
      else thisText
  | _ ->
      thisText


let viewText
    (pt : blankOrType)
    (vs : ViewUtils.viewState)
    (c : htmlConfig list)
    (str : string blankOr) : msg Html.html =
  viewBlankOr text pt vs c str


let viewTipe
    (pt : blankOrType)
    (vs : ViewUtils.viewState)
    (c : htmlConfig list)
    (str : tipe blankOr) : msg Html.html =
  viewBlankOr tipe pt vs c str
