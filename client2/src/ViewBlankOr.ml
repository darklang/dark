open Tea
open! Porting
module B = Blank
module Attrs = Html.Attributes
open Prelude
module RT = Runtime
module TL = Toplevel
open Types
open ViewUtils

type htmlConfig =
  | WithClass of string
  | ClickSelectAs of id
  | ClickSelect
  | MouseoverAs of id
  | Mouseover
  | WithID of id
  | WithFF
  | WithEditFn of tlid

let wc (s : string) : htmlConfig = WithClass s

let idConfigs : htmlConfig list = [ClickSelect; Mouseover]

let atom : htmlConfig = wc "atom"

let nested : htmlConfig = wc "nested"

let text (vs : viewState) (c : htmlConfig list) (str : string) : msg Html.html
    =
  ( (div vs c <| [Html.div [Attrs.class_ "quote quote-start"] []])
  ^ [Html.text str] )
  ^ [Html.div [Attrs.class_ "quote quote-end"] []]

let keyword (vs : viewState) (c : htmlConfig list) (name : string) :
    msg Html.html =
  text vs (((atom :: wc "keyword") :: wc name) :: c) name

let tipe (vs : viewState) (c : htmlConfig list) (t : tipe) : msg Html.html =
  text vs c (Runtime.tipe2str t)

let withFeatureFlag (vs : viewState) (v : 'a blankOr) : htmlConfig list =
  if idOf vs.cursorState = Some (B.toID v) then [WithFF] else []

let withEditFn (vs : viewState) (v : nExpr blankOr) : htmlConfig list =
  if idOf vs.cursorState = Some (B.toID v) then
    match v with
    | F (_, FnCall (name, _, _)) -> (
      match List.find (Functions.sameName name) vs.ufns with
      | Some fn -> [WithEditFn fn.tlid]
      | _ -> [] )
    | _ -> []
  else []

let getLiveValue (lvs : lvDict) (ID id : id) : dval option = Dict.get id lvs

let renderLiveValue (vs : viewState) (id : id option) : string =
  let cursorLiveValue =
    match id with
    | Some (ID id) -> Dict.get id vs.currentResults.liveValues
    | _ -> None
  in
  match cursorLiveValue with Some dv -> RT.toRepr dv | _ -> ""

let div (vs : viewState) (configs : htmlConfig list)
    (content : msg Html.html list) : msg Html.html =
  let getFirst fn = configs |> List.filterMap fn |> List.head in
  let _ = "comment" in
  let thisID =
    getFirst (fun a -> match a with WithID id -> Some id | _ -> None)
  in
  let clickAs =
    getFirst (fun a ->
        match a with
        | ClickSelectAs id -> Some id
        | ClickSelect -> thisID
        | _ -> None )
  in
  let mouseoverAs =
    getFirst (fun a ->
        match a with
        | MouseoverAs id -> Some id
        | Mouseover -> thisID
        | _ -> None )
  in
  let classes =
    configs
    |> List.filterMap (fun a ->
           match a with WithClass c -> Some c | _ -> None )
  in
  let showFeatureFlag = List.member WithFF configs in
  let editFn =
    getFirst (fun a -> match a with WithEditFn id -> Some id | _ -> None)
  in
  let isCommandTarget =
    match vs.cursorState with
    | SelectingCommand (_, id) -> thisID = Some id
    | _ -> false
  in
  let selectedID =
    match vs.cursorState with Selecting (_, Some id) -> Some id | _ -> None
  in
  let selected = thisID = selectedID && Option.isJust thisID in
  let displayLivevalue =
    (thisID = idOf vs.cursorState && Option.isJust thisID) && vs.showLivevalue
  in
  let mouseover = mouseoverAs = vs.hovering && Option.isJust mouseoverAs in
  let idClasses =
    match thisID with
    | Some id -> ["blankOr"; "id-" ^ string_of_int (deID id)]
    | _ -> []
  in
  let allClasses =
    ( ( ( (classes ^ idClasses)
        ^ if displayLivevalue then ["display-livevalue"] else [] )
      ^ if selected then ["selected"] else [] )
    ^ if isCommandTarget then ["commandTarget"] else [] )
    ^ if mouseover then ["mouseovered"] else []
  in
  let classAttr = Attrs.class_ (String.join " " allClasses) in
  let events =
    match clickAs with
    | Some id ->
        [ eventNoPropagation "click" (BlankOrClick (vs.tl.id, id))
        ; eventNoPropagation "dblclick" (BlankOrDoubleClick (vs.tl.id, id))
        ; eventNoPropagation "mouseenter" (BlankOrMouseEnter (vs.tl.id, id))
        ; eventNoPropagation "mouseleave" (BlankOrMouseLeave (vs.tl.id, id)) ]
    | _ -> []
  in
  let liveValueAttr =
    Attrs.attribute "data-live-value" (renderLiveValue vs thisID)
  in
  let featureFlagHtml = if showFeatureFlag then [viewFeatureFlag] else [] in
  let editFnHtml =
    match editFn with
    | Some editFn_ -> [viewEditFn editFn_ showFeatureFlag]
    | None -> if showFeatureFlag then [viewCreateFn] else []
  in
  let rightSideHtml =
    Html.div [Attrs.class_ "expr-actions"] (featureFlagHtml ^ editFnHtml)
  in
  let attrs = (liveValueAttr :: classAttr) :: events in
  Html.div attrs (content ^ [rightSideHtml])

let viewText (pt : pointerType) (vs : viewState) (c : htmlConfig list)
    (str : string blankOr) : msg Html.html =
  viewBlankOr text pt vs c str

let viewTipe (pt : pointerType) (vs : viewState) (c : htmlConfig list)
    (str : tipe blankOr) : msg Html.html =
  viewBlankOr tipe pt vs c str

let placeHolderFor (vs : viewState) (id : id) (pt : pointerType) : string =
  let paramPlaceholder =
    vs.tl |> TL.asHandler
    |> Option.map (fun x -> x.ast)
    |> Option.andThen (fun ast ->
           match AST.getParamIndex ast id with
           | Some (name, index) -> (
             match Autocomplete.findFunction vs.ac name with
             | Some {parameters} -> List.getAt index parameters
             | None -> None )
           | _ -> None )
    |> Option.map (fun p -> ((p.name ^ ": ") ^ RT.tipe2str p.tipe) ^ "")
    |> Option.withDefault ""
  in
  match pt with
  | VarBind -> "varname"
  | EventName -> (
    match vs.handlerSpace with
    | HSHTTP -> "route"
    | HSCron -> "event name"
    | HSOther -> "event name"
    | HSEmpty -> "event name" )
  | EventModifier -> (
    match vs.handlerSpace with
    | HSHTTP -> "verb"
    | HSCron -> "event interval"
    | HSOther -> "event modifier"
    | HSEmpty -> "event modifier" )
  | EventSpace -> "event space"
  | Expr -> paramPlaceholder
  | Field -> "fieldname"
  | Key -> "keyname"
  | DBColName -> "db field name"
  | DBColType -> "db type"
  | DarkType -> "type"
  | DarkTypeField -> "fieldname"
  | FFMsg -> "flag name"
  | FnName -> "function name"
  | ParamName -> "param name"
  | ParamTipe -> "param type"

let viewBlankOr
    (htmlFn : ((viewState -> htmlConfig list) -> 'a) -> msg Html.html)
    (pt : pointerType) (vs : viewState) (c : htmlConfig list) (bo : 'a blankOr)
    : msg Html.html =
  let wID id = [WithID id] in
  let drawBlank id =
    div vs
      (([WithClass "blank"] ^ c) ^ wID id)
      [Html.text (placeHolderFor vs id pt)]
  in
  let drawFilled id fill =
    let configs = wID id ^ c in
    htmlFn vs configs fill
  in
  let thisText =
    match bo with
    | F (id, fill) -> drawFilled id fill
    | Blank id -> drawBlank id
  in
  match vs.cursorState with
  | Entering (Filling (_, thisID)) ->
      let id = B.toID bo in
      if id = thisID then
        if vs.showEntry then
          let allowStringEntry =
            if pt = Expr then StringEntryAllowed else StringEntryNotAllowed
          in
          let stringEntryWidth =
            if vs.tooWide then StringEntryShortWidth
            else StringEntryNormalWidth
          in
          let placeholder = placeHolderFor vs id pt in
          div vs
            (c ^ wID id)
            [ ViewEntry.entryHtml allowStringEntry stringEntryWidth placeholder
                vs.ac ]
        else Html.text vs.ac.value
      else thisText
  | SelectingCommand (tlid, id) ->
      if id = B.toID bo then
        Html.div
          [Attrs.class_ "selecting-command"]
          [ thisText
          ; ViewEntry.entryHtml StringEntryNotAllowed StringEntryNormalWidth
              "command" vs.ac ]
      else thisText
  | _ -> thisText

let viewFeatureFlag : msg Html.html =
  Html.div
    [ Attrs.class_ "flag"
    ; eventNoPropagation "click" (fun _ -> StartFeatureFlag) ]
    [fontAwesome "flag"]

let viewCreateFn : msg Html.html =
  Html.div
    [ Attrs.class_ "exfun"
    ; eventNoPropagation "click" (fun _ -> ExtractFunction) ]
    [fontAwesome "share-square"]

let viewEditFn (tlid : tlid) (hasFlagAlso : bool) : msg Html.html =
  let rightOffset = if hasFlagAlso then "-34px" else "-16px" in
  Html.a
    [ Attrs.class_ "edit-fn"
    ; Attrs.style [("right", rightOffset)]
    ; Attrs.href (Url.urlFor (Fn (tlid, Defaults.centerPos))) ]
    [fontAwesome "edit"]
