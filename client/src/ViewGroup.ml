open Types
open Prelude
open Tc

(* Dark *)
module B = Blank
module TL = Toplevel

type viewState = ViewUtils.viewState

type htmlConfig = ViewBlankOr.htmlConfig

let idConfigs = ViewBlankOr.idConfigs

let fontAwesome = ViewUtils.fontAwesome

let enterable = ViewBlankOr.Enterable

let wc = ViewBlankOr.wc

let blankOr2String (name : dbName blankOr) : dbName =
  B.valueWithDefault "" name


let viewGroupName (vs : viewState) (g : group) (preview : bool) : msg Html.html
    =
  if preview
  then
    Html.div
      [Html.class' "group-name"]
      [Html.p [] [Html.text (blankOr2String g.name)]]
  else
    let c = (enterable :: idConfigs) @ [wc ""] in
    let nameField = ViewBlankOr.viewText GroupName vs c g.name in
    Html.div [Html.class' "group-name form"] [nameField]


let previewMembers (tl : toplevel) : msg Html.html =
  let body =
    match tl with
    | TLHandler h ->
        let viewSpace =
          let space = blankOr2String h.spec.space in
          Html.p [Html.class' "member-text"] [Html.text space]
        in
        let viewMod =
          let modifier = blankOr2String h.spec.modifier in
          Html.p [Html.class' "member-text"] [Html.text modifier]
        in
        let viewName =
          let name = blankOr2String h.spec.name in
          Html.p [Html.class' "member-text"] [Html.text name]
        in
        [Html.div [Html.class' "member"] [viewSpace; viewMod; viewName]]
    | TLDB db ->
        let name = blankOr2String db.dbName in
        [ Html.div
            [Html.class' "member"]
            [Html.p [Html.class' "member-text"] [Html.text ("DB:  " ^ name)]]
        ]
    | _ ->
        []
  in
  Html.div [Html.class' "member-wrap"] body


let viewMember (vs : viewState) (tl : toplevel) : msg Html.html =
  let body, data =
    match tl with
    | TLHandler h ->
        (ViewCode.viewHandler vs h, ViewData.viewData vs h.ast)
    | TLDB db ->
        (ViewDB.viewDB vs db, [])
    | _ ->
        ([], [])
  in
  Html.div [Html.class' "member"] (body @ data)


let viewGroupMembers
    (m : model) (vs : viewState) (members : tlid list) (preview : bool) :
    msg Html.html =
  if List.length members == 0
  then
    let innerTxt = if preview then "" else "Drag inside here" in
    Html.div [Html.class' "children"] [Html.p [] [Html.text innerTxt]]
  else
    let memberList =
      members
      |> List.map ~f:(fun tlid ->
             let tl = TL.getExn m tlid in
             if preview then previewMembers tl else viewMember vs tl )
    in
    Html.div
      [Html.classList [("member-list", true); ("preview", preview)]]
      memberList


let viewGroup (m : model) (vs : viewState) (group : group) : msg Html.html =
  let isPreview = not (Some group.gTLID = tlidOf m.cursorState) in
  let nameView = viewGroupName vs group isPreview in
  (* Check here to see if group is empty *)
  let closeIcon =
    let hasMembers = List.length group.members > 0 in
    let delEvent =
      if (isPreview && hasMembers) || ((not isPreview) && hasMembers)
      then ViewUtils.nothingMouseEvent "click"
      else
        ViewUtils.eventNeither
          ~key:("entry-" ^ showTLID group.gTLID)
          "click"
          (fun _ -> DeleteGroup group.gTLID)
    in
    let errorText =
      if hasMembers
      then
        [ Html.p
            [Html.class' "error-text"]
            [Html.text "Can't delete groups with things inside"] ]
      else []
    in
    Html.div
      [ Html.classList
          [ ("delete-btn-wrap", true)
          ; ("preview", isPreview)
          ; ("empty", not hasMembers) ] ]
      ( [ Html.a
            [delEvent; Html.classList [("delete-btn", true)]]
            [fontAwesome "times"] ]
      @ errorText )
  in
  let groupMemberView = viewGroupMembers m vs group.members isPreview in
  Html.div
    [Html.class' "group-data"]
    [Html.div [Html.class' "group-top"] [nameView; closeIcon]; groupMemberView]
