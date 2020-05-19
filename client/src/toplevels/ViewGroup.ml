open Prelude

(* Dark *)
module B = BlankOr
module TL = Toplevel

type viewProps = ViewUtils.viewProps

type domEventList = ViewUtils.domEventList

let fontAwesome = ViewUtils.fontAwesome

(* Spec for group views: https://docs.google.com/document/d/19dcGeRZ4c7PW9hYNTJ9A7GsXkS2wggH2h2ABqUw7R6A/edit#heading=h.isw58ukngvvf *)

let blankOr2String (name : string blankOr) : string = B.valueWithDefault "" name

let viewGroupName (vp : viewProps) (g : group) (preview : bool) : msg Html.html
    =
  if preview
  then
    Html.div
      [Html.class' "group-name"]
      [Html.p [] [Html.text (blankOr2String g.gName)]]
  else
    let nameField =
      ViewBlankOr.viewText ~enterable:true ~classes:[] GroupName vp g.gName
    in
    Html.div [Html.class' "group-name form"] [nameField]


let previewMembers (gTLID : TLID.t) (tl : toplevel) : msg Html.html =
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
            [Html.p [Html.class' "member-text"] [Html.text ("DB:  " ^ name)]] ]
    | _ ->
        []
  in
  let tlid =
    match tl with
    | TLHandler h ->
        h.hTLID
    | TLDB db ->
        db.dbTLID
    | _ ->
        recover
          "No other toplevel should be in a group"
          ~debug:tl
          (TLID.fromString "fake-id")
  in
  let event =
    ViewUtils.eventNoPropagation
      ~key:("tlmd-" ^ TLID.toString tlid)
      "dragend"
      (fun x -> DragGroupMember (gTLID, tlid, x))
  in
  Html.div
    [event; Vdom.attribute "" "draggable" "true"; Html.class' "member-wrap"]
    body


let viewMember (vp : viewProps) (tl : toplevel) : msg Html.html =
  let body, data =
    match tl with
    | TLHandler h ->
        (ViewHandler.view vp h [], ViewData.viewData vp)
    | TLDB db ->
        (ViewDB.viewDB vp db [], [])
    | _ ->
        ([], [])
  in
  Html.div [Html.class' "member"] (body @ data)


let viewGroupMembers
    (m : model)
    (vp : viewProps)
    (gTLID : TLID.t)
    (members : TLID.t list)
    (preview : bool) : msg Html.html =
  if List.length members == 0
  then
    let innerTxt = if preview then "" else "Drag inside here" in
    Html.div [Html.class' "children"] [Html.p [] [Html.text innerTxt]]
  else
    let memberList =
      members
      |> List.map ~f:(fun tlid ->
             match TL.get m tlid with
             | Some tl ->
                 if preview then previewMembers gTLID tl else viewMember vp tl
             | None ->
                 Html.noNode)
    in
    Html.div
      [Html.classList [("member-list", true); ("preview", preview)]]
      memberList


let viewGroup
    (m : model) (vp : viewProps) (group : group) (dragEvents : domEventList) :
    msg Html.html =
  (* Disabling detail view for now *)
  (* let isPreview = not (Some group.gTLID = tlidOf m.cursorState) in *)
  let isPreview = true in
  let nameView = viewGroupName vp group isPreview in
  (* Check here to see if group is empty *)
  let closeIcon =
    let hasMembers = List.length group.members > 0 in
    let delEvent =
      if (isPreview && hasMembers) || ((not isPreview) && hasMembers)
      then ViewUtils.nothingMouseEvent "click"
      else
        ViewUtils.eventNeither
          ~key:("entry-" ^ TLID.toString group.gTLID)
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
  let groupMemberView =
    viewGroupMembers m vp group.gTLID group.members isPreview
  in
  Html.div
    (Html.class' "group-data" :: dragEvents)
    [Html.div [Html.class' "group-top"] [nameView; closeIcon]; groupMemberView]
