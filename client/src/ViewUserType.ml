open Tc
open Types
open Prelude

(* Dark *)
module B = Blank

let fontAwesome = ViewUtils.fontAwesome

type viewState = ViewUtils.viewState

type htmlConfig = ViewBlankOr.htmlConfig

let idConfigs = ViewBlankOr.idConfigs

let wc = ViewBlankOr.wc

let enterable = ViewBlankOr.Enterable

let viewTipeName (vs : viewState) (t : userTipe) : msg Html.html =
  let nameField =
    let c = (enterable :: idConfigs) @ [wc "ut-name"] in
    ViewBlankOr.viewText TypeName vs c t.utName
  in
  Html.div [Html.class' "typetitle"] [nameField]


let viewFieldName (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  let configs = (enterable :: idConfigs) @ c in
  ViewBlankOr.viewText TypeFieldName vs configs v


let viewFieldType (vs : viewState) (c : htmlConfig list) (v : tipe blankOr) :
    msg Html.html =
  let configs = (enterable :: idConfigs) @ c in
  ViewBlankOr.viewTipe TypeFieldTipe vs configs v


let viewKillFieldBtn (t : userTipe) (field : userRecordField) : msg Html.html =
  Html.div
    [ Html.class' "field-btn allowed"
    ; ViewUtils.eventNoPropagation
        ~key:
          ( "dutf-"
          ^ showTLID t.utTLID
          ^ "-"
          ^ (field.urfName |> B.toID |> showID) )
        "click"
        (fun _ -> DeleteUserTypeField (t.utTLID, field)) ]
    [fontAwesome "times-circle"]


let viewTipeField
    (vs : viewState)
    (t : userTipe)
    (fieldCount : int)
    (field : userRecordField) : msg Html.html =
  let button =
    if fieldCount > 1 && vs.permission = Some ReadWrite
    then viewKillFieldBtn t field
    else Vdom.noNode
  in
  let row =
    [ viewFieldName vs [wc "name"] field.urfName
    ; viewFieldType vs [wc "type"] field.urfTipe
    ; button ]
  in
  Html.div [Html.class' "field"] row


let viewUserTipe (vs : viewState) (t : userTipe) : msg Html.html =
  match t.utDefinition with UTRecord fields ->
    let nameDiv = viewTipeName vs t in
    let fieldDivs =
      let fieldCount = List.length fields in
      Html.div
        [Html.class' "fields"]
        (List.map ~f:(viewTipeField vs t fieldCount) fields)
    in
    Html.div [Html.class' "user-type"] [nameDiv; fieldDivs]
