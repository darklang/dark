open Tc
open Types
open Prelude

(* Dark *)
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


let viewTipeField (vs : viewState) (field : userRecordField) : msg Html.html =
  let row =
    [ viewFieldName vs [wc "name"] field.urfName
    ; viewFieldType vs [wc "type"] field.urfTipe ]
  in
  Html.div [Html.class' "field"] row


let viewUserTipe (vs : viewState) (t : userTipe) : msg Html.html =
  match t.utDefinition with UTRecord fields ->
    let nameDiv = viewTipeName vs t in
    let fieldDivs =
      Html.div [Html.class' "fields"] (List.map ~f:(viewTipeField vs) fields)
    in
    Html.div [Html.class' "user-type"] [nameDiv; fieldDivs]
