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
    let c = (enterable :: idConfigs) @ [wc "dbname"] in
    ViewBlankOr.viewText TypeName vs c t.utName
  in
  Html.div [Html.class' "dbtitle"] [nameField]


let viewFieldName (vs : viewState) (_c : htmlConfig list) (v : string blankOr)
    : msg Html.html =
  let configs = enterable :: idConfigs in
  ViewBlankOr.viewText TypeFieldName vs configs v


let viewFieldType (vs : viewState) (_c : htmlConfig list) (v : tipe blankOr) :
    msg Html.html =
  let configs = enterable :: idConfigs in
  ViewBlankOr.viewTipe TypeFieldTipe vs configs v


let viewTipeField (vs : viewState) (field : userRecordField) : msg Html.html =
  let row =
    [ viewFieldName vs [wc "name"] field.urfName
    ; viewFieldType vs [wc "type"] field.urfTipe ]
  in
  Html.div [Html.class' "col"] row


let viewUserTipe (vs : viewState) (t : userTipe) : msg Html.html =
  match t.utDefinition with UTRecord fields ->
    Js.log2 "fields" fields ;
    let nameDiv = viewTipeName vs t in
    let fieldDivs = List.map ~f:(viewTipeField vs) fields in
    Html.div [Html.class' "user-type-toplevel"] (nameDiv :: fieldDivs)
