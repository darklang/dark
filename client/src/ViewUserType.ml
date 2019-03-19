open Types
open Prelude

(* Dark *)
type viewState = ViewUtils.viewState

let idConfigs = ViewBlankOr.idConfigs

let wc = ViewBlankOr.wc

let enterable = ViewBlankOr.Enterable

let viewTipeName (vs : viewState) (t : userTipe) : msg Html.html =
  let nameField =
    let c = (enterable :: idConfigs) @ [wc "dbname"] in
    ViewBlankOr.viewText DBName vs c t.utName
  in
  Html.div [Html.class' "dbtitle"] [nameField]


let viewUserTipe (vs : viewState) (t : userTipe) : msg Html.html =
  let namediv = viewTipeName vs t in
  Html.div [Html.class' "user-type-toplevel"] [namediv]
