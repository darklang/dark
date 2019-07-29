(* open Tc *)
open Types
open Prelude

(* Dark *)
module B = Blank

type viewState = ViewUtils.viewState

type htmlConfig = ViewBlankOr.htmlConfig

let idConfigs = ViewBlankOr.idConfigs

let fontAwesome = ViewUtils.fontAwesome

let enterable = ViewBlankOr.Enterable

let wc = ViewBlankOr.wc

let groupName2String (name : dbName blankOr) : dbName = B.valueWithDefault "" name

let viewGroupName (vs : viewState) (g : group) : msg Html.html =
  let nameField =
    let c = (enterable :: idConfigs) @ [wc ""] in
    ViewBlankOr.viewText GroupName vs c g.name
  in
  Html.div [Html.class' "group-name"] [nameField]

let viewGroup (vs : viewState) (group: group): msg Html.html =
  let nameView = viewGroupName vs group in
  let closeIcon = Html.div
  [ Html.class' "delete-btn"]
  [fontAwesome "times"]
  in
  let groupChilren = [Html.div [Html.class' "children"] [Html.p [] [Html.text "Drag inside here"]]] in
  Html.div [Html.class' "group-data"] ([Html.div [Html.class' "group-top"] [nameView; closeIcon]] @ groupChilren)
