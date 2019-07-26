(* open Tc *)
open Types
open Prelude

(* Dark *)
module B = Blank

type viewState = ViewUtils.viewState

type htmlConfig = ViewBlankOr.htmlConfig

let idConfigs = ViewBlankOr.idConfigs

let fontAwesome = ViewUtils.fontAwesome

let groupName2String (name : dbName blankOr) : dbName =
  B.valueWithDefault "" name


let viewGroup (_vs : viewState) (group : group) : msg Html.html =
  let name = groupName2String group.name in
  let nameView = [Html.p [Html.class' "group-name"] [Html.text name]] in
  let closeIcon =
    [Html.div [Html.class' "delete-btn"] [fontAwesome "times"]]
  in
  let groupChilren =
    [ Html.div
        [Html.class' "children"]
        [Html.p [] [Html.text "Or drag inside here"]] ]
  in
  Html.div
    [Html.class' "group-data"]
    ([Html.div [Html.class' "group-top"] (nameView @ closeIcon)] @ groupChilren)
