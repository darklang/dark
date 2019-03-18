open Types
open Prelude

(* Dark *)
type viewState = ViewUtils.viewState

let viewUserTipe (_vs : viewState) (_t : userTipe) : msg Html.html =
  Html.div
    [Html.class' "user-type-toplevel"]
    [Html.div [Html.class' "wassup"] [Html.text "TODO"]]
