open Prelude

(* Dark *)
module TL = Toplevel
module P = Pointer
module TD = TLIDDict

let viewMessage : msg Html.html =
  Html.div
    [Html.class' "warning"]
    [ Html.p
        [Html.class' "title"]
        [ Html.text
            "We're sorry, but Chrome is the only browser which currently supports the browser features we need to build Dark. We plan to extend to Firefox/Safari users once they have support for the features we use."
        ] ]


let html (m : model) : msg Html.html =
  if m.unsupportedBrowser && m.integrationTestState == NoIntegrationTest
  then
    Html.div
      [ ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; Html.class' "modal-overlay"
      ; Html.id "overlay" ]
      [ Html.div
          [ ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; ViewUtils.nothingMouseEvent "click"
          ; Html.class' "modal" ]
          [viewMessage] ]
  else Vdom.noNode
