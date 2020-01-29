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
            "Unfortunately we only support Dark on desktop Chrome right now. Between browser different input models, differences in scripting and rendering performance, and differing web platform support, we don't have the capacity to support other browsers at the moment. We hope to support Firefox, Safari, and mobile use once we've really nailed the experience on Chrome. Thanks for understanding!"
        ] ]


let html (m : model) : msg Html.html =
  if m.unsupportedBrowser
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
