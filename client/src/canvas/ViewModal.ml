open Prelude

(* Dark *)
module TL = Toplevel
module P = Pointer
module TD = TLIDDict

let viewBrowserMessage : msg Html.html =
  Html.div
    [Html.class' "warning"]
    [ Html.p
        [Html.class' "title"]
        [ Html.text
            "Unfortunately we only support Dark on desktop Chrome right now. Between browser different input models, differences in scripting and rendering performance, and differing web platform support, we don't have the capacity to support other browsers at the moment. We hope to support Firefox, Safari, and mobile use once we've really nailed the experience on Chrome. Thanks for understanding!"
        ]
    ; Html.p
        [Html.class' "title"]
        [ Html.text "A "
        ; Html.a
            [ Html.href "http://darklang.com/desktop-client"
            ; Html.target "_blank" ]
            [Html.text "desktop client"]
        ; Html.text
            " is available as well. It is still in the beta phase, so the experience may be slightly different than in Chrome."
        ] ]


let unsupportedBrowser ~(show : bool) : msg Html.html =
  if show
  then
    Html.div
      [ Html.class' "modal-overlay"
      ; Html.id "unsupportedBrowser"
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup" ]
      [Html.div [Html.classList [("modal", true)]] [viewBrowserMessage]]
  else Vdom.noNode
