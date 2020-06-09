module Attr = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module Html = Tea_html_extended
module Cmd = Tea.Cmd


let update (msg : SecretTypes.msg) (m : SecretTypes.createModal) : SecretTypes.createModal =
  match msg with
  | OpenCreateModal -> {m with visible = true}
  | CloseCreateModal -> {m with visible = false}
  | SaveNewSecret -> SecretTypes.defaultCreateModal

let view (m : SecretTypes.createModal) : Types.msg Html.html =
  if m.visible
  then
    Html.div
      [ Html.class' "modal-overlay"
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup" ]
      [Html.div [Html.class' "modal"] [Html.text "tell me a secret"]]
  else Vdom.noNode

