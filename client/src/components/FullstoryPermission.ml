(* open Tc *)

module Cmd = Tea.Cmd
module Attr = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module Html = Tea_html_extended

let html () : Types.msg Html.html =
  Html.div [Html.class' "fullstory-modal"]
  [ Html.p [] [Html.text "Hi! While we're in private beta, we're really interested in peoples' experience learning Dark, and videos are the best way to see."]
  ; Html.p [] [Html.text "Are you willing to help us out and let us see your session after this? We'll never share it with anyone else."]
  ; Html.div [Html.class' "consent"]
    [ Html.div [Html.class' "choice"] [Html.input' [Html.type' "radio"; Html.id "fs-consent-yes"; Html.name "fs-consent"] []
    ; Html.label [Html.for' "fs-consent-yes"] [Html.text "Yes! Record me using Dark."]]
    ; Html.div [Html.class' "choice"] [Html.input' [Html.type' "radio"; Html.id "fs-consent-no"; Html.name "fs-consent"] []
    ; Html.label [Html.for' "fs-consent-no"] [Html.text "No. Don't me using Dark."]]
    ]
  ]