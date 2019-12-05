open Prelude
open Types

let msgLink ~(key : string) (content : msg Html.html) (handler : msg) :
    msg Html.html =
  let event = ViewUtils.eventNeither ~key "mouseup" (fun _ -> handler) in
  Html.a [event; Html.class' ""] [content]


let html (_m : model) =
  (* If you need to add a topbar, the steps are:
    * - set the default in Defaults.ml
    * - edit the text and links below
    * - change the name of the key in serializedEditor, in encoders.ml
    *   and decoders.ml. Otherwise, the user's old "showTopbar" setting
    *   will be used.
    *)
  if false (* m.showTopbar *)
  then
    let url =
      let qp = "" in
      let loc = {(Tea.Navigation.getLocation ()) with search = qp} in
      loc.protocol ^ "//" ^ loc.host ^ loc.pathname ^ loc.search ^ loc.hash
    in
    [ Html.div
        [Html.styles []; Html.classList [("topbar", true)]]
        [ Html.a
            [ Html.href url
            ; ViewUtils.eventNoPropagation
                ~key:"toggle-topbar"
                "mouseup"
                (fun _ -> IgnoreMsg ) ]
            [Html.text "Fill in message here"]
        ; msgLink ~key:"hide-topbar" (Html.text "(hide)") HideTopbar ] ]
  else []
