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
        ] ]


let viewWelcomeToDark (username : string) : msg Html.html =
  let btnEvent =
    ViewUtils.eventNoPropagation ~key:"close-welcome-modal" "click" (fun _ ->
        CloseWelcomeModal)
  in
  let vidSrc = "//" ^ Native.Ext.staticHost () ^ "/gif/helloWorld.gif" in
  let gif =
    [ Html.img
        [ Html.src vidSrc
        ; Vdom.prop "alt" "Gif showing how to create a hello world handler" ]
        [] ]
  in
  Html.div
    [Html.class' "welcome"]
    [ Html.h1 [Html.class' "title"] [Html.text ("Hello, " ^ username ^ "!")]
    ; Html.p
        [Html.class' "subtext"]
        [ Html.text
            "Dark was created to reduce accidental complexity - look how easy it is to build Hello World!"
        ]
    ; Html.div [Html.class' "gif"] gif
    ; Html.div
        [Html.class' "btn-wrap"]
        [ Html.div
            [Html.class' "btn"; btnEvent]
            [Html.p [Html.class' "txt"] [Html.text "Get Started"]] ] ]


let html (m : model) : msg Html.html =
  let view, events, id =
    if m.unsupportedBrowser
    then
      ( viewBrowserMessage
      , [ ViewUtils.nothingMouseEvent "mousedown"
        ; ViewUtils.nothingMouseEvent "mouseup" ]
      , "unsupportedBrowser" )
    else
      ( viewWelcomeToDark m.username
      , [ ViewUtils.nothingMouseEvent "mousedown"
        ; ViewUtils.nothingMouseEvent "mouseup"
        ; ViewUtils.eventNoPropagation
            ~key:"close-welcome-modal"
            "click"
            (fun _ -> CloseWelcomeModal) ]
      , "welcomeToDark" )
  in
  Html.div
    ([Html.class' "modal-overlay"; Html.id id] @ events)
    [Html.div [Html.classList [("modal", true)]] [view]]
