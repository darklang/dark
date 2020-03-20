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
  Html.div
    [Html.class' "welcome"]
    [ Html.h1 [Html.class' "title"] [Html.text ("Hello, " ^ username ^ "!")]
    ; Html.p
        [Html.class' "subtext"]
        [ Html.text
            "Welcome to Dark - we're excited to help you get started.  We've created a sample canvas for you to explore, with supporting documentation available here: "
        ; Html.a
            [Html.class' "doc-link"; Html.href "https://darklang.com/docs"]
            [Html.text "https://darklang.com/docs"] ]
    ; Html.div
        [Html.class' "btn-wrap"]
        [ Html.a
            [ Html.href
                (Printf.sprintf
                   "https://darklang.com/a/%s-getting-started"
                   username)
            ; btnEvent ]
            [ Html.div
                [Html.class' "btn"]
                [Html.p [Html.class' "txt"] [Html.text "Get Started"]] ] ]
    ; Html.p
        [Html.class' "subtext"]
        [ Html.span
            []
            [ Html.text
                "If you'd like to create a blank canvas, just change the \"getting-started\" part of this URL to something else, and a new canvas will be created for you."
            ] ] ]


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
