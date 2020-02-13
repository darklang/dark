open Prelude

(* Dark *)
module TL = Toplevel
module P = Pointer
module TD = TLIDDict

let userSettingsView (m : model) : msg Html.html =
  let canvases =
    if List.length m.canvas_list > 0
    then
      List.map m.canvas_list ~f:(fun c ->
          Html.li
            ~unique:c
            []
            [Html.a [Html.href ("/a/" ^ c)] [Html.text ("/a/" ^ c)]])
      |> Html.ul []
    else Html.p [] [Html.text "No other canvases"]
  in
  let canvasView =
    [ Html.p [Html.class' "canvas-list-title"] [Html.text "Canvases:"]
    ; Html.div [Html.class' "canvas-list"] [canvases]
    ; Html.p [] [Html.text "Create a new canvas by navigating to the URL"] ]
  in
  Html.div
    [Html.class' "setting-tab-wrapper"]
    ([Html.h2 [] [Html.text "User Settings"]] @ canvasView)


let settingsTabToHtml (m : model) : msg Html.html =
  let tab = m.settings.tab in
  match tab with UserSettings -> userSettingsView m


let html (m : model) : msg Html.html =
  Html.div
    [ Html.class' "settings modal-overlay"
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.eventNoPropagation ~key:"close-setting-modal" "click" (fun _ ->
          ToggleSettings) ]
    [ Html.div
        [ Html.classList [("modal", true)]
        ; ViewUtils.nothingMouseEvent "mousedown"
        ; ViewUtils.nothingMouseEvent "mouseup"
        ; ViewUtils.nothingMouseEvent "click"
        ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
              EnablePanning false)
        ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
              EnablePanning true) ]
        [settingsTabToHtml m] ]
