open Prelude

(* Dark *)
module TL = Toplevel
module P = Pointer
module TD = TLIDDict

let fontAwesome = ViewUtils.fontAwesome

let userSettingsView (m : model) : msg Html.html =
  let canvasLink c =
    let url = "/a/" ^ c in
    Html.li ~unique:c [] [Html.a [Html.href url] [Html.text url]]
  in
  let canvases =
    if List.length m.canvas_list > 0
    then List.map m.canvas_list ~f:canvasLink |> Html.ul []
    else Html.p [] [Html.text "No other personal canvases"]
  in
  let canvasView =
    [ Html.p [Html.class' "canvas-list-title"] [Html.text "Personal canvases:"]
    ; Html.div [Html.class' "canvas-list"] [canvases]
    ; Html.p [] [Html.text "Create a new canvas by navigating to the URL"] ]
  in
  let orgs = List.map m.org_list ~f:canvasLink |> Html.ul [] in
  let orgView =
    if List.length m.org_list > 0
    then
      [ Html.p [Html.class' "canvas-list-title"] [Html.text "Shared canvases:"]
      ; Html.div [Html.class' "canvas-list"] [orgs] ]
    else [Vdom.noNode]
  in
  Html.div
    [Html.class' "setting-tab-wrapper"]
    ([Html.h2 [] [Html.text "Account"]] @ orgView @ canvasView)


let settingsTabToHtml (m : model) : msg Html.html =
  let tab = m.settings.tab in
  match tab with UserSettings -> userSettingsView m


let html (m : model) : msg Html.html =
  let closingBtn =
    Html.div
      [ Html.class' "close-btn"
      ; ViewUtils.eventNoPropagation
          ~key:"close-setting-modal"
          "click"
          (fun _ -> ToggleSettings false) ]
      [fontAwesome "times"]
  in
  Html.div
    [ Html.class' "settings modal-overlay"
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.eventNoPropagation ~key:"close-setting-modal" "click" (fun _ ->
          ToggleSettings false) ]
    [ Html.div
        [ Html.class' "modal"
        ; ViewUtils.nothingMouseEvent "click"
        ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
              EnablePanning false)
        ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
              EnablePanning true) ]
        [settingsTabToHtml m; closingBtn] ]
