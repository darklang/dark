open Prelude

(* Dark *)
module TL = Toplevel
module P = Pointer
module TD = TLIDDict

let fontAwesome = ViewUtils.fontAwesome

let update (m : model) (msg : accountMsg) : model =
  match msg with
  | ToggleAccountView opened ->
      let enablePan = if opened then m.canvasProps.enablePan else true in
      { m with
        accountView = {m.accountView with opened}
      ; canvasProps = {m.canvasProps with enablePan} }


let openAccountView (m : model) : model = update m (ToggleAccountView false)

let userAccountView (m : model) : msg Html.html =
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
    [Html.class' "account-tab-wrapper"]
    ([Html.h2 [] [Html.text "Account"]] @ orgView @ canvasView)


let accountTabToHtml (m : model) : msg Html.html =
  let tab = m.accountView.tab in
  match tab with UserAccount -> userAccountView m


let html (m : model) : msg Html.html =
  let closingBtn =
    Html.div
      [ Html.class' "close-btn"
      ; ViewUtils.eventNoPropagation
          ~key:"close-setting-modal"
          "click"
          (fun _ -> AccountViewMsg (ToggleAccountView false)) ]
      [fontAwesome "times"]
  in
  Html.div
    [ Html.class' "account modal-overlay"
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.eventNoPropagation ~key:"close-setting-modal" "click" (fun _ ->
          AccountViewMsg (ToggleAccountView false)) ]
    [ Html.div
        [ Html.class' "modal"
        ; ViewUtils.nothingMouseEvent "click"
        ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
              EnablePanning false)
        ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
              EnablePanning true) ]
        [accountTabToHtml m; closingBtn] ]
