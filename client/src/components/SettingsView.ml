open Prelude

(* Dark *)
module Events = Tea.Html2.Events
module K = FluidKeyboard

let fontAwesome = ViewUtils.fontAwesome

let defaultInviteFields : inviteFields = {email = {value = ""; error = None}}

let allTabs = [UserSettings; InviteUser defaultInviteFields]

let validateEmail (email : formField) : formField =
  let error =
    let emailVal = email.value in
    if String.length emailVal = 0
    then Some "Field Required"
    else if not (Entry.validateEmail emailVal)
    then Some "Invalid Email"
    else None
  in
  {email with error}


let validateForm (tab : settingsTab) : bool * settingsTab =
  match tab with
  | InviteUser form ->
      let text = validateEmail form.email in
      let email = {email = text} in
      let isInvalid = Option.is_some text.error in
      (isInvalid, InviteUser email)
  | _ ->
      (* shouldnt get here *)
      (false, tab)


let submitForm (m : model) (tab : settingsTab) : model =
  match tab with
  | InviteUser _ ->
      (* TODO Add API call and callback *)
      (* In callback, show error OR stop loading and show confirm *)
      {m with settingsView = {m.settingsView with loading = true}}
  | _ ->
      m


let update (m : model) (msg : settingsMsg) : model =
  match msg with
  | ToggleSettingsView (opened, tab) ->
      (* TODO -> Set to correct tab on open *)
      let enablePan = if opened then m.canvasProps.enablePan else true in
      let tab = tab |> Option.withDefault ~default:UserSettings in
      { m with
        settingsView = {m.settingsView with opened; tab; loading = false}
      ; canvasProps = {m.canvasProps with enablePan} }
  | SwitchSettingsTabs tab ->
      {m with settingsView = {m.settingsView with tab; loading = false}}
  | UpdateInviteForm value ->
      let form = {email = {value; error = None}} in
      {m with settingsView = {m.settingsView with tab = InviteUser form}}
  | SubmitForm ->
      let isInvalid, newTab = validateForm m.settingsView.tab in
      if isInvalid
      then {m with settingsView = {m.settingsView with tab = newTab}}
      else submitForm m m.settingsView.tab


(* View functions *)

let settingsTabToText (tab : settingsTab) : string =
  match tab with UserSettings -> "Canvases" | InviteUser _ -> "Share"


(* View code *)

let viewUserCanvases (acc : settingsViewState) : msg Html.html list =
  let canvasLink c =
    let url = "/a/" ^ c in
    Html.li ~unique:c [] [Html.a [Html.href url] [Html.text url]]
  in
  let canvases =
    if List.length acc.canvas_list > 0
    then List.map acc.canvas_list ~f:canvasLink |> Html.ul []
    else Html.p [] [Html.text "No other personal canvases"]
  in
  let canvasView =
    [ Html.p [Html.class' "canvas-list-title"] [Html.text "Personal canvases:"]
    ; Html.div [Html.class' "canvas-list"] [canvases]
    ; Html.p [] [Html.text "Create a new canvas by navigating to the URL"] ]
  in
  let orgs = List.map acc.org_list ~f:canvasLink |> Html.ul [] in
  let orgView =
    if List.length acc.org_list > 0
    then
      [ Html.p [Html.class' "canvas-list-title"] [Html.text "Shared canvases:"]
      ; Html.div [Html.class' "canvas-list"] [orgs] ]
    else [Vdom.noNode]
  in
  orgView @ canvasView


let viewInviteUserToDark (svs : settingsViewState) : msg Html.html list =
  let introText =
    [ Html.h2 [] [Html.text "Share Dark with a friend"]
    ; Html.p
        []
        [ Html.text
            "Friends don't let friends stay on waitlists. Help your friends skip the line by inviting them directly to Dark."
        ]
    ; Html.p
        []
        [ Html.text
            "Note: This will not add them to any of your existing organizations or canvases."
        ] ]
  in
  let inviteform =
    let error =
      match svs.tab with
      | InviteUser x ->
          x.email.error |> Option.withDefault ~default:""
      | _ ->
          ""
    in
    let submitBtn =
      let event =
        if svs.loading
        then []
        else
          [ ViewUtils.eventNoPropagation
              ~key:"close-settings-modal"
              "click"
              (fun _ -> SettingsViewMsg SubmitForm) ]
      in
      let btn =
        if svs.loading
        then [ViewUtils.fontAwesome "spinner"; Html.h3 [] [Html.text "Loading"]]
        else [Html.h3 [] [Html.text "Submit"]]
      in
      Html.div ([Html.class' "submit-btn"] @ event) btn
    in
    [ Html.div
        [Html.class' "invite-form"]
        [ Html.div
            [Html.class' "form-field"]
            [ Html.h3 [] [Html.text "Email:"]
            ; Html.div
                []
                [ Html.p [Html.class' "error-text"] [Html.text error]
                ; Html.input'
                    [ Vdom.attribute "" "spellcheck" "false"
                    ; Events.onInput (fun str ->
                          SettingsViewMsg (UpdateInviteForm str)) ]
                    [] ] ]
        ; submitBtn ] ]
  in
  introText @ inviteform


let settingsTabToHtml (svs : settingsViewState) : msg Html.html list =
  let tab = svs.tab in
  match tab with
  | UserSettings ->
      viewUserCanvases svs
  | InviteUser _ ->
      viewInviteUserToDark svs


let tabTitleView (tab : settingsTab) : msg Html.html =
  let tabTitle (t : settingsTab) =
    let isSameTab =
      match (tab, t) with
      | UserSettings, UserSettings | InviteUser _, InviteUser _ ->
          true
      | _ ->
          false
    in
    Html.h3
      [ Html.classList [("tab-title", true); ("selected", isSameTab)]
      ; ViewUtils.eventNoPropagation
          ~key:"close-settings-modal"
          "click"
          (fun _ -> SettingsViewMsg (SwitchSettingsTabs t)) ]
      [Html.text (settingsTabToText t)]
  in
  Html.div [Html.class' "settings-tab-titles"] (List.map allTabs ~f:tabTitle)


let onKeydown (evt : Web.Node.event) : Types.msg option =
  K.eventToKeyEvent evt
  |> Option.andThen ~f:(fun e ->
         match e with
         | {K.key = K.Enter; _} ->
             Some (SettingsViewMsg SubmitForm)
         | _ ->
             None)


let settingViewWrapper (acc : settingsViewState) : msg Html.html =
  let tabView = settingsTabToHtml acc in
  Html.div
    [Html.class' "settings-tab-wrapper"]
    ([Html.h1 [] [Html.text "Account"]; tabTitleView acc.tab] @ tabView)


let html (m : model) : msg Html.html =
  let closingBtn =
    Html.div
      [ Html.class' "close-btn"
      ; ViewUtils.eventNoPropagation
          ~key:"close-settings-modal"
          "click"
          (fun _ -> SettingsViewMsg (ToggleSettingsView (false, None))) ]
      [fontAwesome "times"]
  in
  Html.div
    [ Html.class' "settings modal-overlay"
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.eventNoPropagation ~key:"close-setting-modal" "click" (fun _ ->
          SettingsViewMsg (ToggleSettingsView (false, None))) ]
    [ Html.div
        [ Html.class' "modal"
        ; ViewUtils.nothingMouseEvent "click"
        ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
              EnablePanning false)
        ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
              EnablePanning true)
        ; Html.onCB "keydown" "keydown" onKeydown ]
        [settingViewWrapper m.settingsView; closingBtn] ]
