open SettingsViewTypes
open Tc

(* Dark *)
module Cmd = Tea.Cmd
module Attributes = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module K = FluidKeyboard
module Html = Tea_html_extended

let fontAwesome = ViewUtils.fontAwesome

let allTabs = [UserSettings; Privacy; InviteUser defaultInviteFields]

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


let submitForm (m : Types.model) : Types.model * Types.msg Cmd.t =
  let tab = m.settingsView.tab in
  match tab with
  | InviteUser info ->
      let sendInviteMsg =
        { email = info.email.value
        ; inviterUsername = m.username
        ; inviterName = m.account.name }
      in
      ( {m with settingsView = {m.settingsView with loading = true}}
      , API.sendInvite m sendInviteMsg )
  | _ ->
      (m, Cmd.none)


let update (settingsView : settingsViewState) (msg : settingsMsg) :
    settingsViewState =
  match msg with
  | SetSettingsView (canvasList, username, orgs, orgCanvasList) ->
      {settingsView with canvasList; username; orgs; orgCanvasList}
  | OpenSettingsView tab ->
      {settingsView with opened = true; tab; loading = false}
  | CloseSettingsView _ ->
      {settingsView with opened = false; loading = false}
  | SwitchSettingsTabs tab ->
      {settingsView with tab; loading = false}
  | UpdateInviteForm value ->
      let form = {email = {value; error = None}} in
      {settingsView with tab = InviteUser form}
  | TriggerSendInviteCallback (Ok _) ->
      {settingsView with tab = InviteUser defaultInviteFields; loading = false}
  | TriggerSendInviteCallback (Error _) ->
      {settingsView with tab = InviteUser defaultInviteFields; loading = false}
  | SubmitForm ->
      settingsView
  | InitRecordConsent recordConsent ->
      {settingsView with privacy = {recordConsent}}
  | SetRecordConsent allow ->
      {settingsView with privacy = {recordConsent = Some allow}}


let getModifications (m : Types.model) (msg : settingsMsg) :
    Types.modification list =
  match msg with
  | TriggerSendInviteCallback (Error err) ->
      [ SettingsViewUpdate msg
      ; HandleAPIError
          (APIError.make
             ~context:"TriggerSendInviteCallback"
             ~importance:ImportantError
             ~reload:false
             err) ]
  | OpenSettingsView tab ->
      [ SettingsViewUpdate msg
      ; ReplaceAllModificationsWithThisOne
          (fun m ->
            let cmd = Url.navigateTo (SettingsModal tab) in
            ( {m with cursorState = Deselected; currentPage = SettingsModal tab}
            , cmd )) ]
  | TriggerSendInviteCallback (Ok _) ->
      [ SettingsViewUpdate msg
      ; ReplaceAllModificationsWithThisOne
          (fun m ->
            ( {m with toast = {toastMessage = Some "Sent!"; toastPos = None}}
            , Cmd.none )) ]
  | CloseSettingsView _ ->
      [ SettingsViewUpdate msg
      ; ReplaceAllModificationsWithThisOne
          (fun m ->
            ( {m with canvasProps = {m.canvasProps with enablePan = true}}
            , Cmd.none ))
      ; Deselect
      ; MakeCmd (Url.navigateTo Architecture) ]
  | SwitchSettingsTabs tab ->
      [ SettingsViewUpdate msg
      ; ReplaceAllModificationsWithThisOne
          (fun m ->
            let cmd = Url.navigateTo (SettingsModal tab) in
            ({m with currentPage = SettingsModal tab}, cmd)) ]
  | SubmitForm ->
      let isInvalid, newTab = validateForm m.settingsView.tab in
      if isInvalid
      then
        [ SettingsViewUpdate msg
        ; ReplaceAllModificationsWithThisOne
            (fun m ->
              ( {m with settingsView = {m.settingsView with tab = newTab}}
              , Cmd.none )) ]
      else
        [ SettingsViewUpdate msg
        ; ReplaceAllModificationsWithThisOne (fun m -> submitForm m) ]
  | SetRecordConsent allow ->
      [ SettingsViewUpdate msg
      ; MakeCmd (FullstoryView.FullstoryJs.setConsent allow) ]
  | _ ->
      [SettingsViewUpdate msg]


(* View functions *)

let settingsTabToText (tab : settingsTab) : string =
  match tab with
  | NewCanvas ->
      "NewCanvas"
  | UserSettings ->
      "Canvases"
  | InviteUser _ ->
      "Share"
  | Privacy ->
      "Privacy"


(* View code *)

let viewUserCanvases (acc : settingsViewState) : Types.msg Html.html list =
  let canvasLink c =
    let url = "/a/" ^ c in
    Html.li ~unique:c [] [Html.a [Html.href url] [Html.text url]]
  in
  let canvases =
    if List.length acc.canvasList > 0
    then List.map acc.canvasList ~f:canvasLink |> Html.ul []
    else Html.p [] [Html.text "No other personal canvases"]
  in
  let canvasView =
    [ Html.p [Html.class' "canvas-list-title"] [Html.text "Personal canvases:"]
    ; Html.div [Html.class' "canvas-list"] [canvases]
    ; Html.p [] [Html.text "Create a new canvas by navigating to the URL"] ]
  in
  let orgs = List.map acc.orgCanvasList ~f:canvasLink |> Html.ul [] in
  let orgView =
    if List.length acc.orgCanvasList > 0
    then
      [ Html.p [Html.class' "canvas-list-title"] [Html.text "Shared canvases:"]
      ; Html.div [Html.class' "canvas-list"] [orgs] ]
    else [Vdom.noNode]
  in
  orgView @ canvasView


let viewInviteUserToDark (svs : settingsViewState) : Types.msg Html.html list =
  let introText =
    [ Html.h2 [] [Html.text "Share Dark with a friend or colleague"]
    ; Html.p
        []
        [ Html.text
            "Share the love! Invite a friend, and we'll send them an email saying you invited them."
        ]
    ; Html.p
        []
        [ Html.text
            "Note: This will not add them to any of your existing organizations or canvases."
        ] ]
  in
  let error, inputVal =
    match svs.tab with
    | InviteUser x ->
        (x.email.error |> Option.withDefault ~default:"", x.email.value)
    | _ ->
        ("", "")
  in
  let inviteform =
    let submitBtn =
      let btn =
        if svs.loading
        then [ViewUtils.fontAwesome "spinner"; Html.h3 [] [Html.text "Loading"]]
        else [Html.h3 [] [Html.text "Send invite"]]
      in
      Html.button
        [ Html.class' "submit-btn"
        ; Html.Attributes.disabled svs.loading
        ; ViewUtils.eventNoPropagation
            ~key:"close-settings-modal"
            "click"
            (fun _ -> Types.SettingsViewMsg SubmitForm) ]
        btn
    in
    [ Html.div
        [Html.class' "invite-form"]
        [ Html.div
            [Html.class' "form-field"]
            [ Html.h3 [] [Html.text "Email:"]
            ; Html.div
                []
                [ Html.input'
                    [ Vdom.attribute "" "spellcheck" "false"
                    ; Events.onInput (fun str ->
                          Types.SettingsViewMsg (UpdateInviteForm str))
                    ; Attributes.value inputVal ]
                    []
                ; Html.p [Html.class' "error-text"] [Html.text error] ] ]
        ; submitBtn ] ]
  in
  introText @ inviteform


let viewNewCanvas (svs : settingsViewState) : Types.msg Html.html list =
  let text =
    Printf.sprintf
      "Create a new canvas (or go to it if it already exists) by visiting /a/%s-canvasname"
      svs.username
  in
  let text =
    if List.isEmpty svs.orgs
    then text ^ "."
    else
      text
      ^ Printf.sprintf
          " or /a/orgname-canvasname, where orgname may be any of (%s)."
          (svs.orgs |> String.join ~sep:", ")
  in
  let introText =
    [Html.h2 [] [Html.text "New Canvas"]; Html.p [] [Html.text text]]
  in
  introText


let viewPrivacy (s : privacySettings) : Types.msg Html.html list =
  [FullstoryView.consentRow s.recordConsent ~longLabels:false]


let settingsTabToHtml (svs : settingsViewState) : Types.msg Html.html list =
  let tab = svs.tab in
  match tab with
  | NewCanvas ->
      viewNewCanvas svs
  | UserSettings ->
      viewUserCanvases svs
  | InviteUser _ ->
      viewInviteUserToDark svs
  | Privacy ->
      viewPrivacy svs.privacy


let tabTitleView (tab : settingsTab) : Types.msg Html.html =
  let tabTitle (t : settingsTab) =
    let isSameTab =
      match (tab, t) with InviteUser _, InviteUser _ -> true | _ -> tab == t
    in
    Html.h3
      [ Html.classList [("tab-title", true); ("selected", isSameTab)]
      ; ViewUtils.eventNoPropagation
          ~key:"close-settings-modal"
          "click"
          (fun _ -> Types.SettingsViewMsg (SwitchSettingsTabs t)) ]
      [Html.text (settingsTabToText t)]
  in
  Html.div [Html.class' "settings-tab-titles"] (List.map allTabs ~f:tabTitle)


let onKeydown (evt : Web.Node.event) : Types.msg option =
  K.eventToKeyEvent evt
  |> Option.andThen ~f:(fun e ->
         match e with
         | {K.key = K.Enter; _} ->
             Some (Types.SettingsViewMsg SubmitForm)
         | _ ->
             None)


let settingViewWrapper (acc : settingsViewState) : Types.msg Html.html =
  let tabView = settingsTabToHtml acc in
  Html.div
    [Html.class' "settings-tab-wrapper"]
    ([Html.h1 [] [Html.text "Settings"]; tabTitleView acc.tab] @ tabView)


let html (m : Types.model) : Types.msg Html.html =
  let svs = m.settingsView in
  let closingBtn =
    Html.div
      [ Html.class' "close-btn"
      ; ViewUtils.eventNoPropagation
          ~key:"close-settings-modal"
          "click"
          (fun _ -> Types.SettingsViewMsg (CloseSettingsView svs.tab)) ]
      [fontAwesome "times"]
  in
  Html.div
    [ Html.class' "settings modal-overlay"
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.eventNoPropagation ~key:"close-setting-modal" "click" (fun _ ->
          Types.SettingsViewMsg (CloseSettingsView svs.tab)) ]
    [ Html.div
        [ Html.class' "modal"
        ; ViewUtils.nothingMouseEvent "click"
        ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
              EnablePanning false)
        ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
              EnablePanning true)
        ; Html.onCB "keydown" "keydown" onKeydown ]
        [settingViewWrapper svs; closingBtn] ]
