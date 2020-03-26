open SettingsViewTypes
open Tc

(* Dark *)
module Cmd = Tea.Cmd
module Attributes = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module K = FluidKeyboard
module Html = Tea_html_extended

let fontAwesome = ViewUtils.fontAwesome

let defaultInviteFields : inviteFields = {email = {value = ""; error = None}}

let allTabs = [CanvasInfo; UserSettings; InviteUser defaultInviteFields]

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
  Entry.sendSegmentMessage InviteUser ;
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


let sendCanvasInformation (m : Types.model) : Types.msg Cmd.t =
  let msg =
    let canvasShipped =
      match m.settingsView.canvasInformation.shippedDate with
      | Some date ->
          date |> Js.Date.toUTCString
      | None ->
          ""
    in
    let canvasCreation =
      match m.settingsView.canvasInformation.createdAt with
      | Some date ->
          date |> Js.Date.toUTCString
      | None ->
          ""
    in
    { canvasName = m.settingsView.canvasInformation.canvasName
    ; canvasDescription = m.settingsView.canvasInformation.canvasDescription
    ; canvasShipped
    ; canvasCreation }
  in
  API.sendCanvasInfo m msg


let update (settingsView : settingsViewState) (msg : settingsMsg) :
    settingsViewState =
  match msg with
  | SetSettingsView (canvasName, canvasList, orgList, creationDate) ->
      { settingsView with
        canvasList
      ; orgList
      ; canvasInformation =
          { settingsView.canvasInformation with
            canvasName
          ; createdAt = Some creationDate } }
  | OpenSettingsView tab ->
      {settingsView with opened = true; tab; loading = false}
  | CloseSettingsView _ ->
      {settingsView with opened = false; loading = false}
  | SwitchSettingsTabs tab ->
      {settingsView with tab; loading = false}
  | UpdateInviteForm value ->
      let form = {email = {value; error = None}} in
      {settingsView with tab = InviteUser form}
  | UpdateCanvasDescription value ->
      { settingsView with
        canvasInformation =
          {settingsView.canvasInformation with canvasDescription = value} }
  | SetCanvasDeployStatus ship ->
      let shippedDate =
        let rawDate = Js.Date.now () |> Js.Date.fromFloat in
        let formattedDate = Util.formatDate (rawDate, "L") in
        if ship
        then (
          Entry.sendSegmentMessage (MarkCanvasAsInDevelopment formattedDate) ;
          Some rawDate )
        else (
          Entry.sendSegmentMessage (MarkCanvasAsInDevelopment formattedDate) ;
          None )
      in
      { settingsView with
        canvasInformation = {settingsView.canvasInformation with shippedDate} }
  | TriggerSendInviteCallback (Ok _) ->
      {settingsView with tab = InviteUser defaultInviteFields; loading = false}
  | TriggerSendInviteCallback (Error _) ->
      {settingsView with tab = InviteUser defaultInviteFields; loading = false}
  | TriggerGetCanvasInfoCallback (Ok data) ->
      let shippedDate =
        if String.length data.shippedDate == 0
        then None
        else Some (Js.Date.fromString data.shippedDate)
      in
      { settingsView with
        canvasInformation =
          { settingsView.canvasInformation with
            canvasDescription = data.canvasDescription
          ; shippedDate } }
  | SubmitForm
  | TriggerUpdateCanvasInfoCallback _
  | TriggerGetCanvasInfoCallback (Error _) ->
      settingsView


let getModifications (m : Types.model) (msg : settingsMsg) :
    Types.modification list =
  match msg with
  | TriggerSendInviteCallback (Error err) ->
      [ SettingsViewUpdate msg
      ; HandleAPIError
          (APIError.make
             ~context:"TriggerSendInviteCallback"
             ~importance:IgnorableError
             ~reload:false
             err) ]
  | TriggerUpdateCanvasInfoCallback (Error err) ->
      [ HandleAPIError
          (APIError.make
             ~context:"TriggerUpdateCanvasInfoCallback"
             ~importance:IgnorableError
             ~reload:false
             err) ]
  | TriggerGetCanvasInfoCallback (Error err) ->
      [ HandleAPIError
          (APIError.make
             ~context:"TriggerGetCanvasInfoCallback"
             ~importance:IgnorableError
             ~reload:false
             err) ]
  | OpenSettingsView _ ->
      [ SettingsViewUpdate msg
      ; ReplaceAllModificationsWithThisOne
          (fun m -> CursorState.setCursorState Deselected m) ]
  | TriggerUpdateCanvasInfoCallback (Ok _) ->
      [ SettingsViewUpdate msg
      ; ReplaceAllModificationsWithThisOne
          (fun m -> ({m with toast = Toast.show DidSaveCanvasInfo}, Cmd.none))
      ]
  | TriggerSendInviteCallback (Ok _) ->
      [ SettingsViewUpdate msg
      ; ReplaceAllModificationsWithThisOne
          (fun m -> ({m with toast = Toast.show DidSendInvite}, Cmd.none)) ]
  | CloseSettingsView tab ->
      let cmd =
        match tab with CanvasInfo -> sendCanvasInformation m | _ -> Cmd.none
      in
      [ SettingsViewUpdate msg
      ; ReplaceAllModificationsWithThisOne
          (fun m ->
            ({m with canvasProps = {m.canvasProps with enablePan = true}}, cmd))
      ]
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
  | _ ->
      [SettingsViewUpdate msg]


(* View functions *)

let settingsTabToText (tab : settingsTab) : string =
  match tab with
  | CanvasInfo ->
      "About"
  | UserSettings ->
      "Canvases"
  | InviteUser _ ->
      "Share"


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
  let orgs = List.map acc.orgList ~f:canvasLink |> Html.ul [] in
  let orgView =
    if List.length acc.orgList > 0
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
            "Friends don't let friends stay on waitlists. Help your friends skip the line by inviting them directly to Dark."
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
        else [Html.h3 [] [Html.text "Submit"]]
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


let viewCanvasInfo (canvas : canvasInformation) : Types.msg Html.html list =
  let shipped, shippedText =
    match canvas.shippedDate with
    | Some date ->
        let formattedDate = Util.formatDate (date, "L") in
        (true, " (as of " ^ formattedDate ^ ")")
    | None ->
        (false, "")
  in
  let create_at_text =
    match canvas.createdAt with
    | Some date ->
        "Canvas created on: " ^ Util.formatDate (date, "L")
    | None ->
        ""
  in
  [ Html.div
      [Html.class' "canvas-info"]
      [ Html.h2 [] [Html.text "About"]
      ; Html.p
          []
          [ Html.text
              "Tell us about what you're building. This will help us figure out what to build into Dark."
          ]
      ; Html.div
          [Html.class' "canvas-desc"]
          [ Html.h3 [] [Html.text "Canvas description:"]
          ; Html.textarea
              [ Vdom.attribute "" "spellcheck" "false"
              ; Events.onInput (fun str ->
                    Types.SettingsViewMsg (UpdateCanvasDescription str))
              ; Attributes.value canvas.canvasDescription ]
              [] ]
      ; Html.div
          [ Html.class' "canvas-shipped-info"
          ; ViewUtils.eventNoPropagation
              ~key:("SetCanvasDeployStatus" ^ shippedText)
              "mouseup"
              (fun _ ->
                Types.SettingsViewMsg (SetCanvasDeployStatus (not shipped))) ]
          [ Html.input' [Html.type' "checkbox"; Html.checked shipped] []
          ; Html.p [] [Html.text ("Project is live" ^ shippedText)] ]
      ; Html.p
          [Html.class' "sub-text"]
          [ Html.text
              "*If your project has gone live, we'll use it to help us determine the health of the Dark infrastructure*"
          ]
      ; Html.p [Html.class' "created-text"] [Html.text create_at_text] ] ]


let settingsTabToHtml (svs : settingsViewState) : Types.msg Html.html list =
  let tab = svs.tab in
  match tab with
  | CanvasInfo ->
      viewCanvasInfo svs.canvasInformation
  | UserSettings ->
      viewUserCanvases svs
  | InviteUser _ ->
      viewInviteUserToDark svs


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
    ([Html.h1 [] [Html.text "Account"]; tabTitleView acc.tab] @ tabView)


let html (m : Types.model) : Types.msg Html.html =
  let closingBtn =
    Html.div
      [ Html.class' "close-btn"
      ; ViewUtils.eventNoPropagation
          ~key:"close-settings-modal"
          "click"
          (fun _ ->
            Types.SettingsViewMsg (CloseSettingsView m.settingsView.tab)) ]
      [fontAwesome "times"]
  in
  Html.div
    [ Html.class' "settings modal-overlay"
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.eventNoPropagation ~key:"close-setting-modal" "click" (fun _ ->
          Types.SettingsViewMsg (CloseSettingsView m.settingsView.tab)) ]
    [ Html.div
        [ Html.class' "modal"
        ; ViewUtils.nothingMouseEvent "click"
        ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
              EnablePanning false)
        ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
              EnablePanning true)
        ; Html.onCB "keydown" "keydown" onKeydown ]
        [settingViewWrapper m.settingsView; closingBtn] ]
