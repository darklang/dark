open Tc

module Attr = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module Html = Tea_html_extended
module Cmd = Tea.Cmd

open SecretTypes

let fontAwesome = ViewUtils.fontAwesome

let nameValidator = "[A-Z0-9_]+"

let update (msg : msg) (model : Types.model) : Types.modification =
  let updateMod createSecretModal =
    Types.ReplaceAllModificationsWithThisOne
      (fun m -> ( {m with createSecretModal}, Cmd.none ))
  in
  let m = model.createSecretModal in
  match msg with
    | OpenCreateModal ->
      let usedNames = List.map ~f:(fun s -> s.secretName) model.secrets in
      updateMod {m with visible = true; usedNames}
    | CloseCreateModal -> updateMod {m with visible = false}
    | OnUpdateName newSecretName ->
      let error =
        if not (Util.Regex.exactly ~re:nameValidator newSecretName)
        then Some "Secret name can only contain alphanumberic characters and underscores"
        else if List.member ~value:newSecretName m.usedNames
        then Some (newSecretName ^ " is already defined as a secret")
        else None
      in
      let isNameValid = error = None in
      updateMod {m with newSecretName; isNameValid; error}
    | OnUpdateValue newSecretValue -> updateMod {m with newSecretValue}
    | SaveNewSecret ->
      let isValueValid = m.newSecretValue <> "" in
      let isNameValid = Util.Regex.exactly ~re:nameValidator m.newSecretName in
      if isValueValid && isNameValid
      then
        updateMod SecretTypes.defaultCreateModal
      else
        let error = Some "Both secret name and secret values must be filled" in
        updateMod {m with isValueValid; isNameValid; error}

let view (m : createModal) : Types.msg Html.html =
  if m.visible
  then
    let inside =
      let closeBtn =
        Html.div
        [ Html.class' "close-btn"
        ; ViewUtils.eventNoPropagation
            ~key:"close-create-secret-modal"
            "click"
            (fun _ -> Types.SecretMsg CloseCreateModal) ]
        [fontAwesome "times"]
      in
      let content =
        let title = Html.div [Html.class' "title"] [Html.text "Add Secret Key"] in
        let form =
          Html.form [Html.class' "create-secret-form"]
          [ Html.input'
            [ Attr.placeholder "secret name"; Attr.name "secret-name" ; Attr.value m.newSecretName
            ; Html.classList [("modal-form-input", true);("error", not m.isNameValid)]
            ; Events.onInput (fun str -> Types.SecretMsg (OnUpdateName (Tc.String.toUpper str)))
            ] []
          ; Html.input'
            [ Attr.placeholder "secret value"; Attr.name "secret-value" ;Attr.value m.newSecretValue
            ; Html.classList [("modal-form-input", true);("error", not m.isValueValid)]
            ; Events.onInput (fun str -> Types.SecretMsg (OnUpdateValue str))] []
          ; Html.button
            [ Html.type' "button"; Html.class' "modal-form-button"
            ; ViewUtils.eventNoPropagation ~key:"save-secret" "click" (fun _ -> Types.SecretMsg SaveNewSecret)]
            [Html.text "Save"]
          ]
        in
        let errorMsg =
          let msg = match m.error with Some msg -> msg | None -> "" in
          Html.p [Html.class' "form-error"] [Html.text msg]
        in
        Html.div
          [Html.class' "modal-content"]
          [title; form; errorMsg]
      in
      [content; closeBtn]
    in
    Html.div
      [ Html.class' "modal-overlay"
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup" ]
      [Html.div [Html.class' "modal"] inside]
  else Vdom.noNode

