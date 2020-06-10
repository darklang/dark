module Attr = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module Html = Tea_html_extended
module Cmd = Tea.Cmd

open SecretTypes

let fontAwesome = ViewUtils.fontAwesome

let nameValidator = "[A-Z0-9_]+"

let update (msg : msg) (m : createModal) : createModal =
  match msg with
  | OpenCreateModal -> {m with visible = true}
  | CloseCreateModal -> {m with visible = false}
  | OnUpdateName secretName ->
    let isNameValid = Util.Regex.exactly ~re:nameValidator secretName in
    let error = if isNameValid then None else Some "Secret name can only contain alphanumberic characters and underscores" in
    {m with secretName; isNameValid; error}
  | OnUpdateValue secretValue -> {m with secretValue}
  | SaveNewSecret ->
    let isValueValid = m.secretValue <> "" in
    let isNameValid = Util.Regex.exactly ~re:nameValidator m.secretName in
    if isValueValid && isNameValid
    then SecretTypes.defaultCreateModal
    else
      let error = Some "Both secret name and secret values must be filled" in
      {m with isValueValid; isNameValid; error}

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
            [ Attr.placeholder "secret name"; Attr.name "secret-name" ; Attr.value m.secretName
            ; Html.classList [("modal-form-input", true);("error", not m.isNameValid)]
            ; Events.onInput (fun str -> Types.SecretMsg (OnUpdateName (Tc.String.toUpper str)))
            ] []
          ; Html.input'
            [ Attr.placeholder "secret value"; Attr.name "secret-value" ;Attr.value m.secretValue
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

