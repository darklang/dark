open Tc
module Attr = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module Html = Tea_html_extended
module Cmd = Tea.Cmd
open SecretTypes

let fontAwesome = ViewUtils.fontAwesome

let nameValidator = "[A-Z0-9_]+"

let validateName (s : string) : bool = Util.Regex.exactly ~re:nameValidator s

let validateValue (s : string) : bool = s <> ""

let isNameAlreadyUsed (m : insertModal) (value : string) : bool =
  List.member ~value m.usedNames


let update (msg : msg) : Types.modification =
  match msg with
  | OpenCreateModal ->
      Types.ReplaceAllModificationsWithThisOne
        (fun m ->
          let usedNames = List.map ~f:(fun s -> s.secretName) m.secrets in
          let insertSecretModal =
            {m.insertSecretModal with visible = true; usedNames}
          in
          ({m with insertSecretModal}, Cmd.none))
  | CloseCreateModal ->
      Types.ReplaceAllModificationsWithThisOne
        (fun m ->
          let insertSecretModal = SecretTypes.defaultInsertModal in
          ({m with insertSecretModal}, Cmd.none))
  | OnUpdateName newSecretName ->
      Types.ReplaceAllModificationsWithThisOne
        (fun m ->
          let error =
            if not (validateName newSecretName)
            then
              Some
                "Secret name can only contain uppercase alphanumeric characters and underscores"
            else if isNameAlreadyUsed m.insertSecretModal newSecretName
            then Some (newSecretName ^ " is already defined as a secret")
            else None
          in
          let isNameValid = error = None in
          let insertSecretModal =
            {m.insertSecretModal with newSecretName; isNameValid; error}
          in
          ({m with insertSecretModal}, Cmd.none))
  | OnUpdateValue newSecretValue ->
      Types.ReplaceAllModificationsWithThisOne
        (fun m ->
          let isValueValid = validateValue newSecretValue in
          let insertSecretModal =
            {m.insertSecretModal with newSecretValue; isValueValid}
          in
          ({m with insertSecretModal}, Cmd.none))
  | SaveNewSecret ->
      Types.ReplaceAllModificationsWithThisOne
        (fun m ->
          let isValueValid = validateValue m.insertSecretModal.newSecretValue in
          let isNameValid = validateName m.insertSecretModal.newSecretName in
          let isNameUnique =
            not
              (isNameAlreadyUsed
                 m.insertSecretModal
                 m.insertSecretModal.newSecretName)
          in
          let insertSecretModal, cmd =
            if isValueValid && isNameValid && isNameUnique
            then
              ( SecretTypes.defaultInsertModal
              , API.insertSecret
                  m
                  { secretName = m.insertSecretModal.newSecretName
                  ; secretValue = m.insertSecretModal.newSecretValue } )
            else
              let error =
                Some
                  "Both secret name and secret values must be filled. And secret name must be unique within this canvas."
              in
              ( {m.insertSecretModal with isValueValid; isNameValid; error}
              , Cmd.none )
          in
          ({m with insertSecretModal}, cmd))


let onKeydown (evt : Web.Node.event) : Types.msg option =
  match FluidKeyboard.eventToKeyEvent evt with
  | Some {FluidKeyboard.key = FluidKeyboard.Enter; _} ->
      evt##stopPropagation () ;
      (* prevents omnibox from opening *)
      Some (Types.SecretMsg SaveNewSecret)
  | _ ->
      None


let view (m : insertModal) : Types.msg Html.html =
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
        let title =
          Html.div [Html.class' "title"] [Html.text "Add Secret Key"]
        in
        let form =
          Html.form
            [Html.class' "insert-secret-form"]
            [ Html.input'
                [ Attr.placeholder "secret name"
                ; Attr.name "secret-name"
                ; Attr.value m.newSecretName
                ; Html.classList
                    [("modal-form-input", true); ("error", not m.isNameValid)]
                ; Events.onInput (fun str ->
                      Types.SecretMsg (OnUpdateName (Tc.String.toUpper str))) ]
                []
            ; Html.input'
                [ Attr.placeholder "secret value"
                ; Attr.name "secret-value"
                ; Attr.value m.newSecretValue
                ; Html.classList
                    [("modal-form-input", true); ("error", not m.isValueValid)]
                ; Events.onInput (fun str ->
                      Types.SecretMsg (OnUpdateValue str)) ]
                []
            ; Html.button
                [ Html.type' "button"
                ; Html.class' "modal-form-button"
                ; ViewUtils.eventNoPropagation
                    ~key:"save-secret"
                    "click"
                    (fun _ -> Types.SecretMsg SaveNewSecret) ]
                [Html.text "Save"] ]
        in
        let errorMsg =
          let msg = match m.error with Some msg -> msg | None -> "" in
          Html.p [Html.class' "form-error"] [Html.text msg]
        in
        Html.div [Html.class' "modal-content"] [title; form; errorMsg]
      in
      [content; closeBtn]
    in
    Html.div
      [ Html.class' "modal-overlay"
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; ViewUtils.nothingMouseEvent "click"
      ; Html.onCB "keydown" "keydown" onKeydown ]
      [Html.div [Html.class' "modal insert-secret"] inside]
  else Vdom.noNode
