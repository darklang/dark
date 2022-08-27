open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events
module Cmd = Tea.Cmd

module ST = SecretTypes
module Mod = AppTypes.Modification
module Msg = AppTypes.Msg
type modification = AppTypes.modification
type msg = AppTypes.msg

let nameValidator = "[A-Z0-9_]+"

let validateName = (s: string): bool => Regex.exactly(~re=nameValidator, s)

let validateValue = (s: string): bool => s != ""

let isNameAlreadyUsed = (m: ST.insertModal, value: string): bool => List.member(~value, m.usedNames)

let secretNameInputID = "new-secret-name-input"

let update = (msg: ST.msg): modification =>
  switch msg {
  | OpenCreateModal =>
    Mod.ReplaceAllModificationsWithThisOne(
      m => {
        let usedNames = List.map(~f=s => s.secretName, m.secrets)
        let insertSecretModal = {...m.insertSecretModal, visible: true, usedNames: usedNames}

        (
          {...m, insertSecretModal: insertSecretModal, cursorState: Deselected},
          Tea_html_cmds.focus(secretNameInputID),
        )
      },
    )
  | CloseCreateModal =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let insertSecretModal = SecretTypes.defaultInsertModal
        ({...m, insertSecretModal: insertSecretModal}, Cmd.none)
      },
    )
  | OnUpdateName(newSecretName) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let error = if !validateName(newSecretName) {
          Some("Secret name can only contain uppercase alphanumeric characters and underscores")
        } else if isNameAlreadyUsed(m.insertSecretModal, newSecretName) {
          Some(newSecretName ++ " is already defined as a secret")
        } else {
          None
        }

        let isNameValid = error == None
        let insertSecretModal = {
          ...m.insertSecretModal,
          newSecretName: newSecretName,
          isNameValid: isNameValid,
          error: error,
        }

        ({...m, insertSecretModal: insertSecretModal}, Cmd.none)
      },
    )
  | OnUpdateValue(newSecretValue) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let isValueValid = validateValue(newSecretValue)
        let insertSecretModal = {
          ...m.insertSecretModal,
          newSecretValue: newSecretValue,
          isValueValid: isValueValid,
        }

        ({...m, insertSecretModal: insertSecretModal}, Cmd.none)
      },
    )
  | SaveNewSecret =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let isValueValid = validateValue(m.insertSecretModal.newSecretValue)
        let isNameValid = validateName(m.insertSecretModal.newSecretName)
        let isNameUnique = !isNameAlreadyUsed(
          m.insertSecretModal,
          m.insertSecretModal.newSecretName,
        )

        let (insertSecretModal, cmd) = if isValueValid && (isNameValid && isNameUnique) {
          (
            SecretTypes.defaultInsertModal,
            API.insertSecret(
              m,
              {
                secretName: m.insertSecretModal.newSecretName,
                secretValue: m.insertSecretModal.newSecretValue,
              },
            ),
          )
        } else {
          let error = Some(
            "Both secret name and secret values must be filled, and secret name must be unique within this canvas.",
          )

          (
            {
              ...m.insertSecretModal,
              isValueValid: isValueValid,
              isNameValid: isNameValid,
              error: error,
            },
            Cmd.none,
          )
        }

        ({...m, insertSecretModal: insertSecretModal}, cmd)
      },
    )
  }

let onKeydown = (evt: Web.Node.event): option<AppTypes.msg> =>
  switch FluidKeyboard.eventToKeyEvent(evt) {
  | Some({FluidKeyboard.key: FluidKeyboard.Enter, _}) =>
    evt["stopPropagation"]()
    evt["preventDefault"]()
    // prevents omnibox from opening
    Some(Msg.SecretMsg(SaveNewSecret))
  | _ => None
  }

let view = (m: ST.insertModal): Html.html<msg> =>
  if m.visible {
    let inside = {
      let closeBtn = Html.div(
        list{
          Attrs.class'("close-btn"),
          ViewUtils.eventNoPropagation(
            ~key="close-create-secret-modal",
            "click",
            _ => Msg.SecretMsg(CloseCreateModal),
          ),
        },
        list{Icons.fontAwesome("times")},
      )

      let content = {
        let title = Html.div(list{Attrs.class'("title")}, list{Html.text("Add Secret Key")})

        let form = Html.form(
          list{Attrs.class'("insert-secret-form")},
          list{
            Html.input'(
              list{
                Attrs.id(secretNameInputID),
                Attrs.autofocus(true),
                Attrs.placeholder("secret name"),
                Attrs.name("secret-name"),
                Attrs.value(m.newSecretName),
                Attrs.classList(list{("modal-form-input", true), ("error", !m.isNameValid)}),
                Events.onInput(str => Msg.SecretMsg(OnUpdateName(Tc.String.toUppercase(str)))),
              },
              list{},
            ),
            Html.textarea(
              list{
                Attrs.placeholder("secret value"),
                Attrs.name("secret-value"),
                Attrs.classList(list{("modal-form-input", true), ("error", !m.isValueValid)}),
                Events.onInput(str => Msg.SecretMsg(OnUpdateValue(str))),
              },
              list{Html.text(m.newSecretValue)},
            ),
            Html.button(
              list{
                Attrs.type'("button"),
                Attrs.class'("modal-form-button"),
                ViewUtils.eventNoPropagation(~key="save-secret", "click", _ => Msg.SecretMsg(
                  SaveNewSecret,
                )),
              },
              list{Html.text("Save")},
            ),
          },
        )

        let errorMsg = {
          let msg = switch m.error {
          | Some(msg) => msg
          | None => ""
          }
          Html.p(list{Attrs.class'("form-error")}, list{Html.text(msg)})
        }

        Html.div(list{Attrs.class'("modal-content")}, list{title, form, errorMsg})
      }

      list{content, closeBtn}
    }

    Html.div(
      list{
        Attrs.class'("modal-overlay"),
        ViewUtils.nothingMouseEvent("mousedown"),
        ViewUtils.nothingMouseEvent("mouseup"),
        ViewUtils.nothingMouseEvent("click"),
        Events.onCB("keydown", "keydown", onKeydown),
      },
      list{Html.div(list{Attrs.class'("modal insert-secret")}, inside)},
    )
  } else {
    Vdom.noNode
  }
