open Prelude
module Attr = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module Html = Tea_html_extended
module Cmd = Tea.Cmd
module ST = SecretTypes

let fontAwesome = ViewUtils.fontAwesome

let nameValidator = "[A-Z0-9_]+"

let validateName = (s: string): bool => Util.Regex.exactly(~re=nameValidator, s)

let validateValue = (s: string): bool => s != ""

let isNameAlreadyUsed = (m: ST.insertModal, value: string): bool => List.member(~value, m.usedNames)

let secretNameInputID = "new-secret-name-input"

let update = (msg: ST.msg): Types.modification =>
  switch msg {
  | OpenCreateModal =>
    Types.ReplaceAllModificationsWithThisOne(
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
    Types.ReplaceAllModificationsWithThisOne(
      m => {
        let insertSecretModal = SecretTypes.defaultInsertModal
        ({...m, insertSecretModal: insertSecretModal}, Cmd.none)
      },
    )
  | OnUpdateName(newSecretName) =>
    Types.ReplaceAllModificationsWithThisOne(
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
    Types.ReplaceAllModificationsWithThisOne(
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
    Types.ReplaceAllModificationsWithThisOne(
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

let onKeydown = (evt: Web.Node.event): option<Types.msg> =>
  switch FluidKeyboard.eventToKeyEvent(evt) {
  | Some({FluidKeyboard.key: FluidKeyboard.Enter, _}) =>
    evt["stopPropagation"]()
    evt["preventDefault"]()
    // prevents omnibox from opening
    Some(Types.SecretMsg(SaveNewSecret))
  | _ => None
  }

let view = (m: ST.insertModal): Html.html<Types.msg> =>
  if m.visible {
    let inside = {
      let closeBtn = Html.div(
        list{
          Html.class'("close-btn"),
          ViewUtils.eventNoPropagation(
            ~key="close-create-secret-modal",
            "click",
            _ => Types.SecretMsg(CloseCreateModal),
          ),
        },
        list{fontAwesome("times")},
      )

      let content = {
        let title = Html.div(list{Html.class'("title")}, list{Html.text("Add Secret Key")})

        let form = Html.form(
          list{Html.class'("insert-secret-form")},
          list{
            Html.input'(
              list{
                Html.id(secretNameInputID),
                Html.autofocus(true),
                Attr.placeholder("secret name"),
                Attr.name("secret-name"),
                Attr.value(m.newSecretName),
                Html.classList(list{("modal-form-input", true), ("error", !m.isNameValid)}),
                Events.onInput(str => Types.SecretMsg(OnUpdateName(Tc.String.toUppercase(str)))),
              },
              list{},
            ),
            Html.textarea(
              list{
                Attr.placeholder("secret value"),
                Attr.name("secret-value"),
                Html.classList(list{("modal-form-input", true), ("error", !m.isValueValid)}),
                Events.onInput(str => Types.SecretMsg(OnUpdateValue(str))),
              },
              list{Html.text(m.newSecretValue)},
            ),
            Html.button(
              list{
                Html.type'("button"),
                Html.class'("modal-form-button"),
                ViewUtils.eventNoPropagation(~key="save-secret", "click", _ => Types.SecretMsg(
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
          Html.p(list{Html.class'("form-error")}, list{Html.text(msg)})
        }

        Html.div(list{Html.class'("modal-content")}, list{title, form, errorMsg})
      }

      list{content, closeBtn}
    }

    Html.div(
      list{
        Html.class'("modal-overlay"),
        ViewUtils.nothingMouseEvent("mousedown"),
        ViewUtils.nothingMouseEvent("mouseup"),
        ViewUtils.nothingMouseEvent("click"),
        Html.onCB("keydown", "keydown", onKeydown),
      },
      list{Html.div(list{Html.class'("modal insert-secret")}, inside)},
    )
  } else {
    Vdom.noNode
  }
