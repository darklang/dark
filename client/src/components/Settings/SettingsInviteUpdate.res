open Tc

module Utils = SettingsUtils

module T = SettingsInviteState

let validateEmail = (email: Utils.formField): Utils.formField => {
  let error = {
    let emailVal = email.value
    if String.length(emailVal) == 0 {
      Some("Field Required")
    } else if !Entry.validateEmail(emailVal) {
      Some("Invalid Email")
    } else {
      None
    }
  }

  {...email, error: error}
}

let validateForm = (i: T.t): (bool, T.t) => {
  let email = validateEmail(i.email)
  let isInvalid = Option.is_some(email.error)
  (isInvalid, {...i, email: email})
}

let submitForm = (i: T.t): (T.t, T.effect) => {
  let sendInviteMsg = {
    T.Params.email: i.email.value,
    inviterUsername: i.inviter.username,
    inviterName: i.inviter.name,
  }

  ({...i, loading: true}, SendAPICall(sendInviteMsg))
}

let update = (i: T.t, msg: T.msg): (T.t, option<T.effect>) =>
  switch msg {
  | Update(value) => ({...i, T.email: {value: value, error: None}}, None)

  | TriggerSendCallback(Ok(_)) => ({...T.default, loading: false}, Some(UpdateToast("Sent!")))

  | TriggerSendCallback(Error(err)) => (
      {
        ...T.default,
        loading: false,
      },
      Some(
        HandleAPIError(
          APIError.make(
            ~context="TriggerSendInviteCallback",
            ~importance=ImportantError,
            ~reload=false,
            err,
          ),
        ),
      ),
    )

  | Submit =>
    let (isInvalid, newForm) = validateForm(i)
    if isInvalid {
      (newForm, None)
    } else {
      let (newState, effect) = submitForm(i)
      (newState, Some(effect))
    }
  }
