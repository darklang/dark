open Tc

module Utils = SettingsUtils

module T = SettingsInviteState

let update = (i: T.t, msg: T.msg) =>
  switch msg {
  | Update(value) => {...i, T.email: {value: value, error: None}}
  | TriggerSendCallback(Ok(_)) => {
      ...T.default,
      loading: false,
    }
  | TriggerSendCallback(Error(_)) => {
      ...T.default,
      loading: false,
    }
  | Submit => i
  }

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
