open Tc

module Utils = SettingsUtils

// --------------------------------
/// Model
// --------------------------------
@ppx.deriving(show)
type rec inviter = {name: string, username: string}

@ppx.deriving(show)
type rec t = {email: Utils.formField, loading: bool, inviter: inviter}

module Params = {
  @ppx.deriving(show)
  type rec t = {
    email: string,
    inviterUsername: string,
    inviterName: string,
  }
  let encode = (params: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("email", string(params.email)),
      ("inviterUsername", string(params.inviterUsername)),
      ("inviterName", string(params.inviterName)),
    })
  }
}

@ppx.deriving(show)
type rec msg =
  | Update(string)
  | TriggerSendCallback(result<unit, Tea.Http.error<string>>)
  | Submit

@ppx.deriving(show)
type rec intent =
  | SendAPICall(Params.t)
  | HandleAPIError(APIError.t)
  | UpdateToast(string)

let title = "Share"

let default = {
  email: Utils.defaultFormField,
  loading: false,
  inviter: {
    username: "",
    name: "",
  },
}

// --------------------------------
/// Update
// --------------------------------

let setInviter = (state: t, username: string, name: string): t => {
  ...state,
  inviter: {username: username, name: name},
}

@module("validator") external validator: string => bool = "isEmail"

let validateEmail = (email: Utils.formField): Utils.formField => {
  let error = {
    let emailVal = email.value
    if String.length(emailVal) == 0 {
      Some("Field Required")
    } else if !validator(emailVal) {
      Some("Invalid Email")
    } else {
      None
    }
  }

  {...email, error: error}
}

let validateForm = (i: t): (bool, t) => {
  let email = validateEmail(i.email)
  let isInvalid = Option.isSome(email.error)
  (isInvalid, {...i, email: email})
}

let submitForm = (i: t): (t, intent) => {
  let sendInviteMsg = {
    Params.email: i.email.value,
    inviterUsername: i.inviter.username,
    inviterName: i.inviter.name,
  }

  ({...i, loading: true}, SendAPICall(sendInviteMsg))
}

let update = (i: t, msg: msg): (t, option<intent>) =>
  switch msg {
  | Update(value) => ({...i, email: {value: value, error: None}}, None)

  | TriggerSendCallback(Ok(_)) => ({...default, loading: false}, Some(UpdateToast("Sent!")))

  | TriggerSendCallback(Error(err)) => (
      {
        ...default,
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
