// open Tc

module Utils = SettingsUtils

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
  | @printer(Types.opaque("TriggerSendCallback"))
  TriggerSendCallback(Tea_result.t<unit, Tea.Http.error<string>>)
  | Submit

type rec effect =
  | SendAPICall(Params.t)
  | UpdateToast(option<string>)

let title = "Share"

let default = {
  email: Utils.defaultFormField,
  loading: false,
  inviter: {
    username: "",
    name: "",
  },
}

let setInviter = (state: t, username: string, name: string): t => {
  ...state,
  inviter: {username: username, name: name},
}
